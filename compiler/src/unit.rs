use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt;

use redscript::ast::{Expr, Ident, Seq, SourceAst, Span, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr};
use redscript::definition::*;
use redscript::mapper::{MultiMapper, PoolMapper};
use redscript::Ref;

use crate::assembler::Assembler;
use crate::cte;
use crate::diagnostics::return_val::ReturnValueCheck;
use crate::diagnostics::unused::UnusedCheck;
use crate::diagnostics::{DiagnosticPass, FunctionMetadata};
use crate::error::{Cause, Error, ResultSpan};
use crate::parser::*;
use crate::scope::{Reference, Scope, TypeId, Value};
use crate::source_map::Files;
use crate::sugar::Desugar;
use crate::symbol::{FunctionSignature, Import, ModulePath, Symbol, SymbolMap};
use crate::transform::ExprTransformer;
use crate::typechecker::{collect_supertypes, Callable, TypeChecker, TypedAst};

type ProxyMap = HashMap<PoolIndex<Function>, PoolIndex<Function>>;

pub struct CompilationUnit<'a> {
    pool: &'a mut ConstantPool,
    symbols: SymbolMap,
    scope: Scope,
    function_bodies: Vec<FunctionBody>,
    wrappers: ProxyMap,
    proxies: ProxyMap,
    diagnostics: Vec<Diagnostic>,
    diagnostic_passes: Vec<Box<dyn DiagnosticPass>>,
}

impl<'a> CompilationUnit<'a> {
    pub fn new_with_defaults(pool: &'a mut ConstantPool) -> Result<Self, Error> {
        let passes: Vec<Box<dyn DiagnosticPass>> = vec![Box::new(UnusedCheck), Box::new(ReturnValueCheck)];
        Self::new(pool, passes)
    }

    pub fn new(pool: &'a mut ConstantPool, passes: Vec<Box<dyn DiagnosticPass>>) -> Result<Self, Error> {
        let symbols = SymbolMap::new(pool)?;
        let mut scope = Scope::new(pool)?;

        symbols.populate_import(
            Import::All(vec![], ModulePath::EMPTY, Span::ZERO),
            &mut scope,
            Visibility::Private,
        )?;

        Ok(CompilationUnit {
            pool,
            symbols,
            scope,
            function_bodies: Vec::new(),
            wrappers: HashMap::new(),
            proxies: HashMap::new(),
            diagnostics: vec![],
            diagnostic_passes: passes,
        })
    }

    pub fn compile(mut self, modules: Vec<SourceModule>) -> Result<Vec<Diagnostic>, Error> {
        let funcs = self.compile_modules(modules, true, false)?;
        self.finish(funcs)
    }

    pub fn compile_files(self, files: &Files) -> Result<Vec<Diagnostic>, Error> {
        self.compile(Self::parse(files)?)
    }

    pub fn typecheck(
        mut self,
        modules: Vec<SourceModule>,
        desugar: bool,
        permissive: bool,
    ) -> Result<(Vec<CompiledFunction>, Vec<Diagnostic>), Error> {
        let funcs = self.compile_modules(modules, desugar, permissive)?;
        Ok((funcs, self.diagnostics))
    }

    pub fn typecheck_files(
        self,
        files: &Files,
        desugar: bool,
        permissive: bool,
    ) -> Result<(Vec<CompiledFunction>, Vec<Diagnostic>), Error> {
        self.typecheck(Self::parse(files)?, desugar, permissive)
    }

    pub fn compile_and_report(self, files: &Files) -> Result<(), Error> {
        log::info!("Compiling files: {}", files);

        match self.compile_files(files) {
            Ok(diagnostics) => {
                let is_fatal = diagnostics.iter().any(Diagnostic::is_fatal);
                for diagnostic in &diagnostics {
                    Self::print_diagnostic(files, diagnostic);
                }

                if is_fatal {
                    Err(Error::MultipleErrors(
                        diagnostics
                            .iter()
                            .filter(|d| d.is_fatal())
                            .map(Diagnostic::span)
                            .collect(),
                    ))
                } else {
                    log::info!("Compilation complete");
                    Ok(())
                }
            }
            Err(err) => match Diagnostic::from_error(err) {
                Ok(diagnostic) => {
                    Self::print_diagnostic(files, &diagnostic);

                    if diagnostic.is_fatal() {
                        Err(Error::MultipleErrors(vec![diagnostic.span()]))
                    } else {
                        Ok(())
                    }
                }
                Err(other) => {
                    log::error!("{}: {}", "Unexpected error during compilation", other);
                    Err(other)
                }
            },
        }
    }

    fn print_diagnostic(files: &Files, diagnostic: &Diagnostic) {
        match diagnostic {
            Diagnostic::MethodConflict(_, pos) => {
                let loc = files.lookup(*pos).unwrap();
                Self::print_message(
                    format_args!("At {loc}:\n Conflicting method replacement"),
                    diagnostic.is_fatal(),
                );
            }
            Diagnostic::Deprecation(msg, pos) => {
                let loc = files.lookup(*pos).unwrap();
                Self::print_message(format_args!("At {loc}: {msg}"), diagnostic.is_fatal());
            }
            Diagnostic::UnusedLocal(pos) => {
                let loc = files.lookup(*pos).unwrap();
                Self::print_message(format_args!("At {loc}: Unused variable"), diagnostic.is_fatal());
            }
            Diagnostic::MissingReturn(pos) => {
                let loc = files.lookup(*pos).unwrap();
                Self::print_message(
                    format_args!("At {loc}: Function might not return a value"),
                    diagnostic.is_fatal(),
                );
            }
            Diagnostic::CompileError(err, pos) => {
                let loc = files.lookup(*pos).expect("Unknown file");
                let line = loc.enclosing_line().trim_end().replace('\t', " ");
                let padding = " ".repeat(loc.start.col);
                let underscore_len = if loc.start.line == loc.end.line {
                    (loc.end.col - loc.start.col).max(1)
                } else {
                    3
                };
                let underscore = "^".repeat(underscore_len);

                Self::print_message(
                    format_args!("At {}:\n {}\n {}{}\n {}", loc, line, padding, underscore, err),
                    diagnostic.is_fatal(),
                );
            }
        }
    }

    fn print_message(args: fmt::Arguments, fatal: bool) {
        if fatal {
            log::error!("{}", args);
        } else {
            log::warn!("{}", args);
        }
    }

    fn parse(files: &Files) -> Result<Vec<SourceModule>, Error> {
        let mut modules = vec![];
        for file in files.files() {
            let parsed = parse_file(file).map_err(|err| {
                let message = format!("Syntax error, expected {}", err.expected);
                let pos = file.byte_offset() + err.location.offset;
                Error::SyntaxError(message, Span::new(pos, pos))
            })?;
            modules.push(parsed);
        }
        Ok(modules)
    }

    fn compile_modules(
        &mut self,
        modules: Vec<SourceModule>,
        desugar: bool,
        permissive: bool,
    ) -> Result<Vec<CompiledFunction>, Error> {
        let mut seen_funcs = HashSet::new();
        let mut queue = Vec::with_capacity(modules.len());
        let mut compiled_funcs = Vec::new();

        let cte = cte::Context::new(modules.iter().filter_map(|m| m.path.clone()).collect());

        for module in modules {
            let path = module.path.unwrap_or(ModulePath::EMPTY);
            let mut slots = Vec::with_capacity(module.entries.len());

            for entry in module.entries {
                if eval_conditions(&cte, entry.annotations())? {
                    match self.define_symbol(entry, &path, permissive) {
                        Ok(slot) => slots.push(slot),
                        Err(err) => self.report(err)?,
                    };
                }
            }
            queue.push((path, module.imports, slots));
        }

        for (path, imports, slots) in queue {
            let mut module_scope = self.scope.clone();

            if !path.is_empty() {
                self.symbols
                    .populate_import(
                        Import::All(vec![], path, Span::ZERO),
                        &mut module_scope,
                        Visibility::Private,
                    )
                    .ok();
            };

            for import in imports {
                if eval_conditions(&cte, import.annotations())? {
                    self.symbols
                        .populate_import(import, &mut module_scope, Visibility::Public)?;
                }
            }

            for slot in slots {
                let res = match slot {
                    Slot::Function {
                        index,
                        parent,
                        base,
                        source,
                        visibility,
                        wrapped,
                        is_replacement,
                    } => {
                        let pos = source.declaration.span;
                        if seen_funcs.contains(&index) {
                            self.diagnostics.push(Diagnostic::MethodConflict(index, pos));
                        } else {
                            seen_funcs.insert(index);
                        }
                        let flags = if !parent.is_undefined() {
                            Some(self.pool.class(parent)?.flags)
                        } else {
                            None
                        };
                        self.define_function(
                            index,
                            parent,
                            flags,
                            base,
                            wrapped,
                            is_replacement,
                            visibility,
                            source,
                            &mut module_scope,
                        )
                    }
                    Slot::Class {
                        index,
                        source,
                        visibility,
                    } => self.define_class(index, visibility, source, false, &mut module_scope),
                    Slot::Struct {
                        index,
                        source,
                        visibility,
                    } => self.define_class(index, visibility, source, true, &mut module_scope),
                    Slot::Field {
                        index,
                        source,
                        visibility,
                    } => self.define_global_let(index, visibility, source, &mut module_scope),
                    Slot::Enum { index, source } => self.define_enum(index, source),
                };
                if let Err(err) = res {
                    self.report(err)?;
                }
            }
        }

        // create function proxies
        for (wrapped, wrapper) in self.wrappers.drain() {
            let proxy = self.proxies.get(&wrapped).unwrap();
            Self::construct_proxy(*proxy, wrapped, wrapper, &mut self.scope, self.pool)?;
        }

        // compile function bodies
        for item in self.function_bodies.drain(..) {
            let was_callback = item.was_callback;
            match Self::compile_function(item, self.pool, desugar, permissive) {
                Ok((func, diagnostics)) => {
                    self.diagnostics.extend(diagnostics);

                    let flags = self.pool.function(func.index)?.flags;
                    let metadata = FunctionMetadata::new(flags, was_callback, func.span);
                    for pass in &self.diagnostic_passes {
                        self.diagnostics.extend(pass.diagnose(&func.code, &metadata));
                    }

                    compiled_funcs.push(func);
                }
                Err(err) => self.diagnostics.push(Diagnostic::from_error(err)?),
            }
        }

        Ok(compiled_funcs)
    }

    fn finish(self, functions: Vec<CompiledFunction>) -> Result<Vec<Diagnostic>, Error> {
        for mut func in functions {
            let code = Assembler::from_body(func.code, &mut func.scope, self.pool)?;
            let function = self.pool.function_mut(func.index)?;
            function.code = code;
            function.locals = func.locals;
        }

        // swap proxies with the functions they wrap
        for (wrapped, proxy) in self.proxies {
            let wrapped_name = self.pool.definition(wrapped)?.name;
            let proxy_name = self.pool.definition(proxy)?.name;

            // wrapped functions should not preserve the callback flag (redscript #63)
            let func = self.pool.function_mut(wrapped)?;
            func.flags = func.flags.with_is_callback(false);

            Self::remap_locals(proxy, wrapped, self.pool)?;
            self.pool.rename(wrapped, proxy_name);
            self.pool.rename(proxy, wrapped_name);
            self.pool.swap_definition(wrapped, proxy);
        }

        Self::cleanup_pool(self.pool);
        Ok(self.diagnostics)
    }

    fn define_symbol(&mut self, entry: SourceEntry, module: &ModulePath, permissive: bool) -> Result<Slot, Error> {
        match entry {
            SourceEntry::Class(source) => {
                let path = module.with_child(source.name.clone());
                let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);

                if let Ok(Symbol::Class(_, _) | Symbol::Struct(_, _)) =
                    self.symbols.get_symbol(&path).with_span(source.span)
                {
                    if !permissive {
                        return Err(Cause::symbol_redefinition().with_span(source.span));
                    }
                }

                let name_index = self.pool.names.add(path.render().to_owned());
                let index = self.pool.stub_definition(name_index);
                self.symbols.add_class(&path, index, visibility);

                // add to globals when no module
                if module.is_empty() {
                    self.scope
                        .add_symbol(source.name.clone(), Symbol::Class(index, visibility));
                }

                let slot = Slot::Class {
                    index,
                    source,
                    visibility,
                };
                Ok(slot)
            }
            SourceEntry::Struct(source) => {
                let path = module.with_child(source.name.clone());
                let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);

                if let Ok(Symbol::Class(_, _) | Symbol::Struct(_, _)) =
                    self.symbols.get_symbol(&path).with_span(source.span)
                {
                    if !permissive {
                        return Err(Cause::symbol_redefinition().with_span(source.span));
                    }
                }

                let name_index = self.pool.names.add(path.render().to_owned());
                let index = self.pool.stub_definition(name_index);
                self.symbols.add_struct(&path, index, visibility);

                // add to globals when no module
                if module.is_empty() {
                    self.scope
                        .add_symbol(source.name.clone(), Symbol::Struct(index, visibility));
                }

                let slot = Slot::Struct {
                    index,
                    source,
                    visibility,
                };
                Ok(slot)
            }
            SourceEntry::Function(fun) => {
                let slot = self.determine_function_location(fun, module)?;
                Ok(slot)
            }
            SourceEntry::GlobalLet(source) => {
                let name_index = self.pool.names.add(source.declaration.name.to_owned());
                let index = self.pool.stub_definition(name_index);
                let visibility = source
                    .declaration
                    .qualifiers
                    .visibility()
                    .unwrap_or(Visibility::Private);

                let slot = Slot::Field {
                    index,
                    source,
                    visibility,
                };
                Ok(slot)
            }
            SourceEntry::Enum(source) => {
                let path = module.with_child(source.name.clone());
                let name_index = self.pool.names.add(path.render().to_owned());
                let index = self.pool.stub_definition(name_index);

                self.symbols.add_enum(&path, index);

                // add to globals when no module
                if module.is_empty() {
                    self.scope.add_symbol(source.name.clone(), Symbol::Enum(index));
                }

                let slot = Slot::Enum { index, source };
                Ok(slot)
            }
        }
    }

    fn define_class(
        &mut self,
        class_idx: PoolIndex<Class>,
        visibility: Visibility,
        source: ClassSource,
        is_struct: bool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let is_import_only = source.qualifiers.contain(Qualifier::ImportOnly);
        let is_class_native = is_import_only || source.qualifiers.contain(Qualifier::Native);
        let is_class_abstract = !is_struct && source.qualifiers.contain(Qualifier::Abstract);
        let is_class_final = source.qualifiers.contain(Qualifier::Final);

        let flags = ClassFlags::new()
            .with_is_abstract(is_class_abstract)
            .with_is_final(is_class_final)
            .with_is_native(is_class_native)
            .with_is_import_only(is_import_only)
            .with_is_struct(is_struct);
        let mut functions = vec![];
        let mut fields = vec![];

        for member in source.members {
            match member {
                MemberSource::Function(fun) => {
                    if is_struct && !fun.declaration.qualifiers.contain(Qualifier::Static) {
                        let err =
                            Cause::unsupported("Defining non-static struct methods").with_span(fun.declaration.span);
                        self.report(err)?;
                    }

                    let fun_sig = FunctionSignature::from_source(&fun);
                    let name_idx = self.pool.names.add(Ref::new(fun_sig.into_owned()));
                    let fun_idx = self.pool.stub_definition(name_idx);

                    self.define_function(
                        fun_idx,
                        class_idx,
                        Some(flags),
                        None,
                        None,
                        false,
                        visibility,
                        fun,
                        scope,
                    )?;
                    functions.push(fun_idx);
                }
                MemberSource::Field(let_) => {
                    let name_idx = self.pool.names.add(let_.declaration.name.to_owned());
                    let field_idx = self.pool.stub_definition(name_idx);

                    self.define_field(field_idx, class_idx, flags, visibility, let_, scope)?;
                    fields.push(field_idx);
                }
            }
        }

        let base_idx = if is_struct {
            PoolIndex::UNDEFINED
        } else if let Some(base_name) = source.base {
            if let Symbol::Class(base_idx, _) = scope.resolve_symbol(base_name.clone()).with_span(source.span)? {
                base_idx
            } else {
                return Err(Cause::class_not_found(base_name).with_span(source.span));
            }
        } else if let Ok(Symbol::Class(class, _)) = scope
            .resolve_symbol(Ident::Static("IScriptable"))
            .with_span(source.span)
        {
            class
        } else {
            log::warn!("No IScriptable in scope, defaulting to no implicit base class");
            PoolIndex::UNDEFINED
        };

        let class = Class {
            visibility,
            flags,
            base: base_idx,
            functions,
            fields,
            overrides: vec![],
        };
        let name_idx = self.pool.definition(class_idx)?.name;

        self.pool.put_definition(class_idx, Definition::class(name_idx, class));
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn define_function(
        &mut self,
        fun_idx: PoolIndex<Function>,
        class_idx: PoolIndex<Class>,
        class_flags: Option<ClassFlags>,
        base_method: Option<PoolIndex<Function>>,
        wrapped: Option<PoolIndex<Function>>,
        is_replacement: bool,
        visibility: Visibility,
        source: FunctionSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = &source.declaration;
        let is_native = !is_replacement && decl.qualifiers.contain(Qualifier::Native);
        let is_static = decl.qualifiers.contain(Qualifier::Static) || class_idx.is_undefined();
        let is_callback = decl.qualifiers.contain(Qualifier::Callback);

        if is_native && class_flags.map(|f| !f.is_native()).unwrap_or(false) {
            self.report(Cause::unexpected_native().with_span(source.declaration.span))?;
        }
        if !is_native && class_flags.map(|f| !f.is_abstract()).unwrap_or(true) && source.body.is_none() {
            self.report(Cause::expected_body().with_span(source.declaration.span))?;
        }
        if is_native && source.body.is_some() {
            self.report(Cause::native_with_body().with_span(source.declaration.span))?;
        }

        let return_type = match source.type_ {
            None => None,
            Some(type_) if type_.name.as_ref() == "Void" => None,
            Some(type_) => {
                let type_ = self.try_resolve_type(&type_, scope, decl.span)?;
                Some(scope.get_type_index(&type_, self.pool).with_span(decl.span)?)
            }
        };

        let mut parameters = Vec::new();

        for param in &source.parameters {
            let type_ = self.try_resolve_type(&param.type_, scope, decl.span)?;
            let type_idx = scope.get_type_index(&type_, self.pool).with_span(decl.span)?;
            let flags = ParameterFlags::new()
                .with_is_optional(param.qualifiers.contain(Qualifier::Optional))
                .with_is_out(param.qualifiers.contain(Qualifier::Out))
                .with_is_const(param.qualifiers.contain(Qualifier::Const));
            let name = self.pool.names.add(param.name.to_owned());
            let param = Parameter { type_: type_idx, flags };
            let idx = self.pool.add_definition(Definition::param(name, fun_idx.cast(), param));
            parameters.push(idx);
        }

        let source_ref = SourceReference {
            file: PoolIndex::DEFAULT_SOURCE,
            line: 0,
        };

        let flags = FunctionFlags::new()
            .with_is_static(is_static)
            .with_is_native(is_native)
            .with_is_cast(decl.name.as_ref() == "Cast")
            .with_is_exec(decl.qualifiers.contain(Qualifier::Exec))
            .with_is_final(decl.qualifiers.contain(Qualifier::Final))
            .with_is_callback(is_callback && wrapped.is_none())
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_quest(decl.qualifiers.contain(Qualifier::Quest))
            .with_has_return_value(return_type.is_some())
            .with_has_parameters(!parameters.is_empty());

        let function = Function {
            visibility,
            flags,
            source: Some(source_ref),
            return_type,
            unk1: false,
            base_method: base_method.filter(|_| !flags.is_static()),
            parameters,
            locals: vec![],
            operator: None,
            cast: 0,
            code: Code::EMPTY,
            unk2: vec![],
            unk3: None,
        };
        let name_idx = self.pool.definition(fun_idx)?.name;
        let definition = Definition::function(name_idx, class_idx.cast(), function);

        if let Some(code) = source.body {
            let item = FunctionBody {
                class: class_idx,
                index: fun_idx,
                wrapped,
                code,
                scope: scope.clone(),
                was_callback: is_callback,
                span: source.span,
            };
            self.function_bodies.push(item)
        }

        self.pool.put_definition(fun_idx, definition);
        Ok(())
    }

    fn define_field(
        &mut self,
        field_idx: PoolIndex<Field>,
        class_idx: PoolIndex<Class>,
        class_flags: ClassFlags,
        visibility: Visibility,
        source: FieldSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = source.declaration;
        let is_native = decl.qualifiers.contain(Qualifier::Native);
        let is_persistent = decl.qualifiers.contain(Qualifier::Persistent);

        if is_native && !class_flags.is_native() {
            self.report(Cause::unexpected_native().with_span(decl.span))?;
        }

        fn can_be_persistent(typ: &TypeName) -> bool {
            const DISALLOWED_TYPES: &[TypeName] = &[TypeName::STRING, TypeName::VARIANT, TypeName::RESOURCE];
            if DISALLOWED_TYPES.contains(typ) {
                false
            } else {
                typ.arguments.iter().all(can_be_persistent)
            }
        }

        if is_persistent && !can_be_persistent(&source.type_) {
            self.report(Cause::unsupported(format_args!("Persistent {}", source.type_)).with_span(decl.span))?;
        }

        let type_ = self.try_resolve_type(&source.type_, scope, decl.span)?;
        let type_idx = scope.get_type_index(&type_, self.pool).with_span(decl.span)?;
        let flags = FieldFlags::new()
            .with_is_browsable(true)
            .with_is_native(is_native)
            .with_is_persistent(is_persistent);
        let field = Field {
            visibility,
            type_: type_idx,
            flags,
            hint: None,
            attributes: vec![],
            defaults: vec![],
        };
        let name_index = self.pool.definition(field_idx)?.name;
        let definition = Definition::field(name_index, class_idx.cast(), field);

        self.pool.put_definition(field_idx, definition);
        Ok(())
    }

    fn define_enum(&mut self, index: PoolIndex<Enum>, source: EnumSource) -> Result<(), Error> {
        let mut members = Vec::with_capacity(source.members.len());

        for member in source.members {
            let name_index = self.pool.names.add(member.name.to_owned());
            let def = Definition::enum_value(name_index, index, member.value);
            members.push(self.pool.add_definition(def));
        }

        let enum_ = Enum {
            flags: 0,
            size: 4,
            members,
            unk1: false,
        };
        let name_index = self.pool.definition(index)?.name;
        self.pool.put_definition(index, Definition::enum_(name_index, enum_));
        Ok(())
    }

    fn define_global_let(
        &mut self,
        index: PoolIndex<Field>,
        visibility: Visibility,
        source: FieldSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = &source.declaration;
        for ann in &source.declaration.annotations {
            if ann.kind == AnnotationKind::AddField {
                let (ident, _) = ann
                    .args
                    .first()
                    .and_then(Expr::as_ident)
                    .ok_or_else(|| Cause::invalid_annotation_args().with_span(ann.span))?;
                if let Symbol::Class(target_class, _) = scope.resolve_symbol(ident.clone()).with_span(ann.span)? {
                    let flags = self.pool.class(target_class)?.flags;
                    self.define_field(index, target_class, flags, visibility, source, scope)?;
                    self.pool.class_mut(target_class)?.fields.push(index);
                    return Ok(());
                } else {
                    return Err(Cause::class_not_found(ident).with_span(ann.span));
                }
            }
        }
        Err(Cause::unsupported("Let binding").with_span(decl.span))
    }

    fn determine_function_location(&mut self, source: FunctionSource, module: &ModulePath) -> Result<Slot, Error> {
        let name = source.declaration.name.clone();
        let sig = FunctionSignature::from_source(&source);
        let visibility = source
            .declaration
            .qualifiers
            .visibility()
            .unwrap_or(Visibility::Private);

        for ann in &source.declaration.annotations {
            match ann.kind {
                AnnotationKind::WrapMethod => {
                    if source.declaration.qualifiers.contain(Qualifier::Native) {
                        return Err(Cause::unsupported("Wrapping natives").with_span(ann.span));
                    }
                    let (class_name, _) = ann
                        .args
                        .first()
                        .and_then(Expr::as_ident)
                        .ok_or_else(|| Cause::invalid_annotation_args().with_span(ann.span))?;

                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone()).with_span(ann.span)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Cause::class_not_found(class_name).with_span(ann.span)),
                    };
                    let fun_idx = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool)
                        .with_span(ann.span)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Cause::function_not_found(name).with_span(ann.span))?;

                    let wrapped_idx = match self.wrappers.get(&fun_idx) {
                        Some(wrapped) => *wrapped,
                        None => {
                            let proxy = self.pool.reserve();
                            self.proxies.insert(fun_idx, proxy);
                            proxy
                        }
                    };

                    let name_idx = self.pool.names.add(Ref::new(format!("wrapper${wrapped_idx}")));
                    let wrapper_idx = self.pool.stub_definition(name_idx);
                    let base = self.pool.function(fun_idx)?.base_method;

                    self.wrappers.insert(fun_idx, wrapper_idx);
                    self.pool.class_mut(target_class_idx)?.functions.push(wrapper_idx);

                    let slot = Slot::Function {
                        index: wrapper_idx,
                        parent: target_class_idx,
                        base,
                        wrapped: Some(wrapped_idx),
                        is_replacement: true,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationKind::ReplaceMethod => {
                    let (class_name, _) = ann
                        .args
                        .first()
                        .and_then(Expr::as_ident)
                        .ok_or_else(|| Cause::invalid_annotation_args().with_span(ann.span))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone()).with_span(ann.span)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Cause::class_not_found(class_name).with_span(ann.span)),
                    };
                    let fun_idx = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool)
                        .with_span(ann.span)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Cause::function_not_found(name).with_span(ann.span))?;
                    let base = self.pool.function(fun_idx)?.base_method;
                    let slot = Slot::Function {
                        index: fun_idx,
                        parent: target_class_idx,
                        base,
                        wrapped: None,
                        is_replacement: true,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationKind::ReplaceGlobal => {
                    let fun_idx = self
                        .scope
                        .resolve_function(name.clone())
                        .with_span(ann.span)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Cause::function_not_found(name).with_span(ann.span))?;

                    let slot = Slot::Function {
                        index: fun_idx,
                        parent: PoolIndex::UNDEFINED,
                        base: None,
                        wrapped: None,
                        is_replacement: true,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationKind::AddMethod => {
                    let (class_name, _) = ann
                        .args
                        .first()
                        .and_then(Expr::as_ident)
                        .ok_or_else(|| Cause::invalid_annotation_args().with_span(ann.span))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone()).with_span(ann.span)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Cause::class_not_found(class_name).with_span(ann.span)),
                    };
                    let class = self.pool.class(target_class_idx)?;
                    let base_method = if class.base != PoolIndex::UNDEFINED {
                        let base = self.pool.class(class.base)?;
                        base.functions
                            .iter()
                            .find(|fun| self.pool.def_name(**fun).unwrap().as_str() == sig.as_ref())
                            .cloned()
                    } else {
                        None
                    };
                    let name_idx = self.pool.names.add(Ref::new(sig.into_owned()));
                    let fun_idx = self.pool.stub_definition(name_idx);
                    self.pool.class_mut(target_class_idx)?.functions.push(fun_idx);

                    let slot = Slot::Function {
                        index: fun_idx,
                        parent: target_class_idx,
                        base: base_method,
                        wrapped: None,
                        is_replacement: false,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationKind::AddField => {}
                AnnotationKind::If => {}
            }
        }

        let name_idx = self.pool.names.add(module.with_function(sig).render().to_owned());
        let fun_idx = self.pool.stub_definition(name_idx);

        let path = module.with_child(name.clone());
        self.symbols.add_function(&path, fun_idx, visibility);

        // add to globals when no module
        if module.is_empty() {
            self.scope
                .add_symbol(name, Symbol::Functions(vec![(fun_idx, visibility)]));
        }

        let slot = Slot::Function {
            index: fun_idx.cast(),
            parent: PoolIndex::UNDEFINED,
            base: None,
            wrapped: None,
            is_replacement: false,
            source,
            visibility,
        };
        Ok(slot)
    }

    fn compile_function(
        item: FunctionBody,
        pool: &mut ConstantPool,
        desugar: bool,
        permissive: bool,
    ) -> Result<(CompiledFunction, Vec<Diagnostic>), Error> {
        let fun = pool.function(item.index)?;

        let mut local_scope = if fun.flags.is_static() {
            item.scope.with_context(None, item.index)
        } else {
            item.scope.with_context(Some(item.class), item.index)
        };

        for param in &fun.parameters {
            let ident = Ident::Owned(pool.def_name(*param)?);
            local_scope.add_parameter(ident, *param);
        }

        if let Some(wrapped) = item.wrapped {
            let wrapped_ident = Ident::Static("wrappedMethod");
            local_scope.add_symbol(wrapped_ident, Symbol::Functions(vec![(wrapped, Visibility::Public)]));
        }

        let mut checker = TypeChecker::new(pool, permissive);
        let checked = checker.check_seq(&item.code, &mut local_scope)?;
        let (diagnostics, mut locals) = checker.finish();

        let ast = if desugar {
            let mut desugar = Desugar::new(&mut local_scope, pool);
            let desugared = desugar.on_seq(checked)?;
            locals.extend(desugar.locals());
            desugared
        } else {
            checked
        };

        let compiled = CompiledFunction {
            index: item.index,
            code: ast,
            locals,
            scope: local_scope,
            span: item.span,
        };
        Ok((compiled, diagnostics))
    }

    fn construct_proxy(
        slot: PoolIndex<Function>,
        wrapped: PoolIndex<Function>,
        wrapper: PoolIndex<Function>,
        scope: &mut Scope,
        pool: &mut ConstantPool,
    ) -> Result<(), Error> {
        let def = pool.definition(wrapped)?.clone();
        let fun = def.value.into_function().expect("Invalid proxy");
        let mut parameters = vec![];
        let mut args = vec![];

        for param_idx in &fun.parameters {
            let param = pool.definition(*param_idx)?.clone();
            let proxy_idx = pool.add_definition(param);
            parameters.push(proxy_idx);
            args.push(Expr::Ident(Reference::Value(Value::Parameter(proxy_idx)), Span::ZERO));
        }

        let call = Expr::Call(Callable::Function(wrapper), vec![], args, Span::ZERO);
        let expr = if fun.return_type.is_some() {
            Expr::Return(Some(Box::new(call)), Span::ZERO)
        } else {
            call
        };
        let code = Assembler::from_body(Seq::new(vec![expr]), scope, pool)?;

        let compiled = Function {
            code,
            locals: vec![],
            parameters,
            ..fun
        };
        let name = pool.names.add(Ref::new(format!("proxy${wrapper}")));
        pool.put_definition(slot, Definition::function(name, def.parent.cast(), compiled));
        if !def.parent.is_undefined() {
            pool.class_mut(def.parent.cast())?.functions.push(slot);
        }
        Ok(())
    }

    fn remap_locals(
        proxy: PoolIndex<Function>,
        target: PoolIndex<Function>,
        pool: &mut ConstantPool,
    ) -> Result<(), Error> {
        // this is a workaround for a game crash which happens when the game loads
        // locals that are not placed adjacent to the parent function in the pool
        let locals = pool.function(target)?.locals.clone();
        let mut mapped_locals = HashMap::new();

        for local_idx in locals {
            let mut local = pool.definition(local_idx)?.clone();
            local.parent = proxy.cast();
            mapped_locals.insert(local_idx, pool.add_definition(local));
        }

        let fun = pool.function_mut(target)?;
        fun.locals = mapped_locals.values().copied().collect();

        for instr in fun.code.0.iter_mut() {
            if let Instr::Local(local) = instr {
                *instr = Instr::Local(*mapped_locals.get(local).unwrap())
            }
        }
        Ok(())
    }

    fn cleanup_pool(pool: &mut ConstantPool) {
        // this is a workaround for a game crash which happens when the game loads
        // a class which has a base class that is placed after the subclass in the pool
        let mut unsorted = BTreeSet::new();

        // find any classes that appear before their base class in the pool
        for (def_idx, def) in pool.definitions() {
            if let AnyDefinition::Class(class) = &def.value {
                let pos: u32 = def_idx.into();
                if pos < class.base.into() {
                    unsorted.insert(def_idx.cast());
                    unsorted.insert(class.base);
                }
            }
        }

        // additional iteration to find classes that extend the ones that need sorting
        for (def_idx, def) in pool.definitions() {
            if let AnyDefinition::Class(class) = &def.value {
                if unsorted.contains(&class.base) {
                    unsorted.insert(def_idx.cast());
                }
            }
        }

        // find out the order based on cardinality
        let mut sorted: Vec<PoolIndex<Class>> = unsorted.iter().copied().collect();
        sorted.sort_by_key(|idx| collect_supertypes(*idx, pool).unwrap().len());

        #[allow(clippy::needless_collect)]
        let definitions: Vec<_> = sorted.iter().map(|k| pool.definition(*k).unwrap().clone()).collect();

        // insert classes based on the determined order
        for (def, target) in definitions.into_iter().zip(&unsorted) {
            pool.put_definition(*target, def);
        }

        // fix any references to the reordered classes
        if !unsorted.is_empty() {
            let mappings = sorted.into_iter().zip(unsorted).collect();
            PoolMapper::default()
                .with_class_mapper(MultiMapper::new(mappings))
                .map(pool);
        }
    }

    fn try_resolve_type(&mut self, name: &TypeName, scope: &Scope, span: Span) -> Result<TypeId, Error> {
        match scope.resolve_type(name, self.pool) {
            Ok(ty) => Ok(ty),
            Err(err) => {
                // report the fatal error and return a dummy type
                self.report(err.with_span(span))?;
                Ok(TypeId::Null)
            }
        }
    }

    fn report(&mut self, err: Error) -> Result<(), Error> {
        self.diagnostics.push(Diagnostic::from_error(err)?);
        Ok(())
    }
}

struct FunctionBody {
    class: PoolIndex<Class>,
    index: PoolIndex<Function>,
    wrapped: Option<PoolIndex<Function>>,
    code: Seq<SourceAst>,
    scope: Scope,
    was_callback: bool,
    span: Span,
}

pub struct CompiledFunction {
    pub index: PoolIndex<Function>,
    pub code: Seq<TypedAst>,
    pub locals: Vec<PoolIndex<Local>>,
    pub scope: Scope,
    pub span: Span,
}

enum Slot {
    Function {
        index: PoolIndex<Function>,
        parent: PoolIndex<Class>,
        base: Option<PoolIndex<Function>>,
        wrapped: Option<PoolIndex<Function>>,
        is_replacement: bool,
        source: FunctionSource,
        visibility: Visibility,
    },
    Class {
        index: PoolIndex<Class>,
        source: ClassSource,
        visibility: Visibility,
    },
    Struct {
        index: PoolIndex<Class>,
        source: ClassSource,
        visibility: Visibility,
    },
    Field {
        index: PoolIndex<Field>,
        source: FieldSource,
        visibility: Visibility,
    },
    Enum {
        index: PoolIndex<Enum>,
        source: EnumSource,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Diagnostic {
    MethodConflict(PoolIndex<Function>, Span),
    Deprecation(String, Span),
    UnusedLocal(Span),
    MissingReturn(Span),
    CompileError(String, Span),
}

impl Diagnostic {
    pub fn from_error(error: Error) -> Result<Diagnostic, Error> {
        match error {
            Error::SyntaxError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            Error::CompileError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            Error::ArgumentError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            Error::ResolutionError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            Error::CteError(msg, pos) => Ok(Diagnostic::CompileError(msg, pos)),
            other => Err(other),
        }
    }

    pub fn is_fatal(&self) -> bool {
        match self {
            Diagnostic::MethodConflict(_, _) => false,
            Diagnostic::Deprecation(_, _) => false,
            Diagnostic::UnusedLocal(_) => false,
            Diagnostic::MissingReturn(_) => false,
            Diagnostic::CompileError(_, _) => true,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Diagnostic::MethodConflict(_, span) => *span,
            Diagnostic::Deprecation(_, span) => *span,
            Diagnostic::UnusedLocal(span) => *span,
            Diagnostic::MissingReturn(span) => *span,
            Diagnostic::CompileError(_, span) => *span,
        }
    }

    pub fn unrelated_type_equals(span: Span) -> Self {
        let msg = "Comparing unrelated types, this is will not be allowed in the future".to_owned();
        Self::Deprecation(msg, span)
    }
}

fn eval_conditions(cte: &cte::Context, anns: &[Annotation]) -> Result<bool, Error> {
    anns.iter()
        .filter(|ann| ann.kind == AnnotationKind::If)
        .filter_map(|ann| ann.args.first())
        .try_fold(true, |acc, expr| {
            let res = cte.eval(expr)?;
            let res = res
                .as_bool()
                .ok_or_else(|| Error::CteError("Invalid CTE value".to_owned(), expr.span()))?;
            Ok(acc && *res)
        })
}
