use std::collections::BTreeSet;
use std::path::PathBuf;

use hashbrown::{HashMap, HashSet};
use redscript::ast::{Constant, Expr, Ident, Literal, Seq, SourceAst, Span, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr};
use redscript::definition::*;
use redscript::mapper::{MultiMapper, PoolMapper};
use redscript::Ref;

use crate::assembler::Assembler;
use crate::cte;
use crate::diagnostics::return_val::ReturnValueCheck;
use crate::diagnostics::unused::UnusedCheck;
use crate::diagnostics::{Diagnostic, DiagnosticPass, FunctionMetadata};
use crate::error::{Cause, Error, ResultSpan};
use crate::parser::*;
use crate::scope::{Reference, Scope, TypeId, Value};
use crate::source_map::{Files, SourceLoc};
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
    field_defaults: Vec<FieldDefault>,
    wrappers: ProxyMap,
    proxies: ProxyMap,
    diagnostics: Vec<Diagnostic>,
    file_map: HashMap<PathBuf, PoolIndex<SourceFile>>,
    diagnostic_passes: Vec<Box<dyn DiagnosticPass + Send>>,
}

impl<'a> CompilationUnit<'a> {
    pub fn new_with_defaults(pool: &'a mut ConstantPool) -> Result<Self, Error> {
        let passes: Vec<Box<dyn DiagnosticPass + Send>> = vec![Box::new(UnusedCheck), Box::new(ReturnValueCheck)];
        Self::new(pool, passes)
    }

    pub fn new(pool: &'a mut ConstantPool, passes: Vec<Box<dyn DiagnosticPass + Send>>) -> Result<Self, Error> {
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
            field_defaults: Vec::new(),
            wrappers: HashMap::new(),
            proxies: HashMap::new(),
            diagnostics: vec![],
            file_map: HashMap::new(),
            diagnostic_passes: passes,
        })
    }

    pub fn compile(mut self, modules: Vec<SourceModule>, files: &Files) -> Result<Vec<Diagnostic>, Error> {
        let funcs = self.compile_modules(modules, files, true, false)?;
        self.finish(funcs, files)
    }

    pub fn compile_files(self, files: &Files) -> Result<Vec<Diagnostic>, Error> {
        self.compile(Self::parse(files)?, files)
    }

    fn typecheck(
        mut self,
        modules: Vec<SourceModule>,
        files: &Files,
        desugar: bool,
        permissive: bool,
    ) -> Result<(Vec<CompiledFunction>, Vec<Diagnostic>), Error> {
        let funcs = self.compile_modules(modules, files, desugar, permissive)?;
        Ok((funcs, self.diagnostics))
    }

    pub fn typecheck_files(
        self,
        files: &Files,
        desugar: bool,
        permissive: bool,
    ) -> Result<(Vec<CompiledFunction>, Vec<Diagnostic>), Error> {
        self.typecheck(Self::parse(files)?, files, desugar, permissive)
    }

    pub fn compile_and_report(self, files: &Files) -> Result<(), Error> {
        match self.compile_files(files) {
            Ok(mut diagnostics) => {
                diagnostics.sort_by_key(Diagnostic::is_fatal);

                for diagnostic in &diagnostics {
                    diagnostic.log(files);
                }

                if diagnostics.iter().any(Diagnostic::is_fatal) {
                    let spans = diagnostics
                        .iter()
                        .filter(|d| d.is_fatal())
                        .map(|d| (d.code(), d.span()))
                        .collect();
                    Err(Error::MultipleErrors(spans))
                } else {
                    Ok(())
                }
            }
            Err(err) => match Diagnostic::from_error(err) {
                Ok(diagnostic) => {
                    diagnostic.log(files);

                    if diagnostic.is_fatal() {
                        Err(Error::MultipleErrors(vec![(diagnostic.code(), diagnostic.span())]))
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

    fn parse(files: &Files) -> Result<Vec<SourceModule>, Error> {
        let mut modules = vec![];
        for file in files.files() {
            let parsed = parse_file(file).map_err(|err| {
                let pos = file.byte_offset() + err.location.offset;
                Error::SyntaxError(err.expected, Span::new(pos, pos))
            })?;
            modules.push(parsed);
        }
        Ok(modules)
    }

    fn compile_modules(
        &mut self,
        modules: Vec<SourceModule>,
        files: &Files,
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
                    if let Err(err) = self
                        .symbols
                        .populate_import(import, &mut module_scope, Visibility::Public)
                    {
                        self.report(err)?;
                    }
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
                        let opt_loc = files.lookup(source.declaration.span);
                        let source_ref = opt_loc.map(|loc| self.define_source_ref(loc)).unwrap_or_default();
                        let spec = FunctionSpec {
                            fun_idx: index,
                            class_idx: parent,
                            class_flags: flags,
                            base_method: base,
                            source_ref,
                            wrapped,
                            is_replacement,
                            visibility,
                            source,
                        };
                        self.define_function(spec, &mut module_scope)
                    }
                    Slot::Class {
                        index,
                        source,
                        visibility,
                    } => self.define_class(index, visibility, source, false, files, &mut module_scope),
                    Slot::Struct {
                        index,
                        source,
                        visibility,
                    } => self.define_class(index, visibility, source, true, files, &mut module_scope),
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

        for default in self.field_defaults.drain(..) {
            let diagnostics = Self::compile_default(default, self.pool)?;
            self.diagnostics.extend(diagnostics);
        }

        // create function proxies
        for (wrapped, wrapper) in self.wrappers.drain() {
            let proxy = self.proxies.get(&wrapped).unwrap();
            Self::construct_proxy(*proxy, wrapped, wrapper, files, &mut self.scope, self.pool)?;
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

    fn finish(self, functions: Vec<CompiledFunction>, files: &Files) -> Result<Vec<Diagnostic>, Error> {
        for mut func in functions {
            let code = Assembler::from_body(func.code, files, &mut func.scope, self.pool)?;
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
                        return Err(Cause::SymbolRedefinition.with_span(source.span));
                    }
                }

                let name_index = self.pool.names.add(path.render().to_heap());
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
                        return Err(Cause::SymbolRedefinition.with_span(source.span));
                    }
                }

                let name_index = self.pool.names.add(path.render().to_heap());
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
                let name_index = self.pool.names.add(source.declaration.name.to_heap());
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
                let name_index = self.pool.names.add(path.render().to_heap());
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
        files: &Files,
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
                        let err = Cause::UnsupportedFeature("defining non-static struct methods")
                            .with_span(fun.declaration.span);
                        self.report(err)?;
                    }

                    let fun_sig = FunctionSignature::from_source(&fun);
                    let name_idx = self.pool.names.add(Ref::from(fun_sig.as_ref()));
                    let fun_idx = self.pool.stub_definition(name_idx);
                    let opt_loc = files.lookup(fun.declaration.span);
                    let source_ref = opt_loc.map(|loc| self.define_source_ref(loc)).unwrap_or_default();

                    let spec = FunctionSpec {
                        fun_idx,
                        class_idx,
                        class_flags: Some(flags),
                        base_method: None,
                        source_ref,
                        wrapped: None,
                        is_replacement: false,
                        visibility,
                        source: fun,
                    };

                    self.define_function(spec, scope)?;
                    functions.push(fun_idx);
                }
                MemberSource::Field(let_) => {
                    let name_idx = self.pool.names.add(let_.declaration.name.to_heap());
                    let field_idx = self.pool.stub_definition(name_idx);

                    self.define_field(field_idx, class_idx, flags, visibility, let_, scope)?;
                    fields.push(field_idx);
                }
            }
        }

        let base_idx = if is_struct {
            PoolIndex::UNDEFINED
        } else if let Some(base_name) = source.base {
            if let Ok(Symbol::Class(base_idx, _)) = scope.resolve_symbol(base_name.clone()) {
                base_idx
            } else {
                self.report(Cause::ClassNotFound(base_name).with_span(source.span))?;
                PoolIndex::UNDEFINED
            }
        } else if let Ok(Symbol::Class(class, _)) = scope
            .resolve_symbol(Ident::from_static("IScriptable"))
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

    fn define_function(&mut self, spec: FunctionSpec, scope: &mut Scope) -> Result<(), Error> {
        let decl = &spec.source.declaration;
        let is_native = !spec.is_replacement && decl.qualifiers.contain(Qualifier::Native);
        let is_static = decl.qualifiers.contain(Qualifier::Static) || spec.class_idx.is_undefined();
        let is_callback = decl.qualifiers.contain(Qualifier::Callback);

        if is_native && spec.class_flags.map_or(false, |f| !f.is_native()) {
            self.report(Cause::UnexpectedNative.with_span(spec.source.declaration.span))?;
        }
        if !is_native && spec.class_flags.map_or(true, |f| !f.is_abstract()) && spec.source.body.is_none() {
            self.report(Cause::MissingBody.with_span(spec.source.declaration.span))?;
        }
        if is_native && spec.source.body.is_some() {
            self.report(Cause::UnexpectedBody.with_span(spec.source.declaration.span))?;
        }

        let return_type = match spec.source.type_ {
            None => None,
            Some(type_) if type_ == TypeName::VOID => None,
            Some(type_) => {
                let type_ = self.try_resolve_type(&type_, scope, decl.span)?;
                Some(scope.get_type_index(&type_, self.pool).with_span(decl.span)?)
            }
        };

        let mut parameters = Vec::new();

        for param in &spec.source.parameters {
            let type_ = self.try_resolve_type(&param.type_, scope, decl.span)?;
            let type_idx = scope.get_type_index(&type_, self.pool).with_span(decl.span)?;
            let flags = ParameterFlags::new()
                .with_is_optional(param.qualifiers.contain(Qualifier::Optional))
                .with_is_out(param.qualifiers.contain(Qualifier::Out))
                .with_is_const(param.qualifiers.contain(Qualifier::Const));
            let name = self.pool.names.add(param.name.to_heap());
            let param = Parameter { type_: type_idx, flags };
            let idx = self
                .pool
                .add_definition(Definition::param(name, spec.fun_idx.cast(), param));
            parameters.push(idx);
        }

        let flags = FunctionFlags::new()
            .with_is_static(is_static)
            .with_is_native(is_native)
            .with_is_cast(decl.name.as_ref() == "Cast")
            .with_is_exec(decl.qualifiers.contain(Qualifier::Exec))
            .with_is_final(decl.qualifiers.contain(Qualifier::Final))
            .with_is_callback(is_callback && spec.wrapped.is_none())
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_quest(decl.qualifiers.contain(Qualifier::Quest))
            .with_has_return_value(return_type.is_some())
            .with_has_parameters(!parameters.is_empty());

        let function = Function {
            visibility: spec.visibility,
            flags,
            source: Some(spec.source_ref),
            return_type,
            unk1: false,
            base_method: spec.base_method.filter(|_| !flags.is_static()),
            parameters,
            locals: vec![],
            operator: None,
            cast: 0,
            code: Code::EMPTY,
            unk2: vec![],
            unk3: None,
        };
        let name_idx = self.pool.definition(spec.fun_idx)?.name;
        let definition = Definition::function(name_idx, spec.class_idx.cast(), function);

        if let Some(code) = spec.source.body {
            let item = FunctionBody {
                class: spec.class_idx,
                index: spec.fun_idx,
                wrapped: spec.wrapped,
                code,
                scope: scope.clone(),
                was_callback: is_callback,
                span: spec.source.span,
            };
            self.function_bodies.push(item);
        }

        self.pool.put_definition(spec.fun_idx, definition);
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
            self.report(Cause::UnexpectedNative.with_span(decl.span))?;
        }

        fn can_be_persistent(typ: &TypeName) -> bool {
            const DISALLOWED_TYPES: &[TypeName] = &[TypeName::STRING, TypeName::VARIANT, TypeName::RESOURCE];
            if DISALLOWED_TYPES.contains(typ) {
                false
            } else {
                typ.arguments().iter().all(can_be_persistent)
            }
        }

        if is_persistent && !can_be_persistent(&source.type_) {
            self.report(Cause::UnsupportedPersistent(source.type_.pretty()).with_span(decl.span))?;
        }

        let type_ = self.try_resolve_type(&source.type_, scope, decl.span)?;
        let type_idx = scope.get_type_index(&type_, self.pool).with_span(decl.span)?;
        let flags = FieldFlags::new()
            .with_is_browsable(true)
            .with_is_native(is_native)
            .with_is_persistent(is_persistent);

        let attributes = decl
            .annotations
            .iter()
            .filter(|ann| ann.kind == AnnotationKind::RuntimeProperty)
            .map(|ann| match &ann.args[..] {
                [Expr::Constant(Constant::String(Literal::String, key), _), Expr::Constant(Constant::String(Literal::String, val), _)] =>
                    Ok(Property { name: key.as_ref().to_owned(), value: val.as_ref().to_owned() }),
                _ => Err(Cause::InvalidAnnotationArgs.with_span(ann.span)),
            })
            .collect::<Result<_, Error>>()?;

        let field = Field {
            visibility,
            type_: type_idx,
            flags,
            hint: None,
            attributes,
            defaults: vec![],
        };
        let name_index = self.pool.definition(field_idx)?.name;
        let definition = Definition::field(name_index, class_idx.cast(), field);

        if let Some(value) = source.default {
            self.field_defaults.push(FieldDefault {
                class: class_idx,
                index: field_idx,
                value,
                scope: scope.clone(),
            });
        }

        self.pool.put_definition(field_idx, definition);
        Ok(())
    }

    fn define_enum(&mut self, index: PoolIndex<Enum>, source: EnumSource) -> Result<(), Error> {
        let mut members = Vec::with_capacity(source.members.len());

        for member in source.members {
            let name_index = self.pool.names.add(member.name.to_heap());
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
                    .ok_or_else(|| Cause::InvalidAnnotationArgs.with_span(ann.span))?;
                if let Symbol::Class(target_class, _) = scope.resolve_symbol(ident.clone()).with_span(ann.span)? {
                    if Scope::resolve_field(decl.name.clone(), target_class, self.pool).is_ok() {
                        self.diagnostics
                            .push(Diagnostic::FieldConflict(source.declaration.span));
                        // we avoid redefining the field because it'd crash the game
                        return Ok(());
                    }
                    let flags = self.pool.class(target_class)?.flags;
                    self.define_field(index, target_class, flags, visibility, source, scope)?;
                    self.pool.class_mut(target_class)?.fields.push(index);
                    return Ok(());
                };
                return Err(Cause::ClassNotFound(ident.clone()).with_span(ann.span));
            }
        }
        Err(Cause::UnsupportedFeature("global let binding").with_span(decl.span))
    }

    fn define_source_ref(&mut self, loc: SourceLoc<'_>) -> SourceReference {
        let count = self.file_map.len();
        let file = self.file_map.entry_ref(loc.file.path()).or_insert_with(|| {
            let file = SourceFile {
                id: count as u32,
                path_hash: 0, // TODO: consider hashing the path
                path: loc.file.path().to_owned(),
            };
            self.pool.add_definition(Definition::source_file(file))
        });
        SourceReference {
            file: *file,
            line: loc.start.line as u32,
        }
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
                        return Err(Cause::UnsupportedFeature("wrapping natives").with_span(ann.span));
                    }
                    let (class_name, _) = ann
                        .args
                        .first()
                        .and_then(Expr::as_ident)
                        .ok_or_else(|| Cause::InvalidAnnotationArgs.with_span(ann.span))?;

                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone()).with_span(ann.span)? {
                        Symbol::Struct(idx, _) | Symbol::Class(idx, _) => idx,
                        _ => return Err(Cause::ClassNotFound(class_name.clone()).with_span(ann.span)),
                    };
                    let fun_idx = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool)
                        .with_span(ann.span)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Cause::MethodNotFound(name, class_name.clone()).with_span(ann.span))?;

                    let wrapped_idx = if let Some(wrapped) = self.wrappers.get(&fun_idx) {
                        *wrapped
                    } else {
                        let proxy = self.pool.reserve();
                        self.proxies.insert(fun_idx, proxy);
                        proxy
                    };

                    let name_idx = self.pool.names.add(Ref::from(format!("wrapper${wrapped_idx}")));
                    let wrapper_idx = self.pool.stub_definition(name_idx);
                    let base = self.pool.function(fun_idx).ok().and_then(|fun| fun.base_method);

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
                        .ok_or_else(|| Cause::InvalidAnnotationArgs.with_span(ann.span))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone()).with_span(ann.span)? {
                        Symbol::Struct(idx, _) | Symbol::Class(idx, _) => idx,
                        _ => return Err(Cause::ClassNotFound(class_name.clone()).with_span(ann.span)),
                    };
                    let fun_idx = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool)
                        .with_span(ann.span)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Cause::MethodNotFound(name, class_name.clone()).with_span(ann.span))?;
                    let base = self.pool.function(fun_idx).ok().and_then(|fun| fun.base_method);
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
                        .ok_or_else(|| Cause::FunctionNotFound(name).with_span(ann.span))?;

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
                        .ok_or_else(|| Cause::InvalidAnnotationArgs.with_span(ann.span))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone()).with_span(ann.span)? {
                        Symbol::Struct(idx, _) | Symbol::Class(idx, _) => idx,
                        _ => return Err(Cause::ClassNotFound(class_name.clone()).with_span(ann.span)),
                    };
                    let class = self.pool.class(target_class_idx)?;
                    let base_method = if class.base != PoolIndex::UNDEFINED {
                        let base = self.pool.class(class.base)?;
                        base.functions
                            .iter()
                            .find(|fun| self.pool.def_name(**fun).unwrap().as_ref() == sig.as_ref())
                            .copied()
                    } else {
                        None
                    };
                    let name_idx = self.pool.names.add(Ref::from(sig.as_ref()));
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
                AnnotationKind::AddField | AnnotationKind::If | AnnotationKind::RuntimeProperty => {}
            }
        }

        let name_idx = self.pool.names.add(module.with_function(sig).render().to_heap());
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
            let ident = Ident::from_heap(pool.def_name(*param)?);
            local_scope.add_parameter(ident, *param);
        }

        if let Some(wrapped) = item.wrapped {
            let wrapped_ident = Ident::from_static("wrappedMethod");
            local_scope.add_symbol(wrapped_ident, Symbol::Functions(vec![(wrapped, Visibility::Public)]));
        }

        let mut checker = TypeChecker::new(pool, permissive);
        let checked = checker.check_seq(&item.code, &mut local_scope)?;
        let (diagnostics, mut locals) = checker.into_inner();

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

    fn compile_default(mut default: FieldDefault, pool: &mut ConstantPool) -> Result<Vec<Diagnostic>, Error> {
        fn stringify_default(expr: &Expr<SourceAst>) -> Result<String, Error> {
            match expr {
                Expr::Constant(constant, _) => match constant {
                    Constant::String(_, val) => Ok(val.as_ref().to_owned()),
                    Constant::F32(val) => Ok(val.to_string()),
                    Constant::F64(val) => Ok(val.to_string()),
                    Constant::I32(val) => Ok(val.to_string()),
                    Constant::I64(val) => Ok(val.to_string()),
                    Constant::U32(val) => Ok(val.to_string()),
                    Constant::U64(val) => Ok(val.to_string()),
                    Constant::Bool(val) => Ok(val.to_string()),
                },
                Expr::Member(receiver, name, _) => {
                    if let Expr::Ident(receiver_name, _) = receiver.as_ref() {
                        Ok(format!("{}.{}", receiver_name, name))
                    } else {
                        Err(Cause::InvalidConstant.with_span(expr.span()))
                    }
                }
                Expr::Null(_) => Ok("null".to_owned()),
                _ => Err(Cause::InvalidConstant.with_span(expr.span())),
            }
        }

        let type_id = default
            .scope
            .resolve_type_from_pool(pool.field(default.index)?.type_, pool)
            .with_span(default.value.span())?;
        let property = Property {
            name: pool.def_name(default.class)?.as_ref().to_owned(),
            value: stringify_default(&default.value)?,
        };
        pool.field_mut(default.index)?.defaults = vec![property];

        let mut typeck = TypeChecker::new(pool, false);
        typeck.check_and_convert(&default.value, &type_id, &mut default.scope)?;
        Ok(typeck.into_diagnostics())
    }

    fn construct_proxy(
        slot: PoolIndex<Function>,
        wrapped: PoolIndex<Function>,
        wrapper: PoolIndex<Function>,
        files: &Files,
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

        let call = Expr::Call(
            Callable::Function(wrapper),
            [].into(),
            args.into_boxed_slice(),
            Span::ZERO,
        );
        let expr = if fun.return_type.is_some() {
            Expr::Return(Some(Box::new(call)), Span::ZERO)
        } else {
            call
        };
        let code = Assembler::from_body(Seq::new(vec![expr]), files, scope, pool)?;

        let compiled = Function {
            code,
            locals: vec![],
            parameters,
            ..fun
        };
        let name = pool.names.add(Ref::from(format!("proxy${wrapper}")));
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

        for instr in &mut fun.code.0 {
            if let Instr::Local(local) = instr {
                *instr = Instr::Local(*mapped_locals.get(local).unwrap());
            }
        }
        Ok(())
    }

    // this method is a workaround for a game crash which happens when the game loads
    // a class which has a base class that is placed after the subclass in the pool
    fn cleanup_pool(pool: &mut ConstantPool) {
        fn collect_subtypes(
            class_idx: PoolIndex<Class>,
            hierarchy: &HashMap<PoolIndex<Class>, Vec<PoolIndex<Class>>>,
            acc: &mut BTreeSet<PoolIndex<Class>>,
        ) {
            acc.insert(class_idx);
            for sub in hierarchy.get(&class_idx).map(Vec::as_slice).unwrap_or(&[]) {
                collect_subtypes(*sub, hierarchy, acc);
            }
        }

        let mut hierarchy: HashMap<PoolIndex<Class>, Vec<PoolIndex<Class>>> = HashMap::new();

        // build up a hierarchy pointing from base types to all subtypes
        for (def_idx, def) in pool.definitions() {
            if let Some(class) = &def.value.as_class().filter(|cls| !cls.base.is_undefined()) {
                hierarchy.entry(class.base).or_default().push(def_idx.cast());
            }
        }

        let mut unsorted = BTreeSet::new();

        // for each class that appears before one of it's subclasses, find the deepest superclass affected
        for (def_idx, _) in pool.definitions() {
            let mut cur = def_idx.cast();
            loop {
                match pool.class(cur) {
                    Ok(cls) if u32::from(def_idx) < u32::from(cls.base) => cur = cls.base,
                    _ => break,
                }
            }
            if cur != def_idx.cast() {
                collect_subtypes(cur, &hierarchy, &mut unsorted);
            }
        }

        if let Some(&min_unsorted) = unsorted.iter().next() {
            // for each class appearning out of order, capture any superclasses that could potentially break after sorting
            for cls in unsorted.clone() {
                let mut cur = cls;
                loop {
                    match pool.class(cur) {
                        Ok(cls) if u32::from(min_unsorted) < u32::from(cls.base) && !unsorted.contains(&cls.base) => {
                            cur = cls.base;
                        }
                        _ => break,
                    }
                }
                if cur != cls {
                    collect_subtypes(cur, &hierarchy, &mut unsorted);
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

struct FieldDefault {
    class: PoolIndex<Class>,
    index: PoolIndex<Field>,
    value: Expr<SourceAst>,
    scope: Scope,
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

#[derive(Debug)]
struct FunctionSpec {
    fun_idx: PoolIndex<Function>,
    class_idx: PoolIndex<Class>,
    class_flags: Option<ClassFlags>,
    base_method: Option<PoolIndex<Function>>,
    source_ref: SourceReference,
    wrapped: Option<PoolIndex<Function>>,
    is_replacement: bool,
    visibility: Visibility,
    source: FunctionSource,
}

fn eval_conditions(cte: &cte::Context, anns: &[Annotation]) -> Result<bool, Error> {
    anns.iter()
        .filter(|ann| ann.kind == AnnotationKind::If)
        .filter_map(|ann| ann.args.first())
        .try_fold(true, |acc, expr| {
            let res = cte.eval(expr)?;
            let res = res
                .as_bool()
                .ok_or_else(|| Error::CteError("invalid value", expr.span()))?;
            Ok(acc && *res)
        })
}
