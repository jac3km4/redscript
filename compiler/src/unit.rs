use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use redscript::ast::{Expr, Ident, Pos, Seq, SourceAst};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr};
use redscript::definition::*;
use redscript::error::Error;
use redscript::mapper::{MultiMapper, PoolMapper};

use crate::assembler::Assembler;
use crate::parser::*;
use crate::scope::{Reference, Scope, Value};
use crate::source_map::{Files, SourceLocation};
use crate::sugar::Desugar;
use crate::symbol::{FunctionSignature, Import, ModulePath, Symbol, SymbolMap};
use crate::transform::ExprTransformer;
use crate::typechecker::{Callable, TypeChecker};

pub struct CompilationUnit<'a> {
    pool: &'a mut ConstantPool,
    symbols: SymbolMap,
    scope: Scope,
    function_bodies: Vec<FunctionBody>,
    wrappers: HashMap<PoolIndex<Function>, PoolIndex<Function>>,
    proxies: HashMap<PoolIndex<Function>, PoolIndex<Function>>,
}

impl<'a> CompilationUnit<'a> {
    pub fn new(pool: &'a mut ConstantPool) -> Result<CompilationUnit<'a>, Error> {
        let symbols = SymbolMap::new(pool)?;
        let mut scope = Scope::new(pool)?;

        symbols.populate_import(
            Import::All(ModulePath::EMPTY, Pos::ZERO),
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
        })
    }

    pub fn compile(self, files: &Files) -> Result<(), Error> {
        log::info!("Compiling files: {}", files);

        match self.try_compile(files) {
            Ok(diagnostics) => {
                for diagnostic in diagnostics {
                    Self::print_diagnostic(files, diagnostic);
                }
                log::info!("Compilation complete");
                Ok(())
            }
            Err(Error::CompileError(err, pos)) => {
                Self::print_error(&err, files.lookup(pos).expect("Unknown file"));
                Err(Error::CompileError(err, pos))
            }
            Err(Error::TypeError(err, pos)) => {
                Self::print_error(&err, files.lookup(pos).expect("Unknown file"));
                Err(Error::CompileError(err, pos))
            }
            Err(Error::ResolutionError(err, pos)) => {
                Self::print_error(&err, files.lookup(pos).expect("Unknown file"));
                Err(Error::CompileError(err, pos))
            }
            Err(Error::SyntaxError(err, pos)) => {
                Self::print_error(&err, files.lookup(pos).expect("Unknown file"));
                Err(Error::SyntaxError(err, pos))
            }
            Err(other) => {
                log::error!("{}: {:?}", "Unexpected error during compilation", other);
                Err(other)
            }
        }
    }

    fn try_compile(self, files: &Files) -> Result<Vec<Diagnostic>, Error> {
        let mut modules = vec![];
        for file in files.files() {
            let parsed = parse_file(file).map_err(|err| {
                let message = format!("Syntax error, expected {}", err.expected);
                Error::SyntaxError(message, file.byte_offset() + err.location.offset)
            })?;
            modules.push(parsed);
        }
        self.compile_modules(modules)
    }

    fn print_diagnostic(files: &Files, diagnostic: Diagnostic) {
        match diagnostic {
            Diagnostic::MethodConflict(_, pos) => {
                let loc = files.lookup(pos).unwrap();
                log::warn!("At {}:\n Conflicting method replacement", loc)
            }
        }
    }

    fn print_error(error: &str, loc: SourceLocation) {
        let line = loc.enclosing_line().trim_end();
        log::error!(
            "At {}:\n \
             {}\n \
             {}^^^\n \
             {}",
            loc,
            line,
            " ".repeat(loc.position.col),
            error
        );
    }

    pub fn compile_modules(mut self, modules: Vec<SourceModule>) -> Result<Vec<Diagnostic>, Error> {
        let mut compiled_funs = HashSet::new();
        let mut diagnostics = Vec::new();
        let mut queue = Vec::with_capacity(modules.len());

        for module in modules {
            let path = module.path.unwrap_or(ModulePath::EMPTY);
            let mut slots = Vec::with_capacity(module.entries.len());
            for entry in module.entries {
                slots.push(self.define_symbol(entry, &path)?);
            }
            queue.push((path, module.imports, slots));
        }

        for (path, imports, slots) in queue {
            let mut module_scope = self.scope.clone();

            if !path.is_empty() {
                self.symbols
                    .populate_import(Import::All(path, Pos::ZERO), &mut module_scope, Visibility::Private)
                    .ok();
            };

            for import in imports {
                self.symbols
                    .populate_import(import, &mut module_scope, Visibility::Public)?;
            }

            for slot in slots {
                match slot {
                    Slot::Function {
                        index,
                        parent,
                        base,
                        source,
                        visibility,
                        wrapped,
                    } => {
                        let pos = source.declaration.pos;
                        self.define_function(index, parent, base, wrapped, visibility, source, &mut module_scope)?;
                        if compiled_funs.contains(&index) {
                            diagnostics.push(Diagnostic::MethodConflict(index, pos));
                        } else {
                            compiled_funs.insert(index);
                        }
                    }
                    Slot::Class {
                        index,
                        source,
                        visibility,
                    } => {
                        self.define_class(index, visibility, source, &mut module_scope)?;
                    }
                    Slot::Field {
                        index,
                        source,
                        visibility,
                    } => {
                        self.define_global_let(index, visibility, source, &mut module_scope)?;
                    }
                    Slot::Enum { index, source } => {
                        self.define_enum(index, source)?;
                    }
                }
            }
        }

        // create function proxies
        for (wrapped, wrapper) in self.wrappers {
            let proxy = self.proxies.get(&wrapped).unwrap();
            Self::construct_proxy(*proxy, wrapped, wrapper, &mut self.scope, self.pool)?;
        }

        // compile function bodies
        for item in self.function_bodies {
            Self::compile_function(item, self.pool)?;
        }

        // swap proxies with the functions they wrap
        for (wrapped, proxy) in self.proxies {
            let wrapped_name = self.pool.definition(wrapped)?.name;
            let proxy_name = self.pool.definition(proxy)?.name;

            Self::remap_locals(proxy, wrapped, self.pool)?;
            self.pool.rename(wrapped, proxy_name);
            self.pool.rename(proxy, wrapped_name);
            self.pool.swap_definition(wrapped, proxy);
        }

        Self::cleanup_pool(self.pool);
        Ok(diagnostics)
    }

    fn define_symbol(&mut self, entry: SourceEntry, module: &ModulePath) -> Result<Slot, Error> {
        match entry {
            SourceEntry::Class(source) => {
                let path = module.with_child(source.name.clone());
                let name_index = self.pool.names.add(path.render().to_owned());
                let type_index = self.pool.add_definition(Definition::type_(name_index, Type::Class));
                let index = self.pool.stub_definition(name_index);
                let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);

                self.scope.add_type(path.render(), type_index);
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
                let type_index = self.pool.add_definition(Definition::type_(name_index, Type::Class));
                let index = self.pool.stub_definition(name_index);

                self.scope.add_type(path.render(), type_index);
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
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let flags = ClassFlags::new()
            .with_is_abstract(source.qualifiers.contain(Qualifier::Abstract))
            .with_is_final(source.qualifiers.contain(Qualifier::Final));
        let mut functions = vec![];
        let mut fields = vec![];

        for member in source.members {
            match member {
                MemberSource::Function(fun) => {
                    let fun_sig = FunctionSignature::from_source(&fun);
                    let name_idx = self.pool.names.add(Rc::new(fun_sig.into_owned()));
                    let fun_idx = self.pool.stub_definition(name_idx);

                    self.define_function(fun_idx, class_idx, None, None, visibility, fun, scope)?;
                    functions.push(fun_idx);
                }
                MemberSource::Field(let_) => {
                    let name_idx = self.pool.names.add(let_.declaration.name.to_owned());
                    let field_idx = self.pool.stub_definition(name_idx);

                    self.define_field(field_idx, class_idx, visibility, let_, scope)?;
                    fields.push(field_idx);
                }
            }
        }

        let base_idx = if let Some(base_name) = source.base {
            if let Symbol::Class(base_idx, _) = scope.resolve_symbol(base_name.clone(), source.pos)? {
                base_idx
            } else {
                return Err(Error::class_not_found(base_name, source.pos));
            }
        } else if let Ok(Symbol::Class(class, _)) = scope.resolve_symbol(Ident::Static("IScriptable"), source.pos) {
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

    fn define_function(
        &mut self,
        fun_idx: PoolIndex<Function>,
        parent_idx: PoolIndex<Class>,
        base_method: Option<PoolIndex<Function>>,
        wrapped: Option<PoolIndex<Function>>,
        visibility: Visibility,
        source: FunctionSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = &source.declaration;
        let flags = FunctionFlags::new()
            .with_is_static(decl.qualifiers.contain(Qualifier::Static) || parent_idx == PoolIndex::UNDEFINED)
            .with_is_exec(decl.qualifiers.contain(Qualifier::Exec))
            .with_is_final(decl.qualifiers.contain(Qualifier::Final))
            .with_is_native(decl.qualifiers.contain(Qualifier::Native) && source.body.is_none())
            .with_is_callback(decl.qualifiers.contain(Qualifier::Callback))
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_quest(decl.qualifiers.contain(Qualifier::Quest))
            .with_is_cast(decl.name.as_ref() == "Cast");

        let return_type = match source.type_ {
            None => None,
            Some(type_) if type_.name.as_ref() == "Void" => None,
            Some(type_) => {
                let type_ = scope.resolve_type(&type_, self.pool, decl.pos)?;
                Some(scope.get_type_index(&type_, self.pool)?)
            }
        };

        let mut parameters = Vec::new();

        for param in &source.parameters {
            let type_ = scope.resolve_type(&param.type_, self.pool, decl.pos)?;
            let type_idx = scope.get_type_index(&type_, self.pool)?;
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
        };
        let name_idx = self.pool.definition(fun_idx)?.name;
        let definition = Definition::function(name_idx, parent_idx.cast(), function);

        if let Some(code) = source.body {
            let item = FunctionBody {
                class: parent_idx,
                function: fun_idx,
                wrapped,
                code,
                scope: scope.clone(),
            };
            self.function_bodies.push(item)
        }

        self.pool.put_definition(fun_idx, definition);
        Ok(())
    }

    fn define_field(
        &mut self,
        index: PoolIndex<Field>,
        parent: PoolIndex<Class>,
        visibility: Visibility,
        source: FieldSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = source.declaration;
        let type_ = scope.resolve_type(&source.type_, self.pool, decl.pos)?;
        let type_idx = scope.get_type_index(&type_, self.pool)?;
        let flags = FieldFlags::new()
            .with_is_mutable(true)
            .with_is_native(decl.qualifiers.contain(Qualifier::Native));
        let field = Field {
            visibility,
            type_: type_idx,
            flags,
            hint: None,
            attributes: vec![],
            defaults: vec![],
        };
        let name_index = self.pool.definition(index)?.name;
        let definition = Definition::field(name_index, parent.cast(), field);

        self.pool.put_definition(index, definition);
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
            match ann.name {
                AnnotationName::AddField => {
                    let ident = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    if let Symbol::Class(target_class, _) = scope.resolve_symbol(ident.clone(), ann.pos)? {
                        self.define_field(index, target_class, visibility, source, scope)?;
                        self.pool.class_mut(target_class)?.fields.push(index);
                        return Ok(());
                    } else {
                        return Err(Error::class_not_found(ident, ann.pos));
                    }
                }
                _ => {}
            }
        }
        Err(Error::unsupported("Let binding", decl.pos))
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
            match ann.name {
                AnnotationName::WrapMethod => {
                    if source.declaration.qualifiers.contain(Qualifier::Native) {
                        return Err(Error::unsupported("Wrapping natives", ann.pos));
                    }
                    let class_name = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone(), ann.pos)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Error::class_not_found(class_name, ann.pos)),
                    };
                    let fun_idx = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool, ann.pos)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Error::function_not_found(name, ann.pos))?;

                    let wrapped_idx = match self.wrappers.get(&fun_idx) {
                        Some(wrapped) => *wrapped,
                        None => {
                            let proxy = self.pool.reserve();
                            self.proxies.insert(fun_idx, proxy);
                            proxy
                        }
                    };

                    let name_idx = self.pool.names.add(Rc::new(format!("wrapper${}", wrapped_idx)));
                    let wrapper_idx = self.pool.stub_definition(name_idx);
                    let base = self.pool.function(fun_idx)?.base_method;

                    self.wrappers.insert(fun_idx, wrapper_idx);
                    self.pool.class_mut(target_class_idx)?.functions.push(wrapper_idx);

                    let slot = Slot::Function {
                        index: wrapper_idx,
                        parent: target_class_idx,
                        base,
                        wrapped: Some(wrapped_idx),
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::ReplaceMethod => {
                    let class_name = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone(), ann.pos)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Error::class_not_found(class_name, ann.pos)),
                    };
                    let fun_idx = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool, ann.pos)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Error::function_not_found(name, ann.pos))?;
                    let base = self.pool.function(fun_idx)?.base_method;
                    let slot = Slot::Function {
                        index: fun_idx,
                        parent: target_class_idx,
                        base,
                        wrapped: None,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::ReplaceGlobal => {
                    let fun_idx = self
                        .scope
                        .resolve_function(name.clone(), ann.pos)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Error::function_not_found(name, ann.pos))?;

                    let slot = Slot::Function {
                        index: fun_idx,
                        parent: PoolIndex::UNDEFINED,
                        base: None,
                        wrapped: None,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::AddMethod => {
                    let class_name = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone(), ann.pos)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Error::class_not_found(class_name, ann.pos)),
                    };
                    let class = self.pool.class(target_class_idx)?;
                    let base_method = if class.base != PoolIndex::UNDEFINED {
                        let base = self.pool.class(class.base)?;
                        base.functions
                            .iter()
                            .find(|fun| self.pool.definition_name(**fun).unwrap().as_str() == sig.as_ref())
                            .cloned()
                    } else {
                        None
                    };
                    let name_idx = self.pool.names.add(Rc::new(sig.into_owned()));
                    let fun_idx = self.pool.stub_definition(name_idx);
                    self.pool.class_mut(target_class_idx)?.functions.push(fun_idx);

                    let slot = Slot::Function {
                        index: fun_idx,
                        parent: target_class_idx,
                        base: base_method,
                        wrapped: None,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::AddField => {}
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
            source,
            visibility,
        };
        Ok(slot)
    }

    fn compile_function(item: FunctionBody, pool: &mut ConstantPool) -> Result<(), Error> {
        let fun = pool.function(item.function)?;

        let mut local_scope = if fun.flags.is_static() {
            item.scope.with_context(None, item.function)
        } else {
            item.scope.with_context(Some(item.class), item.function)
        };

        for param in &fun.parameters {
            let ident = Ident::Owned(pool.definition_name(*param)?);
            local_scope.add_parameter(ident, *param);
        }

        if let Some(wrapped) = item.wrapped {
            let wrapped_ident = Ident::Static("wrappedMethod");
            local_scope.add_symbol(wrapped_ident, Symbol::Functions(vec![(wrapped, Visibility::Public)]));
        }

        let mut checker = TypeChecker::new(pool);
        let checked = checker.check_seq(&item.code, &mut local_scope)?;
        let mut locals = checker.locals();

        let mut desugar = Desugar::new(&mut local_scope, pool);
        let desugared = desugar.on_seq(checked)?;
        locals.extend(desugar.locals());

        let code = Assembler::from_body(desugared, &mut local_scope, pool)?;
        let function = pool.function_mut(item.function)?;
        function.code = code;
        function.locals = locals;
        Ok(())
    }

    fn construct_proxy(
        slot: PoolIndex<Function>,
        wrapped: PoolIndex<Function>,
        wrapper: PoolIndex<Function>,
        scope: &mut Scope,
        pool: &mut ConstantPool,
    ) -> Result<(), Error> {
        let def = pool.definition(wrapped)?.clone();
        if let AnyDefinition::Function(fun) = def.value {
            let mut parameters = vec![];
            let mut args = vec![];

            for param_idx in &fun.parameters {
                let param = pool.definition(*param_idx)?.clone();
                let proxy_idx = pool.add_definition(param);
                parameters.push(proxy_idx);
                args.push(Expr::Ident(Reference::Value(Value::Parameter(proxy_idx)), Pos::ZERO));
            }

            let call = Expr::Call(Callable::Function(wrapper), args, Pos::ZERO);
            let expr = if fun.return_type.is_some() {
                Expr::Return(Some(Box::new(call)), Pos::ZERO)
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
            let name = pool.names.add(Rc::new(format!("proxy${}", wrapper)));
            pool.put_definition(slot, Definition::function(name, def.parent.cast(), compiled));
            if !def.parent.is_undefined() {
                pool.class_mut(def.parent.cast())?.functions.push(slot);
            }
            Ok(())
        } else {
            panic!("Invalid proxy")
        }
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
    fn class_cardinality(idx: PoolIndex<Class>, pool: &ConstantPool) -> usize {
        let class = pool.class(idx).unwrap();
        if class.base.is_undefined() {
            0
        } else {
            Self::class_cardinality(class.base, pool) + 1
        }
    }

    fn cleanup_pool(pool: &mut ConstantPool) {
        // this is a workaround for a game crash which happens when the game loads
        // a class which has a base class that is placed after the subclass in the pool
        let mut need_sorting = Vec::new();

        for (def_idx, def) in pool.definitions() {
            if let AnyDefinition::Class(class) = &def.value {
                let pos: u32 = def_idx.into();
                if pos < class.base.into() {
                    need_sorting.push(def_idx.cast());
                    need_sorting.push(class.base);
                }
            }
        }
        let mut sorted = need_sorting.clone();
        sorted.sort_by_key(|k| Self::class_cardinality(*k, pool));

        let definitions: Vec<Definition> = need_sorting
            .iter()
            .map(|k| pool.definition(*k).unwrap())
            .cloned()
            .collect();

        for (def, target) in definitions.into_iter().zip(&sorted) {
            pool.put_definition(*target, def);
        }

        let mappings = need_sorting.into_iter().zip(sorted).collect();
        PoolMapper::default()
            .with_class_mapper(MultiMapper::new(mappings))
            .map(pool);
    }
}

struct FunctionBody {
    class: PoolIndex<Class>,
    function: PoolIndex<Function>,
    wrapped: Option<PoolIndex<Function>>,
    code: Seq<SourceAst>,
    scope: Scope,
}

enum Slot {
    Function {
        index: PoolIndex<Function>,
        parent: PoolIndex<Class>,
        base: Option<PoolIndex<Function>>,
        wrapped: Option<PoolIndex<Function>>,
        source: FunctionSource,
        visibility: Visibility,
    },
    Class {
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
pub enum Diagnostic {
    MethodConflict(PoolIndex<Function>, Pos),
}
