use std::mem;
use std::ops::Not;
use std::rc::Rc;
use std::str::FromStr;

use ahash::RandomState;
use hashbrown::HashMap;
use itertools::{chain, Itertools};
use redscript::ast::{Seq, SourceAst, Span};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::Intrinsic;
use redscript::definition::{
    AnyDefinition, Class as PoolClass, ClassFlags, Enum as PoolEnum, Field as PoolField, Function as PoolFunction, FunctionFlags, ParameterFlags, Type as PoolType, Visibility
};
use redscript::Str;
use sequence_trie::SequenceTrie;

use crate::autobox::Autobox;
use crate::codegen::builders::{ClassBuilder, EnumBuilder, FieldBuilder, FunctionBuilder, ParamBuilder, TypeCache};
use crate::codegen::{names, CodeGen, LocalIndices};
use crate::error::{CompileError, CompileResult, ParseError};
use crate::parser::{
    self, ClassSource, EnumSource, FunctionSource, Import, MemberSource, ModulePath, ParameterSource, Qualifier, SourceEntry, SourceModule
};
use crate::scoped_map::ScopedMap;
use crate::source_map::Files;
use crate::type_repo::*;
use crate::typer::*;
use crate::{IndexMap, StringInterner};

#[derive(Debug)]
pub struct Compiler<'id> {
    type_repo: TypeRepo<'id>,
    interner: &'id StringInterner,
    defined_types: Vec<TypeId<'id>>,
    modules: ModuleMap<'id>,
    compile_queue: Vec<Module<'id>>,
    reporter: ErrorReporter<'id>,
}

impl<'id> Compiler<'id> {
    pub fn new(type_repo: TypeRepo<'id>, interner: &'id StringInterner) -> Self {
        Self {
            type_repo,
            interner,
            defined_types: vec![],
            modules: ModuleMap::default(),
            compile_queue: vec![],
            reporter: ErrorReporter::default(),
        }
    }

    pub fn run(mut self, files: &Files) -> Result<CompilationOutputs<'id>, ParseError> {
        let mut types: TypeScope = self.type_repo.type_iter().map(|id| (id.as_str().into(), id)).collect();

        let mut names = NameScope::default();
        for (name, idx) in self.type_repo.globals().iter_by_name() {
            names
                .top_mut()
                .entry_ref(name.name())
                .or_default()
                .push(Global::Func(idx));
        }

        let modules: Vec<_> = Self::parse_modules(files).try_collect()?;
        let mut scopes = vec![];
        for module in &modules {
            if module.path.is_empty() {
                for entry in &module.entries {
                    self.populate_entry(&module.path, entry, &mut types);
                }
                scopes.push(HashMap::default());
            } else {
                let mut local = types.introduce_scope();
                for entry in &module.entries {
                    self.populate_entry(&module.path, entry, &mut local);
                }
                scopes.push(local.pop_scope());
            };
        }

        for (module, scope) in modules.into_iter().zip(scopes) {
            let res = self.compile_module(module, types.push_scope(scope), &mut names);
            self.compile_queue.push(res);
        }
        Ok(self.process_queue(&types, &names))
    }

    fn parse_modules(files: &Files) -> impl Iterator<Item = Result<SourceModule, ParseError>> + '_ {
        files.iter().map(|file| {
            parser::parse_file(file).map_err(|err| {
                let pos = file.byte_offset() + err.location.offset;
                ParseError(err.expected, Span::new(pos, pos))
            })
        })
    }

    fn compile_module(
        &mut self,
        module: SourceModule,
        mut types: TypeScope<'_, 'id>,
        names: &mut NameScope<'_, 'id>,
    ) -> Module<'id> {
        for import in module.imports {
            let res = self.populate_import(import, &mut types, names);
            self.reporter.unwrap_err(res);
        }
        let mut items = vec![];
        for entry in module.entries {
            let res = self.preprocess_entry(&module.path, entry, &types, names);
            let Some(item) = self.reporter.unwrap_err(res).flatten() else { continue };
            items.push(item);
        }
        let types = types.pop_scope();
        let names = if names.is_top_level() {
            HashMap::default()
        } else {
            mem::take(names.top_mut())
        };
        Module { types, names, items }
    }

    fn populate_entry(&mut self, path: &ModulePath, entry: &SourceEntry, types: &mut TypeScope<'_, 'id>) {
        match entry {
            SourceEntry::Class(ClassSource { name, .. })
            | SourceEntry::Struct(ClassSource { name, .. })
            | SourceEntry::Enum(EnumSource { name, .. }) => {
                let type_id = generate_type_id(&name, path, self.interner);
                self.modules.add_type(type_id);
                types.insert(name.clone(), type_id);
            }
            SourceEntry::Function(func) => {
                let name = ScopedName::new(func.declaration.name.clone(), path.clone());
                let idx = self.type_repo.globals_mut().reserve_name(name.clone());
                self.modules.add_function(&name, idx);
            }
            SourceEntry::GlobalLet(_) => {}
        }
    }

    fn populate_import(
        &mut self,
        import: Import,
        types: &mut TypeScope<'_, 'id>,
        names: &mut NameScope<'_, 'id>,
    ) -> CompileResult<'id, ()> {
        match import {
            Import::Exact(_, path, span) => {
                let import = self
                    .modules
                    .get(path.iter())
                    .ok_or_else(|| CompileError::UnresolvedImport(path.into_iter().collect(), span))?;
                Self::populate_import_item(&import, &self.type_repo, types, names);
            }
            Import::Selected(_, path, selected, span) => {
                for name in selected {
                    let path = path.iter().chain(Some(&name));
                    let import = self
                        .modules
                        .get(path.clone())
                        .ok_or_else(|| CompileError::UnresolvedImport(path.cloned().collect(), span))?;
                    Self::populate_import_item(&import, &self.type_repo, types, names);
                }
            }
            Import::All(_, path, span) => {
                for descendant in self
                    .modules
                    .get_direct_descendants(path.iter())
                    .ok_or_else(|| CompileError::UnresolvedImport(path.iter().cloned().collect(), span))?
                {
                    Self::populate_import_item(&descendant, &self.type_repo, types, names);
                }
            }
        };
        Ok(())
    }

    fn populate_import_item(
        imported: &ImportItem<'id>,
        repo: &TypeRepo<'id>,
        types: &mut TypeScope<'_, 'id>,
        names: &mut NameScope<'_, 'id>,
    ) {
        match *imported {
            ImportItem::Type(typ) => {
                types.insert(typ.name().into(), typ);
            }
            ImportItem::Func(func) => {
                let name = repo.globals().get_name(func).unwrap();
                names
                    .top_mut()
                    .entry_ref(name.name())
                    .or_default()
                    .push(Global::Func(func));
            }
        }
    }

    fn preprocess_entry(
        &mut self,
        path: &ModulePath,
        entry: SourceEntry,
        types: &TypeScope<'_, 'id>,
        names: &mut NameScope<'_, 'id>,
    ) -> CompileResult<'id, Option<ModuleItem<'id>>> {
        let is_struct = matches!(entry, SourceEntry::Struct(_));
        match entry {
            SourceEntry::Class(class) | SourceEntry::Struct(class) => {
                let type_id = generate_type_id(&class.name, path, self.interner);
                let mut type_vars = ScopedMap::default();
                let env = TypeEnv::new(types, &type_vars);
                let class_type_vars = class
                    .tparams
                    .iter()
                    .map(|typ| env.instantiate_var(typ))
                    .collect::<CompileResult<Box<[_]>, _>>()
                    .with_span(class.span)?;
                for var in class_type_vars.iter() {
                    type_vars.insert(var.name.clone(), InferType::from_var_mono(var, &type_vars));
                }
                let extends = class
                    .base
                    .map(|base| TypeEnv::new(types, &type_vars).resolve_param_type(&base))
                    .transpose()
                    .with_span(class.span)?;
                let mut data_type = ClassType {
                    type_vars: class_type_vars,
                    extends,
                    fields: FieldMap::default(),
                    methods: FuncMap::default(),
                    statics: FuncMap::default(),
                    is_abstract: class.qualifiers.contain(Qualifier::Abstract),
                    is_struct,
                };
                let mut methods = vec![];

                for member in class.members {
                    match member {
                        MemberSource::Method(method) => {
                            let is_static = method.declaration.qualifiers.contain(Qualifier::Static);
                            let is_final = is_static || method.declaration.qualifiers.contain(Qualifier::Final);
                            let res = self.preprocess_function(&method, types, &type_vars);
                            let Some((env, typ)) = self.reporter.unwrap_err(res) else { continue };
                            let idx = if is_static {
                                data_type.statics.add(method.declaration.name, typ, is_final)
                            } else {
                                data_type.methods.add(method.declaration.name, typ, is_final)
                            };
                            if let Some(body) = method.body {
                                methods.push(CompileBody {
                                    index: idx,
                                    env,
                                    parameters: method.parameters,
                                    body,
                                    is_static,
                                });
                            }
                        }
                        MemberSource::Field(field) => {
                            let env = TypeEnv::new(types, &type_vars);
                            let res = env.resolve_type(&field.type_).with_span(field.declaration.span);
                            let Some(typ) = self.reporter.unwrap_err(res) else { continue };
                            data_type.fields.add(field.declaration.name, typ);
                        }
                    }
                }
                self.type_repo.add_type(type_id, DataType::Class(data_type));
                self.defined_types.push(type_id);
                Ok(Some(ModuleItem::Class(type_id, type_vars.pop_scope(), methods)))
            }
            SourceEntry::Enum(enum_) => {
                let type_id = generate_type_id(&enum_.name, path, self.interner);
                let members = enum_.members.iter().map(|m| (m.name.clone(), m.value)).collect();
                self.type_repo.add_type(type_id, DataType::Enum(EnumType { members }));
                self.defined_types.push(type_id);
                Ok(None)
            }
            SourceEntry::Function(func) => {
                let (env, typ) = self.preprocess_function(&func, types, &ScopedMap::default())?;
                let name = ScopedName::new(func.declaration.name.clone(), path.clone());
                let index = self.type_repo.globals_mut().add(name, typ, true);
                let global = if let Ok(intrinsic) = Intrinsic::from_str(&func.declaration.name) {
                    Global::Intrinsic(index.overload(), intrinsic)
                } else {
                    Global::Func(index.overload())
                };
                let res = if let Some(body) = func.body {
                    let body = CompileBody {
                        index,
                        env,
                        parameters: func.parameters,
                        body,
                        is_static: true,
                    };
                    Some(ModuleItem::Global(body))
                } else {
                    None
                };
                let overloads = names.top_mut().entry(func.declaration.name).or_default();
                if !overloads.contains(&global) {
                    overloads.push(global);
                }
                Ok(res)
            }
            SourceEntry::GlobalLet(_) => todo!(),
        }
    }

    fn process_queue(mut self, types: &TypeScope<'_, 'id>, names: &NameScope<'_, 'id>) -> CompilationOutputs<'id> {
        let mut items = vec![];

        for module in self.compile_queue {
            let types = types.push_scope(module.types);
            let names = names.push_scope(module.names);
            for item in module.items {
                match item {
                    ModuleItem::Class(owner, env, funcs) => {
                        let class = self.type_repo.get_type(owner).unwrap().as_class().unwrap();
                        let type_vars = ScopedMap::Tail(env);
                        let this_args = class
                            .type_vars
                            .iter()
                            .map(|var| InferType::from_var_mono(var, &type_vars))
                            .collect();
                        let this = InferType::data(Data::new(owner, this_args));
                        for func in funcs {
                            let is_static = func.is_static;
                            let (_, method) = if is_static {
                                class.statics.get_overload(func.index).unwrap()
                            } else {
                                class.methods.get_overload(func.index).unwrap()
                            };
                            let this = func.is_static.not().then(|| this.clone());
                            let mid = MethodId::new(owner, func.index);
                            let (body, params) = Self::compile_function(
                                func,
                                &method.typ,
                                &self.type_repo,
                                &types,
                                &names,
                                &type_vars,
                                this,
                                &mut self.reporter,
                            );
                            items.push(CodeGenItem::AssembleMethod(mid, params, body, is_static));
                        }
                    }
                    ModuleItem::Global(body) => {
                        let idx = body.index;
                        let func = self.type_repo.get_global(&GlobalId::new(body.index)).unwrap();
                        let type_vars = ScopedMap::default();
                        let (body, params) = Self::compile_function(
                            body,
                            &func.typ,
                            &self.type_repo,
                            &types,
                            &names,
                            &type_vars,
                            None,
                            &mut self.reporter,
                        );
                        items.push(CodeGenItem::AssembleGlobal(GlobalId::new(idx), params, body));
                    }
                }
            }
        }
        CompilationOutputs {
            type_repo: self.type_repo,
            defined_types: self.defined_types,
            codegen_queue: items,
            reporter: self.reporter,
        }
    }

    fn preprocess_function(
        &mut self,
        func: &FunctionSource,
        types: &TypeScope<'_, 'id>,
        vars: &Vars<'_, 'id>,
    ) -> CompileResult<'id, (HashMap<Str, InferType<'id>>, FuncType<'id>)> {
        let env = TypeEnv::new(types, vars);
        let method_type_vars = func
            .tparams
            .iter()
            .map(|ty| env.instantiate_var(ty))
            .collect::<CompileResult<Box<[_]>, _>>()
            .with_span(func.declaration.span)?;
        let mut local_vars = vars.introduce_scope();
        for var in method_type_vars.iter() {
            local_vars.insert(var.name.clone(), InferType::from_var_mono(var, &local_vars));
        }
        let env = env.with_vars(&local_vars);
        let params = func
            .parameters
            .iter()
            .map(|param| {
                let typ = env.resolve_type(&param.type_)?;
                Ok(FuncParam::custom(typ, param.qualifiers.contain(Qualifier::Out)))
            })
            .try_collect()
            .with_span(func.declaration.span)?;
        let ret = func
            .type_
            .as_ref()
            .map(|typ| env.resolve_type(typ))
            .unwrap_or(Ok(Type::Prim(Prim::Unit)))
            .with_span(func.declaration.span)?;
        let func_type = FuncType::new(method_type_vars, params, ret.clone());
        Ok((local_vars.pop_scope(), func_type))
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_function(
        func: CompileBody<'id>,
        typ: &FuncType<'id>,
        repo: &TypeRepo<'id>,
        types: &TypeScope<'_, 'id>,
        names: &NameScope<'_, 'id>,
        vars: &Vars<'_, 'id>,
        this: Option<InferType<'id>>,
        reporter: &mut ErrorReporter<'id>,
    ) -> (Seq<CheckedAst<'id>>, IndexMap<Local, Type<'id>>) {
        let local_vars = vars.push_scope(func.env);
        let mut id_alloc = IdAlloc::default();
        let mut locals = ScopedMap::default();
        let mut params = IndexMap::default();

        for (lhs, rhs) in func.parameters.iter().zip(typ.params.iter()) {
            let info = id_alloc.allocate_param(InferType::from_type(&rhs.typ, &local_vars));
            params.insert(info.local, rhs.typ.clone());
            locals.insert(lhs.name.clone(), info);
        }
        if let Some(this) = this {
            locals.insert(Str::from_static("this"), LocalInfo::new(Local::This, this));
        }
        let ret = InferType::from_type(&typ.ret, &local_vars);
        let env = TypeEnv::new(types, &local_vars);
        let mut seq = Typer::run(repo, names, env, &func.body, &mut locals, ret, reporter);
        Autobox::run(&mut seq, repo);
        (seq, params)
    }
}

#[derive(Debug)]
pub struct CompilationOutputs<'id> {
    type_repo: TypeRepo<'id>,
    defined_types: Vec<TypeId<'id>>,
    codegen_queue: Vec<CodeGenItem<'id>>,
    reporter: ErrorReporter<'id>,
}

impl<'id> CompilationOutputs<'id> {
    pub fn commit(mut self, db: &mut CompilationDb<'id>, cache: &mut TypeCache, pool: &mut ConstantPool) {
        self.defined_types
            .sort_by_key(|typ| self.type_repo.upper_iter(*typ).count());

        for &item in &self.defined_types {
            match self.type_repo.get_type(item).unwrap() {
                DataType::Class(_) => {
                    db.classes.insert(item, pool.reserve());
                }
                DataType::Enum(_) => {
                    db.enums.insert(item, pool.reserve());
                }
                _ => {}
            }
        }

        for item in self.defined_types {
            Self::build_type(item, &self.type_repo, db, cache, pool);
        }

        for item in &self.codegen_queue {
            if let &CodeGenItem::AssembleGlobal(id, _, _) = item {
                let (sig, method) = self.type_repo.globals().get_overload(id.into()).unwrap();
                let idx =
                    Self::build_function(sig.clone(), &method.typ, true).commit_global(&self.type_repo, pool, cache);
                db.globals.insert(id, idx);
            }
        }

        for item in self.codegen_queue {
            match item {
                CodeGenItem::AssembleMethod(mid, params, body, is_static) => {
                    let &idx = if is_static {
                        db.statics.get(&mid).unwrap()
                    } else {
                        db.methods.get(&mid).unwrap()
                    };
                    let param_indices =
                        LocalIndices::new(params, pool.function(idx).unwrap().parameters.iter().copied().collect());
                    let (locals, code) = CodeGen::build_function(body, param_indices, &self.type_repo, db, pool, cache);
                    pool.complete_function(idx, locals.into_vec(), code).unwrap();
                }
                CodeGenItem::AssembleGlobal(gid, params, body) => {
                    let &idx = db.globals.get(&gid).unwrap();
                    let param_indices =
                        LocalIndices::new(params, pool.function(idx).unwrap().parameters.iter().copied().collect());
                    let (locals, code) = CodeGen::build_function(body, param_indices, &self.type_repo, db, pool, cache);
                    pool.complete_function(idx, locals.into_vec(), code).unwrap();
                }
            }
        }
    }

    fn build_type(
        id: TypeId<'id>,
        repo: &TypeRepo<'id>,
        db: &mut CompilationDb<'id>,
        cache: &mut TypeCache,
        pool: &mut ConstantPool,
    ) {
        let item = repo.get_type(id).unwrap();
        match item {
            DataType::Class(class_type) => {
                let &class_idx = db.classes.get(&id).unwrap();
                let base = class_type.extends.as_ref().and_then(|c| db.classes.get(&c.id)).copied();
                let fields = class_type.fields.iter().map(|entry| {
                    FieldBuilder::builder()
                        .name(entry.name.clone())
                        .typ(entry.typ.clone())
                        .build()
                });
                let methods = chain!(
                    class_type
                        .statics
                        .iter()
                        .map(|e| Self::build_function(e.signature.clone(), &e.function.typ, true)),
                    class_type.methods.iter().map(|e| Self::build_function(
                        e.signature.clone(),
                        &e.function.typ,
                        false
                    ))
                );
                let idx = ClassBuilder::builder()
                    .name(id.as_str())
                    .fields(fields)
                    .methods(methods)
                    .flags(
                        ClassFlags::new()
                            .with_is_abstract(class_type.is_abstract)
                            .with_is_struct(class_type.is_struct),
                    )
                    .build()
                    .commit_as(
                        class_idx,
                        base.unwrap_or_else(|| *db.classes.get(&predef::ISCRIPTABLE).unwrap()),
                        repo,
                        pool,
                        cache,
                    );

                let class = pool.class(idx).unwrap();
                for (entry, &idx) in class_type.fields.iter().zip(&class.fields) {
                    db.fields.insert(FieldId::new(id, entry.index), idx);
                }
                let mut ms = class.methods.iter();
                for (entry, &idx) in class_type.statics.iter().zip(ms.by_ref()) {
                    db.statics.insert(MethodId::new(id, entry.index), idx);
                }
                for (entry, &idx) in class_type.methods.iter().zip(ms.by_ref()) {
                    db.methods.insert(MethodId::new(id, entry.index), idx);
                }
            }
            DataType::Enum(enum_) => {
                let idx = EnumBuilder::builder()
                    .name(id.as_str())
                    .members(enum_.iter().map(|e| (e.name.clone(), e.value)))
                    .build()
                    .commit(pool);
                let class = pool.enum_(idx).unwrap();
                for (entry, &member) in enum_.iter().zip(&class.members) {
                    db.enum_members.insert(FieldId::new(id, entry.index), member);
                }
            }
            DataType::Builtin { .. } => {}
        }
    }

    fn build_function(signature: FuncSignature, typ: &FuncType<'id>, is_static: bool) -> FunctionBuilder<'id> {
        let params = typ.params.iter().enumerate().map(|(i, param)| {
            ParamBuilder::builder()
                .name(names::param(i))
                .typ(param.typ.clone())
                .flags(ParameterFlags::new().with_is_out(param.is_out))
                .build()
        });
        FunctionBuilder::builder()
            .flags(FunctionFlags::new().with_is_static(is_static).with_is_final(is_static))
            .visibility(Visibility::Public)
            .name(signature.into_str())
            .return_type(typ.ret.clone())
            .params(params)
            .build()
    }

    pub fn reporter(&self) -> &ErrorReporter<'id> {
        &self.reporter
    }

    pub fn into_errors(self) -> Vec<CompileError<'id>> {
        self.reporter.into_errors()
    }
}

#[derive(Debug, Default)]
pub struct CompilationDb<'id> {
    pub(crate) classes: HashMap<TypeId<'id>, PoolIndex<PoolClass>>,
    pub(crate) fields: HashMap<FieldId<'id>, PoolIndex<PoolField>>,
    pub(crate) methods: HashMap<MethodId<'id>, PoolIndex<PoolFunction>>,
    pub(crate) statics: HashMap<MethodId<'id>, PoolIndex<PoolFunction>>,
    pub(crate) globals: HashMap<GlobalId, PoolIndex<PoolFunction>>,
    pub(crate) enums: HashMap<TypeId<'id>, PoolIndex<PoolEnum>>,
    pub(crate) enum_members: HashMap<FieldId<'id>, PoolIndex<i64>>,
}

impl<'id> CompilationDb<'id> {
    fn load_class(
        &mut self,
        owner: TypeId<'id>,
        idx: PoolIndex<PoolClass>,
        pool: &ConstantPool,
        interner: &'id StringInterner,
    ) -> ClassType<'id> {
        self.classes.insert(owner, idx);
        let class = pool.class(idx).unwrap();
        let mut fields = FieldMap::default();
        for &idx in &class.fields {
            let name = pool.def_name(idx).unwrap();
            let field = pool.field(idx).unwrap();
            let typ = CompilationDb::load_type(field.type_, pool, interner);
            let index = fields.add(name.clone(), typ);
            self.fields.insert(FieldId::new(owner, index), idx);
        }

        let mut methods = FuncMap::default();
        let mut statics = FuncMap::default();
        for &pool_idx in &class.methods {
            let method = pool.function(pool_idx).unwrap();
            let (short_name, signature, ftyp) = CompilationDb::load_function(pool_idx, pool, interner);
            if method.flags.is_static() {
                let index = statics.add_with_signature(short_name, signature, ftyp, true);
                self.statics.insert(MethodId::new(owner, index), pool_idx);
            } else {
                let index = methods.add_with_signature(short_name, signature, ftyp, method.flags.is_final());
                self.methods.insert(MethodId::new(owner, index), pool_idx);
            }
        }

        ClassType {
            type_vars: [].into(),
            extends: None,
            fields,
            methods,
            statics,
            is_abstract: class.flags.is_abstract(),
            is_struct: class.flags.is_struct(),
        }
    }

    fn load_enum(&mut self, owner: TypeId<'id>, idx: PoolIndex<PoolEnum>, pool: &ConstantPool) -> EnumType {
        self.enums.insert(owner, idx);
        let enum_ = pool.enum_(idx).unwrap();
        let mut typ = EnumType::default();
        for &idx in &enum_.members {
            let name = pool.def_name(idx).unwrap();
            let value = pool.enum_value(idx).unwrap();
            let i = typ.add_member(name, value);
            self.enum_members.insert(FieldId::new(owner, i), idx);
        }
        typ
    }

    fn load_function(
        idx: PoolIndex<PoolFunction>,
        pool: &ConstantPool,
        interner: &'id StringInterner,
    ) -> (Str, FuncSignature, FuncType<'id>) {
        let name = pool.def_name(idx).unwrap();
        let func = pool.function(idx).unwrap();
        let ret = func
            .return_type
            .map_or(Type::Prim(Prim::Unit), |idx| Self::load_type(idx, pool, interner));
        let params = func
            .parameters
            .iter()
            .map(|&idx| {
                let param = pool.parameter(idx).unwrap();
                let typ = Self::load_type(param.type_, pool, interner);
                FuncParam::custom(typ, param.flags.is_out())
            })
            .collect();
        let short_name = name.split_once(';').map_or_else(|| name.clone(), |(s, _)| s.into());
        let ftyp = FuncType::new([].into(), params, ret);
        (short_name, FuncSignature::new(name), ftyp)
    }

    fn load_type(idx: PoolIndex<PoolType>, pool: &ConstantPool, interner: &'id StringInterner) -> Type<'id> {
        match pool.type_(idx).unwrap() {
            PoolType::Prim => {
                let str = pool.def_name(idx).unwrap();
                Type::Prim(Prim::from_str(&str).unwrap())
            }
            PoolType::Class => {
                let name = pool.def_name(idx).unwrap();
                let name = name.as_str();
                Type::Data(Parameterized::new(get_type_id(name, interner), Rc::new([])))
            }
            &PoolType::Ref(inner) => Self::load_type(inner, pool, interner),
            &PoolType::WeakRef(inner) => {
                let inner = Self::load_type(inner, pool, interner);
                Type::Data(Parameterized::new(predef::WREF, Rc::new([inner])))
            }
            &PoolType::ScriptRef(inner) => {
                let inner = Self::load_type(inner, pool, interner);
                Type::Data(Parameterized::new(predef::SCRIPT_REF, Rc::new([inner])))
            }
            &PoolType::Array(inner) | &PoolType::StaticArray(inner, _) => {
                let inner = Self::load_type(inner, pool, interner);
                Type::Data(Parameterized::new(predef::ARRAY, Rc::new([inner])))
            }
        }
    }
}

#[derive(Debug)]
pub struct CompilationResources<'id> {
    pub type_repo: TypeRepo<'id>,
    pub type_cache: TypeCache,
    pub db: CompilationDb<'id>,
}

impl<'id> CompilationResources<'id> {
    pub fn load(pool: &ConstantPool, interner: &'id StringInterner) -> Self {
        let mut type_repo = TypeRepo::default();
        let mut type_cache = TypeCache::default();
        let mut db = CompilationDb::default();

        for (idx, def) in pool.definitions() {
            match def.value {
                AnyDefinition::Type(_) => {
                    let mangled = pool.names.get(def.name).unwrap();
                    type_cache.add(mangled, idx.cast());
                }
                AnyDefinition::Class(_) => {
                    let name = pool.names.get(def.name).unwrap();
                    let owner = get_type_id(&name, interner);
                    let class = db.load_class(owner, idx.cast(), pool, interner);
                    type_repo.add_type(owner, DataType::Class(class));
                }
                AnyDefinition::Function(_) if def.parent.is_undefined() => {
                    let (name, sig, ftyp) = CompilationDb::load_function(idx.cast(), pool, interner);
                    let id = type_repo
                        .globals_mut()
                        .add_with_signature(ScopedName::top_level(name), sig, ftyp, true);
                    db.globals.insert(GlobalId::new(id), idx.cast());
                }
                AnyDefinition::Enum(_) => {
                    let name = pool.names.get(def.name).unwrap();
                    let owner = get_type_id(&name, interner);
                    let enum_ = db.load_enum(owner, idx.cast(), pool);
                    type_repo.add_type(owner, DataType::Enum(enum_));
                }
                _ => {}
            }
        }
        Self {
            type_repo,
            type_cache,
            db,
        }
    }
}

fn generate_type_id<'id>(name: &Str, path: &ModulePath, interner: &'id StringInterner) -> TypeId<'id> {
    if path.is_empty() {
        return get_type_id(name, interner);
    }
    let str = path.iter().chain(Some(name)).join(".");
    TypeId::from_interned(interner.intern(str))
}

fn get_type_id<'id>(name: &str, interner: &'id StringInterner) -> TypeId<'id> {
    TypeId::get_predefined_by_name(name).unwrap_or_else(|| TypeId::from_interned(interner.intern(name)))
}

#[derive(Debug)]
struct Module<'id> {
    names: HashMap<Str, Vec<Global>>,
    types: HashMap<Str, TypeId<'id>>,
    items: Vec<ModuleItem<'id>>,
}

#[derive(Debug)]
enum ModuleItem<'id> {
    Class(TypeId<'id>, HashMap<Str, InferType<'id>>, Vec<CompileBody<'id>>),
    Global(CompileBody<'id>),
}

#[derive(Debug)]
struct CompileBody<'id> {
    index: OverloadIndex,
    env: HashMap<Str, InferType<'id>>,
    parameters: Vec<ParameterSource>,
    body: Seq<SourceAst>,
    is_static: bool,
}

#[derive(Debug)]
enum CodeGenItem<'id> {
    AssembleMethod(MethodId<'id>, IndexMap<Local, Type<'id>>, Seq<CheckedAst<'id>>, bool),
    AssembleGlobal(GlobalId, IndexMap<Local, Type<'id>>, Seq<CheckedAst<'id>>),
}

#[derive(Debug, Clone)]
enum ImportItem<'id> {
    Type(TypeId<'id>),
    Func(FuncIndex),
}

#[derive(Debug, Default)]
struct ModuleMap<'id> {
    map: SequenceTrie<Str, ImportItem<'id>, RandomState>,
}

impl<'id> ModuleMap<'id> {
    pub fn get_direct_descendants<'this>(
        &'this self,
        path: impl IntoIterator<Item = &'this Str> + 'this,
    ) -> Option<impl Iterator<Item = ImportItem<'id>> + 'this> {
        let node = self.map.get_node(path)?;
        Some(node.children().filter_map(SequenceTrie::value).cloned())
    }

    #[inline]
    pub fn get<'this>(&'this self, path: impl IntoIterator<Item = &'this Str> + 'this) -> Option<ImportItem<'id>> {
        self.map.get(path).cloned()
    }

    pub fn add_function(&mut self, name: &ScopedName, f: FuncIndex) {
        self.map.insert_owned(name.as_parts().cloned(), ImportItem::Func(f));
    }

    pub fn add_type(&mut self, typ: TypeId<'id>) {
        self.map
            .insert_owned(typ.as_parts().map(Str::from), ImportItem::Type(typ));
    }
}
