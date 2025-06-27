use std::borrow::Cow;
use std::cell::Cell;
use std::hash::{BuildHasher, Hash};
use std::{fmt, mem, ops};

use hashbrown::HashMap;
use identity_hash::BuildIdentityHasher;
use indexmap::Equivalent;
use redscript_compiler_frontend::ast::{FileId, SourceMap, Span};
use redscript_compiler_frontend::utils::ScopedMap;
use redscript_compiler_frontend::{
    Field, FreeFunction, FreeFunctionIndex, FunctionIndex, FunctionKind, LoweredCompilationUnit,
    LoweredFunction, Method, MethodId, MonoType, Param, QualifiedName, Symbols, TypeId, TypeSchema,
    ir, predef,
};
use redscript_io::{
    Class as PoolClass, ClassFlags as PoolClassFlags, ClassIndex as PoolClassIndex,
    Enum as PoolEnum, EnumIndex as PoolEnumIndex, EnumMember as PoolEnumMember, Field as PoolField,
    FieldFlags as PoolFieldFlags, FieldIndex as PoolFieldIndex, Function as PoolFunction,
    FunctionFlags as PoolFunctionFlags, FunctionIndex as PoolFunctionIndex, Instr,
    LocalIndex as PoolLocalIndex, Parameter as PoolParameter, ParameterFlags as PoolParameterFlags,
    ParameterIndex as PoolParameterIndex, Property, ScriptBundle, SourceFile,
    SourceFileIndex as PoolSourceFileIndex, SourceReference, Type as PoolType,
    TypeIndex as PoolTypeIndex, TypeKind as PoolTypeKind, Visibility,
};
use smallvec::{SmallVec, smallvec};

use crate::IndexMap;
use crate::assemble::{AssembleError, assemble_block};
use crate::inputs::{ClassMappings, Signature};

pub struct Monomorphizer<'ctx> {
    sources: &'ctx SourceMap,
    files: HashMap<FileId, PoolSourceFileIndex, BuildIdentityHasher<i32>>,

    classes: IncrementalMap<MonoType<'ctx>, ClassMappings>,
    enums: IndexMap<TypeId<'ctx>, PoolEnumIndex>,
    methods: IncrementalMap<Signature<'ctx, MethodWithReceiver<'ctx>>, PoolFunctionIndex>,
    functions: IncrementalMap<Signature<'ctx, FreeFunctionIndex>, PoolFunctionIndex>,
    types: IndexMap<MonoType<'ctx>, PoolTypeIndex>,
}

impl<'ctx> Monomorphizer<'ctx> {
    pub fn new(
        sources: &'ctx SourceMap,
        classes: IndexMap<MonoType<'ctx>, ClassMappings>,
        enums: IndexMap<TypeId<'ctx>, PoolEnumIndex>,
        functions: IndexMap<Signature<'ctx, FreeFunctionIndex>, PoolFunctionIndex>,
        types: IndexMap<MonoType<'ctx>, PoolTypeIndex>,
    ) -> Self {
        Self {
            sources,
            files: HashMap::default(),

            classes: IncrementalMap::from_predefined(classes),
            enums,
            methods: IncrementalMap::default(),
            functions: IncrementalMap::from_predefined(functions),
            types,
        }
    }

    pub fn monomorphize(
        &mut self,
        unit: &LoweredCompilationUnit<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        define_predefs(bundle);

        for &id in &unit.enums {
            self.add_enum_to_pool(id, symbols, bundle);
        }

        for (&id, _) in &unit.classes {
            if symbols[id].params().is_empty() {
                self.class(&MonoType::nullary(id), symbols, bundle);
            }
        }

        for (&idx, _) in &unit.functions {
            if symbols[idx].type_().type_params().is_empty() {
                self.function(Signature::new(idx, []), symbols, bundle);
            }
        }

        self.process_added_fields(unit, symbols, bundle)?;
        self.process_added_methods(unit, symbols, bundle)?;
        self.process_replaced_methods(unit, symbols, bundle)?;
        self.process_wrapped_methods(unit, symbols, bundle)?;

        self.process_backlog(symbols, unit, bundle)
    }

    fn process_backlog(
        &mut self,
        symbols: &Symbols<'ctx>,
        unit: &LoweredCompilationUnit<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        while self.has_backlog() {
            while let Some((typ, class)) = self.classes.next_unmarked() {
                let repr = &unit.classes[&typ.id()];
                let aggregate = symbols[typ.id()]
                    .schema()
                    .as_aggregate()
                    .expect("every class should be an aggregate");

                let methods = aggregate
                    .methods()
                    .iter()
                    .filter(|entry| {
                        !entry.func().flags().is_unimplemented()
                            && !entry.func().flags().is_native()
                            && should_method_monomorphize(entry.func(), typ)
                    })
                    .map(|entry| (class.methods()[entry.key()], &repr.methods[entry.key()]))
                    .collect::<Vec<_>>();

                for (idx, default) in &repr.fields {
                    let mangled = MangledConstant::new(default, symbols);
                    let field_idx = bundle[class.index()].fields_mut()[usize::from(*idx)];
                    bundle[field_idx]
                        .set_defaults([Property::new(typ.id().as_str(), mangled.to_string())]);
                }

                let env = typ.type_env(symbols);
                methods.into_iter().try_for_each(|(index, func)| {
                    self.function_body(index, func, &env, symbols, bundle)
                })?;
            }

            while let Some((sig, &index)) = self.methods.next_unmarked() {
                let func = &symbols[sig.id().method()];
                if func.flags().is_native() {
                    continue;
                }

                let repr = &unit.classes[&sig.id().receiver.id()].methods[&sig.id().index];

                let mut env = sig.id().receiver.type_env(symbols);
                env.extend(func.type_().type_vars().zip(sig.types().iter().cloned()));

                self.function_body(index, repr, &env, symbols, bundle)?;
            }

            while let Some((sig, &index)) = self.functions.next_unmarked() {
                let func = &symbols[*sig.id()];
                if func.flags().is_native() {
                    continue;
                }
                let repr = &unit.functions[sig.id()];
                let env = func
                    .type_()
                    .type_vars()
                    .zip(sig.types().iter().cloned())
                    .collect();
                self.function_body(index, repr, &env, symbols, bundle)?;
            }
        }
        Ok(())
    }

    pub fn function(
        &mut self,
        sig: Signature<'ctx, FreeFunctionIndex>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolFunctionIndex {
        if let Some(&idx) = self.functions.get(&sig) {
            return idx;
        }
        let (name, func) = symbols
            .get_free_function(*sig.id())
            .expect("signature function should exist");

        let env = func
            .type_()
            .type_vars()
            .zip(sig.types().iter().cloned())
            .collect();

        let return_t = func.type_().return_type().assume_mono(&env);
        let return_t = if return_t.id() == predef::VOID {
            None
        } else {
            Some(self.type_(&return_t, symbols, bundle))
        };

        let mangled = MangledFreeFunction::new(name, &sig, func, &env).to_string();
        let cname = bundle.cnames_mut().add(mangled);
        let flags = PoolFunctionFlags::default()
            .with_is_static(true)
            .with_is_exec(func.flags().is_exec())
            .with_is_final(true)
            .with_is_native(func.flags().is_native());
        let source = func
            .span()
            .filter(|_| !flags.is_native())
            .map(|span| self.source_ref(span, bundle));
        let def = PoolFunction::new(cname, Visibility::Public, flags)
            .with_return_type(return_t)
            .with_source(source);

        let idx = bundle.define_and_init(def, |bundle, idx, f| {
            let params = func
                .type_()
                .params()
                .iter()
                .map(|param| self.add_parameter_to_pool(param, idx, &env, symbols, bundle))
                .collect::<Vec<_>>();
            f.with_parameters(params)
        });

        self.functions.insert(sig, idx);
        idx
    }

    pub fn method(
        &mut self,
        sig: Signature<'ctx, MethodWithReceiver<'ctx>>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolFunctionIndex {
        if let Some(&idx) = self.methods.get(&sig) {
            return idx;
        }

        let parent = self.class(&sig.id().receiver, symbols, bundle);

        let (name, method) = symbols
            .get_method(sig.id().method())
            .expect("signature method should exist");

        let mut env = sig.id().receiver.type_env(symbols);
        env.extend(method.type_().type_vars().zip(sig.types().iter().cloned()));

        let base = method.overloaded().map(|id| &symbols[id]);
        let mangled = MangledMethod::new(name, &sig, method, base, &env).to_string();
        let idx = self.add_method_to_pool(mangled, method, parent, &env, symbols, bundle);

        self.methods.insert(sig, idx);
        bundle[parent].add_method(idx);

        idx
    }

    pub fn mono_method(
        &mut self,
        typ: &MonoType<'ctx>,
        id: FunctionIndex,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolFunctionIndex {
        self.class(typ, symbols, bundle);
        self.classes[typ].methods()[&id]
    }

    pub fn class(
        &mut self,
        typ: &MonoType<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolClassIndex {
        let mut types: SmallVec<[_; 1]> = smallvec![];
        let mut current = Cow::Borrowed(typ);

        let root = loop {
            let class = self.classes.get(current.as_ref());
            if let Some(class) = class {
                break class.index();
            }
            match current.instantiate_base(symbols) {
                Some(next) => {
                    types.push(current);
                    current = Cow::Owned(next);
                }
                None => break self.add_class_to_pool(current.into_owned(), None, symbols, bundle),
            }
        };

        types.into_iter().rev().fold(root, |base, typ| {
            self.add_class_to_pool(typ.into_owned(), Some(base), symbols, bundle)
        })
    }

    pub fn type_(
        &mut self,
        typ: &MonoType<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolTypeIndex {
        fn define_class<'ctx>(
            arg: &MonoType<'ctx>,
            bundle: &mut ScriptBundle<'ctx>,
        ) -> PoolTypeIndex {
            let inner_name = bundle.cnames_mut().add(arg.to_string());
            bundle.define(PoolType::new(inner_name, PoolTypeKind::Class))
        }

        fn define_type<'ctx>(
            typ: &MonoType<'ctx>,
            pool_type: PoolTypeKind,
            symbols: &Symbols<'ctx>,
            bundle: &mut ScriptBundle<'ctx>,
        ) -> PoolTypeIndex {
            let cname = bundle
                .cnames_mut()
                .add(MangledType::new(typ, symbols).to_string());
            bundle.define(PoolType::new(cname, pool_type))
        }

        let schema = symbols[typ.id()].schema();

        match schema {
            TypeSchema::Aggregate(agg)
                if !agg.flags().is_struct() && !agg.flags().is_never_ref() =>
            {
                return self.type_(&MonoType::new(predef::REF, [typ.clone()]), symbols, bundle);
            }
            _ => {}
        }

        if let Some(&idx) = self.types.get(typ) {
            return idx;
        }

        let idx = match schema {
            TypeSchema::Primitive => match (typ.id(), typ.args()) {
                (id, [arg]) if id == predef::REF => {
                    self.class(arg, symbols, bundle);

                    let class = define_class(arg, bundle);
                    define_type(typ, PoolTypeKind::Ref(class), symbols, bundle)
                }
                (id, [arg]) if id == predef::WREF => {
                    self.class(arg, symbols, bundle);

                    let class = define_class(arg, bundle);
                    define_type(typ, PoolTypeKind::WeakRef(class), symbols, bundle)
                }
                (id, [arg]) if id == predef::SCRIPT_REF => {
                    let arg = self.type_(arg, symbols, bundle);
                    define_type(typ, PoolTypeKind::ScriptRef(arg), symbols, bundle)
                }
                (id, [arg]) if id == predef::ARRAY => {
                    let arg = self.type_(arg, symbols, bundle);
                    define_type(typ, PoolTypeKind::Array(arg), symbols, bundle)
                }
                (id, args) => {
                    if let ([arg], Some(size)) = (args, id.static_array_size()) {
                        let element_type = self.type_(arg, symbols, bundle);
                        let array_t = PoolTypeKind::StaticArray {
                            element_type,
                            size: size.into(),
                        };
                        define_type(typ, array_t, symbols, bundle)
                    } else {
                        define_class(typ, bundle)
                    }
                }
            },
            _ => define_class(typ, bundle),
        };
        self.types.insert(typ.clone(), idx);

        idx
    }

    fn function_body(
        &mut self,
        index: PoolFunctionIndex,
        func: &LoweredFunction<'ctx>,
        env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        let locals = func.locals.iter().cloned();
        let code = assemble_block(index, locals, [], &func.block, symbols, env, bundle, self)?;

        bundle[index].set_code(code);
        Ok(())
    }

    pub(super) fn source_ref(
        &mut self,
        span: Span,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> SourceReference {
        let file = self
            .sources
            .get(span.file)
            .expect("source file should exist");
        let (line, _) = file.line_and_offset(span.start);
        let line = line as u32 + 1; // lines are 1-indexed
        let idx = if let Some(&idx) = self.files.get(&span.file) {
            idx
        } else {
            let index = 0xCAFE_BABE + self.files.len() as u32;
            let path_hash = self.functions.map.hasher().hash_one(file.path());
            let lower_hash = path_hash as u32;
            let upper_hash = (path_hash >> 32) as u32;

            let file =
                SourceFile::new(index, lower_hash, upper_hash, file.path().to_string_lossy());
            let idx = bundle.define(file);
            self.files.insert(span.file, idx);
            idx
        };
        SourceReference::new(idx, line)
    }

    pub(super) fn source_line(&self, span: Span) -> u16 {
        let file = self
            .sources
            .get(span.file)
            .expect("source file should exist");
        file.line_and_offset(span.start).0 as u16 + 1 // lines are 1-indexed
    }

    fn process_added_fields(
        &mut self,
        unit: &LoweredCompilationUnit<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        let env = ScopedMap::default();

        for (id, expr) in &unit.added_fields {
            let (name, field) = symbols.get_field(*id).expect("field should exist");
            let class = self.classes[&MonoType::nullary(id.parent())].index();
            let idx = self.add_field_to_pool(name, field, class, &env, symbols, bundle);
            bundle[class].add_field(idx);

            if let Some(default) = expr {
                let default = MangledConstant::new(default, symbols).to_string();
                bundle[idx].set_defaults([Property::new(id.parent().as_str(), default)]);
            }
        }

        Ok(())
    }

    fn process_added_methods(
        &mut self,
        unit: &LoweredCompilationUnit<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        let env = ScopedMap::default();

        for (id, _) in &unit.added_methods {
            let parent_t = MonoType::nullary(id.parent());
            let class_idx = self.classes[&parent_t].index();
            let (name, func) = symbols.get_method(*id).expect("added method should exist");
            let sig = Signature::new((), []);
            let mangled = MangledMethod::new(name, &sig, func, None, &env).to_string();
            let idx = self.add_method_to_pool(mangled, func, class_idx, &env, symbols, bundle);
            bundle[class_idx].add_method(idx);

            self.classes[&parent_t]
                .methods_mut()
                .insert(id.index(), idx);
        }

        for (id, body) in &unit.added_methods {
            let Some(body) = body else { continue };
            let parent_t = MonoType::nullary(id.parent());
            let index = self.classes[&parent_t].methods()[&id.index()];
            self.function_body(index, body, &env, symbols, bundle)?;
        }

        Ok(())
    }

    fn process_replaced_methods(
        &mut self,
        unit: &LoweredCompilationUnit<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        let env = ScopedMap::default();
        for (id, func) in &unit.method_replacements {
            let parent_t = MonoType::nullary(id.parent());
            let index = self.classes[&parent_t].methods()[&id.index()];
            self.function_body(index, func, &env, symbols, bundle)?;
        }
        Ok(())
    }

    fn process_wrapped_methods(
        &mut self,
        unit: &LoweredCompilationUnit<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> Result<(), AssembleError<'ctx>> {
        let env = ScopedMap::default();

        for (&id, funcs) in &unit.method_wrappers {
            let parent_t = MonoType::nullary(id.parent());
            let class = &self.classes[&parent_t];
            let mut index = class.methods()[&id.index()];
            let class_index = class.index();

            let wrapped = bundle[index].clone();
            let wrapped_name = wrapped.name();

            for (i, func) in funcs.iter().rev().enumerate() {
                let name = format!("wrapper{i}${}", bundle[wrapped_name]);
                let cname = bundle.cnames_mut().add(name);
                let flags = PoolFunctionFlags::default()
                    .with_is_static(wrapped.flags().is_static())
                    .with_is_timer(wrapped.flags().is_timer())
                    .with_is_final(wrapped.flags().is_final())
                    .with_is_quest(wrapped.flags().is_quest())
                    .with_is_callback(wrapped.flags().is_callback() && i == 0);

                let source = func.block.span().map(|span| self.source_ref(span, bundle));
                let def = PoolFunction::new(cname, wrapped.visibility(), flags)
                    .with_class(wrapped.class())
                    .with_base_method(wrapped.base_method().filter(|_| i == 0))
                    .with_return_type(wrapped.return_type())
                    .with_source(source);
                let next = bundle.define_and_init(def, |bundle, next, def| {
                    let parameters = wrapped
                        .parameters()
                        .iter()
                        .map(|&i| bundle.define(bundle[i].clone().with_function(next)))
                        .collect::<Vec<_>>();
                    def.with_parameters(parameters)
                });
                bundle[class_index].add_method(next);

                self.classes[&parent_t].set_alias(id.index(), next);
                self.function_body(index, func, &env, symbols, bundle)?;

                index = next;
            }

            // annoying workaround which is necessary to prevent a game crash which occurs when
            // the function locals and params appear before the function that defines them in
            // the pool
            let params = mem::take(bundle[index].parameters_mut());
            let locals = wrapped
                .locals()
                .iter()
                .map(|&i| bundle.define(bundle[i].clone().with_function(index)))
                .collect::<Vec<_>>();

            let local_map: HashMap<PoolLocalIndex, PoolLocalIndex, BuildIdentityHasher<u32>> =
                wrapped
                    .locals()
                    .iter()
                    .copied()
                    .zip(locals.iter().copied())
                    .collect();
            let param_map: HashMap<
                PoolParameterIndex,
                PoolParameterIndex,
                BuildIdentityHasher<u32>,
            > = wrapped
                .parameters()
                .iter()
                .copied()
                .zip(params.iter().copied())
                .collect();

            let code = wrapped
                .body()
                .code_iter()
                .map(|i| match i? {
                    Instr::Local(l) => Ok(Instr::Local(local_map[&l])),
                    Instr::Param(p) => Ok(Instr::Param(param_map[&p])),
                    other => Ok(other),
                })
                .collect::<Result<Vec<_>, AssembleError<'ctx>>>()?;

            let flags = wrapped.flags();

            bundle[index] = wrapped
                .with_name(bundle[index].name())
                .with_flags(flags.with_is_callback(false))
                .with_locals(locals)
                .with_parameters(params)
                .with_code(code);
        }

        Ok(())
    }

    fn add_class_to_pool(
        &mut self,
        typ: MonoType<'ctx>,
        base: Option<PoolClassIndex>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolClassIndex {
        let schema = symbols[typ.id()].schema();
        let aggregate = schema
            .as_aggregate()
            .expect("every class should be an aggregate");

        let flags = PoolClassFlags::default()
            .with_is_native(aggregate.flags().is_native())
            .with_is_abstract(aggregate.flags().is_abstract())
            .with_is_final(aggregate.flags().is_final())
            .with_is_import_only(aggregate.flags().is_import_only())
            .with_is_struct(aggregate.flags().is_struct());

        let name = aggregate
            .named_implementation(&typ)
            .map_or_else(|| typ.to_string(), ToString::to_string);
        let cname = bundle.cnames_mut().add(name);
        let class = PoolClass::new(cname, Visibility::Public, flags).with_base(base);
        let idx = bundle.define(class);

        self.classes
            .insert(typ.clone(), ClassMappings::new(idx, IndexMap::default()));

        let class_env = typ.type_env(symbols);

        for entry in aggregate.fields().iter() {
            let name = entry.name();
            let field =
                self.add_field_to_pool(name, entry.field(), idx, &class_env, symbols, bundle);
            bundle[idx].add_field(field);
        }

        let mut methods = IndexMap::default();
        for entry in aggregate.methods().iter() {
            let func = entry.func();
            if !should_method_monomorphize(func, &typ) {
                continue;
            }

            let sig = Signature::new((), []);
            let base = func.overloaded().map(|id| &symbols[id]);
            let mangled =
                MangledMethod::new(entry.name(), &sig, func, base, &class_env).to_string();
            let method = self.add_method_to_pool(mangled, func, idx, &class_env, symbols, bundle);

            bundle[idx].add_method(method);
            methods.insert(*entry.key(), method);
        }

        mem::swap(self.classes[&typ].methods_mut(), &mut methods);

        idx
    }

    pub fn add_enum_to_pool(
        &mut self,
        id: TypeId<'ctx>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) {
        let name = bundle.cnames_mut().add(id.as_str());
        let schema = symbols[id].schema();
        let enum_ = schema
            .as_enum()
            .expect("every enum should have enum schema");
        let (min, max) = enum_
            .variants()
            .fold((0, 0), |(min, max), (_, val)| (min.min(val), max.max(val)));
        let variants = enum_.variants();

        let size = match (min, max) {
            (-128..=127, -128..=127) => 1,
            (-32_768..=32_767, -32_768..=32_767) => 2,
            (-2_147_483_648..=2_147_483_647, -2_147_483_648..=2_147_483_647) => 4,
            _ => 8,
        };

        let enum_ = PoolEnum::new(name, Visibility::Public, size);

        let idx = bundle.define_and_init(enum_, |bundle, idx, enum_| {
            let values = variants
                .map(|(name, val)| {
                    let name = bundle.cnames_mut().add(name);
                    bundle.define(PoolEnumMember::new(name, idx, val))
                })
                .collect::<Vec<_>>();
            enum_.with_values(values)
        });

        self.enums.insert(id, idx);
    }

    fn add_method_to_pool(
        &mut self,
        name: String,
        method: &Method<'ctx>,
        parent: PoolClassIndex,
        env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolFunctionIndex {
        let cname = bundle.cnames_mut().add(name);

        let return_t = method.type_().return_type().assume_mono(env);
        let return_t = if return_t.id() == predef::VOID || return_t.id() == predef::NOTHING {
            None
        } else {
            Some(self.type_(&return_t, symbols, bundle))
        };

        let flags = PoolFunctionFlags::default()
            .with_is_static(method.flags().is_static())
            .with_is_final(method.flags().is_final())
            .with_is_native(method.flags().is_native())
            .with_is_callback(method.flags().is_callback());

        let source = method
            .span()
            .filter(|_| !flags.is_native())
            .map(|span| self.source_ref(span, bundle));
        let def = PoolFunction::new(cname, Visibility::Public, flags)
            .with_class(Some(parent))
            .with_return_type(return_t)
            .with_source(source);

        bundle.define_and_init(def, |bundle, idx, func| {
            let params = method
                .type_()
                .params()
                .iter()
                .map(|param| self.add_parameter_to_pool(param, idx, env, symbols, bundle))
                .collect::<Vec<_>>();
            func.with_parameters(params)
        })
    }

    fn add_parameter_to_pool(
        &mut self,
        param: &Param<'ctx>,
        function: PoolFunctionIndex,
        env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolParameterIndex {
        let name = bundle.cnames_mut().add(param.name());
        let resolved = param.type_().assume_mono(env);
        let typ = self.type_(&resolved, symbols, bundle);
        let flags = PoolParameterFlags::default()
            .with_is_optional(param.flags().is_optional())
            .with_is_out(param.flags().is_out());
        bundle.define(PoolParameter::new(name, function, typ, flags))
    }

    fn add_field_to_pool(
        &mut self,
        name: &'ctx str,
        field: &Field<'ctx>,
        class: PoolClassIndex,
        env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
        symbols: &Symbols<'ctx>,
        bundle: &mut ScriptBundle<'ctx>,
    ) -> PoolFieldIndex {
        let cname = bundle.cnames_mut().add(name);
        let typ = field.type_().assume_mono(env);
        let typ = self.type_(&typ, symbols, bundle);
        let flags = PoolFieldFlags::default()
            .with_is_native(field.flags().is_native())
            .with_is_editable(field.flags().is_editable())
            .with_is_inline(field.flags().is_inline())
            .with_is_const(field.flags().is_const())
            .with_is_persistent(field.flags().is_persistent());
        let attrs = field
            .properties()
            .iter()
            .map(|(k, v)| Property::new(k.clone(), v.clone()))
            .collect::<Vec<_>>();
        let def =
            PoolField::new(cname, class, Visibility::Public, typ, flags).with_attributes(attrs);
        bundle.define(def)
    }

    #[inline]
    pub fn existing_enum(&self, typ: TypeId<'ctx>) -> Option<PoolEnumIndex> {
        self.enums.get(&typ).copied()
    }

    #[inline]
    pub fn existing_type(&self, typ: &MonoType<'ctx>) -> Option<PoolTypeIndex> {
        self.types.get(typ).copied()
    }

    pub fn existing_alias(&self, id: MethodId<'ctx>) -> Option<PoolFunctionIndex> {
        self.classes
            .get(&MonoType::nullary(id.parent()))?
            .aliases()
            .get(&id.index())
            .copied()
    }

    #[inline]
    fn has_backlog(&self) -> bool {
        self.classes.has_backlog() || self.methods.has_backlog() || self.functions.has_backlog()
    }
}

fn define_predefs(bundle: &mut ScriptBundle<'_>) {
    let nothing_name = bundle.cnames_mut().add("Nothing");
    bundle.define(PoolClass::new(
        nothing_name,
        Visibility::Public,
        PoolClassFlags::default()
            .with_is_abstract(true)
            .with_is_struct(true),
    ));

    let nothing_name = bundle.cnames_mut().add("Void");
    bundle.define(PoolClass::new(
        nothing_name,
        Visibility::Public,
        PoolClassFlags::default()
            .with_is_abstract(true)
            .with_is_struct(true),
    ));
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MethodWithReceiver<'ctx> {
    receiver: MonoType<'ctx>,
    index: FunctionIndex,
}

impl<'ctx> MethodWithReceiver<'ctx> {
    #[inline]
    pub fn new(receiver: MonoType<'ctx>, index: FunctionIndex) -> Self {
        Self { receiver, index }
    }

    pub fn method(&self) -> MethodId<'ctx> {
        MethodId::new(self.receiver.id(), self.index)
    }
}

#[derive(Debug)]
struct IncrementalMap<K, V> {
    map: IndexMap<K, V>,
    pos: Cell<usize>,
}

impl<K, V> IncrementalMap<K, V> {
    fn from_predefined(map: IndexMap<K, V>) -> Self {
        Self {
            pos: Cell::new(map.len()),
            map,
        }
    }

    #[inline]
    fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: Equivalent<K> + Hash,
    {
        self.map.get(key)
    }

    #[inline]
    fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Eq + Hash,
    {
        self.map.insert(key, value)
    }

    #[inline]
    fn next_unmarked(&self) -> Option<(&K, &V)> {
        let next = self.map.get_index(self.pos.get())?;
        self.pos.set(self.pos.get() + 1);
        Some(next)
    }

    #[inline]
    fn has_backlog(&self) -> bool {
        self.pos.get() < self.map.len()
    }
}

impl<K, V> Default for IncrementalMap<K, V> {
    fn default() -> Self {
        Self {
            map: IndexMap::default(),
            pos: Cell::new(0),
        }
    }
}

impl<K: Hash + Eq, V> ops::Index<&K> for IncrementalMap<K, V> {
    type Output = V;

    #[inline]
    fn index(&self, index: &K) -> &Self::Output {
        &self.map[index]
    }
}

impl<K: Hash + Eq, V> ops::IndexMut<&K> for IncrementalMap<K, V> {
    #[inline]
    fn index_mut(&mut self, index: &K) -> &mut Self::Output {
        &mut self.map[index]
    }
}

pub fn should_method_monomorphize(m: &Method<'_>, this: &MonoType<'_>) -> bool {
    (!m.flags().is_static() || this.args().is_empty()) && m.type_().type_params().is_empty()
}

#[derive(Debug)]
struct MangledType<'a, 'ctx> {
    typ: &'a MonoType<'ctx>,
    symbols: &'a Symbols<'ctx>,
}

impl<'a, 'ctx> MangledType<'a, 'ctx> {
    #[inline]
    fn new(typ: &'a MonoType<'ctx>, symbols: &'a Symbols<'ctx>) -> Self {
        Self { typ, symbols }
    }
}

impl fmt::Display for MangledType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.symbols[self.typ.id()].schema() {
            TypeSchema::Aggregate(agg) => {
                if !agg.flags().is_struct() && !agg.flags().is_never_ref() {
                    write!(f, "ref:")?;
                }
                if let Some(name) = agg.named_implementation(self.typ) {
                    write!(f, "{name}")
                } else {
                    write!(f, "{}", self.typ)
                }
            }
            TypeSchema::Enum(_) => {
                write!(f, "{}", self.typ)
            }
            TypeSchema::Primitive => match (
                self.typ.id(),
                self.typ.id().static_array_size(),
                self.typ.args(),
            ) {
                (_, Some(size), [arg]) => {
                    write!(f, "{}[{}]", MangledType::new(arg, self.symbols), size)
                }
                (id, _, [arg]) if id == predef::REF || id == predef::WREF => {
                    write!(f, "{id}:{arg}")
                }
                (id, _, [arg]) => write!(f, "{id}:{}", MangledType::new(arg, self.symbols)),
                (id, _, _) => write!(f, "{id}"),
            },
        }
    }
}

#[derive(Debug)]
struct MangledFreeFunction<'a, 'ctx, K> {
    name: &'a QualifiedName<'ctx>,
    signature: &'a Signature<'ctx, K>,
    func: &'a FreeFunction<'ctx>,
    env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
}

impl<'a, 'ctx, K> MangledFreeFunction<'a, 'ctx, K> {
    fn new(
        name: &'a QualifiedName<'ctx>,
        signature: &'a Signature<'ctx, K>,
        func: &'a FreeFunction<'ctx>,
        env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
    ) -> Self {
        Self {
            name,
            signature,
            func,
            env,
        }
    }
}

impl<K> fmt::Display for MangledFreeFunction<'_, '_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.name, self.signature.display_args())?;
        if self.func.flags().is_exec() || self.func.flags().is_native() {
            return Ok(());
        };
        write!(f, ";{}", MangledParams::new(self.func, self.env))
    }
}

#[derive(Debug)]
struct MangledMethod<'a, 'ctx, K> {
    name: &'a str,
    signature: &'a Signature<'ctx, K>,
    method: &'a Method<'ctx>,
    base: Option<&'a Method<'ctx>>,
    env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
}

impl<'a, 'ctx, K> MangledMethod<'a, 'ctx, K> {
    fn new(
        name: &'a str,
        signature: &'a Signature<'ctx, K>,
        method: &'a Method<'ctx>,
        base: Option<&'a Method<'ctx>>,
        env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
    ) -> Self {
        Self {
            name,
            signature,
            method,
            base,
            env,
        }
    }
}

impl<K> fmt::Display for MangledMethod<'_, '_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.name, self.signature.display_args())?;
        if self.base.is_some_and(|m| m.flags().is_native())
            || self.method.flags().is_callback()
            || self.method.flags().is_native()
        {
            return Ok(());
        };
        write!(f, ";{}", MangledParams::new(self.method, self.env))
    }
}

#[derive(Debug)]
struct MangledParams<'a, 'ctx, F> {
    func: F,
    env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>,
}

impl<'a, 'ctx, F> MangledParams<'a, 'ctx, F> {
    fn new(func: F, env: &'a ScopedMap<'a, &'ctx str, MonoType<'ctx>>) -> Self {
        Self { func, env }
    }
}

impl<'ctx, F: FunctionKind<'ctx>> fmt::Display for MangledParams<'_, 'ctx, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.func.type_().params().iter().try_for_each(|param| {
            if param.flags().is_out() {
                write!(f, "Out")?;
            }
            let typ = param.type_().unwrap_ref_or_self().assume_mono(self.env);
            write!(f, "{typ}",)
        })
    }
}

#[derive(Debug)]
struct MangledConstant<'a, 'ctx> {
    const_: &'a ir::Const<'ctx>,
    symbols: &'a Symbols<'ctx>,
}

impl<'a, 'ctx> MangledConstant<'a, 'ctx> {
    fn new(const_: &'a ir::Const<'ctx>, symbols: &'a Symbols<'ctx>) -> Self {
        Self { const_, symbols }
    }
}

impl fmt::Display for MangledConstant<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.const_ {
            ir::Const::Str(cow)
            | ir::Const::CName(cow)
            | ir::Const::ResRef(cow)
            | ir::Const::TweakDbId(cow) => write!(f, "{cow}"),
            ir::Const::EnumVariant(field_id) => {
                let (name, _) = self
                    .symbols
                    .get_enum_variant(*field_id)
                    .expect("field should exist");
                write!(f, "{}.{}", field_id.parent(), name)
            }
            ir::Const::F32(val) => write!(f, "{val}"),
            ir::Const::F64(val) => write!(f, "{val}"),
            ir::Const::I8(val) => write!(f, "{val}"),
            ir::Const::I16(val) => write!(f, "{val}"),
            ir::Const::I32(val) => write!(f, "{val}"),
            ir::Const::I64(val) => write!(f, "{val}"),
            ir::Const::U8(val) => write!(f, "{val}"),
            ir::Const::U16(val) => write!(f, "{val}"),
            ir::Const::U32(val) => write!(f, "{val}"),
            ir::Const::U64(val) => write!(f, "{val}"),
            ir::Const::Bool(val) => write!(f, "{val}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use redscript_compiler_frontend::{
        CtxVar, FreeFunctionFlags, FunctionType, MethodFlags, ParamFlags, Type, Variance,
    };
    use similar_asserts::assert_eq;

    use super::*;

    #[test]
    fn type_mangling() {
        let symbols = Symbols::with_default_types();

        assert_eq!(
            MangledType::new(&MonoType::nullary(predef::STRING), &symbols).to_string(),
            "String"
        );
        assert_eq!(
            MangledType::new(&MonoType::nullary(predef::INT32), &symbols).to_string(),
            "Int32"
        );
        assert_eq!(
            MangledType::new(
                &MonoType::new(predef::ARRAY, [MonoType::nullary(predef::INT8)]),
                &symbols
            )
            .to_string(),
            "array:Int8"
        );
        assert_eq!(
            MangledType::new(
                &MonoType::new(predef::ARRAY1, [MonoType::nullary(predef::UINT8)]),
                &symbols
            )
            .to_string(),
            "Uint8[1]"
        );
        assert_eq!(
            MangledType::new(&MonoType::nullary(predef::ISCRIPTABLE), &symbols).to_string(),
            "ref:IScriptable"
        );
    }

    #[test]
    fn function_mangling() {
        let func = FreeFunction::new(
            FreeFunctionFlags::default(),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledFreeFunction::new(
                &QualifiedName::from("foo"),
                &sig,
                &func,
                &ScopedMap::default()
            )
            .to_string(),
            "foo;Int32"
        );
    }

    #[test]
    fn native_function_mangling() {
        let func = FreeFunction::new(
            FreeFunctionFlags::default().with_is_native(true),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledFreeFunction::new(
                &QualifiedName::from("foo"),
                &sig,
                &func,
                &ScopedMap::default()
            )
            .to_string(),
            "foo"
        );
    }

    #[test]
    fn generic_function_mangling() {
        let func = FreeFunction::new(
            FreeFunctionFlags::default(),
            FunctionType::new([], [param("a", ctx("A"))], Type::nullary(predef::VOID)),
            [],
            None,
        );

        let sig = Signature::new((), [MonoType::nullary(predef::INT32)]);
        assert_eq!(
            MangledFreeFunction::new(
                &QualifiedName::from("foo"),
                &sig,
                &func,
                &[("A", MonoType::nullary(predef::INT32))]
                    .into_iter()
                    .collect(),
            )
            .to_string(),
            "foo<Int32>;Int32"
        );
    }

    #[test]
    fn method_mangling() {
        let func = Method::new(
            MethodFlags::default(),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            None,
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledMethod::new("foo", &sig, &func, None, &ScopedMap::default()).to_string(),
            "foo;Int32"
        );
    }

    #[test]
    fn native_method_mangling() {
        let func = Method::new(
            MethodFlags::default().with_is_native(true),
            FunctionType::new(
                [],
                [param("a", Type::nullary(predef::INT32))],
                Type::nullary(predef::VOID),
            ),
            None,
            [],
            None,
        );
        let sig = Signature::new((), []);

        assert_eq!(
            MangledMethod::new("foo", &sig, &func, None, &ScopedMap::default()).to_string(),
            "foo"
        );
    }

    #[test]
    fn generic_method_mangling() {
        let func = Method::new(
            MethodFlags::default(),
            FunctionType::new([], [param("a", ctx("A"))], Type::nullary(predef::VOID)),
            None,
            [],
            None,
        );

        let sig = Signature::new((), [MonoType::nullary(predef::INT32)]);
        assert_eq!(
            MangledMethod::new(
                "foo",
                &sig,
                &func,
                None,
                &[("A", MonoType::nullary(predef::INT32))]
                    .into_iter()
                    .collect(),
            )
            .to_string(),
            "foo<Int32>;Int32"
        );
    }

    fn param<'ctx>(name: &'ctx str, typ: Type<'ctx>) -> Param<'ctx> {
        Param::new(name, ParamFlags::default(), typ, None)
    }

    fn ctx<'ctx>(name: &'ctx str) -> Type<'ctx> {
        Type::Ctx(CtxVar::new(name, Variance::Invariant, None, None).into())
    }
}
