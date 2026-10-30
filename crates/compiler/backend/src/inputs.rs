use core::fmt;
use std::borrow::Cow;
use std::rc::Rc;

use hashbrown::HashMap;
use identity_hash::BuildIdentityHasher;
use redscript_compiler_frontend::ast::SourceMap;
use redscript_compiler_frontend::utils::fmt::{DisplayFn, sep_by};
use redscript_compiler_frontend::{
    Aggregate, AggregateFlags, Enum, Field, FieldFlags, FieldMap, FreeFunction, FreeFunctionFlags,
    FreeFunctionIndex, FunctionIndex, FunctionMap, FunctionType, Immutable, Method, MethodFlags,
    MethodId, Mono, MonoType, Param, ParamFlags, RefType, Symbols, Type, TypeApp, TypeDef, TypeId,
    TypeInterner, TypeKind, TypeSchema, Visibility, predef,
};
use redscript_io::{
    CNameIndex, ClassIndex as PoolClassIndex, EnumIndex as PoolEnumIndex, Function as PoolFunction,
    FunctionIndex as PoolFunctionIndex, IndexedDefinition, Parameter as PoolParameter,
    ScriptBundle, Type as PoolType, TypeIndex as PoolTypeIndex, TypeKind as PoolTypeKind,
};
use thiserror::Error;

use crate::IndexMap;
use crate::monomorph::Monomorphizer;
use crate::type_flags::TypeFlagRegistry;

#[derive(Debug)]
pub struct CompilationInputs<'ctx> {
    symbols: Symbols<'ctx>,
    mapping: PoolMappings<'ctx>,
}

impl<'ctx> CompilationInputs<'ctx> {
    pub fn load(
        bundle: &ScriptBundle<'ctx>,
        interner: &'ctx TypeInterner,
        flags: &TypeFlagRegistry,
    ) -> Result<Self, Error> {
        Self::load_with::<true>(bundle, interner, flags)
    }

    pub fn load_without_mapping(
        bundle: &ScriptBundle<'ctx>,
        interner: &'ctx TypeInterner,
        flags: &TypeFlagRegistry,
    ) -> Result<Symbols<'ctx>, Error> {
        Ok(Self::load_with::<false>(bundle, interner, flags)?.symbols)
    }

    fn load_with<const LOAD_MAPPING: bool>(
        bundle: &ScriptBundle<'ctx>,
        interner: &'ctx TypeInterner,
        type_flags: &TypeFlagRegistry,
    ) -> Result<Self, Error> {
        let mut inputs = CompilationInputs::default();
        let mut virtual_map: HashMap<
            TypeId<'ctx>,
            HashMap<CNameIndex, MethodId<'ctx>, BuildIdentityHasher<u32>>,
            BuildIdentityHasher<usize>,
        > = HashMap::default();

        for def in bundle.indexed_definitions() {
            match def {
                IndexedDefinition::Type(index, typ) => {
                    if typ.kind() == PoolTypeKind::Primitive {
                        let name = bundle.get_item(typ.name()).ok_or_else(|| {
                            Error::MissingPoolItem("type name", typ.name().into())
                        })?;
                        let type_id = interner.intern(name.as_ref());
                        let def = TypeDef::new([], TypeSchema::Primitive, []);
                        inputs.symbols.add_type_if_none(type_id, def);
                    }
                    let LoadedType { typ, unsupported } =
                        load_type::<Mono>(typ, bundle, interner, type_flags)?;

                    if LOAD_MAPPING && !unsupported {
                        inputs.mapping.types.insert(typ, index);
                    }
                }
                IndexedDefinition::Class(index, cls) => {
                    let cls_name = bundle
                        .get_item(cls.name())
                        .ok_or_else(|| Error::MissingPoolItem("class name", cls.name().into()))?;
                    let cls_id = interner.intern(cls_name.as_ref());

                    let base = cls
                        .base()
                        .map(|base| {
                            let class = bundle
                                .get_item(base)
                                .ok_or_else(|| Error::MissingPoolItem("base class", base.into()))?;
                            let base_name = bundle.get_item(class.name()).ok_or_else(|| {
                                Error::MissingPoolItem("base class name", class.name().into())
                            })?;
                            Ok(TypeApp::nullary(interner.intern(base_name.as_ref())))
                        })
                        .transpose()?;

                    let mut fields = FieldMap::default();
                    let mut methods = FunctionMap::default();

                    let mut mappings = ClassMappings::default();

                    for &field_idx in cls.fields() {
                        let field = bundle
                            .get_item(field_idx)
                            .ok_or_else(|| Error::MissingPoolItem("field", field_idx.into()))?;
                        let name = bundle.get_item(field.name()).ok_or_else(|| {
                            Error::MissingPoolItem("field name", field.name().into())
                        })?;
                        let typ = bundle.get_item(field.type_()).ok_or_else(|| {
                            Error::MissingPoolItem("field type", field.type_().into())
                        })?;
                        let LoadedType { typ, .. } =
                            load_type::<Immutable>(typ, bundle, interner, type_flags)?;
                        let flags = FieldFlags::new()
                            .with_is_native(field.flags().is_native())
                            .with_is_editable(field.flags().is_editable())
                            .with_is_inline(field.flags().is_inline())
                            .with_is_const(field.flags().is_const())
                            .with_is_instance_editable(field.flags().is_instance_editable())
                            .with_is_replicated(field.flags().is_replicated())
                            .with_is_test_only(field.flags().is_test_only())
                            .with_is_persistent(field.flags().is_persistent())
                            .with_visibility(Visibility::Public);

                        fields
                            .add(
                                assert_borrowed(name),
                                Field::new(flags, typ, [], [], None)
                                    .with_hint(field.hint().map(|hint| hint.to_owned().into())),
                            )
                            .map_err(|_| Error::DuplicateField(field_idx.into()))?;
                    }

                    let mut virtuals = base
                        .as_ref()
                        .and_then(|base| virtual_map.get(&base.id()).cloned())
                        .unwrap_or_default();
                    let mut script_ref_receiver_methods = vec![];

                    for &method_idx in cls.methods() {
                        let method = bundle
                            .get_item(method_idx)
                            .ok_or_else(|| Error::MissingPoolItem("method", method_idx.into()))?;
                        let name = bundle.get_item(method.name()).ok_or_else(|| {
                            Error::MissingPoolItem("method name", method.name().into())
                        })?;
                        let name = demangled_name(assert_borrowed(name));

                        let flags = MethodFlags::new()
                            .with_is_static(method.flags().is_static())
                            .with_is_final(method.flags().is_final())
                            .with_is_native(method.flags().is_native())
                            .with_is_callback(method.flags().is_callback())
                            .with_is_unimplemented(
                                method.body().is_empty() && !method.flags().is_native(),
                            )
                            .with_visibility(Visibility::Public);
                        let func_t = load_function_type(method, bundle, interner, type_flags)?;
                        let base = virtuals.get(&method.name()).copied();

                        let idx = methods.add_with(name, |idx| {
                            // Handle static methods that accept the struct by reference as the first
                            // parameter. They're treated like instance methods.
                            if flags.is_static()
                                && let [fst, rem @ ..] = func_t.params()
                                && (fst.type_() == &Type::nullary(cls_id)
                                    || fst.type_().strip_ref()
                                        == Some((RefType::Script, &Type::nullary(cls_id))))
                            {
                                let func_t = func_t.clone().with_params(rem);
                                let flags =
                                    flags.with_is_static(false).with_is_static_forwarder(true);
                                let method = Method::new(flags, func_t, base, [], None)
                                    .with_aliased(MethodId::new(cls_id, idx));
                                script_ref_receiver_methods.push((name, method));
                            }

                            Method::new(flags, func_t, base, [], None)
                        });
                        mappings.methods.insert(idx, method_idx);

                        let method_id = MethodId::new(cls_id, idx);
                        if !method.flags().is_final() && !method.flags().is_static() {
                            virtuals.insert(method.name(), method_id);
                        }
                    }

                    for (name, method) in script_ref_receiver_methods {
                        // Avoid adding an alias when it could conflict with an existing method.
                        if methods.by_name(name).count() > 1 {
                            continue;
                        }
                        methods.add(name, method);
                    }

                    let type_flags = type_flags.get(cls_name.as_ref());
                    let flags = AggregateFlags::new()
                        .with_is_abstract(cls.flags().is_abstract())
                        .with_is_native(cls.flags().is_native())
                        .with_is_final(cls.flags().is_final())
                        .with_is_import_only(cls.flags().is_import_only())
                        .with_is_test_only(cls.flags().is_test_only())
                        .with_is_struct(cls.flags().is_struct())
                        .with_is_never_ref(type_flags.is_never_ref())
                        .with_is_mixed_ref(type_flags.is_mixed_ref())
                        .with_is_fully_defined(type_flags.is_fully_defined());
                    let agg =
                        Aggregate::new(flags, base, fields, methods, HashMap::default(), None);
                    let def = TypeDef::new([], TypeSchema::Aggregate(agg.into()), []);
                    inputs.symbols.add_type(cls_id, def);

                    if LOAD_MAPPING {
                        let mappings = ClassMappings::new(index, mappings.methods);
                        inputs
                            .mapping
                            .classes
                            .insert(TypeApp::nullary(cls_id), mappings);
                    }

                    virtual_map.insert(cls_id, virtuals);
                }
                IndexedDefinition::Enum(idx, enm) => {
                    let name = bundle
                        .get_item(enm.name())
                        .ok_or_else(|| Error::MissingPoolItem("enum name", enm.name().into()))?;
                    let enum_id = interner.intern(name.as_ref());

                    let mut enum_ = Enum::default();
                    for &value in enm.values() {
                        let val = bundle
                            .get_item(value)
                            .ok_or_else(|| Error::MissingPoolItem("enum value", value.into()))?;
                        let name = bundle.get_item(val.name()).ok_or_else(|| {
                            Error::MissingPoolItem("enum value name", val.name().into())
                        })?;
                        enum_.add_variant(assert_borrowed(name), val.value());
                    }
                    inputs.symbols.add_type(
                        enum_id,
                        TypeDef::new([], TypeSchema::Enum(enum_.into()), []),
                    );

                    if LOAD_MAPPING {
                        inputs.mapping.enums.insert(enum_id, idx);
                    }
                }
                IndexedDefinition::Function(idx, func) => {
                    if func.class().is_some() {
                        continue;
                    }

                    let name = bundle.get_item(func.name()).ok_or_else(|| {
                        Error::MissingPoolItem("free function name", func.name().into())
                    })?;
                    let name = demangled_name(assert_borrowed(name));
                    let flags = FreeFunctionFlags::new()
                        .with_is_exec(func.flags().is_exec())
                        .with_is_native(func.flags().is_native());
                    let typ = load_function_type(func, bundle, interner, type_flags)?;
                    let func = FreeFunction::new(flags, typ, [], None);
                    let func_idx = inputs.symbols.add_free_function(name, func);

                    if LOAD_MAPPING {
                        inputs
                            .mapping
                            .functions
                            .insert(Signature::new(func_idx, []), idx);
                    }
                }
                _ => {}
            }
        }

        Ok(inputs)
    }

    pub fn into_inner(self) -> (Symbols<'ctx>, PoolMappings<'ctx>) {
        (self.symbols, self.mapping)
    }
}

impl Default for CompilationInputs<'_> {
    fn default() -> Self {
        Self {
            symbols: Symbols::with_default_types(),
            mapping: PoolMappings::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct PoolMappings<'ctx> {
    classes: IndexMap<MonoType<'ctx>, ClassMappings>,
    functions: IndexMap<Signature<'ctx, FreeFunctionIndex>, PoolFunctionIndex>,
    enums: IndexMap<TypeId<'ctx>, PoolEnumIndex>,
    types: IndexMap<MonoType<'ctx>, PoolTypeIndex>,
}

impl<'ctx> PoolMappings<'ctx> {
    pub fn into_monomorphizer(self, sources: &'ctx SourceMap) -> Monomorphizer<'ctx> {
        Monomorphizer::new(
            sources,
            self.classes,
            self.enums,
            self.functions,
            self.types,
        )
    }
}

#[derive(Debug, Default)]
pub struct ClassMappings {
    index: PoolClassIndex,
    methods: IndexMap<FunctionIndex, PoolFunctionIndex>,
    aliases: IndexMap<FunctionIndex, PoolFunctionIndex>,
}

impl ClassMappings {
    pub fn new(index: PoolClassIndex, methods: IndexMap<FunctionIndex, PoolFunctionIndex>) -> Self {
        Self {
            index,
            methods,
            aliases: IndexMap::default(),
        }
    }

    #[inline]
    pub fn index(&self) -> PoolClassIndex {
        self.index
    }

    #[inline]
    pub fn methods(&self) -> &IndexMap<FunctionIndex, PoolFunctionIndex> {
        &self.methods
    }

    #[inline]
    pub fn methods_mut(&mut self) -> &mut IndexMap<FunctionIndex, PoolFunctionIndex> {
        &mut self.methods
    }

    #[inline]
    pub fn aliases(&self) -> &IndexMap<FunctionIndex, PoolFunctionIndex> {
        &self.aliases
    }

    pub fn set_alias(&mut self, index: FunctionIndex, alias: PoolFunctionIndex) {
        self.aliases.insert(index, alias);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature<'ctx, A> {
    id: A,
    types: Box<[MonoType<'ctx>]>,
}

impl<'ctx, A> Signature<'ctx, A> {
    pub fn new(item: A, types: impl Into<Box<[MonoType<'ctx>]>>) -> Self {
        Self {
            id: item,
            types: types.into(),
        }
    }

    #[inline]
    pub fn id(&self) -> &A {
        &self.id
    }

    #[inline]
    pub fn types(&self) -> &[MonoType<'ctx>] {
        &self.types
    }

    pub fn display_args(&self) -> impl fmt::Display + use<'_, 'ctx, A> {
        DisplayFn::new(|f: &mut fmt::Formatter<'_>| {
            if self.types.is_empty() {
                return Ok(());
            }
            write!(f, "<{}>", sep_by(&self.types, ","))
        })
    }
}

fn load_type<'ctx, K: TypeKind>(
    typ: &PoolType,
    bundle: &ScriptBundle<'ctx>,
    interner: &'ctx TypeInterner,
    flags: &TypeFlagRegistry,
) -> Result<LoadedType<'ctx, K>, Error> {
    let (id, arg) = match typ.kind() {
        PoolTypeKind::Class | PoolTypeKind::Primitive => {
            let name = bundle
                .get_item(typ.name())
                .ok_or_else(|| Error::MissingPoolItem("type name", typ.name().into()))?;
            let unsupported = typ.kind() == PoolTypeKind::Class
                && flags.get(name.as_ref()).is_mixed_or_never_ref();
            let typ = K::Type::from((interner.intern(name.as_ref()), Rc::new([])));
            return Ok(LoadedType { typ, unsupported });
        }
        PoolTypeKind::Ref(inner) => {
            let arg = bundle
                .get_item(inner)
                .ok_or_else(|| Error::MissingPoolItem("referenced type", inner.into()))?;
            let LoadedType { typ, unsupported } = load_type::<K>(arg, bundle, interner, flags)?;

            let name = bundle
                .get_item(arg.name())
                .ok_or_else(|| Error::MissingPoolItem("type name", arg.name().into()))?;
            if !flags.get(name.as_ref()).is_mixed_ref() {
                return Ok(LoadedType { typ, unsupported });
            }
            return Ok(LoadedType {
                typ: K::Type::from((predef::REF, Rc::new([typ]))),
                unsupported,
            });
        }
        PoolTypeKind::WeakRef(inner) => (predef::WREF, inner),
        PoolTypeKind::Array(inner) => (predef::ARRAY, inner),
        PoolTypeKind::ScriptRef(inner) => (predef::SCRIPT_REF, inner),
        PoolTypeKind::StaticArray { element_type, size } => {
            let id = TypeId::array_with_size(size as usize)
                .ok_or(Error::UnsupportedStaticArray(size))?;
            (id, element_type)
        }
    };
    let arg = bundle
        .get_item(arg)
        .ok_or_else(|| Error::MissingPoolItem("type argument", arg.into()))?;
    let LoadedType { typ, unsupported } = load_type::<K>(arg, bundle, interner, flags)?;
    Ok(LoadedType {
        typ: K::Type::from((id, Rc::new([typ]))),
        unsupported,
    })
}

fn load_function_type<'ctx>(
    func: &PoolFunction<'ctx>,
    bundle: &ScriptBundle<'ctx>,
    interner: &'ctx TypeInterner,
    flags: &TypeFlagRegistry,
) -> Result<FunctionType<'ctx>, Error> {
    let typ = func
        .return_type()
        .map(|typ| {
            let typ = bundle
                .get_item(typ)
                .ok_or_else(|| Error::MissingPoolItem("function return type", typ.into()))?;
            let LoadedType { typ, .. } = load_type::<Immutable>(typ, bundle, interner, flags)?;
            Ok(typ)
        })
        .transpose()?;
    let params = func
        .parameters()
        .iter()
        .map(|&param| {
            let param = bundle
                .get_item(param)
                .ok_or_else(|| Error::MissingPoolItem("function parameter", param.into()))?;
            load_param(param, bundle, interner, flags)
        })
        .collect::<Result<Box<_>, _>>()?;
    let return_t = typ.unwrap_or_else(|| Type::nullary(predef::VOID));
    Ok(FunctionType::new([], params, return_t))
}

fn load_param<'ctx>(
    param: &PoolParameter,
    bundle: &ScriptBundle<'ctx>,
    interner: &'ctx TypeInterner,
    flags: &TypeFlagRegistry,
) -> Result<Param<'ctx>, Error> {
    let name = bundle
        .get_item(param.name())
        .ok_or_else(|| Error::MissingPoolItem("parameter name", param.name().into()))?;
    let typ = bundle
        .get_item(param.type_())
        .ok_or_else(|| Error::MissingPoolItem("parameter type", param.type_().into()))?;
    let LoadedType { typ, .. } = load_type::<Immutable>(typ, bundle, interner, flags)?;
    let flags = ParamFlags::new()
        .with_is_optional(param.flags().is_optional())
        .with_is_out(param.flags().is_out())
        .with_is_const(param.flags().is_const());
    Ok(Param::new(assert_borrowed(name), flags, typ, None))
}

#[track_caller]
fn assert_borrowed<'a>(cow: &Cow<'a, str>) -> &'a str {
    match cow {
        Cow::Borrowed(s) => s,
        Cow::Owned(_) => panic!("expected a borrowed str"),
    }
}

fn demangled_name(name: &str) -> &str {
    name.split_once(';').map_or(name, |(name, _)| name)
}

struct LoadedType<'ctx, K: TypeKind> {
    typ: K::Type<'ctx>,
    unsupported: bool,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("missing {0} in the pool at index {1}")]
    MissingPoolItem(&'static str, u32),
    #[error("unsupported static array with size {0}")]
    UnsupportedStaticArray(u32),
    #[error("duplicate field name at index {0}")]
    DuplicateField(u32),
}
