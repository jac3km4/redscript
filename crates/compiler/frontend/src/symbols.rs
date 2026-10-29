use std::borrow::{Borrow, Cow};
use std::hash::Hash;
use std::ops::Not;
use std::rc::Rc;
use std::{fmt, iter, ops};

use bitfield_struct::bitfield;
use hashbrown::{HashMap, HashSet};
use identity_hash::BuildIdentityHasher;
use redscript_ast::Span;
use slab::Slab;
use smallvec::SmallVec;

use crate::types::{CtxVar, Type, TypeApp, TypeId};
use crate::utils::fmt::sep_by;
use crate::{IndexMap, MonoType, Variance, predef};

mod functions;

pub use functions::{FreeFunction, FunctionKind, FunctionType, Method, Param};

#[derive(Debug, Clone)]
pub struct Symbols<'ctx> {
    types: HashMap<TypeId<'ctx>, TypeDef<'ctx>, BuildIdentityHasher<usize>>,
    free_funcs: FreeFunctionMap<'ctx>,
}

impl<'ctx> Symbols<'ctx> {
    pub fn with_default_types() -> Self {
        macro_rules! primitives {
            ($(type <$($arg:ident: $var:ident),*> $id:ident;)*) => {
                [$((
                    paste::paste!(predef::[<$id:snake:upper>]),
                    TypeDef::new(
                        [$(CtxVar::new(stringify!($arg), Variance::$var, None, None).into()),*],
                        TypeSchema::Primitive,
                        [include_str!(concat!("../../../../docs/types/", stringify!($id), ".md"))],
                    )
                )),*]
                .into_iter()
                .collect::<HashMap<_, _, _>>()
            };
        }

        let mut types = primitives! {
            type<> String;
            type<> Cname;
            type<> ResRef;
            type<> TweakDbId;
            type<> Float;
            type<> Double;
            type<> Int8;
            type<> Int16;
            type<> Int32;
            type<> Int64;
            type<> Uint8;
            type<> Uint16;
            type<> Uint32;
            type<> Uint64;
            type<> Bool;
            type<> Variant;
            type<> Void;
            type<> Nothing;

            type<T: Covariant> Ref;
            type<T: Covariant> Wref;
            type<T: Invariant> ScriptRef;
            type<T: Invariant> Array;
        };

        for ty in predef::STATIC_ARRAY_TYPES {
            types.insert(
                *ty,
                TypeDef::new(
                    [CtxVar::new("T", Variance::Invariant, None, None).into()],
                    TypeSchema::Primitive,
                    [],
                ),
            );
        }

        types.insert(
            predef::ISCRIPTABLE,
            TypeDef::new(
                [],
                TypeSchema::Aggregate(Aggregate::default().into()),
                ["The base type of all script classes."],
            ),
        );

        Self {
            types,
            free_funcs: FreeFunctionMap::default(),
        }
    }

    #[inline]
    pub fn get_type(&self, id: TypeId<'ctx>) -> Option<&TypeDef<'ctx>> {
        self.types.get(&id)
    }

    pub fn add_type(&mut self, id: TypeId<'ctx>, def: TypeDef<'ctx>) {
        self.types.insert(id, def);
    }

    pub fn add_type_if_none(&mut self, id: TypeId<'ctx>, def: TypeDef<'ctx>) {
        self.types.entry(id).or_insert(def);
    }

    #[inline]
    pub fn get_free_function(
        &self,
        index: FreeFunctionIndex,
    ) -> Option<(&QualifiedName<'ctx>, &FreeFunction<'ctx>)> {
        self.free_funcs.by_index(index.0)
    }

    pub fn add_free_function(
        &mut self,
        name: impl Into<QualifiedName<'ctx>>,
        func: FreeFunction<'ctx>,
    ) -> FreeFunctionIndex {
        let idx = self.free_funcs.add(name.into(), func);
        FreeFunctionIndex(idx)
    }

    pub fn set_free_function(&mut self, index: FreeFunctionIndex, func: FreeFunction<'ctx>) {
        self.free_funcs.map[index.0.item_index][index.0.overload_index] = func;
    }

    pub fn free_functions(
        &self,
    ) -> impl Iterator<Item = FunctionEntry<FreeFunctionIndex, &QualifiedName<'ctx>, &FreeFunction<'ctx>>>
    {
        self.free_funcs
            .iter()
            .map(|entry| entry.map_key(FreeFunctionIndex))
    }

    pub fn types(&self) -> impl Iterator<Item = (TypeId<'ctx>, &TypeDef<'ctx>)> {
        self.types.iter().map(|(id, def)| (*id, def))
    }

    pub fn get_field(&self, id: FieldId<'ctx>) -> Option<(&'ctx str, &Field<'ctx>)> {
        let def = self.get_type(id.parent)?;
        let agg = def.schema.as_aggregate()?;
        agg.fields.by_index(id.index)
    }

    pub fn get_method(&self, id: MethodId<'ctx>) -> Option<(&'ctx str, &Method<'ctx>)> {
        let def = self.get_type(id.parent)?;
        let agg = def.schema.as_aggregate()?;
        agg.methods
            .by_index(id.index)
            .map(|(&name, val)| (name, val))
    }

    pub fn get_method_mut(&mut self, id: MethodId<'ctx>) -> Option<(&'ctx str, &mut Method<'ctx>)> {
        let def = self.types.get_mut(&id.parent)?;
        let agg = def.schema.as_aggregate_mut()?;
        agg.methods
            .by_index_mut(id.index)
            .map(|(&name, val)| (name, val))
    }

    pub fn get_enum_variant(&self, id: FieldId<'ctx>) -> Option<(&'ctx str, i64)> {
        let def = self.get_type(id.parent)?;
        let enm = def.schema.as_enum()?;
        let (name, &val) = enm.variants.get_index(id.index.0)?;
        Some((name, val))
    }

    pub(super) fn lub(&self, lhs: TypeId<'ctx>, rhs: TypeId<'ctx>) -> Option<TypeId<'ctx>> {
        if lhs == rhs {
            Some(lhs)
        } else {
            let count_left = self.supertype_iter_with_self(lhs).count();
            let count_right = self.supertype_iter_with_self(rhs).count();

            self.supertype_iter_with_self(lhs)
                .skip(count_left.saturating_sub(count_right))
                .zip(
                    self.supertype_iter_with_self(rhs)
                        .skip(count_right.saturating_sub(count_left)),
                )
                .find(|((l, _), (r, _))| l == r)
                .map(|((l, _), _)| l)
        }
    }

    pub(super) fn glb(&self, lhs: TypeId<'ctx>, rhs: TypeId<'ctx>) -> Option<TypeId<'ctx>> {
        if lhs == rhs || self.is_subtype(lhs, rhs) {
            Some(lhs)
        } else if self.is_subtype(rhs, lhs) {
            Some(rhs)
        } else {
            None
        }
    }

    pub(super) fn has_base_type(&self, lhs: TypeId<'ctx>, rhs: TypeId<'ctx>) -> bool {
        self.base_iter_with_self(lhs).any(|(t, _)| t == rhs)
    }

    pub(super) fn is_subtype(&self, lhs: TypeId<'ctx>, rhs: TypeId<'ctx>) -> bool {
        self.supertype_iter_with_self(lhs).any(|(t, _)| t == rhs)
    }

    fn base_iter_with_self_with_mode<Mode: BaseMode>(
        &self,
        typ: TypeId<'ctx>,
    ) -> impl Iterator<Item = (TypeId<'ctx>, &Aggregate<'ctx>)> + use<'_, 'ctx, Mode> {
        let pair = |id| Some((id, self.get_type(id)?.schema.as_aggregate()?));
        iter::successors(pair(typ), move |(_, agg)| pair(Mode::base(agg)?.id()))
    }

    #[inline]
    pub fn base_iter_with_self(
        &self,
        typ: TypeId<'ctx>,
    ) -> impl Iterator<Item = (TypeId<'ctx>, &Aggregate<'ctx>)> + use<'_, 'ctx> {
        self.base_iter_with_self_with_mode::<AnyBaseType>(typ)
    }

    #[inline]
    pub fn supertype_iter_with_self(
        &self,
        typ: TypeId<'ctx>,
    ) -> impl Iterator<Item = (TypeId<'ctx>, &Aggregate<'ctx>)> + use<'_, 'ctx> {
        self.base_iter_with_self_with_mode::<AnySupertype>(typ)
    }

    pub fn query_methods_by_name<'a>(
        &'a self,
        typ: TypeId<'ctx>,
        name: &'a str,
    ) -> impl Iterator<Item = MethodEntry<'a, 'ctx>> + use<'a, 'ctx> {
        self.base_iter_with_self(typ)
            .flat_map(|(id, agg)| {
                agg.methods().by_name(name).map(move |entry| {
                    MethodEntry::new(MethodId::new(id, *entry.key()), entry.name(), entry.func())
                })
            })
            .filter_map(method_dedup())
    }

    pub fn query_methods<'a>(
        &'a self,
        typ: TypeId<'ctx>,
    ) -> impl Iterator<Item = MethodEntry<'a, 'ctx>> + use<'a, 'ctx> {
        self.base_iter_with_self(typ)
            .flat_map(|(id, agg)| {
                agg.methods().iter().map(move |entry| {
                    MethodEntry::new(MethodId::new(id, *entry.key()), entry.name(), entry.func())
                })
            })
            .filter_map(method_dedup())
    }
}

impl<'ctx> ops::Index<TypeId<'ctx>> for Symbols<'ctx> {
    type Output = TypeDef<'ctx>;

    #[inline]
    fn index(&self, index: TypeId<'ctx>) -> &Self::Output {
        self.types.get(&index).expect("type id not found")
    }
}

impl<'ctx> ops::IndexMut<TypeId<'ctx>> for Symbols<'ctx> {
    #[inline]
    fn index_mut(&mut self, index: TypeId<'ctx>) -> &mut Self::Output {
        self.types.get_mut(&index).expect("type id not found")
    }
}

impl<'ctx> ops::Index<FreeFunctionIndex> for Symbols<'ctx> {
    type Output = FreeFunction<'ctx>;

    #[inline]
    fn index(&self, index: FreeFunctionIndex) -> &Self::Output {
        self.free_funcs
            .by_index(index.0)
            .expect("free function index not found")
            .1
    }
}

impl<'ctx> ops::Index<MethodId<'ctx>> for Symbols<'ctx> {
    type Output = Method<'ctx>;

    #[inline]
    fn index(&self, index: MethodId<'ctx>) -> &Self::Output {
        self.get_method(index).expect("method id not found").1
    }
}

impl<'ctx> ops::Index<FieldId<'ctx>> for Symbols<'ctx> {
    type Output = Field<'ctx>;

    #[inline]
    fn index(&self, index: FieldId<'ctx>) -> &Self::Output {
        self.get_field(index).expect("field id not found").1
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef<'ctx> {
    params: Box<[Rc<CtxVar<'ctx>>]>,
    schema: TypeSchema<'ctx>,
    doc: Box<[&'ctx str]>,
}

impl<'ctx> TypeDef<'ctx> {
    pub fn new_primitive(doc: impl Into<Box<[&'ctx str]>>) -> Self {
        Self {
            params: Box::default(),
            schema: TypeSchema::Primitive,
            doc: doc.into(),
        }
    }

    pub fn new(
        params: impl Into<Box<[Rc<CtxVar<'ctx>>]>>,
        schema: TypeSchema<'ctx>,
        doc: impl Into<Box<[&'ctx str]>>,
    ) -> Self {
        Self {
            params: params.into(),
            schema,
            doc: doc.into(),
        }
    }

    #[inline]
    pub fn params(&self) -> &[Rc<CtxVar<'ctx>>] {
        &self.params
    }

    #[inline]
    pub fn schema(&self) -> &TypeSchema<'ctx> {
        &self.schema
    }

    #[inline]
    pub fn schema_mut(&mut self) -> &mut TypeSchema<'ctx> {
        &mut self.schema
    }

    #[inline]
    pub fn doc(&self) -> &[&'ctx str] {
        &self.doc
    }

    pub fn vars(&self) -> impl ExactSizeIterator<Item = &'ctx str> + use<'_, 'ctx> {
        self.params.iter().map(|v| v.name())
    }

    pub fn span(&self) -> Option<Span> {
        match &self.schema {
            TypeSchema::Aggregate(aggregate) => aggregate.span(),
            TypeSchema::Enum(enm) => enm.span(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeSchema<'ctx> {
    Aggregate(Box<Aggregate<'ctx>>),
    Enum(Box<Enum<'ctx>>),
    Primitive,
}

impl<'ctx> TypeSchema<'ctx> {
    #[inline]
    pub fn base_type(&self) -> Option<&TypeApp<'ctx>> {
        self.as_aggregate().and_then(|agg| agg.base.as_ref())
    }

    #[inline]
    pub fn as_aggregate(&self) -> Option<&Aggregate<'ctx>> {
        match self {
            Self::Aggregate(agg) => Some(agg),
            _ => None,
        }
    }

    #[inline]
    pub fn as_aggregate_mut(&mut self) -> Option<&mut Aggregate<'ctx>> {
        match self {
            Self::Aggregate(agg) => Some(agg),
            _ => None,
        }
    }

    #[inline]
    pub fn as_enum(&self) -> Option<&Enum<'ctx>> {
        match self {
            Self::Enum(enm) => Some(enm),
            _ => None,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Enum<'ctx> {
    variants: IndexMap<&'ctx str, i64>,
    span: Option<Span>,
}

impl<'ctx> Enum<'ctx> {
    pub fn new(variants: impl Into<IndexMap<&'ctx str, i64>>, span: Option<Span>) -> Self {
        Self {
            variants: variants.into(),
            span,
        }
    }

    #[inline]
    pub fn variant_by_name(&self, name: &str) -> Option<(FieldIndex, i64)> {
        let (index, _, value) = self.variants.get_full(name)?;
        Some((FieldIndex(index), *value))
    }

    pub fn add_variant(&mut self, name: &'ctx str, value: i64) -> FieldIndex {
        FieldIndex(self.variants.insert_full(name, value).0)
    }

    pub fn variants(&self) -> impl ExactSizeIterator<Item = (&'ctx str, i64)> + use<'_, 'ctx> {
        self.variants.iter().map(|(&name, &value)| (name, value))
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

#[derive(Debug, Default, Clone)]
pub struct Aggregate<'ctx> {
    flags: AggregateFlags,
    base: Option<TypeApp<'ctx>>,
    fields: FieldMap<'ctx>,
    methods: MethodMap<'ctx>,
    implementations: HashMap<MonoType<'ctx>, QualifiedName<'ctx>>,
    constructor_hint: Option<Cow<'ctx, str>>,
    span: Option<Span>,
}

impl<'ctx> Aggregate<'ctx> {
    pub fn new(
        flags: AggregateFlags,
        base: Option<TypeApp<'ctx>>,
        fields: FieldMap<'ctx>,
        methods: MethodMap<'ctx>,
        implementations: HashMap<MonoType<'ctx>, QualifiedName<'ctx>>,
        span: Option<Span>,
    ) -> Self {
        Self {
            flags,
            base,
            fields,
            methods,
            implementations,
            constructor_hint: None,
            span,
        }
    }

    #[inline]
    pub fn flags(&self) -> AggregateFlags {
        self.flags
    }

    pub fn set_flags(&mut self, flags: AggregateFlags) {
        self.flags = flags;
    }

    #[inline]
    pub fn base(&self) -> Option<&TypeApp<'ctx>> {
        self.base.as_ref()
    }

    #[inline]
    pub fn supertype(&self) -> Option<&TypeApp<'ctx>> {
        self.base.as_ref().filter(|_| !self.flags.is_struct())
    }

    #[inline]
    pub fn set_base(&mut self, base: Option<TypeApp<'ctx>>) {
        self.base = base;
    }

    #[inline]
    pub fn fields(&self) -> &FieldMap<'ctx> {
        &self.fields
    }

    #[inline]
    pub fn fields_mut(&mut self) -> &mut FieldMap<'ctx> {
        &mut self.fields
    }

    #[inline]
    pub fn methods(&self) -> &MethodMap<'ctx> {
        &self.methods
    }

    #[inline]
    pub fn methods_mut(&mut self) -> &mut MethodMap<'ctx> {
        &mut self.methods
    }

    #[inline]
    pub fn named_implementation(&self, typ: &MonoType<'ctx>) -> Option<&QualifiedName<'ctx>> {
        self.implementations.get(typ)
    }

    #[inline]
    pub fn constructor_hint(&self) -> Option<&Cow<'ctx, str>> {
        self.constructor_hint.as_ref()
    }

    #[inline]
    pub fn set_constructor_hint(&mut self, hint: impl Into<Cow<'ctx, str>>) {
        self.constructor_hint = Some(hint.into());
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }

    pub fn is_user_defined(&self) -> bool {
        self.span().is_some()
    }
}

#[derive(Debug, Default, Clone)]
pub struct FieldMap<'ctx> {
    map: IndexMap<&'ctx str, Field<'ctx>>,
}

impl<'ctx> FieldMap<'ctx> {
    #[inline]
    pub fn by_name(&self, name: &str) -> Option<(FieldIndex, &Field<'ctx>)> {
        let (index, _, field) = self.map.get_full(name)?;
        Some((FieldIndex(index), field))
    }

    #[inline]
    pub fn by_index(&self, index: FieldIndex) -> Option<(&'ctx str, &Field<'ctx>)> {
        self.map
            .get_index(index.0)
            .map(|(&name, field)| (name, field))
    }

    #[inline]
    pub fn add(
        &mut self,
        name: &'ctx str,
        field: Field<'ctx>,
    ) -> Result<FieldIndex, FieldRedefinition> {
        let (idx, prev) = self.map.insert_full(name, field);
        if prev.is_some() {
            Err(FieldRedefinition)
        } else {
            Ok(FieldIndex(idx))
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = FieldEntry<'_, 'ctx>> + Clone {
        self.map
            .iter()
            .enumerate()
            .map(|(i, (name, field))| FieldEntry {
                index: FieldIndex(i),
                name,
                field,
            })
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

pub type MethodMap<'ctx> = FunctionMap<&'ctx str, Method<'ctx>>;
pub type FreeFunctionMap<'ctx> = FunctionMap<QualifiedName<'ctx>, FreeFunction<'ctx>>;

#[derive(Debug, Clone)]
pub struct FunctionMap<N, V> {
    map: IndexMap<N, Slab<V>>,
}

impl<N, V> FunctionMap<N, V>
where
    N: Eq + Hash,
{
    pub fn by_name<Q>(&self, name: &Q) -> impl Iterator<Item = FunctionEntry<FunctionIndex, &N, &V>>
    where
        N: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.map
            .get_full(name)
            .into_iter()
            .flat_map(|(item_index, name, slab)| {
                slab.iter().map(move |(overload_index, func)| {
                    FunctionEntry::new(FunctionIndex::new(item_index, overload_index), name, func)
                })
            })
    }

    #[inline]
    pub fn by_index(&self, index: FunctionIndex) -> Option<(&N, &V)> {
        let (name, slab) = self.map.get_index(index.item_index)?;
        Some((name, slab.get(index.overload_index)?))
    }

    #[inline]
    pub fn by_index_mut(&mut self, index: FunctionIndex) -> Option<(&N, &mut V)> {
        let (name, slab) = self.map.get_index_mut(index.item_index)?;
        Some((name, slab.get_mut(index.overload_index)?))
    }

    #[inline]
    pub fn add(&mut self, name: N, func: V) -> FunctionIndex {
        self.add_with(name, move |_| func)
    }

    pub fn add_with<F>(&mut self, name: N, f: F) -> FunctionIndex
    where
        F: FnOnce(FunctionIndex) -> V,
    {
        let entry = self.map.entry(name);
        let item_index = entry.index();
        let slab = entry.or_default();
        let index = FunctionIndex {
            item_index,
            overload_index: slab.vacant_key(),
        };
        slab.insert(f(index));
        index
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = FunctionEntry<FunctionIndex, &N, &V>> + use<'_, N, V> {
        self.map
            .iter()
            .enumerate()
            .flat_map(|(item_index, (name, slab))| {
                slab.iter().map(move |(overload_index, func)| {
                    FunctionEntry::new(FunctionIndex::new(item_index, overload_index), name, func)
                })
            })
    }
}

impl<N, V> Default for FunctionMap<N, V> {
    fn default() -> Self {
        Self {
            map: IndexMap::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodId<'id> {
    parent: TypeId<'id>,
    index: FunctionIndex,
}

impl<'id> MethodId<'id> {
    #[inline]
    pub fn new(parent: TypeId<'id>, index: FunctionIndex) -> Self {
        Self { parent, index }
    }

    #[inline]
    pub fn parent(&self) -> TypeId<'id> {
        self.parent
    }

    #[inline]
    pub fn index(&self) -> FunctionIndex {
        self.index
    }
}

impl<'ctx> AsRef<TypeId<'ctx>> for MethodId<'ctx> {
    #[inline]
    fn as_ref(&self) -> &TypeId<'ctx> {
        &self.parent
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId<'id> {
    parent: TypeId<'id>,
    index: FieldIndex,
}

impl<'id> FieldId<'id> {
    #[inline]
    pub fn new(parent: TypeId<'id>, index: FieldIndex) -> Self {
        Self { parent, index }
    }

    #[inline]
    pub fn parent(&self) -> TypeId<'id> {
        self.parent
    }

    #[inline]
    pub fn index(&self) -> FieldIndex {
        self.index
    }
}

impl<'ctx> AsRef<TypeId<'ctx>> for FieldId<'ctx> {
    #[inline]
    fn as_ref(&self) -> &TypeId<'ctx> {
        &self.parent
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionIndex {
    item_index: usize,
    overload_index: usize,
}

impl FunctionIndex {
    #[inline]
    fn new(item_index: usize, overload_index: usize) -> Self {
        Self {
            item_index,
            overload_index,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldIndex(usize);

impl From<FieldIndex> for usize {
    #[inline]
    fn from(index: FieldIndex) -> Self {
        index.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FreeFunctionIndex(FunctionIndex);

pub type FreeFunctionIndexes = SmallVec<[FreeFunctionIndex; 1]>;

pub trait FunctionKey<'ctx> {
    fn parent(&self) -> Option<TypeId<'ctx>>;
}

impl<'ctx> FunctionKey<'ctx> for MethodId<'ctx> {
    #[inline]
    fn parent(&self) -> Option<TypeId<'ctx>> {
        Some(self.parent)
    }
}

impl<'ctx> FunctionKey<'ctx> for FreeFunctionIndex {
    #[inline]
    fn parent(&self) -> Option<TypeId<'ctx>> {
        None
    }
}

pub type MethodEntry<'sym, 'ctx> = FunctionEntry<MethodId<'ctx>, &'ctx str, &'sym Method<'ctx>>;

#[derive(Debug, Clone)]
pub struct FunctionEntry<K, N, V> {
    key: K,
    name: N,
    value: V,
}

impl<K, N, V> FunctionEntry<K, N, V> {
    #[inline]
    pub fn new(key: K, name: N, value: V) -> Self {
        Self { key, name, value }
    }

    #[inline]
    pub fn key(&self) -> &K {
        &self.key
    }

    #[inline]
    pub fn name(&self) -> &N {
        &self.name
    }

    #[inline]
    pub fn func(&self) -> &V {
        &self.value
    }

    #[inline]
    pub fn map_key<K2>(self, f: impl FnOnce(K) -> K2) -> FunctionEntry<K2, N, V> {
        FunctionEntry {
            key: f(self.key),
            name: self.name,
            value: self.value,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field<'ctx> {
    flags: FieldFlags,
    typ: Type<'ctx>,
    properties: Box<[(Cow<'ctx, str>, Cow<'ctx, str>)]>,
    span: Option<Span>,
    doc: Box<[&'ctx str]>,
}

impl<'ctx> Field<'ctx> {
    pub fn new(
        flags: FieldFlags,
        typ: Type<'ctx>,
        properties: impl Into<Box<[(Cow<'ctx, str>, Cow<'ctx, str>)]>>,
        doc: impl Into<Box<[&'ctx str]>>,
        span: Option<Span>,
    ) -> Self {
        Self {
            flags,
            typ,
            properties: properties.into(),
            doc: doc.into(),
            span,
        }
    }

    #[inline]
    pub fn flags(&self) -> FieldFlags {
        self.flags
    }

    #[inline]
    pub fn type_(&self) -> &Type<'ctx> {
        &self.typ
    }

    #[inline]
    pub fn properties(&self) -> &[(Cow<'ctx, str>, Cow<'ctx, str>)] {
        &self.properties
    }

    #[inline]
    pub fn doc(&self) -> &[&'ctx str] {
        &self.doc
    }

    #[inline]
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

#[derive(Debug)]
pub struct FieldEntry<'sym, 'ctx> {
    index: FieldIndex,
    name: &'ctx str,
    field: &'sym Field<'ctx>,
}

impl<'ctx> FieldEntry<'_, 'ctx> {
    #[inline]
    pub fn index(&self) -> FieldIndex {
        self.index
    }

    #[inline]
    pub fn name(&self) -> &'ctx str {
        self.name
    }

    #[inline]
    pub fn field(&self) -> &Field<'ctx> {
        self.field
    }
}

#[bitfield(u8)]
pub struct AggregateFlags {
    pub is_native: bool,
    pub is_abstract: bool,
    pub is_final: bool,
    pub is_import_only: bool,
    pub is_struct: bool,
    pub is_never_ref: bool,
    pub is_mixed_ref: bool,
    pub is_fully_defined: bool,
}

#[bitfield(u8)]
pub struct FreeFunctionFlags {
    pub is_exec: bool,
    pub is_native: bool,
    pub is_cast: bool,
    pub is_operator: bool,
    #[bits(4)]
    __: u8,
}

#[bitfield(u16)]
pub struct MethodFlags {
    #[bits(2)]
    pub visibility: Visibility,
    pub has_implicit_visibility: bool,
    pub is_static: bool,
    pub is_final: bool,
    pub is_native: bool,
    pub is_callback: bool,
    pub is_unimplemented: bool,
    pub is_static_forwarder: bool,

    #[bits(7)]
    __: u16,
}

#[bitfield(u8)]
pub struct ParamFlags {
    pub is_optional: bool,
    pub is_out: bool,
    pub is_const: bool,
    #[bits(5)]
    __: u8,
}

#[bitfield(u8)]
pub struct FieldFlags {
    #[bits(2)]
    pub visibility: Visibility,
    pub is_native: bool,
    pub is_editable: bool,
    pub is_inline: bool,
    pub is_const: bool,
    pub is_persistent: bool,
    #[bits(1)]
    __: u8,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedName<'ctx>(SmallVec<[&'ctx str; 1]>);

impl<'ctx> QualifiedName<'ctx> {
    pub fn from_base_and_name<I>(base: &I, name: &'ctx str) -> Self
    where
        I: ?Sized,
        for<'i> &'i I: IntoIterator<Item = &'i &'ctx str>,
    {
        base.into_iter()
            .copied()
            .chain([name])
            .collect::<QualifiedName<'ctx>>()
    }

    pub fn as_single_component(&self) -> Option<&'ctx str> {
        match &self.0[..] {
            [name] => Some(name),
            _ => None,
        }
    }
}

impl<'ctx> From<&'ctx str> for QualifiedName<'ctx> {
    fn from(value: &'ctx str) -> Self {
        QualifiedName(SmallVec::from_slice(&[value]))
    }
}

impl<'ctx> From<&QualifiedName<'ctx>> for Cow<'ctx, str> {
    fn from(value: &QualifiedName<'ctx>) -> Self {
        value
            .as_single_component()
            .map_or_else(|| value.to_string().into(), Cow::Borrowed)
    }
}

impl<'ctx> AsRef<[&'ctx str]> for QualifiedName<'ctx> {
    #[inline]
    fn as_ref(&self) -> &[&'ctx str] {
        &self.0
    }
}

impl<'a, 'ctx> IntoIterator for &'a QualifiedName<'ctx> {
    type IntoIter = std::iter::Copied<std::slice::Iter<'a, &'ctx str>>;
    type Item = &'ctx str;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().copied()
    }
}

impl<'ctx> FromIterator<&'ctx str> for QualifiedName<'ctx> {
    fn from_iter<T: IntoIterator<Item = &'ctx str>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl fmt::Display for QualifiedName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", sep_by(&self.0, "."))
    }
}

#[derive(Debug)]
pub struct FieldRedefinition;

#[inline]
fn method_dedup<'ctx: 'a, 'a>() -> impl FnMut(MethodEntry<'a, 'ctx>) -> Option<MethodEntry<'a, 'ctx>>
{
    let mut dedup = HashSet::new();
    move |entry| {
        if let Some(overridden) = entry.func().overriden() {
            dedup.insert(overridden);
        }
        dedup.remove(entry.key()).not().then_some(entry)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Visibility {
    #[default]
    Private,
    Protected,
    Public,
}

impl Visibility {
    const fn into_bits(self) -> u8 {
        match self {
            Self::Private => 0,
            Self::Protected => 1,
            Self::Public => 2,
        }
    }

    const fn from_bits(bits: u8) -> Self {
        match bits {
            0 => Self::Private,
            1 => Self::Protected,
            2 => Self::Public,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Private => write!(f, "private"),
            Self::Protected => write!(f, "protected"),
            Self::Public => write!(f, "public"),
        }
    }
}

pub trait BaseMode {
    fn base<'a, 'ctx>(agg: &'a Aggregate<'ctx>) -> Option<&'a TypeApp<'ctx>>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct AnyBaseType;

impl BaseMode for AnyBaseType {
    #[inline]
    fn base<'a, 'ctx>(agg: &'a Aggregate<'ctx>) -> Option<&'a TypeApp<'ctx>> {
        agg.base.as_ref()
    }
}

pub struct AnySupertype;

impl BaseMode for AnySupertype {
    #[inline]
    fn base<'a, 'ctx>(agg: &'a Aggregate<'ctx>) -> Option<&'a TypeApp<'ctx>> {
        agg.supertype()
    }
}
