use std::borrow::Borrow;
use std::hash::Hash;
use std::rc::Rc;
use std::{fmt, iter};

use enum_as_inner::EnumAsInner;
use hashbrown::{HashMap, HashSet};
use itertools::{Either, Itertools};
use redscript::ast::{Span, Variance};
use redscript::bytecode::Intrinsic;
use redscript::definition::{ClassFlags, FieldFlags, FunctionFlags};
use redscript::{function_arity_from_str, str_fmt, Str, StrBuf};
use simple_interner::Interned;
use smallvec::SmallVec;
use strum::{EnumString, IntoStaticStr};

use crate::error::TypeError;
use crate::IndexMap;

#[derive(Debug)]
pub struct TypeRepo<'id> {
    types: HashMap<TypeId<'id>, DataType<'id>>,
    globals: FuncMap<'id, ScopedName>,
}

impl<'id> TypeRepo<'id> {
    pub fn new() -> Self {
        let types = HashMap::default();
        let globals = FuncMap::default();
        let mut this = Self { types, globals };

        this.add_type(
            predef::ARRAY,
            DataType::Builtin {
                type_vars: [TypeVar::unconstrained(Str::from_static("A"), Variance::In)].into(),
                is_unboxed: true,
            },
        );
        this.add_type(
            predef::WREF,
            DataType::Builtin {
                type_vars: [TypeVar::unconstrained(Str::from_static("A"), Variance::Co)].into(),
                is_unboxed: false,
            },
        );
        this.add_type(
            predef::REF,
            DataType::Builtin {
                type_vars: [TypeVar::unconstrained(Str::from_static("A"), Variance::Co)].into(),
                is_unboxed: false,
            },
        );
        this.add_type(
            predef::SCRIPT_REF,
            DataType::Builtin {
                type_vars: [TypeVar::unconstrained(Str::from_static("A"), Variance::In)].into(),
                is_unboxed: true,
            },
        );

        this
    }

    #[inline]
    pub fn add_type(&mut self, id: TypeId<'id>, typ: DataType<'id>) {
        self.types.insert(id, typ);
    }

    #[inline]
    pub fn get_type(&self, id: TypeId<'id>) -> Option<&DataType<'id>> {
        self.types.get(&id)
    }

    #[inline]
    pub fn get_type_mut(&mut self, id: TypeId<'id>) -> Option<&mut DataType<'id>> {
        self.types.get_mut(&id)
    }

    pub fn get_field(&self, id: &FieldId<'id>) -> Option<&Field<'id>> {
        let (_, res) = self.types.get(&id.owner)?.as_class()?.fields.get(id.index)?;
        Some(res)
    }

    pub fn get_method(&self, id: &MethodId<'id>) -> Option<&Func<'id>> {
        let (_, res) = self.get_method_with_signature(id)?;
        Some(res)
    }

    pub fn get_method_with_signature(&self, id: &MethodId<'id>) -> Option<(&FuncSignature, &Func<'id>)> {
        self.types.get(&id.owner)?.as_class()?.methods.get_overload(id.index)
    }

    pub fn get_many_method_mut<const N: usize>(&mut self, ids: [&MethodId<'id>; N]) -> Option<[&mut Func<'id>; N]> {
        let methods = self.types.get_many_mut(ids.map(|id| &id.owner))?;
        let mut i = 0;
        let res = methods.map(|el| {
            let res = el
                .as_class_mut()
                .unwrap()
                .methods
                .get_overload_mut(ids[i].index)
                .unwrap();
            i += 1;
            res
        });
        Some(res)
    }

    pub fn get_static(&self, id: &MethodId<'id>) -> Option<&Func<'id>> {
        let (_, res) = self.get_static_with_signature(id)?;
        Some(res)
    }

    pub fn get_static_with_signature(&self, id: &MethodId<'id>) -> Option<(&FuncSignature, &Func<'id>)> {
        self.types.get(&id.owner)?.as_class()?.statics.get_overload(id.index)
    }

    pub fn get_global(&self, id: &GlobalId) -> Option<&Func<'id>> {
        let (_, res) = self.globals.get_overload(id.index)?;
        Some(res)
    }

    pub fn get_method_name(&self, id: &MethodId<'id>) -> Option<&Str> {
        self.types.get(&id.owner)?.as_class()?.methods.get_name(id.index.0)
    }

    pub fn get_static_name(&self, id: &MethodId<'id>) -> Option<&Str> {
        self.types.get(&id.owner)?.as_class()?.statics.get_name(id.index.0)
    }

    #[inline]
    pub fn globals(&self) -> &FuncMap<'id, ScopedName> {
        &self.globals
    }

    #[inline]
    pub fn globals_mut(&mut self) -> &mut FuncMap<'id, ScopedName> {
        &mut self.globals
    }

    #[inline]
    pub fn type_iter(&self) -> impl Iterator<Item = TypeId<'id>> + '_ {
        self.types.keys().copied()
    }

    #[inline]
    pub fn upper_iter(&self, id: TypeId<'id>) -> UpperIter<'_, 'id> {
        UpperIter {
            db: self,
            current: Some(id),
        }
    }

    #[inline]
    pub fn get_upper_types<A>(&self, id: TypeId<'id>) -> A
    where
        A: FromIterator<TypeId<'id>>,
    {
        self.upper_iter(id).map(|(id, _)| id).collect()
    }

    pub fn resolve_methods<'a>(
        &'a self,
        id: TypeId<'id>,
        name: &'a str,
    ) -> impl Iterator<Item = (TypeId<'id>, OverloadEntry<'_, 'id>)> + 'a {
        self.upper_iter(id)
            .flat_map(|(type_id, class)| class.methods.by_name(name).map(move |res| (type_id, res)))
            .scan(HashSet::new(), |acc, (typ, entry)| {
                acc.insert(entry.signature).then_some((typ, entry))
            })
    }

    pub fn resolve_statics<'a>(
        &'a self,
        id: TypeId<'id>,
        name: &'a str,
    ) -> impl Iterator<Item = (TypeId<'id>, OverloadEntry<'_, 'id>)> + 'a {
        self.upper_iter(id)
            .flat_map(|(type_id, class)| class.statics.by_name(name).map(move |res| (type_id, res)))
            .scan(HashSet::new(), |acc, (typ, entry)| {
                acc.insert(entry.signature).then_some((typ, entry))
            })
    }

    pub fn lub(&self, lhs: TypeId<'id>, rhs: TypeId<'id>) -> Option<TypeId<'id>> {
        if lhs == rhs {
            Some(lhs)
        } else {
            let ltypes: SmallVec<[TypeId<'id>; 4]> = self.get_upper_types(lhs);
            let rtypes: SmallVec<[TypeId<'id>; 4]> = self.get_upper_types(rhs);
            ltypes
                .into_iter()
                .rev()
                .zip(rtypes.into_iter().rev())
                .take_while(|(a, b)| a == b)
                .last()
                .map(|(res, _)| res)
        }
    }

    pub fn glb(&self, lhs: TypeId<'id>, rhs: TypeId<'id>) -> Option<TypeId<'id>> {
        if lhs == rhs {
            Some(lhs)
        } else {
            let ltypes: SmallVec<[TypeId<'id>; 4]> = self.get_upper_types(lhs);
            if ltypes.contains(&rhs) {
                return Some(lhs);
            }
            let rtypes: SmallVec<[TypeId<'id>; 4]> = self.get_upper_types(rhs);
            if rtypes.contains(&lhs) {
                return Some(rhs);
            }
            None
        }
    }
}

impl<'id> Default for TypeRepo<'id> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'id> {
    Bottom,
    Top,
    Data(Parameterized<'id>),
    Prim(Prim),
    Var(VarName),
}

impl<'id> Type<'id> {
    pub fn check_well_formed(&self, repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        match self {
            Type::Data(data) => data.check_well_formed(repo),
            _ => Ok(()),
        }
    }
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bottom => f.write_str("Nothing"),
            Self::Top => f.write_str("Any"),
            Self::Data(data) => data.fmt(f),
            Self::Prim(prim) => prim.fmt(f),
            Self::Var(var) => var.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarName {
    Named(Str),
    CompGen(VarId),
}

impl fmt::Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(name) => f.write_str(name),
            Self::CompGen(_) => f.write_str("α"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(u32);

impl VarId {
    #[inline]
    pub(crate) fn new(val: u32) -> Self {
        Self(val)
    }
}

#[derive(Debug, EnumAsInner)]
pub enum DataType<'id> {
    Class(ClassType<'id>),
    Builtin {
        type_vars: Box<[TypeVar<'id>]>,
        is_unboxed: bool,
    },
    Enum(EnumType),
}

impl<'id> DataType<'id> {
    #[inline]
    pub fn type_vars(&self) -> &[TypeVar<'id>] {
        match self {
            Self::Class(ct) => &ct.type_vars,
            Self::Builtin { type_vars, .. } => type_vars,
            _ => &[],
        }
    }

    #[inline]
    pub fn type_var_names(&self) -> impl Iterator<Item = Str> + '_ {
        self.type_vars().iter().map(|v| v.name.clone())
    }

    #[inline]
    pub fn base(&self) -> Option<&Parameterized<'id>> {
        match self {
            Self::Class(ct) => ct.extends.as_ref(),
            _ => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct ClassType<'id> {
    pub flags: ClassFlags,
    pub type_vars: Box<[TypeVar<'id>]>,
    pub extends: Option<Parameterized<'id>>,
    pub fields: FieldMap<'id>,
    pub methods: FuncMap<'id>,
    pub statics: FuncMap<'id>,
    pub span: Option<Span>,
}

#[derive(Debug, Default)]
pub struct FuncMap<'id, K = Str> {
    map: IndexMap<K, IndexMap<FuncSignature, Func<'id>>>,
}

impl<'id, K: Eq + Hash> FuncMap<'id, K> {
    pub fn by_name<Q>(&self, name: &Q) -> impl Iterator<Item = OverloadEntry<'_, 'id>>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq + Hash,
    {
        self.map
            .get_full(name)
            .map(|(i, _, overloads)| Self::func_overloads(FuncIndex(i), overloads))
            .map_or(Either::Left(iter::empty()), Either::Right)
    }

    #[inline]
    pub fn get_overload(&self, idx: OverloadIndex) -> Option<(&FuncSignature, &Func<'id>)> {
        let (_, map) = self.map.get_index(idx.0 .0)?;
        map.get_index(idx.1)
    }

    #[inline]
    pub fn get_overload_mut(&mut self, idx: OverloadIndex) -> Option<&mut Func<'id>> {
        let (_, map) = self.map.get_index_mut(idx.0 .0)?;
        Some(map.get_index_mut(idx.1)?.1)
    }

    pub fn get_overloads(&self, idx: FuncIndex) -> impl Iterator<Item = OverloadEntry<'_, 'id>> {
        self.map
            .get_index(idx.0)
            .map(|(_, overloads)| Self::func_overloads(idx, overloads))
            .map_or(Either::Left(iter::empty()), Either::Right)
    }

    #[inline]
    pub fn get_name(&self, idx: FuncIndex) -> Option<&K> {
        Some(self.map.get_index(idx.0)?.0)
    }

    pub fn iter(&self) -> impl Iterator<Item = OverloadEntry<'_, 'id>> {
        self.map
            .iter()
            .enumerate()
            .flat_map(|(i, (_, fs))| Self::func_overloads(FuncIndex(i), fs))
    }

    #[inline]
    pub fn iter_by_name(&self) -> impl Iterator<Item = (&K, FuncIndex)> {
        self.map.iter().enumerate().map(|(i, (k, _))| (k, FuncIndex(i)))
    }

    pub fn add(&mut self, name: K, typ: FuncType<'id>, flags: FunctionFlags) -> OverloadIndex
    where
        K: fmt::Display,
    {
        if flags.is_final() || self.map.contains_key(&name) {
            let sig = FuncSignature::from_name_and_type(&name, &typ);
            self.add_with_signature(name, sig, typ, flags)
        } else {
            let sig = FuncSignature::new(str_fmt!("{}", name));
            self.add_with_signature(name, sig, typ, flags)
        }
    }

    pub fn add_with_signature(
        &mut self,
        name: K,
        signature: FuncSignature,
        typ: FuncType<'id>,
        flags: FunctionFlags,
    ) -> OverloadIndex {
        let entry = self.map.entry(name);
        let x = FuncIndex(entry.index());
        let (y, _) = entry.or_default().insert_full(signature, Func::new(typ, flags));
        OverloadIndex(x, y)
    }

    #[inline]
    fn func_overloads<'this>(
        i: FuncIndex,
        methods: &'this IndexMap<FuncSignature, Func<'id>>,
    ) -> impl Iterator<Item = OverloadEntry<'this, 'id>> + fmt::Debug {
        methods
            .iter()
            .enumerate()
            .map(move |(j, (signature, method))| OverloadEntry::new(OverloadIndex(i, j), signature, method))
    }

    pub fn reserve_name(&mut self, name: K) -> FuncIndex {
        let e = self.map.entry(name);
        let res = e.index();
        e.or_default();
        FuncIndex(res)
    }
}

#[derive(Debug, Default)]
pub struct EnumType {
    pub members: IndexMap<Str, i64>,
}

impl EnumType {
    #[inline]
    pub fn get_member(&self, name: &str) -> Option<FieldIndex> {
        self.members.get_index_of(name).map(FieldIndex)
    }

    #[inline]
    pub fn add_member(&mut self, name: Str, value: i64) -> FieldIndex {
        let (k, _) = self.members.insert_full(name, value);
        FieldIndex(k)
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = EnumEntry<'_>> + '_ {
        self.members
            .iter()
            .enumerate()
            .map(|(i, (name, val))| EnumEntry::new(FieldIndex(i), name, *val))
    }
}

#[derive(Debug, Default)]
pub struct FieldMap<'id> {
    map: IndexMap<Str, Field<'id>>,
}

impl<'id> FieldMap<'id> {
    #[inline]
    pub fn by_name(&self, name: &str) -> Option<(FieldIndex, &Field<'id>)> {
        let (index, _, field) = self.map.get_full(name)?;
        Some((FieldIndex(index), field))
    }

    #[inline]
    pub fn get(&self, idx: FieldIndex) -> Option<(&str, &Field<'id>)> {
        let (name, field) = self.map.get_index(idx.0)?;
        Some((name, field))
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = FieldEntry<'_, 'id>> {
        self.map
            .iter()
            .enumerate()
            .map(|(i, (name, typ))| FieldEntry::new(FieldIndex(i), name, typ))
    }

    #[inline]
    pub fn add(&mut self, name: Str, field: Field<'id>) -> FieldIndex {
        let (k, _) = self.map.insert_full(name, field);
        FieldIndex(k)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OverloadIndex(FuncIndex, usize);

impl OverloadIndex {
    #[inline]
    pub fn overload(self) -> FuncIndex {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncIndex(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldIndex(usize);

#[derive(Debug, PartialEq, Eq)]
pub enum Global<'id> {
    Func(FuncIndex),
    Intrinsic(FuncIndex, Intrinsic),
    MethodAlias(MethodId<'id>),
    StaticAlias(MethodId<'id>),
}

impl<'id> Global<'id> {
    #[inline]
    pub fn index(&self) -> Option<FuncIndex> {
        match *self {
            Self::Func(i) | Self::Intrinsic(i, _) => Some(i),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct OverloadEntry<'repo, 'id> {
    pub index: OverloadIndex,
    pub signature: &'repo FuncSignature,
    pub function: &'repo Func<'id>,
}

impl<'repo, 'id> OverloadEntry<'repo, 'id> {
    #[inline]
    fn new(index: OverloadIndex, signature: &'repo FuncSignature, function: &'repo Func<'id>) -> Self {
        Self {
            index,
            signature,
            function,
        }
    }
}

#[derive(Debug)]
pub struct FieldEntry<'repo, 'id> {
    pub index: FieldIndex,
    pub name: &'repo Str,
    pub field: &'repo Field<'id>,
}

impl<'repo, 'id> FieldEntry<'repo, 'id> {
    #[inline]
    fn new(index: FieldIndex, name: &'repo Str, field: &'repo Field<'id>) -> Self {
        Self { index, name, field }
    }
}

#[derive(Debug)]
pub struct EnumEntry<'repo> {
    pub index: FieldIndex,
    pub name: &'repo Str,
    pub value: i64,
}

impl<'repo> EnumEntry<'repo> {
    #[inline]
    fn new(index: FieldIndex, name: &'repo Str, value: i64) -> Self {
        Self { index, name, value }
    }
}

#[derive(Debug)]
pub struct UpperIter<'repo, 'id> {
    db: &'repo TypeRepo<'id>,
    current: Option<TypeId<'id>>,
}

impl<'repo, 'id> Iterator for UpperIter<'repo, 'id> {
    type Item = (TypeId<'id>, &'repo ClassType<'id>);

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.current?;
        let class = self.db.get_type(id)?.as_class()?;
        self.current = class.extends.as_ref().map(|typ| typ.id);
        Some((id, class))
    }
}

#[derive(Debug)]
pub struct TypeVar<'id> {
    pub name: Str,
    pub variance: Variance,
    pub lower: Option<Parameterized<'id>>,
    pub upper: Option<Parameterized<'id>>,
}

impl<'id> TypeVar<'id> {
    const fn unconstrained(name: Str, variance: Variance) -> Self {
        Self {
            name,
            variance,
            lower: None,
            upper: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameterized<'id> {
    pub id: TypeId<'id>,
    pub args: Rc<[Type<'id>]>,
}

impl<'id> Parameterized<'id> {
    #[inline]
    pub fn new(id: TypeId<'id>, args: Rc<[Type<'id>]>) -> Self {
        Self { id, args }
    }

    #[inline]
    pub fn without_args(id: TypeId<'id>) -> Self {
        Self { id, args: Rc::new([]) }
    }

    pub fn check_well_formed(&self, repo: &TypeRepo<'id>) -> Result<(), TypeError<'id>> {
        let expected = repo.get_type(self.id).unwrap().type_vars();
        if expected.len() == self.args.len() {
            Ok(())
        } else {
            Err(TypeError::InvalidNumberOfTypeArgs(self.args.len(), expected.len()))
        }
    }
}

impl fmt::Display for Parameterized<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.id.as_str())?;
        if !self.args.is_empty() {
            write!(f, "<{}>", self.args.iter().format(", "))?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Func<'id> {
    pub flags: FunctionFlags,
    pub typ: FuncType<'id>,
    pub base: Option<MethodId<'id>>,
}

impl<'id> Func<'id> {
    #[inline]
    pub fn new(typ: FuncType<'id>, flags: FunctionFlags) -> Self {
        Self { typ, flags, base: None }
    }

    pub fn is_implemented(&self) -> bool {
        self.flags.has_body() || self.flags.is_native()
    }
}

#[derive(Debug)]
pub struct FuncType<'id> {
    pub type_vars: Box<[TypeVar<'id>]>,
    pub params: Box<[FuncParam<'id>]>,
    pub ret: Type<'id>,
    pub is_ret_poly: bool,
}

impl<'id> FuncType<'id> {
    #[inline]
    pub fn new(type_vars: Box<[TypeVar<'id>]>, params: Box<[FuncParam<'id>]>, ret: Type<'id>) -> Self {
        Self {
            type_vars,
            params,
            ret,
            is_ret_poly: false,
        }
    }
}

#[derive(Debug)]
pub struct FuncParam<'id> {
    pub typ: Type<'id>,
    pub is_out: bool,
    pub is_poly: bool,
}

impl<'id> FuncParam<'id> {
    pub fn new(typ: Type<'id>) -> Self {
        Self {
            typ,
            is_out: false,
            is_poly: false,
        }
    }

    pub fn custom(typ: Type<'id>, is_out: bool) -> Self {
        Self {
            typ,
            is_out,
            is_poly: false,
        }
    }
}

#[derive(Debug)]
pub struct Field<'id> {
    pub flags: FieldFlags,
    pub typ: Type<'id>,
}

impl<'id> Field<'id> {
    #[inline]
    pub fn new(typ: Type<'id>, flags: FieldFlags) -> Self {
        Self { typ, flags }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodId<'id> {
    owner: TypeId<'id>,
    index: OverloadIndex,
}

impl<'id> MethodId<'id> {
    #[inline]
    pub fn new(owner: TypeId<'id>, index: OverloadIndex) -> Self {
        Self { owner, index }
    }

    pub fn owner(&self) -> TypeId<'id> {
        self.owner
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId {
    index: OverloadIndex,
}

impl GlobalId {
    #[inline]
    pub fn new(index: OverloadIndex) -> Self {
        Self { index }
    }
}

impl From<GlobalId> for OverloadIndex {
    #[inline]
    fn from(gid: GlobalId) -> Self {
        gid.index
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldId<'id> {
    owner: TypeId<'id>,
    index: FieldIndex,
}

impl<'id> FieldId<'id> {
    #[inline]
    pub fn new(owner: TypeId<'id>, index: FieldIndex) -> Self {
        Self { owner, index }
    }

    pub fn owner(&self) -> TypeId<'id> {
        self.owner
    }
}

/// Signature used to identify methods.
/// No two methods on a class should have the same signature except for method overloads.
/// A signature is composed of a method name and a descriptor which contains an encoded type signature.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FuncSignature(Str);

impl FuncSignature {
    #[inline]
    pub(crate) fn new(str: Str) -> Self {
        Self(str)
    }

    pub fn from_name_and_type(name: impl fmt::Display, typ: &FuncType<'_>) -> Self {
        use std::io::Write;

        let mut buf: StrBuf = StrBuf::new();
        write!(buf, "{name};").unwrap();

        for param in &*typ.params {
            match &param.typ {
                Type::Bottom => write!(buf, "Nothing").unwrap(),
                Type::Data(dt) => write!(buf, "{}", dt.id).unwrap(),
                Type::Prim(prim) => write!(buf, "{prim}").unwrap(),
                Type::Var(_) | Type::Top => write!(buf, "IScriptable").unwrap(),
            }
        }
        let str = std::str::from_utf8(&buf).unwrap();
        Self(Str::from_ref(str))
    }

    pub fn into_str(self) -> Str {
        self.0
    }
}

#[derive(Debug)]
pub struct EnumMember {
    pub name: Str,
    pub value: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumString, IntoStaticStr)]
pub enum Prim {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float,
    Double,
    Bool,
    String,
    CName,
    #[strum(serialize = "TweakDBID")]
    TweakDbId,
    Variant,
    NodeRef,
    LocalizationString,
    #[strum(serialize = "CRUID")]
    Cruid,
    Unit,
    #[strum(serialize = "redResourceReferenceScriptToken")]
    ResRef,
}

impl Prim {
    pub fn boxed_type(self) -> Option<TypeId<'static>> {
        match self {
            Self::Int8 => Some(predef::BOXED_INT8),
            Self::Int16 => Some(predef::BOXED_INT16),
            Self::Int32 => Some(predef::BOXED_INT32),
            Self::Int64 => Some(predef::BOXED_INT64),
            Self::Uint8 => Some(predef::BOXED_UINT8),
            Self::Uint16 => Some(predef::BOXED_UINT16),
            Self::Uint32 => Some(predef::BOXED_UINT32),
            Self::Uint64 => Some(predef::BOXED_UINT64),
            Self::Float => Some(predef::BOXED_FLOAT),
            Self::Double => Some(predef::BOXED_DOUBLE),
            Self::Bool => Some(predef::BOXED_BOOL),
            Self::String => Some(predef::BOXED_STRING),
            Self::CName => Some(predef::BOXED_CNAME),
            Self::TweakDbId => Some(predef::BOXED_TWEAKDB_ID),
            Self::Variant => Some(predef::BOXED_VARIANT),
            Self::NodeRef | Self::LocalizationString | Self::Cruid | Self::ResRef | Self::Unit => None,
        }
    }
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.into())
    }
}

/// Unique fully-qualified string reference identifying a type.
/// The underlying strings are always either interned or static, so that
/// TypeIds can be compared using simple pointer equality.
#[derive(Debug, Clone, Copy)]
pub struct TypeId<'id>(&'id str);

impl<'id> TypeId<'id> {
    #[inline]
    const fn new(name: &'id str) -> Self {
        Self(name)
    }

    #[inline]
    pub(crate) fn from_interned(i: Interned<'id, str>) -> Self {
        Self(Interned::get(&i))
    }

    #[inline]
    pub fn as_str(&self) -> &'id str {
        self.0
    }

    pub fn as_parts(&self) -> impl Iterator<Item = &'id str> {
        self.0.split('.')
    }

    pub fn name(&self) -> &'id str {
        self.0.rsplit_once('.').map(|(_, str)| str).unwrap_or(self.0)
    }

    pub fn is_function(&self) -> bool {
        Self::get_fn_by_name(self.as_str()).is_some()
    }

    pub fn ref_type(&self) -> Option<RefType> {
        if self == &predef::WREF {
            Some(RefType::Weak)
        } else if self == &predef::SCRIPT_REF {
            Some(RefType::Script)
        } else {
            None
        }
    }

    pub fn get_fn_by_name(str: &str) -> Option<Self> {
        str.strip_prefix("Function")
            .and_then(|n| predef::FUNCTION_BY_ARITY.get(function_arity_from_str!(n)?).copied())
    }
}

impl TypeId<'static> {
    pub fn get_predefined_by_name(str: &str) -> Option<Self> {
        predef::types_by_name()
            .get(str)
            .copied()
            .or_else(|| Self::get_fn_by_name(str))
    }

    #[inline]
    pub fn get_fn_by_arity(arity: usize) -> Option<Self> {
        predef::FUNCTION_BY_ARITY.get(arity).copied()
    }
}

impl Eq for TypeId<'_> {}
impl PartialEq for TypeId<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}
impl Ord for TypeId<'_> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_ptr().cmp(&other.0.as_ptr())
    }
}
impl PartialOrd for TypeId<'_> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Hash for TypeId<'_> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}
impl fmt::Display for TypeId<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct ScopedName {
    name: Str,
    scope: Box<[Str]>,
}

impl ScopedName {
    #[inline]
    pub fn top_level(name: Str) -> Self {
        Self {
            name,
            scope: Box::new([]),
        }
    }

    pub fn new(name: Str, scope: impl IntoIterator<Item = Str>) -> Self {
        Self {
            name,
            scope: scope.into_iter().collect(),
        }
    }

    #[inline]
    pub fn name(&self) -> &Str {
        &self.name
    }

    #[inline]
    pub fn as_parts(&self) -> impl Iterator<Item = &Str> + '_ {
        self.scope.iter().chain(Some(&self.name))
    }
}

impl fmt::Display for ScopedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_parts().format(".").fmt(f)
    }
}

#[derive(Debug)]
pub enum RefType {
    Weak,
    Script,
}

pub mod predef {
    use std::sync::OnceLock;

    use hashbrown::HashMap;
    use redscript::function_type_by_arity;

    use super::TypeId;

    macro_rules! setup {
        ($($id:ident => $name:ident),*) => {
            $(pub static $id: TypeId<'static> = TypeId::new(stringify!($name));)*

            pub(super) fn types_by_name() -> &'static HashMap<&'static str, TypeId<'static>> {
                static BY_NAME: OnceLock<HashMap<&str, TypeId<'static>>> = OnceLock::new();

                BY_NAME.get_or_init(|| {
                    [$((stringify!($name), $id)),*].into_iter().collect()
                })
            }
        };
    }

    setup!(
        INT8 => Int8,
        INT16 => Int16,
        INT32 => Int32,
        INT64 => Int64,
        UINT8 => Uint8,
        UINT16 => Uint16,
        UINT32 => Uint32,
        UINT64 => Uint64,
        FLOAT => Float,
        DOUBLE => Double,
        BOOL => Bool,
        STRING => String,
        CNAME => CName,
        TWEAKDB_ID => TweakDBID,
        VARIANT => Variant,
        NODE_REF => NodeRef,
        LOC_STRING => LocalizationString,
        CRUID => CRUID,
        RES_REF => redResourceReferenceScriptToken,
        ISCRIPTABLE => IScriptable,

        REF => ref,
        WREF => wref,
        ARRAY => array,
        SCRIPT_REF => script_ref,

        BOXED_INT8 => BoxedInt8,
        BOXED_INT16 => BoxedInt16,
        BOXED_INT32 => BoxedInt32,
        BOXED_INT64 => BoxedInt64,
        BOXED_UINT8 => BoxedUint8,
        BOXED_UINT16 => BoxedUint16,
        BOXED_UINT32 => BoxedUint32,
        BOXED_UINT64 => BoxedUint64,
        BOXED_FLOAT => BoxedFloat,
        BOXED_DOUBLE => BoxedDouble,
        BOXED_BOOL => BoxedBool,
        BOXED_STRING => BoxedString,
        BOXED_CNAME => BoxedCName,
        BOXED_TWEAKDB_ID => BoxedTweakDBID,
        BOXED_VARIANT => BoxedVariant,
        BOXED_ENUM => BoxedEnum,
        BOXED_STRUCT => BoxedStruct
    );

    pub(super) static FUNCTION_BY_ARITY: &[TypeId<'static>] = function_type_by_arity!(TypeId::new);
}
