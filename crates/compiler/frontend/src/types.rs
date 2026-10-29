use std::borrow::Cow;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;
use std::{fmt, ops};

use derive_where::derive_where;
use redscript_ast as ast;

use crate::utils::ScopedMap;
use crate::utils::fmt::{DisplayFn, sep_by};
use crate::{Symbols, TypeSchema};

mod identity;

pub use identity::{TypeId, TypeIndex, TypeInterner, predef};

pub(super) const MAX_FN_ARITY: usize = 8;
pub(super) const MAX_STATIC_ARRAY_SIZE: usize = 16;

pub type MonoType<'ctx> = TypeApp<'ctx, Mono>;

#[derive_where(Debug, Clone)]
pub enum Type<'ctx, K: TypeKind = Immutable> {
    Nothing,
    Data(TypeApp<'ctx, K>),
    Ctx(Rc<CtxVar<'ctx, K>>),
}

impl<'ctx, K: TypeKind> Type<'ctx, K> {
    #[inline]
    pub fn nullary(id: TypeId<'ctx>) -> Self {
        Self::Data(TypeApp::nullary(id))
    }

    #[inline]
    pub fn app(id: TypeId<'ctx>, args: impl Into<Rc<[K::Type<'ctx>]>>) -> Self {
        Self::Data(TypeApp::new(id, args))
    }

    pub fn upper_bound(&self) -> Option<&TypeApp<'ctx, K>> {
        match self {
            Self::Ctx(v) => v.upper().and_then(Self::upper_bound),
            Self::Data(t) => Some(t),
            _ => None,
        }
    }

    #[inline]
    pub fn unwrap_ref(&self) -> Option<&K::Type<'ctx>> {
        Some(self.strip_ref()?.1)
    }

    #[inline]
    pub fn strip_ref(&self) -> Option<(RefType, &K::Type<'ctx>)> {
        self.upper_bound().and_then(TypeApp::strip_ref)
    }

    #[inline]
    pub fn ref_type(&self) -> Option<RefType> {
        self.upper_bound().and_then(TypeApp::ref_type)
    }

    pub fn is_primitive(&self, symbols: &Symbols<'ctx>) -> bool {
        match self {
            Self::Data(typ) => matches!(
                symbols[typ.id()].schema(),
                TypeSchema::Primitive | TypeSchema::Enum(_)
            ),
            _ => false,
        }
    }

    #[inline]
    pub fn display_compact(&self) -> impl fmt::Display + use<'_, 'ctx, K> {
        self.display(true)
    }

    fn display(&self, compact: bool) -> impl fmt::Display + use<'_, 'ctx, K> {
        DisplayFn::new(move |f: &mut fmt::Formatter<'_>| match self {
            Self::Nothing => write!(f, "Nothing"),
            Self::Data(t) => t.display(compact).fmt(f),
            Self::Ctx(v) => write!(f, "{}", v.name),
        })
    }
}

impl<'ctx> Type<'ctx> {
    #[inline]
    pub fn unwrap_ref_or_self(&self) -> &Self {
        self.unwrap_ref().map_or(self, |t| t)
    }

    pub fn mono(
        &self,
        env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
    ) -> Result<MonoType<'ctx>, UnresolvedVar<'ctx>> {
        match self {
            Self::Ctx(var) => env
                .get(var.name())
                .ok_or_else(|| UnresolvedVar(var.name()))
                .cloned(),
            Self::Data(typ) => typ.mono(env),
            Self::Nothing => Ok(MonoType::nullary(predef::NOTHING)),
        }
    }

    #[inline]
    pub fn assume_mono(&self, env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>) -> MonoType<'ctx> {
        self.mono(env)
            .expect("all vars should be resolved at this point")
    }
}

impl<'ctx> From<(TypeId<'ctx>, Rc<[Type<'ctx>]>)> for Type<'ctx> {
    #[inline]
    fn from((id, args): (TypeId<'ctx>, Rc<[Type<'ctx>]>)) -> Self {
        Self::app(id, args)
    }
}

impl<K: TypeKind> PartialEq for Type<'_, K> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nothing, Self::Nothing) => true,
            (Self::Data(a), Self::Data(b)) => a == b,
            (Self::Ctx(a), Self::Ctx(b)) => a.name == b.name,
            _ => false,
        }
    }
}

impl<K: TypeKind> Eq for Type<'_, K> {}

impl<'ctx> From<(TypeId<'ctx>, Rc<[MonoType<'ctx>]>)> for MonoType<'ctx> {
    #[inline]
    fn from((id, args): (TypeId<'ctx>, Rc<[MonoType<'ctx>]>)) -> Self {
        Self::new(id, args)
    }
}

impl<K: TypeKind> fmt::Display for Type<'_, K> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(false).fmt(f)
    }
}

pub trait TypeKind {
    type Type<'ctx>: fmt::Debug
        + fmt::Display
        + Clone
        + PartialEq
        + Eq
        + From<(TypeId<'ctx>, Rc<[Self::Type<'ctx>]>)>;
}

#[derive(Debug)]
pub struct Immutable;

impl TypeKind for Immutable {
    type Type<'ctx> = Type<'ctx>;
}

#[derive(Debug)]
pub struct Mono;

impl TypeKind for Mono {
    type Type<'ctx> = MonoType<'ctx>;
}

#[derive_where(Debug, Clone, PartialEq, Eq)]
pub struct TypeApp<'ctx, K: TypeKind = Immutable> {
    id: TypeId<'ctx>,
    args: Rc<[K::Type<'ctx>]>,
}

impl<'ctx, K: TypeKind> TypeApp<'ctx, K> {
    #[inline]
    pub fn new(id: TypeId<'ctx>, args: impl Into<Rc<[K::Type<'ctx>]>>) -> Self {
        Self {
            id,
            args: args.into(),
        }
    }

    #[inline]
    pub fn nullary(id: TypeId<'ctx>) -> Self {
        Self::new(id, [])
    }

    #[inline]
    pub fn into_type(self) -> Type<'ctx, K> {
        Type::Data(self)
    }

    #[inline]
    pub fn id(&self) -> TypeId<'ctx> {
        self.id
    }

    #[inline]
    pub fn args(&self) -> &[K::Type<'ctx>] {
        &self.args
    }

    #[inline]
    pub fn strip_ref(&self) -> Option<(RefType, &K::Type<'ctx>)> {
        match (self.ref_type(), &*self.args) {
            (Some(rt), [arg]) => Some((rt, arg)),
            _ => None,
        }
    }

    pub fn ref_type(&self) -> Option<RefType> {
        match self.id {
            id if id == predef::REF => Some(RefType::Strong),
            id if id == predef::WREF => Some(RefType::Weak),
            id if id == predef::SCRIPT_REF => Some(RefType::Script),
            _ => None,
        }
    }

    pub fn type_env<'scope>(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> ScopedMap<'scope, &'ctx str, K::Type<'ctx>> {
        symbols[self.id()]
            .vars()
            .zip(self.args().iter().cloned())
            .collect()
    }

    pub(super) fn display(&self, compact: bool) -> impl fmt::Display + use<'_, 'ctx, K> {
        let sep = if compact { "," } else { ", " };
        DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
            write!(f, "{}", self.id)?;
            if !self.args.is_empty() {
                write!(f, "<{}>", sep_by(&self.args[..], sep))?;
            }
            Ok(())
        })
    }
}

impl<'ctx> TypeApp<'ctx> {
    pub fn mono(
        &self,
        env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>,
    ) -> Result<MonoType<'ctx>, UnresolvedVar<'ctx>> {
        let args = self
            .args()
            .iter()
            .map(|t| t.mono(env))
            .collect::<Result<Rc<_>, _>>()?;
        Ok(MonoType::new(self.id(), args))
    }

    pub fn assume_mono(&self, env: &ScopedMap<'_, &'ctx str, MonoType<'ctx>>) -> MonoType<'ctx> {
        self.mono(env)
            .expect("all vars should be resolved at this point")
    }
}

impl<'ctx> MonoType<'ctx> {
    pub fn instantiate_as(
        &self,
        target: TypeId<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> Option<Cow<'_, Self>> {
        let mut cur = Cow::Borrowed(self);
        while cur.id() != target {
            cur = Cow::Owned(cur.instantiate_base(symbols)?);
        }
        Some(cur)
    }

    pub fn instantiate_base(&self, symbols: &Symbols<'ctx>) -> Option<Self> {
        let class = &symbols[self.id()];
        assert_eq!(
            class.vars().len(),
            self.args().len(),
            "type args should match the underlying type"
        );
        let base = class.schema().base_type()?;
        let env = class.vars().zip(self.args().iter().cloned()).collect();
        Some(base.mono(&env).expect("env should be complete"))
    }
}

impl fmt::Display for TypeApp<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(false).fmt(f)
    }
}

impl fmt::Display for MonoType<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(true).fmt(f)
    }
}

impl Hash for MonoType<'_> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.args.hash(state);
    }
}

#[derive_where(Debug)]
pub struct CtxVar<'ctx, K: TypeKind = Immutable> {
    name: &'ctx str,
    variance: Variance,
    lower: Option<Type<'ctx, K>>,
    upper: Option<Type<'ctx, K>>,
}

impl<'ctx, K: TypeKind> CtxVar<'ctx, K> {
    pub fn new(
        name: &'ctx str,
        variance: Variance,
        lower: Option<Type<'ctx, K>>,
        upper: Option<Type<'ctx, K>>,
    ) -> Self {
        Self {
            name,
            variance,
            lower,
            upper,
        }
    }

    #[inline]
    pub fn name(&self) -> &'ctx str {
        self.name
    }

    #[inline]
    pub fn variance(&self) -> Variance {
        self.variance
    }

    #[inline]
    pub fn lower(&self) -> Option<&Type<'ctx, K>> {
        self.lower.as_ref()
    }

    #[inline]
    pub fn upper(&self) -> Option<&Type<'ctx, K>> {
        self.upper.as_ref()
    }
}

impl<K: TypeKind> Hash for CtxVar<'_, K> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<K: TypeKind> fmt::Display for CtxVar<'_, K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    #[default]
    Covariant,
    Contravariant,
    Invariant,
}

impl Variance {
    #[inline]
    pub fn combined(self, other: Self) -> Self {
        match (self, other) {
            (Variance::Covariant, Variance::Covariant) => Variance::Covariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Contravariant,
            _ => Variance::Invariant,
        }
    }
}

impl ops::Not for Variance {
    type Output = Self;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Self::Covariant => Self::Contravariant,
            Self::Contravariant => Self::Covariant,
            Self::Invariant => Self::Invariant,
        }
    }
}

impl ops::Mul<Variance> for Variance {
    type Output = Self;

    fn mul(self, rhs: Variance) -> Self::Output {
        match (self, rhs) {
            (Variance::Invariant, _) | (_, Variance::Invariant) => Variance::Invariant,
            (Variance::Covariant, Variance::Covariant)
            | (Variance::Contravariant, Variance::Contravariant) => Variance::Covariant,
            _ => Variance::Contravariant,
        }
    }
}

impl From<ast::Variance> for Variance {
    fn from(v: ast::Variance) -> Self {
        match v {
            ast::Variance::Covariant => Variance::Covariant,
            ast::Variance::Contravariant => Variance::Contravariant,
            ast::Variance::Invariant => Variance::Invariant,
        }
    }
}

impl fmt::Display for Variance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variance::Covariant => f.write_str("covariant"),
            Variance::Contravariant => f.write_str("contravariant"),
            Variance::Invariant => f.write_str("invariant"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefType {
    Strong,
    Weak,
    Script,
}

#[derive(Debug)]
pub struct UnresolvedVar<'ctx>(&'ctx str);

impl AsRef<str> for UnresolvedVar<'_> {
    fn as_ref(&self) -> &str {
        self.0
    }
}
