use std::borrow::Cow;
use std::fmt::Display;
use std::{fmt, iter};

use smallvec::SmallVec;

use crate::lower::error::{CoalesceError, InferResult, TypeError};
use crate::lower::simplify::Simplifier;
use crate::symbols::{AnyBaseType, BaseMode, ClassSupertype};
use crate::types::{RefType, Type, TypeApp, TypeId, TypeParamVar, Variance, predef};
use crate::utils::ScopedMap;
use crate::{Shared, Symbols};

mod solver;

#[cfg(test)]
mod tests;

pub use solver::{Var, VarKey};

/// An inference-time type, punned as `Type` throughout `lower`.
///
/// It mirrors [`Type`] but its applications carry [`PolyType`] arguments and
/// its context-variable bounds refer back to itself. Keeping it nominal (rather
/// than aliasing a generic `Type`) preserves the printed `Nothing` / `Data` /
/// `Ctx` shape used by snapshots and keeps every nesting position covariant.
#[derive(Debug, Clone)]
pub enum InferredType<'ctx> {
    Nothing,
    Data(InferredTypeApp<'ctx>),
    Ctx(Shared<InferredTypeParamVar<'ctx>>),
}

pub type InferredTypeApp<'ctx> = TypeApp<'ctx, PolyType<'ctx>>;
pub type InferredTypeParamVar<'ctx> = TypeParamVar<'ctx, InferredType<'ctx>>;

impl<'ctx> InferredType<'ctx> {
    #[inline]
    pub fn nullary(id: TypeId<'ctx>) -> Self {
        Self::Data(InferredTypeApp::nullary(id))
    }

    #[inline]
    pub fn app(id: TypeId<'ctx>, args: impl Into<Shared<[PolyType<'ctx>]>>) -> Self {
        Self::Data(InferredTypeApp::new(id, args))
    }

    pub fn upper_bound(&self) -> Option<&InferredTypeApp<'ctx>> {
        match self {
            Self::Ctx(v) => v.upper().and_then(Self::upper_bound),
            Self::Data(t) => Some(t),
            Self::Nothing => None,
        }
    }

    #[inline]
    pub fn unwrap_ref(&self) -> Option<&PolyType<'ctx>> {
        Some(self.strip_ref()?.1)
    }

    #[inline]
    pub fn strip_ref(&self) -> Option<(RefType, &PolyType<'ctx>)> {
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
                crate::TypeSchema::Primitive | crate::TypeSchema::Enum(_)
            ),
            _ => false,
        }
    }

    #[inline]
    pub fn display_compact(&self) -> impl fmt::Display + use<'_, 'ctx> {
        self.display(true)
    }

    fn display(&self, compact: bool) -> impl fmt::Display + use<'_, 'ctx> {
        crate::utils::fmt::DisplayFn::new(move |f: &mut fmt::Formatter<'_>| match self {
            Self::Nothing => write!(f, "Nothing"),
            Self::Data(t) => t.display(compact).fmt(f),
            Self::Ctx(v) => write!(f, "{}", v.name()),
        })
    }
}

impl PartialEq for InferredType<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nothing, Self::Nothing) => true,
            (Self::Data(a), Self::Data(b)) => a == b,
            (Self::Ctx(a), Self::Ctx(b)) => a.name() == b.name(),
            _ => false,
        }
    }
}

impl Eq for InferredType<'_> {}

impl fmt::Display for InferredType<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(false).fmt(f)
    }
}

impl<'ctx> InferredTypeApp<'ctx> {
    #[inline]
    pub fn into_type(self) -> InferredType<'ctx> {
        InferredType::Data(self)
    }

    fn from_type_with_env(
        typ: &TypeApp<'ctx>,
        env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
    ) -> Result<Self, &'ctx str> {
        let args = typ
            .args()
            .iter()
            .map(|typ| PolyType::from_type_with_env(typ, env))
            .collect::<Result<Shared<_>, _>>()?;
        Ok(TypeApp::new(typ.id(), args))
    }

    pub fn from_type(typ: &TypeApp<'ctx>) -> Self {
        TypeApp::new(
            typ.id(),
            typ.args()
                .iter()
                .map(PolyType::from_type)
                .collect::<Shared<_>>(),
        )
    }

    #[inline]
    pub fn coalesce(&self, symbols: &Symbols<'ctx>) -> Result<TypeApp<'ctx>, CoalesceError<'ctx>> {
        Simplifier::coalesce_app(self, symbols)
    }

    pub fn from_id(id: TypeId<'ctx>, symbols: &Symbols<'ctx>) -> Self {
        let class = &symbols[id];
        let args = class
            .params()
            .iter()
            .map(|_| PolyType::fresh())
            .collect::<Shared<_>>();
        TypeApp::new(id, args)
    }

    pub fn instantiate_as<Mode: BaseMode>(
        self,
        target: TypeId<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> Option<InferredTypeApp<'ctx>> {
        let mut cur = self;
        while cur.id() != target {
            cur = cur.instantiate_base::<Mode>(symbols)?;
        }
        Some(cur)
    }

    pub fn instantiate_base<Mode: BaseMode>(
        self,
        symbols: &Symbols<'ctx>,
    ) -> Option<InferredTypeApp<'ctx>> {
        let class = &symbols[self.id()];
        let args = self
            .args()
            .iter()
            .cloned()
            .chain(iter::repeat_with(PolyType::fresh));
        let env = class.vars().zip(args).collect();
        TypeApp::from_type_with_env(Mode::base(class.schema().as_aggregate()?)?, &env).ok()
    }

    pub fn base_type_env<'scope, 'sym, Mode: BaseMode>(
        self,
        target: TypeId<'ctx>,
        symbols: &'sym Symbols<'ctx>,
    ) -> Option<ScopedMap<'scope, &'ctx str, PolyType<'ctx>>> {
        Some(
            self.instantiate_as::<Mode>(target, symbols)?
                .type_env(symbols),
        )
    }

    fn glb(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        if self.id() != other.id() {
            let id = symbols
                .glb(self.id(), other.id())
                .ok_or_else(|| TypeError::Incompatible(self.clone(), other.clone()))?;
            // Fresh arguments let constraints from the inherited application refine
            // the subtype's arguments, rather than returning one operand unchanged.
            let candidate = Self::from_id(id, symbols);
            let typ = candidate.clone().into_type();
            typ.constrain::<AnyBaseType>(&self.clone().into_type(), symbols)?;
            typ.constrain::<AnyBaseType>(&other.clone().into_type(), symbols)?;
            return Ok(candidate);
        }
        self.merge_args(other, Variance::Contravariant, symbols)
    }

    fn lub(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let id = symbols
            .lub(self.id(), other.id())
            .ok_or_else(|| TypeError::Incompatible(self.clone(), other.clone()))?;
        let lhs = self
            .clone()
            .instantiate_as::<AnyBaseType>(id, symbols)
            .expect("should always match the lub type");
        let rhs = other
            .clone()
            .instantiate_as::<AnyBaseType>(id, symbols)
            .expect("should always match the lub type");
        lhs.merge_args(&rhs, Variance::Covariant, symbols)
    }

    // Both applications have the same ID. Covariance joins their arguments;
    // contravariance meets them, with each parameter's variance composed in.
    fn merge_args(
        &self,
        other: &Self,
        variance: Variance,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, Self> {
        let args = symbols[self.id()]
            .params()
            .iter()
            .zip(self.args().iter().zip(other.args()))
            .map(|(param, (l, r))| match variance * param.variance() {
                Variance::Covariant => l.lub(r, symbols),
                Variance::Contravariant => l.glb(r, symbols),
                Variance::Invariant => {
                    l.constrain_invariant(r, symbols)?;
                    Ok(l.clone())
                }
            })
            .collect::<Result<Shared<_>, _>>()?;
        Ok(TypeApp::new(self.id(), args))
    }
}

impl fmt::Display for InferredTypeApp<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(false).fmt(f)
    }
}

impl<'ctx> InferredType<'ctx> {
    #[inline]
    pub fn into_poly(self) -> PolyType<'ctx> {
        PolyType::Mono(self)
    }

    fn glb(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        if self.proves_subtype::<AnyBaseType>(other, symbols) {
            return Ok(self.clone());
        }
        if other.proves_subtype::<AnyBaseType>(self, symbols) {
            return Ok(other.clone());
        }
        let typ = match (self, other) {
            (Self::Nothing, _) | (_, Self::Nothing) => Self::Nothing,
            (Self::Data(lhs), Self::Data(rhs)) => Self::Data(lhs.glb(rhs, symbols)?),
            (Self::Ctx(lhs), Self::Ctx(rhs)) if lhs.name() == rhs.name() => Self::Ctx(lhs.clone()),
            (Self::Ctx(_), _) | (_, Self::Ctx(_)) => {
                return Err(TypeError::CannotUnify(self.clone(), other.clone()));
            }
        };
        Ok(typ)
    }

    fn lub(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        if self.proves_subtype::<AnyBaseType>(other, symbols) {
            return Ok(other.clone());
        }
        if other.proves_subtype::<AnyBaseType>(self, symbols) {
            return Ok(self.clone());
        }
        let typ = match (self, other) {
            (Self::Nothing, other) => other.clone(),
            (_, Self::Nothing) => self.clone(),
            (Self::Data(lhs), Self::Data(rhs)) => Self::Data(lhs.lub(rhs, symbols)?),
            (Self::Ctx(lhs), Self::Ctx(rhs)) if lhs.name() == rhs.name() => Self::Ctx(lhs.clone()),
            (Self::Ctx(ctx), t) | (t, Self::Ctx(ctx)) => ctx
                .upper()
                .ok_or_else(|| TypeError::CannotUnify(self.clone(), other.clone()))?
                .lub(t, symbols)?,
        };
        Ok(typ)
    }

    fn occurs(&self, var: &Var<'ctx>, seen: &mut SmallVec<[Var<'ctx>; 4]>) -> bool {
        match self {
            Self::Data(app) => app.args().iter().any(|arg| match arg {
                PolyType::Mono(typ) => typ.occurs(var, seen),
                PolyType::Var(v) => {
                    let v = v.repr();
                    if v == var.repr() {
                        return true;
                    }
                    if seen.contains(&v) {
                        return false;
                    }
                    seen.push(v.clone());
                    v.lower().occurs(var, seen) || v.upper().is_some_and(|u| u.occurs(var, seen))
                }
            }),
            Self::Ctx(ctx) => {
                ctx.lower().is_some_and(|t| t.occurs(var, seen))
                    || ctx.upper().is_some_and(|t| t.occurs(var, seen))
            }
            Self::Nothing => false,
        }
    }

    fn proves_subtype<Mode: BaseMode>(&self, other: &Self, symbols: &Symbols<'ctx>) -> bool {
        self.clone()
            .into_poly()
            .proves_subtype::<Mode>(&other.clone().into_poly(), symbols)
    }

    fn constrain<Mode: BaseMode>(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        if self.proves_subtype::<Mode>(other, symbols) {
            return Ok(());
        }
        match (self, other) {
            (Self::Nothing, _) => {}
            (Self::Data(lhs), Self::Data(rhs)) => {
                let l0 = lhs
                    .clone()
                    .instantiate_as::<Mode>(rhs.id(), symbols)
                    .ok_or_else(|| TypeError::Mismatch(self.clone(), other.clone()))?;
                let r0 = rhs;
                for (v, (l, r)) in symbols[r0.id()]
                    .params()
                    .iter()
                    .zip(l0.args().iter().zip(r0.args()))
                {
                    let res = match v.variance() {
                        Variance::Covariant => l.constrain::<ClassSupertype>(r, symbols),
                        Variance::Contravariant => r.constrain::<ClassSupertype>(l, symbols),
                        Variance::Invariant => l.constrain_invariant(r, symbols),
                    };
                    res.map_err(|err| TypeError::Nested(err.into(), self.clone(), other.clone()))?;
                }
            }
            (Self::Ctx(lhs), Self::Ctx(rhs)) if lhs.name() == rhs.name() => {}
            (Self::Ctx(lhs), _) => {
                let upper = lhs
                    .upper()
                    .ok_or_else(|| TypeError::Mismatch(self.clone(), other.clone()))?;
                upper
                    .constrain::<Mode>(other, symbols)
                    .map_err(|err| TypeError::Nested(err.into(), self.clone(), other.clone()))?;
            }

            _ => return Err(TypeError::Mismatch(self.clone(), other.clone())),
        };
        Ok(())
    }

    pub fn constrain_invariant(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        self.constrain::<AnyBaseType>(other, symbols)?;
        other.constrain::<AnyBaseType>(self, symbols)
    }

    fn is_subtype_compatible(
        &self,
        other: &Type<'ctx>,
        variance: Variance,
        symbols: &Symbols<'ctx>,
    ) -> bool {
        #[inline]
        fn check<'ctx>(
            lhs: TypeId<'ctx>,
            rhs: TypeId<'ctx>,
            variance: Variance,
            symbols: &Symbols<'ctx>,
        ) -> bool {
            match variance {
                Variance::Covariant => symbols.has_base_type(lhs, rhs),
                Variance::Contravariant => symbols.has_base_type(rhs, lhs),
                Variance::Invariant => lhs == rhs,
            }
        }

        match (self, other) {
            (Self::Nothing, _) | (_, Type::Ctx(_)) => true,
            (Self::Data(lhs), Type::Data(rhs)) if lhs.id() == rhs.id() => lhs
                .args()
                .iter()
                .zip(rhs.args())
                .zip(symbols[rhs.id()].params())
                .all(|((l, r), t)| l.is_subtype_compatible(r, variance * t.variance(), symbols)),
            (Self::Data(lhs), Type::Data(rhs)) => check(lhs.id(), rhs.id(), variance, symbols),
            (Self::Ctx(lhs), Type::Data(rhs)) => lhs
                .upper()
                .and_then(|typ| typ.upper_bound())
                .is_some_and(|lhs| check(lhs.id(), rhs.id(), variance, symbols)),
            _ => false,
        }
    }
}

impl<'ctx> From<&Type<'ctx>> for InferredType<'ctx> {
    fn from(typ: &Type<'ctx>) -> Self {
        match typ {
            Type::Nothing => Self::Nothing,
            Type::Data(typ) => Self::Data(InferredTypeApp::from_type(typ)),
            Type::Ctx(var) => Self::Ctx(InferredTypeParamVar::from_var(var).into()),
        }
    }
}

impl<'ctx> InferredTypeParamVar<'ctx> {
    pub fn from_var(var: &TypeParamVar<'ctx>) -> Self {
        let lower = var.lower().map(InferredType::from);
        let upper = var.upper().map(InferredType::from);
        Self::new(var.name(), var.variance(), lower, upper)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolyType<'ctx> {
    Mono(InferredType<'ctx>),
    Var(Var<'ctx>),
}

impl<'ctx> PolyType<'ctx> {
    #[inline]
    pub fn nullary(id: TypeId<'ctx>) -> Self {
        InferredType::nullary(id).into_poly()
    }

    /// Create a new `PolyType` from a `Type` within the same environment.
    #[inline]
    pub fn from_type(typ: &Type<'ctx>) -> Self {
        InferredType::from(typ).into_poly()
    }

    /// Create a new `PolyType` from a `Type` within the provided environment.
    pub fn from_type_with_env(
        typ: &Type<'ctx>,
        env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
    ) -> Result<PolyType<'ctx>, &'ctx str> {
        let result = match typ {
            Type::Nothing => InferredType::Nothing,
            Type::Data(typ) => InferredType::Data(InferredTypeApp::from_type_with_env(typ, env)?),
            Type::Ctx(var) => return env.get(var.name()).cloned().ok_or(var.name()),
        };
        Ok(result.into_poly())
    }

    #[inline]
    pub fn coalesce(&self, symbols: &Symbols<'ctx>) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        Simplifier::coalesce(self, symbols)
    }

    #[inline]
    pub fn fresh() -> Self {
        Self::Var(Var::fresh())
    }

    #[inline]
    pub fn with_bounds(lower: InferredType<'ctx>, upper: Option<InferredType<'ctx>>) -> Self {
        Self::Var(Var::new(lower, upper))
    }

    pub fn force_upper_bound(
        &self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, Option<Cow<'_, InferredTypeApp<'ctx>>>> {
        match self {
            Self::Mono(typ) => Ok(typ.upper_bound().map(Cow::Borrowed)),
            Self::Var(var) => {
                let lower = var.lower();
                var.add_upper(&lower, symbols)?;
                Ok(self.upper_bound(symbols))
            }
        }
    }

    pub fn upper_bound(&self, symbols: &Symbols<'ctx>) -> Option<Cow<'_, InferredTypeApp<'ctx>>> {
        match self {
            Self::Mono(typ) => typ.upper_bound().map(Cow::Borrowed),
            Self::Var(var) => {
                let lower = var.lower();
                let bound = if lower.is_primitive(symbols) {
                    Some(lower)
                } else {
                    var.upper()
                };
                bound
                    .as_ref()
                    .and_then(InferredType::upper_bound)
                    .cloned()
                    .map(Cow::Owned)
            }
        }
    }

    fn glb(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let typ = match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => Self::Mono(l.glb(r, symbols)?),
            _ if self == other => self.clone(),
            _ => {
                let result = Self::fresh();
                result.constrain::<ClassSupertype>(self, symbols)?;
                result.constrain::<ClassSupertype>(other, symbols)?;
                result
            }
        };
        Ok(typ)
    }

    pub fn lub(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, Self> {
        let typ = match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => Self::Mono(l.lub(r, symbols)?),
            _ if self == other => self.clone(),
            _ => {
                let result = Self::fresh();
                self.constrain::<ClassSupertype>(&result, symbols)?;
                other.constrain::<ClassSupertype>(&result, symbols)?;
                result
            }
        };
        Ok(typ)
    }

    #[inline]
    pub fn constrain_base(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        self.constrain::<AnyBaseType>(other, symbols)
    }

    pub fn constrain<Mode: BaseMode>(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => l.constrain::<Mode>(r, symbols),
            (Self::Mono(l), Self::Var(r)) => r.add_lower(l, symbols),
            (Self::Var(l), Self::Mono(r)) => l.add_upper(r, symbols),
            (Self::Var(l), Self::Var(r)) => l.subtype(r, symbols),
        }
    }

    pub fn constrain_invariant(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        match (self, other) {
            (Self::Mono(l), Self::Mono(r)) => l.constrain_invariant(r, symbols),
            (Self::Mono(l), Self::Var(r)) => r.select(l, symbols),
            (Self::Var(l), Self::Mono(r)) => {
                l.add_upper(r, symbols)?;
                l.add_lower(r, symbols)
            }
            (Self::Var(l), Self::Var(r)) => l.unify(r, symbols),
        }
    }

    // Prove rigid annotation bounds without treating enclosing type parameters as
    // inference variables. A RHS parameter's upper bound does not prove subtyping.
    pub fn satisfies_bound(&self, other: &Self, symbols: &Symbols<'ctx>) -> bool {
        self.proves_subtype::<AnyBaseType>(other, symbols)
    }

    fn proves_subtype<Mode: BaseMode>(&self, other: &Self, symbols: &Symbols<'ctx>) -> bool {
        if self == other {
            return true;
        }
        match (self, other) {
            (Self::Var(var), _) if var.upper().as_ref() == Some(&var.lower()) => var
                .lower()
                .into_poly()
                .proves_subtype::<Mode>(other, symbols),
            (_, Self::Var(var)) if var.upper().as_ref() == Some(&var.lower()) => {
                self.proves_subtype::<Mode>(&var.lower().into_poly(), symbols)
            }
            (Self::Mono(InferredType::Nothing), _) => true,
            (Self::Mono(InferredType::Ctx(var)), _)
                if var.upper().is_some_and(|upper| {
                    upper
                        .clone()
                        .into_poly()
                        .proves_subtype::<Mode>(other, symbols)
                }) =>
            {
                true
            }
            (_, Self::Mono(InferredType::Ctx(var))) => var.lower().is_some_and(|lower| {
                self.proves_subtype::<Mode>(&lower.clone().into_poly(), symbols)
            }),
            (Self::Mono(InferredType::Data(lhs)), Self::Mono(InferredType::Data(rhs))) => {
                let Some(lhs) = lhs.clone().instantiate_as::<Mode>(rhs.id(), symbols) else {
                    return false;
                };
                let params = symbols[rhs.id()].params();
                lhs.args().len() == params.len()
                    && rhs.args().len() == params.len()
                    && lhs
                        .args()
                        .iter()
                        .zip(rhs.args())
                        .zip(params)
                        .all(|((l, r), param)| match param.variance() {
                            Variance::Covariant => l.proves_subtype::<ClassSupertype>(r, symbols),
                            Variance::Contravariant => {
                                r.proves_subtype::<ClassSupertype>(l, symbols)
                            }
                            Variance::Invariant => {
                                l.proves_subtype::<AnyBaseType>(r, symbols)
                                    && r.proves_subtype::<AnyBaseType>(l, symbols)
                            }
                        })
            }
            _ => false,
        }
    }

    pub fn coerce(
        &self,
        other: &Self,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, Option<Coercion>> {
        match (self.strip_ref(symbols), other.strip_ref(symbols)) {
            (Some((from, pointee)), None) => {
                pointee.constrain::<AnyBaseType>(other, symbols)?;
                Ok(Some(Coercion::FromRef(from)))
            }
            (None, Some((to, pointee))) => {
                self.constrain::<AnyBaseType>(&pointee, symbols)?;
                Ok(Some(Coercion::IntoRef(to)))
            }
            _ if matches!(other.upper_bound(symbols), Some(app) if app.id() == predef::VARIANT)
                && matches!(self.upper_bound(symbols), Some(app) if app.id() != predef::VARIANT) =>
            {
                Ok(Some(Coercion::IntoVariant))
            }
            _ => {
                self.constrain::<AnyBaseType>(other, symbols)?;
                Ok(None)
            }
        }
    }

    #[inline]
    pub fn unwrap_ref(&self, symbols: &Symbols<'ctx>) -> Option<PolyType<'ctx>> {
        Some(self.strip_ref(symbols)?.1)
    }

    #[inline]
    pub fn unwrap_ref_or_self(&self, symbols: &Symbols<'ctx>) -> PolyType<'ctx> {
        self.unwrap_ref(symbols).unwrap_or_else(|| self.clone())
    }

    #[inline]
    pub fn strip_ref(&self, symbols: &Symbols<'ctx>) -> Option<(RefType, PolyType<'ctx>)> {
        self.upper_bound(symbols)
            .as_deref()
            .and_then(TypeApp::strip_ref)
            .map(|(rt, typ)| (rt, typ.clone()))
    }

    pub fn ref_type(&self, symbols: &Symbols<'ctx>) -> Option<RefType> {
        self.upper_bound(symbols)
            .as_deref()
            .and_then(TypeApp::ref_type)
    }

    pub fn is_subtype_compatible(
        &self,
        other: &Type<'ctx>,
        variance: Variance,
        symbols: &Symbols<'ctx>,
    ) -> bool {
        match self {
            Self::Mono(typ) => typ.is_subtype_compatible(other, variance, symbols),
            Self::Var(var) => {
                // For L <: X <: U, eligibility asks whether X can satisfy the
                // candidate, not whether the candidate equals both endpoints.
                // X <: T needs L <: T; T <: X needs T <: U. Equality needs both.
                (variance == Variance::Contravariant
                    || var
                        .lower()
                        .is_subtype_compatible(other, Variance::Covariant, symbols))
                    && (variance == Variance::Covariant
                        || var.upper().is_none_or(|upper| {
                            upper.is_subtype_compatible(other, Variance::Contravariant, symbols)
                        }))
            }
        }
    }

    #[inline]
    pub fn is_fresh(&self) -> bool {
        matches!(self, Self::Var(var) if var.is_fresh())
    }
}

impl<'ctx> From<(TypeId<'ctx>, Shared<[PolyType<'ctx>]>)> for PolyType<'ctx> {
    #[inline]
    fn from((id, args): (TypeId<'ctx>, Shared<[PolyType<'ctx>]>)) -> Self {
        InferredType::app(id, args).into_poly()
    }
}

impl fmt::Display for PolyType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mono(typ) => typ.fmt(f),
            Self::Var(_) => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Coercion {
    FromRef(RefType),
    IntoRef(RefType),
    IntoVariant,
}
