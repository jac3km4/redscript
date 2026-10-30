use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::{fmt, iter, ptr};

use identity_hash::IdentityHashable;
use smallvec::SmallVec;

use crate::lower::error::{CoalesceError, InferResult, TypeError};
use crate::lower::simplify::Simplifier;
use crate::symbols::{AnyBaseType, AnySupertype, BaseMode};
use crate::types::{CtxVar, RefType, Type, TypeApp, TypeId, Variance, predef};
use crate::utils::ScopedMap;
use crate::{Symbols, TypeKind};

#[derive(Debug)]
pub struct Poly;

impl TypeKind for Poly {
    type Type<'ctx> = PolyType<'ctx>;
}

pub type InferredType<'ctx> = Type<'ctx, Poly>;
pub type InferredTypeApp<'ctx> = TypeApp<'ctx, Poly>;
pub type InferredCtxVar<'ctx> = CtxVar<'ctx, Poly>;

impl<'ctx> InferredTypeApp<'ctx> {
    fn from_type_with_env(
        typ: &TypeApp<'ctx>,
        env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
    ) -> Result<Self, &'ctx str> {
        let args = typ
            .args()
            .iter()
            .map(|typ| PolyType::from_type_with_env(typ, env))
            .collect::<Result<Rc<_>, _>>()?;
        Ok(TypeApp::new(typ.id(), args))
    }

    pub fn from_type(typ: &TypeApp<'ctx>) -> Self {
        TypeApp::new(
            typ.id(),
            typ.args()
                .iter()
                .map(PolyType::from_type)
                .collect::<Rc<_>>(),
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
            .collect::<Rc<_>>();
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
            .collect::<Result<Rc<_>, _>>()?;
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
                        Variance::Covariant => l.constrain::<AnySupertype>(r, symbols),
                        Variance::Contravariant => r.constrain::<AnySupertype>(l, symbols),
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
            Type::Ctx(var) => Self::Ctx(InferredCtxVar::from_var(var).into()),
        }
    }
}

impl<'ctx> InferredCtxVar<'ctx> {
    pub fn from_var(var: &CtxVar<'ctx>) -> Self {
        let lower = var.lower().map(Type::from);
        let upper = var.upper().map(Type::from);
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
        Type::nullary(id).into_poly()
    }

    /// Create a new `PolyType` from a `Type` within the same environment.
    #[inline]
    pub fn from_type(typ: &Type<'ctx>) -> Self {
        Type::from(typ).into_poly()
    }

    /// Create a new `PolyType` from a `Type` within the provided environment.
    pub fn from_type_with_env(
        typ: &Type<'ctx>,
        env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
    ) -> Result<PolyType<'ctx>, &'ctx str> {
        let result = match typ {
            Type::Nothing => Type::Nothing,
            Type::Data(typ) => Type::Data(InferredTypeApp::from_type_with_env(typ, env)?),
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
                    .and_then(Type::upper_bound)
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
                result.constrain::<AnySupertype>(self, symbols)?;
                result.constrain::<AnySupertype>(other, symbols)?;
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
                self.constrain::<AnySupertype>(&result, symbols)?;
                other.constrain::<AnySupertype>(&result, symbols)?;
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

    fn constrain_invariant(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
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
            (Self::Mono(Type::Nothing), _) => true,
            (Self::Mono(Type::Ctx(var)), _)
                if var.upper().is_some_and(|upper| {
                    upper
                        .clone()
                        .into_poly()
                        .proves_subtype::<Mode>(other, symbols)
                }) =>
            {
                true
            }
            (_, Self::Mono(Type::Ctx(var))) => var.lower().is_some_and(|lower| {
                self.proves_subtype::<Mode>(&lower.clone().into_poly(), symbols)
            }),
            (Self::Mono(Type::Data(lhs)), Self::Mono(Type::Data(rhs))) => {
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
                            Variance::Covariant => l.proves_subtype::<AnySupertype>(r, symbols),
                            Variance::Contravariant => r.proves_subtype::<AnySupertype>(l, symbols),
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
                var.lower().is_subtype_compatible(other, variance, symbols)
                    && var
                        .upper()
                        .is_none_or(|u| u.is_subtype_compatible(other, !variance, symbols))
            }
        }
    }

    #[inline]
    pub fn is_fresh(&self) -> bool {
        matches!(self, Self::Var(var) if var.is_fresh())
    }
}

impl<'ctx> From<(TypeId<'ctx>, Rc<[PolyType<'ctx>]>)> for PolyType<'ctx> {
    #[inline]
    fn from((id, args): (TypeId<'ctx>, Rc<[PolyType<'ctx>]>)) -> Self {
        Type::app(id, args).into_poly()
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

// External handles own the component; handles stored in nodes have no owner.
// Components retain all nodes until the last external handle is dropped, then
// clear their internal links. This permits mixed bound/edge cycles without
// making ownership decisions constrain the eventual inferred types.
#[derive(Clone)]
pub struct Var<'ctx>(
    Rc<RefCell<VarState<'ctx>>>,
    Option<Rc<RefCell<VarComponent<'ctx>>>>,
);

type VarEdges<'ctx> = SmallVec<[Var<'ctx>; 2]>;

#[derive(Default)]
struct VarComponent<'ctx> {
    nodes: Vec<Rc<RefCell<VarState<'ctx>>>>,
    parent: Option<Rc<RefCell<Self>>>,
}

impl Drop for VarComponent<'_> {
    fn drop(&mut self) {
        // Internal handles do not own the component. Break all node-to-node
        // references when the last external handle releases the component.
        for node in &self.nodes {
            let mut state = node.borrow_mut();
            state.lower = Type::Nothing;
            state.upper = None;
            state.supers.clear();
            state.subs.clear();
            state.repr = None;
        }
    }
}

impl fmt::Debug for Var<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Var").field(&self.key()).finish()
    }
}

impl<'ctx> Var<'ctx> {
    #[inline]
    fn new(lower: InferredType<'ctx>, upper: Option<InferredType<'ctx>>) -> Self {
        let component = Rc::new(RefCell::new(VarComponent::default()));
        let node = Rc::new(RefCell::new(VarState {
            lower: Type::Nothing,
            upper: None,
            repr: None,
            supers: SmallVec::new(),
            subs: SmallVec::new(),
            updating: false,
            component: Rc::downgrade(&component),
        }));
        component.borrow_mut().nodes.push(node.clone());
        let var = Self(node, Some(component));
        var.store_bounds(&lower, upper.as_ref());
        var
    }

    fn external(node: Rc<RefCell<VarState<'ctx>>>) -> Self {
        let component = node
            .borrow()
            .component
            .upgrade()
            .expect("live inference component");
        Self(node, Some(component))
    }

    fn component(&self) -> Rc<RefCell<VarComponent<'ctx>>> {
        let mut component = self.1.clone().unwrap_or_else(|| {
            self.0
                .borrow()
                .component
                .upgrade()
                .expect("live inference component")
        });
        loop {
            let parent = component.borrow().parent.clone();
            match parent {
                Some(parent) => component = parent,
                None => return component,
            }
        }
    }

    // Merge ownership, not type identities. Old external handles keep the
    // merged component alive through the old component's parent link.
    fn retain(&self, other: &Self) -> Self {
        let lhs = self.component();
        let rhs = other.component();
        if !Rc::ptr_eq(&lhs, &rhs) {
            let nodes = std::mem::take(&mut rhs.borrow_mut().nodes);
            for node in &nodes {
                node.borrow_mut().component = Rc::downgrade(&lhs);
            }
            lhs.borrow_mut().nodes.extend(nodes);
            rhs.borrow_mut().parent = Some(lhs);
        }
        Self(other.0.clone(), None)
    }

    fn map_type(
        typ: &InferredType<'ctx>,
        map: &mut impl FnMut(&Self) -> Self,
    ) -> InferredType<'ctx> {
        match typ {
            Type::Nothing => Type::Nothing,
            Type::Data(app) => Type::app(
                app.id(),
                app.args()
                    .iter()
                    .map(|arg| match arg {
                        PolyType::Var(var) => PolyType::Var(map(var)),
                        PolyType::Mono(typ) => Self::map_type(typ, map).into_poly(),
                    })
                    .collect::<Rc<_>>(),
            ),
            Type::Ctx(ctx) => Type::Ctx(Rc::new(CtxVar::new(
                ctx.name(),
                ctx.variance(),
                ctx.lower().map(|t| Self::map_type(t, map)),
                ctx.upper().map(|t| Self::map_type(t, map)),
            ))),
        }
    }

    fn store_bounds(&self, lower: &InferredType<'ctx>, upper: Option<&InferredType<'ctx>>) {
        let lower = Self::map_type(lower, &mut |var| self.retain(var));
        let upper = upper.map(|typ| Self::map_type(typ, &mut |var| self.retain(var)));
        let mut state = self.0.borrow_mut();
        state.lower = lower;
        state.upper = upper;
    }

    #[inline]
    fn fresh() -> Self {
        Self::new(Type::Nothing, None)
    }

    fn is_fresh(&self) -> bool {
        let state = self.0.borrow();
        matches!(&state.lower, Type::Nothing)
            && state.upper.is_none()
            && state.repr.is_none()
            && state.supers.is_empty()
            && state.subs.is_empty()
    }

    #[inline]
    pub fn key(&self) -> VarKey<'ctx> {
        VarKey(self.repr().0.as_ptr())
    }

    fn repr(&self) -> Var<'ctx> {
        let parent = self.0.borrow().repr.clone();
        match parent {
            Some(var) => {
                let rep = var.repr();
                self.0.borrow_mut().repr = Some(Self(rep.0.clone(), None));
                rep
            }
            None => Self::external(self.0.clone()),
        }
    }

    #[inline]
    pub fn lower(&self) -> InferredType<'ctx> {
        let typ = self.repr().0.borrow().lower.clone();
        Self::map_type(&typ, &mut |var| Self::external(var.0.clone()))
    }

    #[inline]
    pub fn upper(&self) -> Option<InferredType<'ctx>> {
        let typ = self.repr().0.borrow().upper.clone();
        typ.as_ref()
            .map(|typ| Self::map_type(typ, &mut |var| Self::external(var.0.clone())))
    }

    pub(super) fn select(
        &self,
        typ: &InferredType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        self.add_lower(typ, symbols)?;
        self.add_upper(typ, symbols)
    }

    fn reaches(&self, target: &Self, seen: &mut SmallVec<[Self; 4]>) -> bool {
        let this = self.repr();
        if this == target.repr() {
            return true;
        }
        if seen.contains(&this) {
            return false;
        }
        seen.push(this.clone());
        let next = this.0.borrow().supers.clone();
        next.iter().any(|v| v.reaches(target, seen))
    }

    fn normalize_edges(&self, vars: impl IntoIterator<Item = Self>) -> VarEdges<'ctx> {
        let mut edges = SmallVec::new();
        for var in vars {
            let var = var.repr();
            if var != *self && !edges.contains(&var) {
                edges.push(self.retain(&var));
            }
        }
        edges
    }

    fn subtype(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        let lhs = self.repr();
        let rhs = other.repr();
        if lhs == rhs {
            Ok(())
        } else if rhs.reaches(&lhs, &mut SmallVec::new()) {
            lhs.collapse_cycle(&rhs, symbols)
        } else {
            lhs.link(&rhs, symbols)
        }
    }

    // Collapse precisely the component closed by self <: other, not unrelated successors.
    fn collapse_cycle(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        let mut pending = vec![other.clone()];
        let mut seen: SmallVec<[_; 4]> = SmallVec::new();
        while let Some(var) = pending.pop() {
            let var = var.repr();
            if seen.contains(&var) {
                continue;
            }
            seen.push(var.clone());
            if var.reaches(self, &mut SmallVec::new()) {
                pending.extend(var.0.borrow().supers.clone());
                self.unify(&var, symbols)?;
            }
        }
        Ok(())
    }

    // Both endpoints must be distinct representatives, with no path back to self.
    fn link(&self, rhs: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        let lhs = self;
        let had_edge = lhs.0.borrow().supers.iter().any(|v| v.repr() == *rhs);
        // Normalize aliases on both sides independently: a surviving forward
        // edge does not imply its reverse edge survived representative merging.
        let supers = std::mem::take(&mut lhs.0.borrow_mut().supers);
        let supers = lhs.normalize_edges(supers.into_iter().chain([rhs.clone()]));
        lhs.0.borrow_mut().supers = supers;
        let subs = std::mem::take(&mut rhs.0.borrow_mut().subs);
        let subs = rhs.normalize_edges(subs.into_iter().chain([lhs.clone()]));
        rhs.0.borrow_mut().subs = subs;
        let result = (|| {
            rhs.add_lower(&lhs.lower(), symbols)?;
            if let Some(upper) = rhs.upper() {
                lhs.add_upper(&upper, symbols)?;
            }
            Ok(())
        })();
        if result.is_err() && !had_edge {
            // Remove a newly added constraint edge when bound propagation fails.
            lhs.0.borrow_mut().supers.retain(|v| v.repr() != *rhs);
            rhs.0.borrow_mut().subs.retain(|v| v.repr() != *lhs);
        }
        result
    }

    fn add_upper(&self, ub: &InferredType<'ctx>, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        self.update(ub, false, symbols, &mut SmallVec::new())
    }

    fn add_lower(&self, lb: &InferredType<'ctx>, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        self.update(lb, true, symbols, &mut SmallVec::new())
    }

    fn update(
        &self,
        bound: &InferredType<'ctx>,
        lower: bool,
        symbols: &Symbols<'ctx>,
        seen: &mut SmallVec<[Self; 4]>,
    ) -> InferResult<'ctx, ()> {
        let repr = self.repr();
        if seen.contains(&repr) {
            return Ok(());
        }
        if repr.0.borrow().updating {
            let implied = if lower {
                bound.proves_subtype::<AnySupertype>(&repr.lower(), symbols)
            } else {
                repr.upper()
                    .is_some_and(|u| u.proves_subtype::<AnySupertype>(bound, symbols))
            };
            return if implied {
                Ok(())
            } else {
                Err(TypeError::CannotUnify(repr.lower(), bound.clone()))
            };
        }
        repr.0.borrow_mut().updating = true;
        seen.push(repr.clone());
        let result = (|| {
            let bound = repr.refine_bound(bound, lower, symbols)?;
            let neighbors = {
                let state = repr.0.borrow();
                if lower { &state.supers } else { &state.subs }.clone()
            };
            for var in neighbors {
                var.update(&bound, lower, symbols, seen)?;
            }
            Ok(())
        })();
        seen.pop();
        repr.0.borrow_mut().updating = false;
        result
    }

    // Validate and store a local refinement before propagating it to neighbors.
    fn refine_bound(
        &self,
        bound: &InferredType<'ctx>,
        lower: bool,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, InferredType<'ctx>> {
        if bound.occurs(self, &mut SmallVec::new()) {
            return Err(TypeError::CannotUnify(self.lower(), bound.clone()));
        }
        let mut new_lower = self.lower();
        let mut new_upper = self.upper();
        if lower {
            new_lower = new_lower.lub(bound, symbols)?;
        } else {
            new_upper = Some(match new_upper {
                Some(upper) => upper.glb(bound, symbols)?,
                None => bound.clone(),
            });
        }
        if let Some(upper) = &new_upper {
            new_lower.constrain::<AnySupertype>(upper, symbols)?;
        }
        self.store_bounds(&new_lower, new_upper.as_ref());
        Ok(if lower { new_lower } else { new_upper.unwrap() })
    }

    fn take_edges(&self) -> (VarEdges<'ctx>, VarEdges<'ctx>) {
        let mut state = self.0.borrow_mut();
        (
            std::mem::take(&mut state.supers),
            std::mem::take(&mut state.subs),
        )
    }

    fn redirect_to(&self, target: &Self) -> (VarEdges<'ctx>, VarEdges<'ctx>) {
        let edges = self.take_edges();
        let parent = self.retain(target);
        self.0.borrow_mut().repr = Some(parent);
        // Normalize the survivor before replaying edges: aliases may now be self-edges.
        let (supers, subs) = target.take_edges();
        let supers = target.normalize_edges(supers);
        let subs = target.normalize_edges(subs);
        let mut state = target.0.borrow_mut();
        state.supers = supers;
        state.subs = subs;
        edges
    }

    fn unify(&self, other: &Var<'ctx>, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
        let repr0 = self.repr();
        let repr1 = other.repr();
        if repr0 == repr1 {
            return Ok(());
        }
        repr1.add_lower(&repr0.lower(), symbols)?;
        if let Some(ub) = &repr0.upper() {
            repr1.add_upper(ub, symbols)?;
        }
        let (supers, subs) = repr0.redirect_to(&repr1);
        for var in supers {
            repr1.subtype(&var, symbols)?;
        }
        for var in subs {
            var.subtype(&repr1, symbols)?;
        }
        Ok(())
    }
}

impl PartialEq for Var<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}

impl Eq for Var<'_> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarKey<'ctx>(*mut VarState<'ctx>);

impl IdentityHashable for VarKey<'_> {}

#[derive(Debug, Clone)]
struct VarState<'ctx> {
    lower: InferredType<'ctx>,
    upper: Option<InferredType<'ctx>>,
    repr: Option<Var<'ctx>>,
    supers: VarEdges<'ctx>,
    subs: VarEdges<'ctx>,
    updating: bool,
    component: Weak<RefCell<VarComponent<'ctx>>>,
}

#[derive(Debug, Clone, Copy)]
pub enum Coercion {
    FromRef(RefType),
    IntoRef(RefType),
    IntoVariant,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols::{Aggregate, TypeDef, TypeSchema};
    use crate::types::TypeInterner;

    fn hierarchy(interner: &TypeInterner) -> Symbols<'_> {
        let mut symbols = Symbols::with_default_types();
        for (name, base) in [
            ("Base", None),
            ("Middle", Some("Base")),
            ("Leaf", Some("Middle")),
        ] {
            let mut aggregate = Aggregate::default();
            aggregate.set_base(base.map(|name| TypeApp::nullary(interner.intern(name))));
            symbols.add_type(
                interner.intern(name),
                TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
            );
        }
        symbols
    }

    #[test]
    fn invariant_constraints_reject_later_widening_and_narrowing() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let middle = PolyType::nullary(interner.intern("Middle"));
        fn wrap<'ctx>(mut typ: PolyType<'ctx>, depth: usize) -> PolyType<'ctx> {
            for _ in 0..depth {
                typ = Type::app(predef::ARRAY, [typ]).into_poly();
            }
            typ
        }
        for depth in [0, 2] {
            let concrete = wrap(middle.clone(), depth);
            for mono_first in [true, false] {
                for (name, widen) in [("Base", true), ("Leaf", false)] {
                    let var = PolyType::fresh();
                    let inferred = wrap(var.clone(), depth);
                    let (lhs, rhs) = if mono_first {
                        (&concrete, &inferred)
                    } else {
                        (&inferred, &concrete)
                    };
                    if depth == 0 {
                        lhs.constrain_invariant(rhs, &symbols).unwrap();
                    } else {
                        lhs.constrain_base(rhs, &symbols).unwrap();
                    }
                    let incompatible = PolyType::nullary(interner.intern(name));
                    let result = if widen {
                        incompatible.constrain::<AnySupertype>(&var, &symbols)
                    } else {
                        var.constrain::<AnySupertype>(&incompatible, &symbols)
                    };
                    assert!(
                        result.is_err(),
                        "depth={depth}, mono_first={mono_first}, {name}"
                    );
                }
            }
        }
    }

    #[test]
    fn invariant_mixed_constraints_accept_compatible_bounds() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let base = PolyType::nullary(interner.intern("Base"));
        let middle = PolyType::nullary(interner.intern("Middle"));
        let leaf = PolyType::nullary(interner.intern("Leaf"));

        for mono_first in [true, false] {
            let var = PolyType::fresh();
            leaf.constrain::<AnySupertype>(&var, &symbols).unwrap();
            var.constrain::<AnySupertype>(&base, &symbols).unwrap();
            let (lhs, rhs) = if mono_first {
                (&middle, &var)
            } else {
                (&var, &middle)
            };
            lhs.constrain_invariant(rhs, &symbols).unwrap();

            middle.constrain::<AnySupertype>(&var, &symbols).unwrap();
            var.constrain::<AnySupertype>(&middle, &symbols).unwrap();
            leaf.constrain::<AnySupertype>(&var, &symbols).unwrap();
            var.constrain::<AnySupertype>(&base, &symbols).unwrap();
            rhs.constrain_invariant(lhs, &symbols).unwrap();
        }
    }

    #[test]
    fn rigid_bounds_follow_lower_bounds_without_equating_upper_bounds() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let base = Type::nullary(interner.intern("Base"));
        let leaf = PolyType::nullary(interner.intern("Leaf"));
        let with_lower = Type::Ctx(Rc::new(CtxVar::new(
            "T",
            Variance::Invariant,
            Some(base.clone()),
            None,
        )))
        .into_poly();
        assert!(leaf.satisfies_bound(&with_lower, &symbols));

        let first = Type::Ctx(Rc::new(CtxVar::new(
            "T",
            Variance::Invariant,
            None,
            Some(base.clone()),
        )))
        .into_poly();
        let second = Type::Ctx(Rc::new(CtxVar::new(
            "U",
            Variance::Invariant,
            None,
            Some(base),
        )))
        .into_poly();
        assert!(!first.satisfies_bound(&second, &symbols));
        assert!(!second.satisfies_bound(&first, &symbols));
    }

    #[test]
    fn rejected_constraints_clear_guards_and_remove_new_edges() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let base = Type::nullary(interner.intern("Base"));
        let leaf = Type::nullary(interner.intern("Leaf"));
        for lower in [false, true] {
            let var = Var::new(leaf.clone(), Some(leaf.clone()));
            let incompatible = if lower { &base } else { &Type::Nothing };
            let mut seen = SmallVec::new();
            assert!(
                var.update(incompatible, lower, &symbols, &mut seen)
                    .is_err()
            );
            assert!(seen.is_empty());
            assert!(!var.0.borrow().updating);
            var.select(&leaf, &symbols).unwrap();
        }

        let lhs = Var::new(base.clone(), None);
        let rhs = Var::new(Type::Nothing, Some(leaf.clone()));
        let weak = [Rc::downgrade(&lhs.0), Rc::downgrade(&rhs.0)];
        assert!(lhs.subtype(&rhs, &symbols).is_err());
        assert!(lhs.0.borrow().supers.is_empty());
        assert!(rhs.0.borrow().subs.is_empty());
        assert!(!lhs.0.borrow().updating);
        assert!(!rhs.0.borrow().updating);
        lhs.select(&base, &symbols).unwrap();
        rhs.select(&leaf, &symbols).unwrap();
        drop((lhs, rhs));
        assert!(weak.iter().all(|var| var.upgrade().is_none()));
    }

    #[test]
    fn directional_edges_propagate_before_and_after_linking() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let base = PolyType::nullary(interner.intern("Base"));
        let leaf = PolyType::nullary(interner.intern("Leaf"));
        for link_first in [false, true] {
            for reverse in [false, true] {
                let a = PolyType::fresh();
                let b = PolyType::fresh();
                let c = PolyType::fresh();
                let link = || {
                    if reverse {
                        b.constrain_base(&c, &symbols).unwrap();
                        a.constrain_base(&b, &symbols).unwrap();
                    } else {
                        a.constrain_base(&b, &symbols).unwrap();
                        b.constrain_base(&c, &symbols).unwrap();
                    }
                };
                if link_first {
                    link();
                }
                leaf.constrain_base(&a, &symbols).unwrap();
                c.constrain_base(&base, &symbols).unwrap();
                base.constrain_base(&c, &symbols).unwrap();
                if !link_first {
                    link();
                }
                for var in [&a, &b] {
                    assert_eq!(
                        var.coalesce(&symbols).unwrap(),
                        leaf.coalesce(&symbols).unwrap()
                    );
                }
                assert_eq!(
                    c.coalesce(&symbols).unwrap(),
                    base.coalesce(&symbols).unwrap()
                );
            }
        }
    }

    #[test]
    fn exact_variables_allow_directional_subtype_constraints() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let base = PolyType::nullary(interner.intern("Base"));
        let leaf = PolyType::nullary(interner.intern("Leaf"));
        for exact_lhs in [true, false] {
            let lhs = PolyType::fresh();
            let rhs = PolyType::fresh();
            if exact_lhs {
                lhs.constrain_invariant(&leaf, &symbols).unwrap();
                base.constrain_base(&rhs, &symbols).unwrap();
            } else {
                leaf.constrain_base(&lhs, &symbols).unwrap();
                rhs.constrain_invariant(&base, &symbols).unwrap();
            }
            lhs.constrain_base(&rhs, &symbols).unwrap();
            assert_eq!(
                lhs.coalesce(&symbols).unwrap(),
                Type::nullary(interner.intern("Leaf"))
            );
            assert_eq!(
                rhs.coalesce(&symbols).unwrap(),
                Type::nullary(interner.intern("Base"))
            );
        }
    }

    #[test]
    fn directional_cycles_merge_only_the_strong_component() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        for reverse in [false, true] {
            let a = PolyType::fresh();
            let b = PolyType::fresh();
            let c = PolyType::fresh();
            let outside = PolyType::fresh();
            let base = PolyType::nullary(interner.intern("Base"));
            let leaf = PolyType::nullary(interner.intern("Leaf"));
            a.constrain_base(&b, &symbols).unwrap();
            b.constrain_base(&c, &symbols).unwrap();
            b.constrain_base(&outside, &symbols).unwrap();
            base.constrain_base(&outside, &symbols).unwrap();
            if reverse {
                leaf.constrain_base(&a, &symbols).unwrap();
            }
            c.constrain_base(&a, &symbols).unwrap();
            if !reverse {
                leaf.constrain_base(&a, &symbols).unwrap();
            }
            for var in [&a, &b, &c] {
                assert_eq!(
                    var.coalesce(&symbols).unwrap(),
                    Type::nullary(interner.intern("Leaf"))
                );
            }
            assert_eq!(
                outside.coalesce(&symbols).unwrap(),
                Type::nullary(interner.intern("Base"))
            );
        }
    }

    fn coalesce_var<'ctx>(var: &Var<'ctx>, symbols: &Symbols<'ctx>) -> Type<'ctx> {
        PolyType::Var(var.clone()).coalesce(symbols).unwrap()
    }

    fn link_mixed_component<'ctx>(
        [x, y, z]: &[Var<'ctx>; 3],
        insertion: [usize; 3],
        symbols: &Symbols<'ctx>,
    ) {
        for step in insertion {
            match step {
                0 => x.subtype(z, symbols),
                1 => z.add_lower(
                    &Type::app(predef::ARRAY, [PolyType::Var(y.clone())]),
                    symbols,
                ),
                _ => y.add_lower(
                    &Type::app(predef::ARRAY, [PolyType::Var(x.clone())]),
                    symbols,
                ),
            }
            .unwrap_or_else(|err| panic!("{insertion:?}: {err:?}"));
        }
    }

    fn permutations<const N: usize>() -> Vec<[usize; N]> {
        fn visit<const N: usize>(
            values: &mut [usize; N],
            start: usize,
            result: &mut Vec<[usize; N]>,
        ) {
            if start == N {
                result.push(*values);
                return;
            }
            for i in start..N {
                values.swap(start, i);
                visit(values, start + 1, result);
                values.swap(start, i);
            }
        }
        let mut result = Vec::new();
        visit(&mut std::array::from_fn(|i| i), 0, &mut result);
        result
    }

    #[test]
    fn nested_predecessors_remain_open_to_late_lower_bounds() {
        let interner = TypeInterner::default();
        let mut symbols = hierarchy(&interner);
        let p = interner.intern("P");
        let mut aggregate = Aggregate::default();
        aggregate.set_base(Some(TypeApp::nullary(predef::ISCRIPTABLE)));
        symbols.add_type(
            p,
            TypeDef::new(
                [Rc::new(CtxVar::new("T", Variance::Covariant, None, None))],
                TypeSchema::Aggregate(aggregate.into()),
                [],
            ),
        );
        // The class hierarchy helper does not implicitly add IScriptable.
        let dog = interner.intern("Dog");
        let mut aggregate = Aggregate::default();
        aggregate.set_base(Some(TypeApp::nullary(predef::ISCRIPTABLE)));
        symbols.add_type(
            dog,
            TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
        );
        for insertion in permutations::<4>() {
            for solve in permutations::<2>() {
                let x = Var::fresh();
                let y = Var::fresh();
                let weak = [Rc::downgrade(&x.0), Rc::downgrade(&y.0)];
                for step in insertion {
                    match step {
                        0 => x.subtype(&y, &symbols),
                        1 => y.add_upper(&Type::nullary(predef::ISCRIPTABLE), &symbols),
                        2 => y.add_lower(&Type::app(p, [PolyType::Var(x.clone())]), &symbols),
                        _ => x.add_lower(&Type::nullary(dog), &symbols),
                    }
                    .unwrap_or_else(|err| panic!("{insertion:?}: {err:?}"));
                }
                assert!(
                    x.upper() != Some(Type::Nothing),
                    "insertion must not select Nothing"
                );
                for index in solve {
                    let (var, expected) = if index == 0 {
                        (&x, dog)
                    } else {
                        (&y, predef::ISCRIPTABLE)
                    };
                    assert_eq!(
                        coalesce_var(var, &symbols),
                        Type::nullary(expected),
                        "{insertion:?} {solve:?}"
                    );
                }
                drop((x, y));
                assert!(weak.iter().all(|v| v.upgrade().is_none()));
            }
        }
    }

    #[test]
    fn coalescing_rechecks_bounds_tightened_by_nested_dependencies() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        for insertion in permutations::<2>() {
            for solve in permutations::<2>() {
                let x = Var::fresh();
                let y = Var::fresh();
                let weak = [Rc::downgrade(&x.0), Rc::downgrade(&y.0)];
                for step in insertion {
                    match step {
                        0 => x.subtype(&y, &symbols),
                        _ => x.add_upper(
                            &Type::app(predef::ARRAY, [PolyType::Var(y.clone())]),
                            &symbols,
                        ),
                    }
                    .unwrap();
                }
                let vars = [&x, &y];
                for index in solve {
                    // The first attempt must succeed, not merely repair bounds
                    // enough for a subsequent call to succeed.
                    assert_eq!(
                        coalesce_var(vars[index], &symbols),
                        Type::Nothing,
                        "{insertion:?} {solve:?}"
                    );
                }
                for var in vars {
                    assert_eq!(var.lower(), Type::Nothing);
                    assert_eq!(var.upper(), Some(Type::Nothing));
                }
                drop((x, y));
                assert!(weak.iter().all(|var| var.upgrade().is_none()));
            }
        }
    }

    #[test]
    fn mixed_component_coalescing_is_independent_of_root_and_insertion_order() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        for insertion in permutations::<3>() {
            for solve in permutations::<3>() {
                let vars = [Var::fresh(), Var::fresh(), Var::fresh()];
                let weak = vars.each_ref().map(|var| Rc::downgrade(&var.0));
                link_mixed_component(&vars, insertion, &symbols);
                let expected = [
                    Type::Nothing,
                    Type::app(predef::ARRAY, [Type::Nothing]),
                    Type::app(predef::ARRAY, [Type::app(predef::ARRAY, [Type::Nothing])]),
                ];
                for index in solve {
                    assert_eq!(
                        coalesce_var(&vars[index], &symbols),
                        expected[index],
                        "{insertion:?} {solve:?}"
                    );
                }
                drop(vars);
                assert!(weak.iter().all(|v| v.upgrade().is_none()));
            }
        }
    }

    #[test]
    fn exported_nested_bounds_retain_their_inference_component() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let x = Var::fresh();
        let y = Var::fresh();
        let weak = [Rc::downgrade(&x.0), Rc::downgrade(&y.0)];
        x.subtype(&y, &symbols).unwrap();
        y.add_lower(
            &Type::app(predef::ARRAY, [PolyType::Var(x.clone())]),
            &symbols,
        )
        .unwrap();
        let bound = y.lower();
        drop((x, y));
        assert!(weak.iter().all(|var| var.upgrade().is_some()));
        assert_eq!(
            bound.into_poly().coalesce(&symbols).unwrap(),
            Type::app(predef::ARRAY, [Type::Nothing])
        );
        assert!(weak.iter().all(|var| var.upgrade().is_none()));
    }

    #[test]
    fn mixed_component_survives_any_remaining_external_handle() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        for survivor in 0..3 {
            let vars = [Var::fresh(), Var::fresh(), Var::fresh()];
            let weak = vars.each_ref().map(|var| Rc::downgrade(&var.0));
            link_mixed_component(&vars, [0, 1, 2], &symbols);
            let remaining = vars[survivor].clone();
            drop(vars);
            assert!(weak.iter().all(|var| var.upgrade().is_some()));
            assert!(PolyType::Var(remaining.clone()).coalesce(&symbols).is_ok());
            drop(remaining);
            assert!(weak.iter().all(|var| var.upgrade().is_none()));
        }
    }

    #[test]
    fn mixed_directional_nested_cycles_have_finite_solutions_and_drop() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        for nodes in [2, 3] {
            for edge_first in [false, true] {
                for finish in [false, true] {
                    let vars: Vec<_> = (0..nodes).map(|_| Var::fresh()).collect();
                    let weak: Vec<_> = vars.iter().map(|var| Rc::downgrade(&var.0)).collect();
                    let x = &vars[0];
                    let y = &vars[nodes - 1];
                    let link = || {
                        for edge in vars.windows(2) {
                            edge[0].subtype(&edge[1], &symbols).unwrap();
                        }
                    };
                    if edge_first {
                        link();
                    }
                    let array_x = Type::app(predef::ARRAY, [PolyType::Var(x.clone())]);
                    y.add_lower(&array_x, &symbols).unwrap();
                    if !edge_first {
                        link();
                    }
                    if finish {
                        let array_nothing = Type::app(predef::ARRAY, [Type::Nothing.into_poly()]);
                        y.add_upper(&array_nothing, &symbols).unwrap();
                        assert_eq!(
                            coalesce_var(y, &symbols),
                            Type::app(predef::ARRAY, [Type::Nothing]),
                            "nodes={nodes}, edge_first={edge_first}"
                        );
                        assert_eq!(coalesce_var(x, &symbols), Type::Nothing);
                    }
                    drop((array_x, vars));
                    assert!(weak.iter().all(|var| var.upgrade().is_none()));
                }
            }
        }
    }

    #[test]
    fn coalescing_guard_rejects_recursive_bounds_without_recursing() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let var = Var::fresh();
        // Simulate a malformed bound bypassing constraint insertion, so the
        // coalescing guard remains independently tested from the occurs check.
        var.0.borrow_mut().lower = Type::app(predef::ARRAY, [PolyType::Var(var.clone())]);
        let result = PolyType::Var(var.clone()).coalesce(&symbols);
        var.0.borrow_mut().lower = Type::Nothing;
        assert!(result.is_err());
        assert!(!var.0.borrow().updating);
        assert_eq!(
            PolyType::Var(var).coalesce(&symbols).unwrap(),
            Type::Nothing
        );
    }

    #[test]
    fn collapsing_cycles_preserves_external_predecessors() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        for reverse in [false, true] {
            let x = Var::fresh();
            let a = Var::fresh();
            let b = Var::fresh();
            x.subtype(&b, &symbols).unwrap();
            a.subtype(&b, &symbols).unwrap();
            b.subtype(&a, &symbols).unwrap();
            let leaf = Type::nullary(interner.intern("Leaf"));
            if reverse {
                b.add_upper(&leaf, &symbols).unwrap();
            } else {
                a.add_upper(&leaf, &symbols).unwrap();
            }
            assert_eq!(x.upper(), Some(leaf.clone()));
            let later = Var::fresh();
            later.subtype(&b, &symbols).unwrap();
            assert_eq!(later.upper(), Some(leaf));
        }
    }

    #[test]
    fn branching_cycles_propagate_and_release_the_component() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        for branches in [2, 3] {
            let a = Var::fresh();
            let d = Var::fresh();
            let middle: Vec<_> = (0..branches).map(|_| Var::fresh()).collect();
            let weak: Vec<_> = [&a, &d]
                .into_iter()
                .chain(&middle)
                .map(|var| Rc::downgrade(&var.0))
                .collect();
            for var in &middle {
                a.subtype(var, &symbols).unwrap();
                var.subtype(&d, &symbols).unwrap();
            }
            assert_eq!(a.0.borrow().supers.spilled(), branches > 2);
            assert_eq!(d.0.borrow().subs.spilled(), branches > 2);
            d.subtype(&a, &symbols).unwrap();
            let leaf = Type::nullary(interner.intern("Leaf"));
            middle[1].add_lower(&leaf, &symbols).unwrap();
            for var in [&a, &d].into_iter().chain(&middle) {
                assert_eq!(var.lower(), leaf);
                assert_eq!(var.key(), a.key());
            }
            drop((a, d, middle));
            assert!(weak.iter().all(|v| v.upgrade().is_none()));
        }
    }

    #[test]
    fn rigid_lattice_bounds_preserve_identity_and_reject_unknown_meets() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let base = Type::nullary(interner.intern("Base"));
        let leaf = Type::nullary(interner.intern("Leaf"));
        let rigid = Type::Ctx(Rc::new(CtxVar::new(
            "T",
            Variance::Invariant,
            None,
            Some(base.clone()),
        )));
        for (l, r) in [(&rigid, &base), (&base, &rigid)] {
            assert_eq!(l.glb(r, &symbols).unwrap(), rigid);
            assert_eq!(l.lub(r, &symbols).unwrap(), base);
        }
        for (l, r) in [(&rigid, &leaf), (&leaf, &rigid)] {
            assert!(l.glb(r, &symbols).is_err());
            assert_eq!(l.lub(r, &symbols).unwrap(), base);
        }
        assert_eq!(rigid.lub(&rigid, &symbols).unwrap(), rigid);
        assert_eq!(rigid.glb(&rigid, &symbols).unwrap(), rigid);
        let lower_bounded = Type::Ctx(Rc::new(CtxVar::new(
            "U",
            Variance::Invariant,
            Some(base.clone()),
            None,
        )));
        for (l, r) in [(&lower_bounded, &leaf), (&leaf, &lower_bounded)] {
            assert_eq!(l.lub(r, &symbols).unwrap(), lower_bounded);
            assert_eq!(l.glb(r, &symbols).unwrap(), leaf);
        }
        let unbounded = Type::Ctx(Rc::new(CtxVar::new("V", Variance::Invariant, None, None)));
        assert!(unbounded.lub(&leaf, &symbols).is_err());
        assert!(leaf.lub(&unbounded, &symbols).is_err());
    }

    #[test]
    fn application_lattice_operations_respect_all_variances() {
        let interner = TypeInterner::default();
        let mut symbols = hierarchy(&interner);
        let base = PolyType::nullary(interner.intern("Base"));
        let leaf = PolyType::nullary(interner.intern("Leaf"));
        for variance in [
            Variance::Covariant,
            Variance::Contravariant,
            Variance::Invariant,
        ] {
            let id = interner.intern(match variance {
                Variance::Covariant => "Producer",
                Variance::Contravariant => "Consumer",
                Variance::Invariant => "Cell",
            });
            symbols.add_type(
                id,
                TypeDef::new(
                    [Rc::new(CtxVar::new("T", variance, None, None))],
                    TypeSchema::Aggregate(Aggregate::default().into()),
                    [],
                ),
            );
            let l = Type::app(id, [leaf.clone()]);
            let r = Type::app(id, [base.clone()]);
            for (l, r) in [(&l, &r), (&r, &l)] {
                if variance == Variance::Invariant {
                    assert!(l.lub(r, &symbols).is_err());
                    assert!(l.glb(r, &symbols).is_err());
                } else {
                    let (joined, met) = if variance == Variance::Covariant {
                        (&base, &leaf)
                    } else {
                        (&leaf, &base)
                    };
                    assert_eq!(l.lub(r, &symbols).unwrap(), Type::app(id, [joined.clone()]));
                    assert_eq!(l.glb(r, &symbols).unwrap(), Type::app(id, [met.clone()]));
                }
            }
            let a = PolyType::fresh();
            let b = PolyType::fresh();
            let l = Type::app(id, [a.clone()]);
            let r = Type::app(id, [b.clone()]);
            let joined = l.lub(&r, &symbols).unwrap();
            a.constrain_invariant(&leaf, &symbols).unwrap();
            if variance == Variance::Invariant {
                assert!(b.constrain_invariant(&base, &symbols).is_err());
            } else {
                b.constrain_invariant(&base, &symbols).unwrap();
                assert_eq!(
                    joined.into_poly().coalesce(&symbols).unwrap(),
                    Type::app(
                        id,
                        [Type::nullary(interner.intern(
                            if variance == Variance::Covariant {
                                "Base"
                            } else {
                                "Leaf"
                            }
                        ))]
                    )
                );
            }
        }
    }

    #[test]
    fn inherited_application_meet_refines_arguments() {
        let interner = TypeInterner::default();
        let mut symbols = hierarchy(&interner);
        let parent = interner.intern("Parent");
        let child = interner.intern("Child");
        let param = Rc::new(CtxVar::new("T", Variance::Covariant, None, None));
        symbols.add_type(
            parent,
            TypeDef::new(
                [param.clone()],
                TypeSchema::Aggregate(Aggregate::default().into()),
                [],
            ),
        );
        let mut aggregate = Aggregate::default();
        aggregate.set_base(Some(TypeApp::new(parent, [Type::Ctx(param.clone())])));
        symbols.add_type(
            child,
            TypeDef::new([param], TypeSchema::Aggregate(aggregate.into()), []),
        );
        let lhs = Type::app(child, [PolyType::nullary(interner.intern("Base"))]);
        let rhs = Type::app(parent, [PolyType::nullary(interner.intern("Leaf"))]);
        for (l, r) in [(&lhs, &rhs), (&rhs, &lhs)] {
            assert_eq!(
                l.glb(r, &symbols)
                    .unwrap()
                    .into_poly()
                    .coalesce(&symbols)
                    .unwrap(),
                Type::app(child, [Type::nullary(interner.intern("Leaf"))])
            );
        }
    }

    #[test]
    fn coalescing_validates_preexisting_bounds() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let invalid = PolyType::with_bounds(
            Type::nullary(interner.intern("Base")),
            Some(Type::nullary(interner.intern("Leaf"))),
        );
        assert!(invalid.coalesce(&symbols).is_err());
    }

    #[test]
    fn variable_lattice_operations_do_not_equate_operands() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let leaf = PolyType::nullary(interner.intern("Leaf"));
        let base = PolyType::nullary(interner.intern("Base"));
        for join in [false, true] {
            for reverse in [false, true] {
                let a = PolyType::fresh();
                let b = PolyType::fresh();
                a.constrain_invariant(&leaf, &symbols).unwrap();
                b.constrain_invariant(&base, &symbols).unwrap();
                let (l, r) = if reverse { (&b, &a) } else { (&a, &b) };
                let result = if join {
                    l.lub(r, &symbols)
                } else {
                    l.glb(r, &symbols)
                }
                .unwrap();
                assert_eq!(
                    result.coalesce(&symbols).unwrap(),
                    Type::nullary(interner.intern(if join { "Base" } else { "Leaf" }))
                );
                assert_eq!(
                    a.coalesce(&symbols).unwrap(),
                    Type::nullary(interner.intern("Leaf"))
                );
                assert_eq!(
                    b.coalesce(&symbols).unwrap(),
                    Type::nullary(interner.intern("Base"))
                );
            }
        }
    }

    #[test]
    fn nested_edges_retain_inference_and_reject_occurs_cycles() {
        let interner = TypeInterner::default();
        let symbols = hierarchy(&interner);
        let leaf = PolyType::nullary(interner.intern("Leaf"));
        let base = PolyType::nullary(interner.intern("Base"));
        for reverse in [false, true] {
            let a = PolyType::fresh();
            let b = PolyType::fresh();
            let x = PolyType::fresh();
            let y = PolyType::fresh();
            let array = |arg| Type::app(predef::ARRAY, [arg]).into_poly();
            a.constrain_base(&b, &symbols).unwrap();
            if reverse {
                b.constrain_base(&array(y.clone()), &symbols).unwrap();
                array(x.clone()).constrain_base(&a, &symbols).unwrap();
            } else {
                array(x.clone()).constrain_base(&a, &symbols).unwrap();
                b.constrain_base(&array(y.clone()), &symbols).unwrap();
            }
            x.constrain_invariant(&leaf, &symbols).unwrap();
            assert_eq!(
                y.coalesce(&symbols).unwrap(),
                Type::nullary(interner.intern("Leaf"))
            );
            assert!(base.constrain_base(&x, &symbols).is_err());
        }
        let recursive = PolyType::fresh();
        assert!(
            Type::app(predef::ARRAY, [recursive.clone()])
                .into_poly()
                .constrain_base(&recursive, &symbols)
                .is_err()
        );
    }
}
