use std::fmt;
use std::marker::PhantomData;

use identity_hash::IdentityHashable;
use smallvec::SmallVec;

use super::{InferredType, PolyType};
use crate::Shared;
use crate::Symbols;
use crate::lower::error::{InferResult, TypeError};
use crate::symbols::AnySupertype;
use crate::sync::{SharedCell, SharedWeak};
use crate::types::{CtxVar, Type};

// External handles own the component; handles stored in nodes have no owner.
// Components retain all nodes until the last external handle is dropped, then
// clear their internal links. This permits mixed bound/edge cycles without
// making ownership decisions constrain the eventual inferred types.
#[derive(Clone)]
pub struct Var<'ctx>(
    pub(super) Shared<SharedCell<VarState<'ctx>>>,
    Option<Shared<SharedCell<VarComponent<'ctx>>>>,
);

pub(super) type VarEdges<'ctx> = SmallVec<[Var<'ctx>; 2]>;

#[derive(Default)]
pub(super) struct VarComponent<'ctx> {
    nodes: Vec<Shared<SharedCell<VarState<'ctx>>>>,
    parent: Option<Shared<SharedCell<Self>>>,
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
    pub(super) fn new(lower: InferredType<'ctx>, upper: Option<InferredType<'ctx>>) -> Self {
        let component = Shared::new(SharedCell::new(VarComponent::default()));
        let node = Shared::new(SharedCell::new(VarState {
            lower: Type::Nothing,
            upper: None,
            repr: None,
            supers: SmallVec::new(),
            subs: SmallVec::new(),
            updating: false,
            component: Shared::downgrade(&component),
        }));
        component.borrow_mut().nodes.push(node.clone());
        let var = Self(node, Some(component));
        var.store_bounds(&lower, upper.as_ref());
        var
    }

    fn external(node: Shared<SharedCell<VarState<'ctx>>>) -> Self {
        let component = node
            .borrow()
            .component
            .upgrade()
            .expect("live inference component");
        Self(node, Some(component))
    }

    fn component(&self) -> Shared<SharedCell<VarComponent<'ctx>>> {
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
        if !Shared::ptr_eq(&lhs, &rhs) {
            let nodes = std::mem::take(&mut rhs.borrow_mut().nodes);
            for node in &nodes {
                node.borrow_mut().component = Shared::downgrade(&lhs);
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
                    .collect::<Shared<_>>(),
            ),
            Type::Ctx(ctx) => Type::Ctx(Shared::new(CtxVar::new(
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
    pub(super) fn fresh() -> Self {
        Self::new(Type::Nothing, None)
    }

    pub(super) fn is_fresh(&self) -> bool {
        let state = self.0.borrow();
        matches!(&state.lower, Type::Nothing)
            && state.upper.is_none()
            && state.repr.is_none()
            && state.supers.is_empty()
            && state.subs.is_empty()
    }

    #[inline]
    pub fn key(&self) -> VarKey<'ctx> {
        VarKey(Shared::as_ptr(&self.repr().0) as usize, PhantomData)
    }

    pub(super) fn repr(&self) -> Var<'ctx> {
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

    pub(in crate::lower) fn select(
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

    pub(super) fn subtype(&self, other: &Self, symbols: &Symbols<'ctx>) -> InferResult<'ctx, ()> {
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

    pub(super) fn add_upper(
        &self,
        ub: &InferredType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        self.update(ub, false, symbols, &mut SmallVec::new())
    }

    pub(super) fn add_lower(
        &self,
        lb: &InferredType<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
        self.update(lb, true, symbols, &mut SmallVec::new())
    }

    pub(super) fn update(
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

    pub(super) fn unify(
        &self,
        other: &Var<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> InferResult<'ctx, ()> {
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
        Shared::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Var<'_> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarKey<'ctx>(usize, PhantomData<&'ctx ()>);

impl IdentityHashable for VarKey<'_> {}

#[derive(Debug, Clone)]
pub(super) struct VarState<'ctx> {
    pub(super) lower: InferredType<'ctx>,
    upper: Option<InferredType<'ctx>>,
    repr: Option<Var<'ctx>>,
    pub(super) supers: VarEdges<'ctx>,
    pub(super) subs: VarEdges<'ctx>,
    pub(super) updating: bool,
    component: SharedWeak<SharedCell<VarComponent<'ctx>>>,
}
