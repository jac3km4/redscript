use std::rc::Rc;

use hashbrown::{HashMap, HashSet};
use identity_hash::BuildIdentityHasher;

use crate::lower::error::CoalesceError;
use crate::lower::types::{InferredCtxVar, InferredType, InferredTypeApp, PolyType, VarKey};
use crate::symbols::Symbols;
use crate::types::{CtxVar, Type, TypeApp, Variance};

#[derive(Debug)]
pub struct Simplifier<'sym, 'ctx> {
    resolutions: HashMap<VarKey<'ctx>, Resolution<'ctx>, BuildIdentityHasher<usize>>,
    covariant: HashSet<VarKey<'ctx>, BuildIdentityHasher<usize>>,
    contravariant: HashSet<VarKey<'ctx>, BuildIdentityHasher<usize>>,
    symbols: &'sym Symbols<'ctx>,
}

#[derive(Debug)]
enum Resolution<'ctx> {
    Active,
    Resolved(Type<'ctx>),
}

impl<'sym, 'ctx> Simplifier<'sym, 'ctx> {
    fn new(symbols: &'sym Symbols<'ctx>) -> Self {
        Self {
            resolutions: HashMap::default(),
            covariant: HashSet::default(),
            contravariant: HashSet::default(),
            symbols,
        }
    }

    pub fn coalesce(
        typ: &PolyType<'ctx>,
        symbols: &'sym Symbols<'ctx>,
    ) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        let mut this = Self::new(symbols);
        this.analyze_poly(typ, Variance::Covariant);
        this.simplify_poly(typ, Variance::Covariant)
    }

    pub fn coalesce_app(
        typ: &InferredTypeApp<'ctx>,
        symbols: &'sym Symbols<'ctx>,
    ) -> Result<TypeApp<'ctx>, CoalesceError<'ctx>> {
        let mut this = Self::new(symbols);
        this.analyze_app(typ, Variance::Covariant);
        this.simplify_app(typ, Variance::Covariant)
    }

    fn simplify_poly(
        &mut self,
        typ: &PolyType<'ctx>,
        variance: Variance,
    ) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        let res = match typ {
            PolyType::Var(var) => {
                let key = var.key();
                match self.resolutions.get(&key) {
                    Some(Resolution::Active) => {
                        return Err(CoalesceError::CannotCoalesce(var.lower(), var.upper()));
                    }
                    Some(Resolution::Resolved(typ)) => return Ok(typ.clone()),
                    None => {}
                }
                self.resolutions.insert(key, Resolution::Active);
                let typ = loop {
                    let (lower, upper) = (var.lower(), var.upper());
                    let bound = match (&lower, &upper) {
                        (bound, None) | (Type::Nothing, Some(bound)) => bound,
                        (_, Some(upper))
                            if variance == Variance::Contravariant
                                && !self.covariant.contains(&key) =>
                        {
                            upper
                        }
                        (lower, _) => lower,
                    };
                    // Resolve nested dependencies before propagating an exact choice.
                    // Propagating the symbolic bound itself can introduce recursive
                    // bounds in predecessors even when the finite solution is acyclic.
                    let typ = self.simplify(bound, variance)?;
                    // Nested choices can tighten this variable through directional
                    // edges. Discard the stale candidate before it mutates bounds.
                    if var.lower() != lower || var.upper() != upper {
                        continue;
                    }
                    var.select(&InferredType::from(&typ), self.symbols)
                        .map_err(|_| CoalesceError::CannotCoalesce(lower, upper))?;
                    break typ;
                };
                self.resolutions
                    .insert(key, Resolution::Resolved(typ.clone()));
                typ
            }
            PolyType::Mono(c) => self.simplify(c, variance)?,
        };
        Ok(res)
    }

    fn simplify_app(
        &mut self,
        typ: &InferredTypeApp<'ctx>,
        var: Variance,
    ) -> Result<TypeApp<'ctx>, CoalesceError<'ctx>> {
        let vars = self.symbols[typ.id()].params();
        let args = typ
            .args()
            .iter()
            .zip(vars)
            .map(|(arg, v)| self.simplify_poly(arg, var.combined(v.variance())))
            .collect::<Result<Rc<_>, _>>()?;
        Ok(TypeApp::new(typ.id(), args))
    }

    fn simplify_ctx_var(
        &mut self,
        var: &InferredCtxVar<'ctx>,
    ) -> Result<CtxVar<'ctx>, CoalesceError<'ctx>> {
        let lower = var
            .lower()
            .map(|typ| self.simplify(typ, var.variance()))
            .transpose()?;
        let upper = var
            .upper()
            .map(|typ| self.simplify(typ, var.variance()))
            .transpose()?;
        Ok(CtxVar::new(var.name(), var.variance(), lower, upper))
    }

    fn simplify(
        &mut self,
        typ: &InferredType<'ctx>,
        var: Variance,
    ) -> Result<Type<'ctx>, CoalesceError<'ctx>> {
        let res = match typ {
            Type::Data(typ) => Type::Data(self.simplify_app(typ, var)?),
            Type::Ctx(var) => Type::Ctx(self.simplify_ctx_var(var)?.into()),
            Type::Nothing => Type::Nothing,
        };
        Ok(res)
    }

    fn analyze_poly(&mut self, typ: &PolyType<'ctx>, variance: Variance) {
        match typ {
            PolyType::Var(var) => {
                if variance != Variance::Covariant
                    && self.contravariant.insert(var.key())
                    && let Some(upper) = &var.upper()
                {
                    self.analyze(upper, variance);
                }
                if variance != Variance::Contravariant && self.covariant.insert(var.key()) {
                    self.analyze(&var.lower(), variance);
                }
            }
            PolyType::Mono(typ) => self.analyze(typ, variance),
        }
    }

    fn analyze_app(&mut self, typ: &InferredTypeApp<'ctx>, variance: Variance) {
        for (arg, v) in typ.args().iter().zip(self.symbols[typ.id()].params()) {
            self.analyze_poly(arg, variance.combined(v.variance()));
        }
    }

    #[inline]
    fn analyze(&mut self, typ: &InferredType<'ctx>, variance: Variance) {
        if let Type::Data(typ) = typ {
            self.analyze_app(typ, variance);
        }
    }
}
