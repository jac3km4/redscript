use std::cell::Cell;
use std::rc::Rc;

use redscript_community_ast as ast;
use redscript_community_ast::Span;

use crate::lower::error::{Error, LowerResult};
use crate::lower::types::PolyType;
use crate::symbols::{FreeFunctionIndex, FreeFunctionIndexes, FunctionEntry, Symbols};
use crate::types::{CtxVar, Type, TypeId, predef};
use crate::utils::{Lazy, ScopedMap};
use crate::{Aggregate, FreeFunction, IndexMap, ir};

#[derive(Debug)]
pub struct Env<'scope, 'ctx> {
    types: &'scope TypeEnv<'scope, 'ctx>,
    funcs: &'scope ScopedMap<'scope, &'ctx str, FreeFunctionIndexes>,
    locals: ScopedMap<'scope, &'ctx str, ir::LocalInfo<'ctx>>,
}

impl<'scope, 'ctx> Env<'scope, 'ctx> {
    pub fn new(
        types: &'scope TypeEnv<'scope, 'ctx>,
        globals: &'scope ScopedMap<'scope, &'ctx str, FreeFunctionIndexes>,
    ) -> Self {
        Self {
            types,
            funcs: globals,
            locals: ScopedMap::default(),
        }
    }

    #[inline]
    pub fn define_local(&mut self, name: &'ctx str, info: ir::LocalInfo<'ctx>) -> ir::Local {
        let id = info.id;
        self.locals.insert(name, info);
        id
    }

    pub fn introduce_scope(&'scope self) -> Self {
        Self {
            types: self.types,
            funcs: self.funcs,
            locals: self.locals.introduce_scope(),
        }
    }

    pub fn query_free_functions<'a>(
        &self,
        name: &'a str,
        symbols: &'scope Symbols<'ctx>,
    ) -> impl Iterator<Item = FunctionEntry<FreeFunctionIndex, &'a str, &'scope FreeFunction<'ctx>>>
    {
        self.funcs
            .scope_iter()
            .filter_map(|map| map.get(name))
            .flatten()
            .map(move |&idx| FunctionEntry::new(idx, name, &symbols[idx]))
    }

    #[inline]
    pub fn locals(&self) -> &ScopedMap<'scope, &'ctx str, ir::LocalInfo<'ctx>> {
        &self.locals
    }

    #[inline]
    pub fn types(&self) -> &TypeEnv<'scope, 'ctx> {
        self.types
    }
}

type ConditionPolicy = for<'ctx> fn(&PolyType<'ctx>, &Symbols<'ctx>) -> Option<ir::Intrinsic>;
type MemberIntrinsicPolicy = for<'ctx> fn(TypeId<'ctx>, &str) -> Option<(ir::Intrinsic, bool)>;
type TypePolicy = for<'ctx> fn(TypeId<'ctx>, &Symbols<'ctx>, Span) -> LowerResult<'ctx, ()>;
type CoercionPolicy =
    for<'ctx> fn(&PolyType<'ctx>, &PolyType<'ctx>, &Symbols<'ctx>, Span) -> LowerResult<'ctx, ()>;

/// Optional per-file semantic hooks. [`Default`] preserves the standard semantics.
///
/// Hooks are stateless function pointers, selected by the source span's file even
/// in nested scopes. They inspect resolved types without mutating shared symbols.
/// They do not replace source-language capability validation.
#[derive(Debug, Clone, Copy, Default)]
pub struct SemanticPolicy {
    /// Predeclare explicitly typed locals throughout each function body, with
    /// default initialization at entry. Initializers stay at their source sites.
    /// Duplicate hoisted names (including parameters) are errors; untyped locals retain
    /// ordinary lexical scoping. Nested closures have independent declarations.
    pub hoist_locals: bool,
    /// Optionally convert a condition using a unary intrinsic returning `Bool`.
    /// Applies to control-flow conditions, logical operators and ternaries.
    /// `None` from the callback retains the ordinary `Bool` constraint.
    pub condition: Option<ConditionPolicy>,
    /// Map a resolved receiver type and member name to an intrinsic. The receiver
    /// becomes its first argument; the boolean requires a non-temporary receiver.
    /// Only overloads annotated with the exact returned intrinsic are eligible.
    pub member_intrinsic: Option<MemberIntrinsicPolicy>,
    /// Validate a named concrete type during type resolution. Type variables,
    /// special `ref`/`wref` wrappers and implicit array/function constructors are
    /// not passed to this callback; their explicit type arguments still are.
    pub validate_type: Option<TypePolicy>,
    /// Validate a coercion before the standard rules, with source then target type.
    /// Returning `Ok(())` does not bypass ordinary type checking.
    pub validate_coercion: Option<CoercionPolicy>,
    /// Filter the intrinsic selected for an explicit free-function call.
    /// This does not filter ordinary functions or compiler-generated calls.
    /// Rejected calls report an unresolved-function error.
    pub allow_explicit_intrinsic: Option<fn(ir::Intrinsic) -> bool>,
}

#[derive(Debug, Clone)]
pub struct TypeEnv<'scope, 'ctx>(
    ScopedMap<'scope, &'ctx str, TypeRef<'scope, 'ctx>>,
    Rc<[(ast::FileId, SemanticPolicy)]>,
);

impl<'scope, 'ctx> TypeEnv<'scope, 'ctx> {
    pub fn with_default_types() -> Self {
        let mut map = IndexMap::default();
        map.insert("array", TypeRef::Id(predef::ARRAY));
        map.insert("ref", TypeRef::Id(predef::REF));
        map.insert("wref", TypeRef::Id(predef::WREF));
        map.insert("script_ref", TypeRef::Id(predef::SCRIPT_REF));

        Self(ScopedMap::from(map), Rc::default())
    }

    #[inline]
    pub fn introduce_scope(&'scope self) -> Self {
        Self(self.0.introduce_scope(), self.1.clone())
    }

    #[inline]
    pub fn pop_scope(self) -> IndexMap<&'ctx str, TypeRef<'scope, 'ctx>> {
        self.0.pop_scope()
    }

    #[inline]
    pub fn push_scope(&'scope self, scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>) -> Self {
        Self(self.0.push_scope(scope), self.1.clone())
    }

    #[inline]
    pub fn top(&self) -> &IndexMap<&'ctx str, TypeRef<'scope, 'ctx>> {
        self.0.top()
    }

    #[inline]
    pub fn get(&self, name: &'ctx str) -> Option<&TypeRef<'scope, 'ctx>> {
        self.0.get(name)
    }

    #[inline]
    pub fn add(&mut self, name: &'ctx str, typ: TypeRef<'scope, 'ctx>) {
        self.0.insert(name, typ);
    }

    pub(crate) fn set_policies(&mut self, policies: Rc<[(ast::FileId, SemanticPolicy)]>) {
        self.1 = policies;
    }

    pub(crate) fn policy(&self, span: Span) -> SemanticPolicy {
        self.1
            .iter()
            .find(|(file, _)| *file == span.file)
            .map(|(_, policy)| *policy)
            .unwrap_or_default()
    }

    pub fn resolve(
        &self,
        typ: &ast::SourceType<'ctx>,
        symbols: &Symbols<'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, Type<'ctx>> {
        match typ {
            ast::Type::Named { name, args } => match (self.0.get(name), &args[..]) {
                (Some(&TypeRef::Id(id)), [(arg, span)])
                    if id == predef::REF || id == predef::WREF =>
                {
                    let arg = self.resolve(arg, symbols, *span)?;
                    let flags = arg
                        .upper_bound()
                        .and_then(|ub| symbols.get_type(ub.id()))
                        .and_then(|def| def.schema().as_aggregate())
                        .map(Aggregate::flags)
                        .unwrap_or_default();
                    if flags.is_never_ref() {
                        return Err(Error::RefOnNeverRefType(id, arg, *span));
                    }
                    if flags.is_mixed_ref() || id == predef::WREF {
                        Ok(Type::app(id, [arg]))
                    } else {
                        Ok(arg)
                    }
                }
                (Some(&TypeRef::Id(id)), _) => {
                    if let Some(validate) = self.policy(span).validate_type {
                        validate(id, symbols, span)?;
                    }
                    let args = args
                        .iter()
                        .map(|(typ, span)| self.resolve(typ, symbols, *span))
                        .collect::<Result<Rc<_>, _>>()?;
                    Ok(Type::app(id, args))
                }
                (Some(TypeRef::Var(var)), []) => Ok(Type::Ctx(var.clone())),
                (Some(TypeRef::LazyVar(stub)), []) => Ok(Type::Ctx(
                    stub.get((self, symbols))
                        .map_err(|_| Error::CyclicType(span))??,
                )),
                (Some(TypeRef::Var(_) | TypeRef::LazyVar(_)), _) => {
                    Err(Error::InvalidTypeArgCount(0, args.len(), span))
                }
                (None, _) => Err(Error::UnresolvedType(name, span)),
            },
            ast::Type::Array(elem) => {
                let (elem, span) = &**elem;
                let elem = self.resolve(elem, symbols, *span)?;
                Ok(Type::app(predef::ARRAY, [elem]))
            }
            ast::Type::StaticArray(elem, size) => {
                let (elem, elem_span) = &**elem;
                let elem = self.resolve(elem, symbols, *elem_span)?;
                let id = TypeId::array_with_size(*size)
                    .ok_or(Error::UnsupportedStaticArraySize(*size, span))?;
                Ok(Type::app(id, [elem]))
            }
            ast::Type::Fn {
                params,
                return_type,
            } => {
                let args = params
                    .iter()
                    .chain([&**return_type])
                    .map(|(typ, span)| self.resolve(typ, symbols, *span))
                    .collect::<Result<Rc<_>, _>>()?;
                let id = TypeId::fn_with_arity(params.len())
                    .ok_or(Error::UnsupportedArity(params.len(), span))?;
                Ok(Type::app(id, args))
            }
        }
    }

    pub fn resolve_param(
        &self,
        param: &ast::SourceTypeParam<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> LowerResult<'ctx, CtxVar<'ctx>> {
        let (name, _) = param.name;
        let variance = param.variance.into();
        let upper = param
            .upper_bound
            .as_deref()
            .map(|(typ, span)| self.resolve(typ, symbols, *span))
            .transpose()?;
        Ok(CtxVar::new(name, variance, None, upper))
    }
}

impl Default for TypeEnv<'_, '_> {
    fn default() -> Self {
        Self::with_default_types()
    }
}

#[derive(Debug)]
pub struct Locals<'scope, 'ctx> {
    locals: Vec<ir::LocalInfo<'ctx>>,
    counter: &'scope Cell<u16>,
    depth: usize,
}

impl<'scope, 'ctx> Locals<'scope, 'ctx> {
    pub fn new(counter: &'scope Cell<u16>, depth: usize) -> Self {
        Self {
            locals: vec![],
            counter,
            depth,
        }
    }

    #[inline]
    pub fn add_var(
        &mut self,
        name: Option<&'ctx str>,
        typ: PolyType<'ctx>,
        span: Span,
    ) -> &ir::LocalInfo<'ctx> {
        self.add(ir::Local::Var(self.counter.get()), name, typ, Some(span))
    }

    pub fn add_temp(
        &mut self,
        purpose: ir::TemporaryPurpose,
        typ: PolyType<'ctx>,
        span: Span,
    ) -> &ir::LocalInfo<'ctx> {
        let local = ir::Local::Var(self.counter.get());
        self.counter.set(self.counter.get() + 1);
        let len = self.locals.len();
        self.locals
            .push(ir::LocalInfo::generated(local, purpose, typ, Some(span)));
        &self.locals[len]
    }

    #[inline]
    pub fn add_param(&mut self, typ: PolyType<'ctx>, span: Option<Span>) -> &ir::LocalInfo<'ctx> {
        self.add(ir::Local::Param(self.counter.get()), None, typ, span)
    }

    pub fn into_vec(self) -> Vec<ir::LocalInfo<'ctx>> {
        self.locals
    }

    #[inline]
    pub fn counter(&self) -> &'scope Cell<u16> {
        self.counter
    }

    #[inline]
    pub fn depth(&self) -> usize {
        self.depth
    }

    fn add(
        &mut self,
        local: ir::Local,
        name: Option<&'ctx str>,
        typ: PolyType<'ctx>,
        span: Option<Span>,
    ) -> &ir::LocalInfo<'ctx> {
        self.counter.set(self.counter.get() + 1);
        let len = self.locals.len();
        self.locals.push(ir::LocalInfo::new(local, name, typ, span));
        &self.locals[len]
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Capture {
    pub captured: ir::Local,
    pub scope_depth: usize,
}

impl Capture {
    pub fn new(captured: ir::Local, scope_depth: usize) -> Self {
        Self {
            captured,
            scope_depth,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeRef<'scope, 'ctx> {
    Id(TypeId<'ctx>),
    Var(Rc<CtxVar<'ctx>>),
    #[allow(clippy::type_complexity)]
    LazyVar(
        Rc<
            Lazy<
                LowerResult<'ctx, Rc<CtxVar<'ctx>>>,
                Box<
                    dyn Fn(
                            (&TypeEnv<'_, 'ctx>, &Symbols<'ctx>),
                        ) -> LowerResult<'ctx, Rc<CtxVar<'ctx>>>
                        + 'scope,
                >,
            >,
        >,
    ),
}

impl<'ctx> TypeRef<'_, 'ctx> {
    pub fn force(self) -> Option<TypeRef<'static, 'ctx>> {
        match self {
            Self::Id(id) => Some(TypeRef::Id(id)),
            Self::Var(typ) => Some(TypeRef::Var(typ)),
            Self::LazyVar(lazy) => Some(TypeRef::Var(lazy.try_get()?.ok()?)),
        }
    }

    pub fn id(&self) -> Option<TypeId<'ctx>> {
        match self {
            &Self::Id(id) => Some(id),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Variance;

    #[test]
    fn plain_type_vars_are_accepted() {
        let sources = ast::SourceMap::new();
        let file = sources.push_back("test.reds", "T");
        let span = Span::new(0, 1, file);
        let symbols = Symbols::with_default_types();
        let var = Rc::new(CtxVar::new("T", Variance::Invariant, None, None));
        let lazy_var = var.clone();
        let refs = [
            TypeRef::Var(var.clone()),
            TypeRef::LazyVar(Rc::new(Lazy::new(Box::new(move |_| Ok(lazy_var.clone()))))),
        ];

        for type_ref in refs {
            let mut env = TypeEnv::default();
            env.add("T", type_ref);
            let resolved = env.resolve(&ast::Type::plain("T"), &symbols, span).unwrap();
            assert!(matches!(resolved, Type::Ctx(resolved) if Rc::ptr_eq(&resolved, &var)));
        }
    }

    #[test]
    fn applied_type_vars_are_rejected() {
        let sources = ast::SourceMap::new();
        let file = sources.push_back("test.reds", "T<T>");
        let span = Span::new(0, 4, file);
        let arg_span = Span::new(2, 3, file);
        let symbols = Symbols::with_default_types();
        let var = Rc::new(CtxVar::new("T", Variance::Invariant, None, None));
        let refs = [
            TypeRef::Var(var),
            TypeRef::LazyVar(Rc::new(Lazy::new(Box::new(|_| {
                panic!("applied type variables must be rejected without forcing the stub")
            })))),
        ];
        let typ = ast::Type::Named {
            name: "T",
            args: Box::new([(ast::Type::plain("T"), arg_span)]),
        };

        for type_ref in refs {
            let mut env = TypeEnv::default();
            env.add("T", type_ref);
            let error = env.resolve(&typ, &symbols, span).unwrap_err();
            assert!(matches!(error, Error::InvalidTypeArgCount(0, 1, actual) if actual == span));
        }
    }
}
