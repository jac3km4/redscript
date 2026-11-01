use std::cell::Cell;
use std::rc::Rc;
use std::sync::LazyLock;

use hashbrown::HashMap;
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
    /// Permit writes to indexed elements rooted in const parameters (REDmod).
    /// Whole-array writes and mutating array intrinsics remain forbidden.
    pub const_param_element_writes: bool,
    /// Permit forwarding const roots to non-const, non-out reference/value-container
    /// parameters (REDmod). Explicit out arguments remain checked.
    pub const_param_non_out_forwarding: bool,
    /// Permit repeated rebinding and ordinary/out forwarding of const object handles.
    /// Referent writes and non-const receiver calls remain forbidden.
    pub const_local_handle_rebinding: bool,
    /// Dispatch `Cast<T>` to object casting for non-struct class targets, including
    /// identity and upcasts through ordinary coercion. Other targets use overloads.
    pub native_object_cast: bool,
    /// Write through script references in assignment destinations and `out` arguments
    /// instead of assigning/forwarding the reference itself (REDmod).
    /// Compositional coercion and receiver/index unwrapping are always shared.
    pub script_ref_write_through: bool,
    /// Resolve unqualified fields and method calls against the enclosing aggregate
    /// after local lookup, before free-function lookup.
    pub implicit_members: bool,
    /// Default omitted field visibility to public, including fields of classes.
    /// Explicit visibility and method defaults are unchanged.
    pub public_fields: bool,
    /// Suppress community style advice for inherited static calls at this call site.
    pub allow_inherited_static_calls: bool,

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

#[derive(Debug, Clone, Default)]
struct PolicyTable {
    index: Rc<HashMap<ast::FileId, Rc<SemanticPolicy>>>,
}

impl PolicyTable {
    fn new(policies: &[(ast::FileId, Rc<SemanticPolicy>)]) -> Self {
        let mut index = HashMap::with_capacity(policies.len());
        for (file, policy) in policies {
            index.entry(*file).or_insert_with(|| policy.clone());
        }
        Self {
            index: Rc::new(index),
        }
    }

    fn get(&self, file: ast::FileId) -> &SemanticPolicy {
        static DEFAULT: LazyLock<SemanticPolicy> = LazyLock::new(SemanticPolicy::default);
        self.index.get(&file).map_or(&DEFAULT, Rc::as_ref)
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnv<'scope, 'ctx> {
    types: ScopedMap<'scope, &'ctx str, TypeRef<'scope, 'ctx>>,
    policies: PolicyTable,
}

impl<'scope, 'ctx> TypeEnv<'scope, 'ctx> {
    pub fn with_default_types() -> Self {
        let mut map = IndexMap::default();
        map.insert("array", TypeRef::Id(predef::ARRAY));
        map.insert("ref", TypeRef::Id(predef::REF));
        map.insert("wref", TypeRef::Id(predef::WREF));
        map.insert("script_ref", TypeRef::Id(predef::SCRIPT_REF));

        Self {
            types: ScopedMap::from(map),
            policies: PolicyTable::default(),
        }
    }

    #[inline]
    pub fn introduce_scope(&'scope self) -> Self {
        Self {
            types: self.types.introduce_scope(),
            policies: self.policies.clone(),
        }
    }

    #[inline]
    pub fn pop_scope(self) -> IndexMap<&'ctx str, TypeRef<'scope, 'ctx>> {
        self.types.pop_scope()
    }

    #[inline]
    pub fn push_scope(&'scope self, scope: IndexMap<&'ctx str, TypeRef<'scope, 'ctx>>) -> Self {
        Self {
            types: self.types.push_scope(scope),
            policies: self.policies.clone(),
        }
    }

    #[inline]
    pub fn top(&self) -> &IndexMap<&'ctx str, TypeRef<'scope, 'ctx>> {
        self.types.top()
    }

    #[inline]
    pub fn get(&self, name: &'ctx str) -> Option<&TypeRef<'scope, 'ctx>> {
        self.types.get(name)
    }

    #[inline]
    pub fn add(&mut self, name: &'ctx str, typ: TypeRef<'scope, 'ctx>) {
        self.types.insert(name, typ);
    }

    pub(crate) fn set_policies(&mut self, policies: &[(ast::FileId, Rc<SemanticPolicy>)]) {
        self.policies = PolicyTable::new(policies);
    }

    pub(crate) fn policy(&self, span: Span) -> &SemanticPolicy {
        self.policies.get(span.file)
    }

    pub fn resolve(
        &self,
        typ: &ast::SourceType<'ctx>,
        symbols: &Symbols<'ctx>,
        span: Span,
    ) -> LowerResult<'ctx, Type<'ctx>> {
        match typ {
            ast::Type::Named { name, args } => match (self.types.get(name), &args[..]) {
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

    pub fn add_const_var(
        &mut self,
        name: Option<&'ctx str>,
        typ: PolyType<'ctx>,
        span: Span,
        is_const: bool,
    ) -> &ir::LocalInfo<'ctx> {
        self.add_var(name, typ, span);
        let local = self.locals.last_mut().expect("just added local");
        local.is_const = is_const;
        local
    }

    pub fn set_initializer(&mut self, id: ir::Local, span: Span) {
        let index = self
            .locals
            .binary_search_by_key(&id, |local| local.id)
            .expect("initializer belongs to a local in this scope");
        self.locals[index].initializer = Some(span);
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
        debug_assert!(self.locals.windows(2).all(|pair| pair[0].id < pair[1].id));
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
    fn policies_share_handles_across_files_and_scopes_with_first_entry_wins() {
        let sources = ast::SourceMap::new();
        let first = sources.push_front("first.reds", "");
        let second = sources.push_back("second.reds", "");
        let absent = sources.push_back("absent.reds", "");
        let shared = Rc::new(SemanticPolicy {
            hoist_locals: true,
            ..SemanticPolicy::default()
        });
        let duplicate = Rc::new(SemanticPolicy::default());
        let mut env = TypeEnv::default();
        env.set_policies(&[
            (first, shared.clone()),
            (second, shared.clone()),
            (first, duplicate.clone()),
        ]);
        assert_eq!(Rc::strong_count(&shared), 3);
        assert_eq!(Rc::strong_count(&duplicate), 1);

        let nested = env.introduce_scope();
        let pushed = nested.push_scope(IndexMap::default());
        let cloned = env.clone();
        for scope in [&env, &nested, &pushed, &cloned] {
            assert!(Rc::ptr_eq(&env.policies.index, &scope.policies.index));
            for file in [first, second] {
                let policy = scope.policy(Span::new(0, 0, file));
                assert!(policy.hoist_locals);
                assert!(std::ptr::eq(policy, shared.as_ref()));
            }
            assert_eq!(Rc::strong_count(&shared), 3);
        }
        let default_env = TypeEnv::default();
        let default = default_env.policy(Span::new(0, 0, first));
        assert!(!default.hoist_locals);
        for scope in [&env, &nested, &pushed, &cloned] {
            assert!(std::ptr::eq(scope.policy(Span::new(0, 0, absent)), default,));
        }
    }

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
