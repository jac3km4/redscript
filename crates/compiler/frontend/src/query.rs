use thiserror::Error;

use crate::lower::InferredTypeApp;
use crate::symbols::AnyBaseType;
use crate::utils::ScopedMap;
use crate::{
    CoalesceError, FieldId, FreeFunctionIndex, FunctionType, LoweredFunction, MethodId, PolyType,
    Symbols, Type, TypeApp, TypeError, TypeId, ir, predef,
};

#[derive(Debug, Error)]
pub enum QueryError<'ctx> {
    #[error("{0}")]
    Coalesce(CoalesceError<'ctx>),
    #[error("{0}")]
    Type(TypeError<'ctx>),
    #[error("local {0:?} is not present in the supplied function")]
    MissingLocal(ir::Local),
    #[error("type `{0}` is not present in the symbol table")]
    MissingType(TypeId<'ctx>),
    #[error("function {0:?} is not present in the symbol table")]
    MissingFunction(FreeFunctionIndex),
    #[error("method {0:?} is not present in the symbol table")]
    MissingMethod(MethodId<'ctx>),
    #[error("field {0:?} is not present in the symbol table")]
    MissingField(FieldId<'ctx>),
    #[error("type variable `{0}` has no substitution")]
    MissingTypeArgument(&'ctx str),
    #[error("expected {expected} type arguments, found {found}")]
    TypeArgumentCount { expected: usize, found: usize },
    #[error("receiver `{receiver}` cannot be instantiated as `{parent}`")]
    InvalidReceiver {
        receiver: TypeId<'ctx>,
        parent: TypeId<'ctx>,
    },
    #[error("expected an array type, found `{0}`")]
    InvalidArray(Type<'ctx>),
    #[error("expected a function type, found `{0}`")]
    InvalidClosure(TypeApp<'ctx>),
    #[error("the IR does not retain the contextual type of this null literal")]
    UnknownNullType,
}

impl<'ctx> From<CoalesceError<'ctx>> for QueryError<'ctx> {
    fn from(error: CoalesceError<'ctx>) -> Self {
        Self::Coalesce(error)
    }
}

impl<'ctx> From<TypeError<'ctx>> for QueryError<'ctx> {
    fn from(error: TypeError<'ctx>) -> Self {
        Self::Type(error)
    }
}

/// Returns the type of a lowered expression using its function's local table.
///
/// The expression and function must come from the supplied symbol table. Expressions
/// inside a closure require that closure's local table, not the enclosing function's.
/// Conditional types use the same least-upper-bound operation as lowering. A null
/// literal has no retained contextual type; it can only be resolved here when a
/// conditional's other branch supplies one. Coalescing may populate IR type caches.
pub fn expr_type<'ctx>(
    expr: &ir::Expr<'ctx>,
    function: &LoweredFunction<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Result<Type<'ctx>, QueryError<'ctx>> {
    let typ = match expr {
        ir::Expr::NewInstance { class_type, .. } => {
            return reference_type(class_type.coalesced(symbols)?, false, symbols);
        }
        ir::Expr::Construct { struct_type, .. } => struct_type,
        ir::Expr::NewClosure { closure, .. } => &closure.typ,
        ir::Expr::DynCast {
            target_type,
            expr_type,
            ..
        } => {
            let source = expr_type.coalesced(symbols)?;
            let weak = matches!(source, Type::Data(ref app) if app.id() == predef::WREF);
            return reference_type(target_type.coalesced(symbols)?, weak, symbols);
        }
        ir::Expr::Call { call, .. } => return call_type(call, symbols),
        ir::Expr::Assign { .. } => return Ok(Type::nullary(predef::VOID)),
        ir::Expr::Field {
            receiver_type,
            field,
            ..
        } => {
            let (_, def) = symbols
                .get_field(*field)
                .ok_or(QueryError::MissingField(*field))?;
            let receiver = receiver_type.coalesced(symbols)?;
            let env = receiver_env(&receiver, field.parent(), symbols)?;
            return substitute(def.type_(), &env, symbols);
        }
        ir::Expr::Index { array_type, .. } => {
            let typ = array_type.coalesced(symbols)?;
            if let Type::Data(app) = &typ
                && (app.id() == predef::ARRAY || app.id().is_static_array())
                && let [element] = app.args()
            {
                return Ok(element.clone());
            }
            return Err(QueryError::InvalidArray(typ));
        }
        ir::Expr::Conditional { then, else_, .. } => {
            // Null's inference variable is not stored in the IR. Keep its bounds
            // until after the join, rather than prematurely coalescing to IScriptable.
            if null_only(then) && null_only(else_) {
                return Err(QueryError::UnknownNullType);
            }
            return conditional_type(then, function, symbols)?
                .lub(&conditional_type(else_, function, symbols)?, symbols)?
                .coalesce(symbols)
                .map_err(QueryError::from);
        }
        &ir::Expr::Local(local, _) | &ir::Expr::Capture(local, _) => {
            return function
                .find_local(local)
                .ok_or(QueryError::MissingLocal(local))?
                .coalesce(symbols)
                .map_err(QueryError::from);
        }
        ir::Expr::Const(value, _) => return Ok(Type::nullary(value.type_id())),
        ir::Expr::Null { .. } => return Err(QueryError::UnknownNullType),
    };
    Ok(typ.coalesced(symbols)?.into_type())
}

fn reference_type<'ctx>(
    target: TypeApp<'ctx>,
    weak: bool,
    symbols: &Symbols<'ctx>,
) -> Result<Type<'ctx>, QueryError<'ctx>> {
    let def = symbols
        .get_type(target.id())
        .ok_or(QueryError::MissingType(target.id()))?;
    let mixed = def
        .schema()
        .as_aggregate()
        .is_some_and(|agg| agg.flags().is_mixed_ref());
    let target = target.into_type();
    Ok(if weak {
        Type::app(predef::WREF, [target])
    } else if mixed {
        Type::app(predef::REF, [target])
    } else {
        target
    })
}

fn null_only(expr: &ir::Expr<'_>) -> bool {
    match expr {
        ir::Expr::Null { .. } => true,
        ir::Expr::Conditional { then, else_, .. } => null_only(then) && null_only(else_),
        _ => false,
    }
}

fn conditional_type<'ctx>(
    expr: &ir::Expr<'ctx>,
    function: &LoweredFunction<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Result<PolyType<'ctx>, QueryError<'ctx>> {
    match expr {
        ir::Expr::Null { is_weak, .. } => {
            let typ =
                PolyType::with_bounds(Type::Nothing, Some(Type::nullary(predef::ISCRIPTABLE)));
            Ok(if *is_weak {
                Type::app(predef::WREF, [typ]).into_poly()
            } else {
                typ
            })
        }
        ir::Expr::Conditional { then, else_, .. } => Ok(conditional_type(then, function, symbols)?
            .lub(&conditional_type(else_, function, symbols)?, symbols)?),
        _ => Ok(PolyType::from_type(&expr_type(expr, function, symbols)?)),
    }
}

fn call_type<'ctx>(
    call: &ir::Call<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Result<Type<'ctx>, QueryError<'ctx>> {
    let (ft, env, type_args) = match call {
        ir::Call::FreeFunction {
            function,
            type_args,
            ..
        } => (
            symbols
                .get_free_function(*function)
                .ok_or(QueryError::MissingFunction(*function))?
                .1
                .type_(),
            ScopedMap::default(),
            type_args,
        ),
        ir::Call::Instance {
            receiver_type,
            method,
            type_args,
            ..
        } => (
            method_type(*method, symbols)?,
            receiver_env(&receiver_type.coalesced(symbols)?, method.parent(), symbols)?,
            type_args,
        ),
        ir::Call::Static {
            parent_id,
            parent_type_args,
            method,
            type_args,
            ..
        } => {
            let args = parent_type_args
                .iter()
                .map(|typ| typ.coalesced(symbols))
                .collect::<Result<Vec<_>, _>>()?;
            (
                method_type(*method, symbols)?,
                receiver_env(&TypeApp::new(*parent_id, args), method.parent(), symbols)?,
                type_args,
            )
        }
        ir::Call::Closure { closure_type, .. } => {
            let typ = closure_type.coalesced(symbols)?;
            if typ.id().is_fn()
                && let Some(result) = typ.args().last()
            {
                return Ok(result.clone());
            }
            return Err(QueryError::InvalidClosure(typ));
        }
    };
    check_arg_count(ft.type_vars().len(), type_args.len())?;
    let args = type_args
        .iter()
        .map(|typ| Ok(PolyType::from_type(&typ.coalesced(symbols)?)))
        .collect::<Result<Vec<_>, QueryError<'ctx>>>()?;
    let env = env.push_scope(ft.type_vars().zip(args).collect());
    substitute(ft.return_type(), &env, symbols)
}

fn method_type<'a, 'ctx>(
    method: MethodId<'ctx>,
    symbols: &'a Symbols<'ctx>,
) -> Result<&'a FunctionType<'ctx>, QueryError<'ctx>> {
    Ok(symbols
        .get_method(method)
        .ok_or(QueryError::MissingMethod(method))?
        .1
        .type_())
}

fn receiver_env<'scope, 'ctx: 'scope>(
    receiver: &TypeApp<'ctx>,
    parent: TypeId<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Result<ScopedMap<'scope, &'ctx str, PolyType<'ctx>>, QueryError<'ctx>> {
    let def = symbols
        .get_type(receiver.id())
        .ok_or(QueryError::MissingType(receiver.id()))?;
    check_arg_count(def.params().len(), receiver.args().len())?;
    let receiver_type = InferredTypeApp::from_type(receiver)
        .instantiate_as::<AnyBaseType>(parent, symbols)
        .ok_or(QueryError::InvalidReceiver {
            receiver: receiver.id(),
            parent,
        })?;
    Ok(receiver_type.type_env(symbols))
}

fn check_arg_count<'ctx>(expected: usize, found: usize) -> Result<(), QueryError<'ctx>> {
    if expected != found {
        return Err(QueryError::TypeArgumentCount { expected, found });
    }
    Ok(())
}

fn substitute<'ctx>(
    typ: &Type<'ctx>,
    env: &ScopedMap<'_, &'ctx str, PolyType<'ctx>>,
    symbols: &Symbols<'ctx>,
) -> Result<Type<'ctx>, QueryError<'ctx>> {
    Ok(PolyType::from_type_with_env(typ, env)
        .map_err(QueryError::MissingTypeArgument)?
        .coalesce(symbols)?)
}
