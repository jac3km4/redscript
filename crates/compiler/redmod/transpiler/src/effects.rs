//! Conservative expression effects, independent of target syntax eligibility.
use redscript_compiler_frontend::{Symbols, ir};

/// True only when evaluation cannot call user code or write to existing storage.
/// Representation conversions are recognized by resolved intrinsic identity, not
/// their spelling. Allocation and general calls remain conservatively effectful.
pub(crate) fn non_mutating(expr: &ir::Expr<'_>, symbols: &Symbols<'_>) -> bool {
    match expr {
        ir::Expr::Local(..) | ir::Expr::Const(..) | ir::Expr::Null { .. } => true,
        ir::Expr::Field { receiver, .. } => non_mutating(receiver, symbols),
        ir::Expr::Index { array, index, .. } => {
            non_mutating(array, symbols) && non_mutating(index, symbols)
        }
        ir::Expr::DynCast { expr, .. } => non_mutating(expr, symbols),
        ir::Expr::Conditional {
            condition,
            then,
            else_,
            ..
        } => {
            non_mutating(condition, symbols)
                && non_mutating(then, symbols)
                && non_mutating(else_, symbols)
        }
        ir::Expr::Call { call, .. } => {
            matches!(&**call, ir::Call::FreeFunction { function, args, .. }
                if matches!(symbols[*function].intrinsic(), Some(
                    ir::Intrinsic::RefToWeakRef | ir::Intrinsic::WeakRefToRef | ir::Intrinsic::Deref))
                    && matches!(&args[..], [operand] if non_mutating(operand, symbols)))
        }
        _ => false,
    }
}
