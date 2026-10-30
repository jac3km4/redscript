//! Reject source evaluation semantics lost by shared array-literal prefix lifting.
use redscript_redmod_ast::{BinOp, Expr, Span, Spanned};

use crate::CapabilityError;

type Result<T> = std::result::Result<T, (CapabilityError, Span)>;

/// A native expression traversal returning whether a subtree emits array prefixes.
/// Constants alone are safe to move past: even a local read can observe mutations
/// performed by a later array initializer. No purity is inferred from call names.
pub(crate) struct ArrayGuard;

impl ArrayGuard {
    pub(crate) fn check(expr: &Spanned<Expr<'_>>) -> Result<()> {
        Self::visit(expr, false).map(|_| ())
    }

    pub(crate) fn check_lazy(expr: &Spanned<Expr<'_>>) -> Result<()> {
        Self::visit(expr, true).map(|_| ())
    }

    fn reject(span: Span) -> (CapabilityError, Span) {
        (CapabilityError::UnsafeArrayEvaluation, span)
    }

    fn siblings(exprs: &[Spanned<Expr<'_>>], lazy: bool) -> Result<bool> {
        let mut earlier_expression = false;
        let mut has_array = false;
        for expr in exprs {
            let array = Self::visit(expr, lazy)?;
            if earlier_expression && array {
                return Err(Self::reject(expr.1));
            }
            earlier_expression |= !matches!(expr.0, Expr::Constant(_));
            has_array |= array;
        }
        Ok(has_array)
    }

    fn pair(
        before: &Spanned<Expr<'_>>,
        after: &Spanned<Expr<'_>>,
        lazy: bool,
        lazy_rhs: bool,
    ) -> Result<bool> {
        let first_array = Self::visit(before, lazy)?;
        let second_array = Self::visit(after, lazy || lazy_rhs)?;
        if second_array && !matches!(before.0, Expr::Constant(_)) {
            return Err(Self::reject(after.1));
        }
        Ok(first_array || second_array)
    }

    fn visit((expr, span): &Spanned<Expr<'_>>, lazy: bool) -> Result<bool> {
        match expr {
            Expr::ArrayLit(elems) => {
                if lazy {
                    return Err(Self::reject(*span));
                }
                Self::siblings(elems, lazy)?;
                Ok(true)
            }
            Expr::Call { expr, args } => {
                let callee_array = Self::visit(expr, lazy)?;
                let args_array = Self::siblings(args, lazy)?;
                if args_array && !matches!(expr.0, Expr::Ident(_)) {
                    return Err(Self::reject(expr.1));
                }
                Ok(callee_array || args_array)
            }
            Expr::BinOp { lhs, op, rhs } => {
                Self::pair(lhs, rhs, lazy, matches!(op, BinOp::And | BinOp::Or))
            }
            Expr::Index { expr, index } => Self::pair(expr, index, lazy, false),
            Expr::Assign { lhs, rhs } => {
                let lhs_array = Self::visit(lhs, lazy)?;
                let rhs_array = Self::visit(rhs, lazy)?;
                if rhs_array && !matches!(lhs.0, Expr::Ident(_)) {
                    return Err(Self::reject(lhs.1));
                }
                Ok(lhs_array || rhs_array)
            }
            Expr::Conditional { cond, then, else_ } => {
                let cond_array = Self::visit(cond, lazy)?;
                Self::visit(then, true)?;
                Self::visit(else_, true)?;
                Ok(cond_array)
            }
            Expr::Cast { expr, .. }
            | Expr::DynCast { expr, .. }
            | Expr::UnOp { expr, .. }
            | Expr::Member { expr, .. } => Self::visit(expr, lazy),
            Expr::Sequence(exprs) => Self::siblings(exprs, lazy),
            Expr::Ident(_)
            | Expr::Constant(_)
            | Expr::Null
            | Expr::This
            | Expr::Super
            | Expr::New { .. }
            | Expr::Omitted
            | Expr::Error => Ok(false),
        }
    }
}
