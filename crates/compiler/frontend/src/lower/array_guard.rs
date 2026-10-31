//! Fail closed where statement-prefix array construction would change evaluation.
use redscript_community_ast::{BinOp, Expr, SourceExpr, Spanned, StrPart};

use super::{Error, LowerResult};

pub(super) fn check<'ctx>(expr: &Spanned<SourceExpr<'ctx>>) -> LowerResult<'ctx, ()> {
    visit(expr, false).map(|_| ())
}

fn siblings<'a, 'ctx: 'a>(
    exprs: impl IntoIterator<Item = &'a Spanned<SourceExpr<'ctx>>>,
    lazy: bool,
) -> LowerResult<'ctx, bool> {
    let mut earlier_expression = false;
    let mut has_array = false;
    for expr in exprs {
        let array = visit(expr, lazy)?;
        if earlier_expression && array {
            return Err(Error::UnsafeArrayEvaluation(expr.1));
        }
        // Even a local read can observe mutations made by a later initializer.
        earlier_expression |= !prefix_inert(&expr.0);
        has_array |= array;
    }
    Ok(has_array)
}

// A literal-only nested array reads only its private generated temporary after
// construction. Unlike a source local, a later initializer cannot mutate it.
fn prefix_inert(expr: &SourceExpr<'_>) -> bool {
    match expr {
        Expr::Constant(_) | Expr::Omitted => true,
        Expr::ArrayLit(elems) => elems.iter().all(|(expr, _)| prefix_inert(expr)),
        _ => false,
    }
}

fn pair<'ctx>(
    before: &Spanned<SourceExpr<'ctx>>,
    after: &Spanned<SourceExpr<'ctx>>,
    lazy: bool,
    lazy_rhs: bool,
) -> LowerResult<'ctx, bool> {
    let first = visit(before, lazy)?;
    let second = visit(after, lazy || lazy_rhs)?;
    if second && !matches!(before.0, Expr::Constant(_)) {
        return Err(Error::UnsafeArrayEvaluation(after.1));
    }
    Ok(first || second)
}

fn visit<'ctx>((expr, span): &Spanned<SourceExpr<'ctx>>, lazy: bool) -> LowerResult<'ctx, bool> {
    match expr {
        Expr::ArrayLit(elems) => {
            if lazy {
                return Err(Error::UnsafeArrayEvaluation(*span));
            }
            siblings(elems.iter(), lazy)?;
            Ok(true)
        }
        Expr::Call { expr, args, .. } => {
            let callee = visit(expr, lazy)?;
            let args_array = siblings(args.iter(), lazy)?;
            if args_array && !matches!(expr.0, Expr::Ident(_)) {
                return Err(Error::UnsafeArrayEvaluation(expr.1));
            }
            Ok(callee || args_array)
        }
        Expr::New { args, .. } => siblings(args.iter(), lazy),
        Expr::InterpolatedString(parts) => siblings(
            parts.iter().filter_map(|part| match part {
                StrPart::Expr(expr) => Some(expr),
                StrPart::Str(_) => None,
            }),
            lazy,
        ),
        Expr::BinOp { lhs, op, rhs } => pair(lhs, rhs, lazy, matches!(op, BinOp::And | BinOp::Or)),
        Expr::Index { expr, index } => pair(expr, index, lazy, false),
        Expr::Assign { lhs, rhs } => {
            let first = visit(lhs, lazy)?;
            let second = visit(rhs, lazy)?;
            if second && !matches!(lhs.0, Expr::Ident(_)) {
                return Err(Error::UnsafeArrayEvaluation(lhs.1));
            }
            Ok(first || second)
        }
        Expr::Conditional { cond, then, else_ } => {
            let cond_array = visit(cond, lazy)?;
            visit(then, true)?;
            visit(else_, true)?;
            Ok(cond_array)
        }
        Expr::DynCast { expr, .. } | Expr::UnOp { expr, .. } | Expr::Member { expr, .. } => {
            visit(expr, lazy)
        }
        // Closure bodies have their own execution point and lowering scope.
        Expr::Lambda { .. }
        | Expr::Ident(_)
        | Expr::Constant(_)
        | Expr::Null
        | Expr::This
        | Expr::Super
        | Expr::Omitted
        | Expr::Error => Ok(false),
    }
}
