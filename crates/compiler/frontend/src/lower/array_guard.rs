//! Diagnose changed array evaluation while retaining legacy lowering for runtime compatibility.
use redscript_community_ast::{BinOp, Expr, SourceExpr, Span, Spanned, StrPart};

use super::{Error, Lower, LowerResult};

impl<'scope, 'ctx> Lower<'scope, 'ctx> {
    pub(super) fn check_array_guard(&mut self, expr: &Spanned<SourceExpr<'ctx>>, lazy: bool) {
        let result = if lazy { check_lazy(expr) } else { check(expr) };
        self.report_array_guard(expr.1, result);
    }

    pub(super) fn report_array_guard(&mut self, scope: Span, result: LowerResult<'ctx, ()>) {
        if let Err(error) = result {
            // Recursive expression lowering and resolved-callee/skip checks can
            // rediscover the same unsafe prefix. Report once for that subtree.
            if self.array_guard_reports.iter().any(|reported| {
                reported.file == scope.file
                    && ((reported.start <= scope.start && scope.end <= reported.end)
                        || (scope.start <= reported.start && reported.end <= scope.end))
            }) {
                return;
            }
            self.array_guard_reports.push(scope);
            self.reporter.report(error);
        }
    }
}

pub(super) fn check<'ctx>(expr: &Spanned<SourceExpr<'ctx>>) -> LowerResult<'ctx, ()> {
    visit(expr, false).map(|_| ())
}

pub(super) fn check_lazy<'ctx>(expr: &Spanned<SourceExpr<'ctx>>) -> LowerResult<'ctx, ()> {
    visit(expr, true).map(|_| ())
}

// Call lowering invokes this after distinguishing runtime values from static names.
pub(super) fn check_runtime_callee<'ctx>(
    args: &[Spanned<SourceExpr<'ctx>>],
    span: Span,
) -> LowerResult<'ctx, ()> {
    for arg in args {
        if visit(arg, false)? {
            return Err(Error::UnsafeArrayEvaluation(span));
        }
    }
    Ok(())
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
            // Identifier receivers may name a type. Call lowering checks runtime
            // callees once name resolution can distinguish them from static calls.
            let static_syntax = matches!(expr.0, Expr::Ident(_))
                || matches!(&expr.0, Expr::Member { expr, .. } if matches!(expr.0, Expr::Ident(_)));
            if args_array && !static_syntax {
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
