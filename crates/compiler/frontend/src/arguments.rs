use redscript_community_ast::{self as ast, Span, Spanned};

use crate::Param;

/// Construct an absent positional call argument.
pub fn absent_argument<'ctx>(span: Span) -> Spanned<ast::SourceExpr<'ctx>> {
    (ast::Expr::Omitted, span)
}

pub(super) fn is_absent(expr: &ast::SourceExpr<'_>) -> bool {
    matches!(expr, ast::Expr::Omitted)
}

pub(crate) fn accepts_slots<'a, 'ctx: 'a>(
    args: impl Iterator<Item = &'a Spanned<ast::SourceExpr<'ctx>>>,
    params: &[Param<'ctx>],
) -> bool {
    args.zip(params)
        .all(|((expr, _), param)| !is_absent(expr) || param.flags().is_optional())
}
