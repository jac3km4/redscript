//! Class-qualified defaults remain separate from declaration initializers.
use redscript_community_ast::{self as ast, Span, Spanned};

use crate::CapabilityError;

/// Preserve numeric spelling where it is a plain literal (not a parenthesized
/// expression or one containing comments). Official properties retain decimal
/// precision and trailing zeroes, rather than formatting the typed float again.
pub(crate) fn retain_literal_text<'src>(
    items: &mut [Spanned<ast::SourceItemDecl<'src>>],
    source: &'src str,
) {
    for (item, _) in items {
        let default = match &mut item.item {
            ast::Item::Class(aggregate) | ast::Item::Struct(aggregate) => {
                retain_literal_text(&mut aggregate.items, source);
                continue;
            }
            ast::Item::Default(default) => default,
            _ => continue,
        };
        let literal = match &default.value.0 {
            ast::Expr::Constant(constant) => Some(constant),
            ast::Expr::UnOp {
                op: ast::UnOp::Neg,
                expr,
            } => match &expr.0 {
                ast::Expr::Constant(constant) => Some(constant),
                _ => None,
            },
            _ => None,
        };
        if !matches!(
            literal,
            Some(
                ast::Constant::I32(_)
                    | ast::Constant::I64(_)
                    | ast::Constant::U32(_)
                    | ast::Constant::U64(_)
                    | ast::Constant::F32(_)
                    | ast::Constant::F64(_)
            )
        ) {
            continue;
        }
        let span = default.value.1;
        let Some(text) = source.get(span.start as usize..span.end as usize) else {
            continue;
        };
        let text = text.trim_end_matches(['l', 'd', 'f']);
        if !text.is_empty()
            && text.bytes().all(|byte| {
                byte.is_ascii_digit() || matches!(byte, b'-' | b'+' | b'.' | b'e' | b'E')
            })
        {
            default.literal_text = Some(text);
        }
    }
}

/// Retain declaration identity, source order, and all diagnostic spans. Resolve
/// field names only after the shared frontend has completed inheritance.
pub(crate) fn plan<'src>(
    target: Spanned<&'src str>,
    value: Spanned<ast::SourceExpr<'src>>,
) -> Result<ast::SourceFieldDefault<'src>, (CapabilityError, Span)> {
    let supported = match &value.0 {
        ast::Expr::Constant(_) => true,
        ast::Expr::UnOp {
            op: ast::UnOp::Neg,
            expr,
        } => matches!(
            expr.0,
            ast::Expr::Constant(
                ast::Constant::I32(_)
                    | ast::Constant::I64(_)
                    | ast::Constant::F32(_)
                    | ast::Constant::F64(_)
            )
        ),
        ast::Expr::Member { expr, .. } => matches!(expr.0, ast::Expr::Ident(_)),
        _ => false,
    };
    if !supported {
        return Err((CapabilityError::DefaultDirective, value.1));
    }
    Ok(ast::FieldDefault::new(target, Box::new(value)))
}
