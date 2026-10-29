//! Field bracket metadata uses the shared ordered runtime-property path.

use redscript_community_ast::{self as ast, Span, Spanned};
use redscript_redmod_ast as native;

use crate::CapabilityError;

pub(crate) fn annotations<'src>(
    metadata: Vec<Spanned<native::Metadata<'src>>>,
    is_field: bool,
) -> Result<Vec<Spanned<ast::SourceAnnotation<'src>>>, (CapabilityError, Span)> {
    metadata
        .into_iter()
        .map(|(entry, span)| {
            if !is_field {
                return Err((CapabilityError::Metadata, span));
            }
            let (native::Expr::Constant(ast::Constant::String(value)), value_span) = entry.value
            else {
                return Err((CapabilityError::Metadata, entry.value.1));
            };
            // Do not normalize keys or collect into a map: IO properties retain
            // arbitrary spellings, duplicate keys, and source order.
            Ok((
                ast::Annotation::new(
                    "runtimeProperty",
                    vec![
                        (
                            ast::Expr::Constant(ast::Constant::String(entry.key.0.into())),
                            entry.key.1,
                        ),
                        (
                            ast::Expr::Constant(ast::Constant::String(value)),
                            value_span,
                        ),
                    ],
                ),
                span,
            ))
        })
        .collect()
}
