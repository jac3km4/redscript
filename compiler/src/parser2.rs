use crate::lexer::*;
use crate::validators::Span;

/// A dot separated sequence of identifiers.
fn trailer<'a>(i: Span<'a>) -> IResult<(&'a [Span<'a>], Span<'a>)> {
    todo!()
}
