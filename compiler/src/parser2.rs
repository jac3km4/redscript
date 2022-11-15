use nom::{Offset, Slice};
use redscript::ast::Ident;

use crate::lexer::*;
use crate::validators::Span;

type Range = redscript::ast::Span;

/// A dot separated sequence of identifiers.
fn trailer(is: Span) -> IResult<(Vec<Ident>, Range)> {
    let mut vec = vec![];
    let (i, ident) = identifier(is.clone())?;
    let mut i = i;
    vec.push(Ident::from_ref(ident.fragment()));
    while let Ok((ip, _)) = control(i.clone()) && let Ok((ip, ident)) = identifier(ip) {
        i = ip;
        vec.push(Ident::from_ref(ident.fragment()));
    }
    let len = is.offset(&i);
    Ok((i, (vec, Range::with_len(is.location_offset(), len))))
}
