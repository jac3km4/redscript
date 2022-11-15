use bumpalo::collections::Vec;
use nom::{Offset, Slice};

use crate::lexer::*;
use crate::validators::Span;

/// A dot separated sequence of identifiers.
fn trailer<'a>(is: Span<'a>) -> IResult<(Vec<'a, Span<'a>>, Span<'a>)> {
    let mut vec = is.extra.create_vec();
    let (i, ident) = identifier(is.clone())?;
    let mut i = i;
    vec.push(ident.clone());
    while let Ok((ip, _)) = control(i.clone()) && let Ok((ip, ident)) = identifier(ip) {
        i = ip;
        vec.push(ident);
    }
    let len = is.offset(&i);
    Ok((i, (vec, is.slice(..len))))
}
