use std::str::FromStr;

use nom::branch::alt;
use nom::combinator::{consumed, map, verify};
use nom::multi::separated_list1;
use nom::{Offset, Slice};
use redscript::ast::{Constant, Ident, Literal, SourceAst};
use redscript::Str;

use crate::lexer::*;
use crate::validators::*;
use crate::*;

type Range = redscript::ast::Span;
type Expr = redscript::ast::Expr<SourceAst>;

fn to_range(input: &Span, remaining: &Span) -> Range {
    let len = input.slice(..remaining.offset(input)).len();
    Range::with_len(input.location_offset(), len)
}

macro_rules! to_ok {
    ($input:tt, $remaining:tt, $($value:tt)*) => {
        ($($value)*, to_range(&$input, &$remaining))
    };
}

/// A dot separated sequence of identifiers.
fn trailer(is: Span) -> IResult<(Vec<Ident>, Range)> {
    map(
        consumed(separated_list1(verify(control, |(_, c)| c == &Ctrl::Dot), ident)),
        |(span, trailer)| (trailer, span.to_range().into()),
    )(is)
}

fn ident(is: Span) -> IResult<Ident> {
    map(identifier, |ident| ident.to_flex())(is)
}

fn string_literal(is: Span) -> IResult<(Literal, Str)> {
    let (i, (ii, t, s)) = string(is)?;
    Ok((i, (t, s)))
}

pub fn string_interpolation(is: Span) -> IResult<(Str, Vec<(Expr, Str)>, Range)> {
    let (mut i, (_, st, ss)) = string_inter_start(is.clone())?;

    let mut parts = vec![];
    while let Ok((ip, ep)) = expr(i.clone()) {
        if let Ok((ip, (_, sp))) = string_inter_part(ip.clone()) {
            parts.push((ep, sp));
            i = ip;
            continue;
        } else if let Ok((ip, (_, se))) = string_inter_end(ip) {
            parts.push((ep, se));
            i = ip;
            break;
        }
        // fallback must be implemented in the subordinate parsers.
        return Err(nom::Err::Error(nom::error::Error::new(i, nom::error::ErrorKind::Tag)));
    }
    let r = to_ok!(is, i, ss, parts);
    Ok((i, r))
}

fn constant(is: Span) -> IResult<Constant> {
    alt((
        map(string_literal, |(t, s)| Constant::String(t, s)),
        map(number, |(_, n)| n.into()),
        map(boolean, |(_, b)| Constant::Bool(b)),
    ))(is)
}

fn expr(is: Span) -> IResult<Expr> {
    todo!()
}
