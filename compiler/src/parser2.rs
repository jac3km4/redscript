use nom::branch::alt;
use nom::combinator::{consumed, map, verify};
use nom::multi::separated_list1;
use nom::sequence::delimited;
use nom::{Offset, Slice};
use redscript::ast::{Constant, Ident, Literal, SourceAst};
use redscript::Str;

use crate::comb::delimited_list0;
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
    map(
        consumed(delimited_list0(
            map(string_inter_start, |(_, _, s)| s),
            map(string_inter_part, |(_, s)| s),
            expr,
            map(string_inter_end, |(_, s)| s),
        )),
        |(r, (ss, sp))| (ss, sp, r.to_range().into()),
    )(is)
}

fn constant(is: Span) -> IResult<Constant> {
    alt((
        map(string_literal, |(t, s)| Constant::String(t, s)),
        map(consumed(alt((float, integer))), |(_, n)| n.into()),
        map(boolean, |(_, b)| Constant::Bool(b)),
    ))(is)
}

fn type_args(is: Span) -> IResult<Vec<Expr>> {
    delimited(
        verify(operator, |(_, op)| op == &Op::Lt),
        separated_list1(verify(control, |(_, c)| c == &Ctrl::Comma), expr),
        verify(operator, |(_, op)| op == &Op::Gt),
    )(is)
}

fn expr(is: Span) -> IResult<Expr> {
    todo!()
}
