use std::str::FromStr;

use nom::branch::alt;
use nom::combinator::{consumed, map, opt};
use nom::multi::{many0, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::{Offset, Slice};
use redscript::ast::{Constant, Ident, Literal, SourceAst, TypeName, TypeParam};
use redscript::Str;

use crate::comb::{delimited_list0, variant};
use crate::lexer::*;
use crate::parser::{
    Annotation, AnnotationKind, Declaration, FieldSource, FunctionSource, ParameterSource, Qualifiers
};
use crate::validators::*;
use crate::*;

type Range = redscript::ast::Span;
type Expr = redscript::ast::Expr<SourceAst>;
type Seq = redscript::ast::Seq<SourceAst>;

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
        consumed(separated_list1(variant(Ctrl::Dot), ident)),
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

fn type_list(is: Span) -> IResult<Vec<TypeName>> {
    separated_list1(variant(Ctrl::Comma), type_name)(is)
}

fn type_args(is: Span) -> IResult<Vec<TypeName>> {
    delimited(variant(Op::Lt), type_list, variant(Op::Gt))(is)
}

fn ret_type(is: Span) -> IResult<TypeName> {
    preceded(variant(Ctrl::LArrow), type_name)(is)
}

fn type_name(is: Span) -> IResult<TypeName> {
    alt((
        map(pair(ident, opt(type_args)), |(ident, args)| {
            TypeName::new(ident, args.unwrap_or_default())
        }),
        map(
            delimited(variant(Ctrl::LBracket), type_name, variant(Ctrl::RBracket)),
            TypeName::of_array,
        ),
        map(
            pair(
                delimited(variant(Ctrl::LParen), type_list, variant(Ctrl::RParen)),
                ret_type,
            ),
            |(args, ret)| TypeName::of_function(args, ret),
        ),
    ))(is)
}

fn assign_init(is: Span) -> IResult<Option<Expr>> {
    opt(preceded(variant(Op::Eq), expr))(is)
}

fn let_type(is: Span) -> IResult<TypeName> {
    preceded(variant(Ctrl::Colon), type_name)(is)
}

fn let_(is: Span) -> IResult<Expr> {
    map(
        consumed(tuple((preceded(variant(Kw::Let), ident), opt(let_type), assign_init))),
        |(span, (ident, ty, value))| {
            Expr::Declare(ident, ty.map(Box::new), value.map(Box::new), span.to_range().into())
        },
    )(is)
}

fn expr_list(is: Span) -> IResult<Vec<Expr>> {
    separated_list1(variant(Ctrl::Comma), expr)(is)
}

fn args(is: Span) -> IResult<Vec<Expr>> {
    map(
        consumed(delimited(variant(Ctrl::LParen), expr_list, variant(Ctrl::RParen))),
        |(_, args)| args,
    )(is)
}

fn annotation(is: Span) -> IResult<Annotation> {
    let (rem, (span, (ident, args))) = consumed(tuple((preceded(variant(Ctrl::At), ident), opt(args))))(is)?;

    match AnnotationKind::from_str(&ident) {
        Ok(kind) => Ok((rem, Annotation {
            kind,
            args: args.unwrap_or_default(),
            span: span.to_range().into(),
        })),
        Err(_) => Err(nom_error(rem, NomErrorKind::Tag)),
    }
}

fn annotation_list(is: Span) -> IResult<Vec<Annotation>> {
    many0(annotation)(is)
}

fn qualifier_list(is: Span) -> IResult<Qualifiers> {
    map(many0(qualifier), |qs| Qualifiers(qs))(is)
}

fn type_param(is: Span) -> IResult<TypeParam> {
    map(
        tuple((variance, ident, opt(preceded(variant(Kw::Extends), type_name)))),
        |(variance, ident, extends)| TypeParam {
            name: ident,
            variance,
            extends,
        },
    )(is)
}

fn type_params(is: Span) -> IResult<Vec<TypeParam>> {
    delimited(
        variant(Op::Lt),
        separated_list1(variant(Ctrl::Comma), type_param),
        variant(Op::Gt),
    )(is)
}

fn decl<'a, T>(
    mut inner: impl FnMut(Span<'a>) -> IResult<'a, T>,
) -> impl FnMut(Span<'a>) -> IResult<'a, (Declaration, T)> {
    move |is| {
        map(
            consumed(tuple((annotation_list, qualifier_list, &mut inner, ident))),
            |(span, (annotations, qualifiers, inner, ident))| {
                (
                    Declaration {
                        annotations,
                        qualifiers,
                        name: ident,
                        span: span.to_range().into(),
                    },
                    inner,
                )
            },
        )(is)
    }
}

fn field(is: Span) -> IResult<FieldSource> {
    map(
        tuple((decl(variant(Kw::Let)), let_type, assign_init)),
        |((declaration, _), ty, init)| FieldSource {
            declaration,
            type_: ty,
            default: init,
        },
    )(is)
}

fn param(is: Span) -> IResult<ParameterSource> {
    map(tuple((qualifier_list, ident, let_type)), |(qualifiers, name, ty)| {
        ParameterSource {
            qualifiers,
            name,
            type_: ty,
        }
    })(is)
}

fn params(is: Span) -> IResult<Vec<ParameterSource>> {
    delimited(
        variant(Ctrl::LParen),
        separated_list0(variant(Ctrl::Comma), param),
        variant(Ctrl::RParen),
    )(is)
}

fn func(is: Span) -> IResult<FunctionSource> {
    map(
        consumed(tuple((
            decl(variant(Kw::Func)),
            opt(type_params),
            params,
            opt(ret_type),
            opt(func_body),
        ))),
        |(span, ((declaration, _), tparams, parameters, ret_type, body))| FunctionSource {
            tparams: tparams.unwrap_or_default(),
            declaration,
            type_: ret_type,
            parameters,
            body,
            span: span.to_range().into(),
        },
    )(is)
}

fn func_body(is: Span) -> IResult<Seq> {
    todo!()
}

pub fn expr(is: Span) -> IResult<Expr> {
    todo!()
}
