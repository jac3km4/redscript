use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while_m_n};
use nom::character::complete::{
    alpha1, anychar, char, digit0, digit1, hex_digit0, line_ending, multispace1, none_of, oct_digit0, one_of, satisfy
};
use nom::combinator::{consumed, map, not, opt, recognize};
use nom::error::ParseError;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, separated_pair};
use nom::AsChar;
use redscript::ast::{Constant, Literal};
use redscript::Str;
use strum::{Display, IntoStaticStr};

use crate::comb::many_till_balanced1;
use crate::validators::*;
use crate::*;

pub trait ParseErr<'a>: ParseError<Span<'a>> {}
pub type IResult<'a, O> = nom::IResult<Span<'a>, O>;
pub type NomError<'a> = nom::Err<nom::error::Error<Span<'a>>>;
pub type NomErrorKind = nom::error::ErrorKind;

pub fn nom_error(input: Span, kind: NomErrorKind) -> NomError {
    NomError::Error(nom::error::Error::new(input, kind))
}

#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum Trivia {
    Comment,
    Whitespace,
    LineEnd,
}

#[derive(Debug, Clone, Copy, PartialEq, Display, IntoStaticStr)]
pub enum Num {
    F32(f32),
    F64(f64),
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
}

impl From<Num> for Constant {
    fn from(value: Num) -> Self {
        match value {
            Num::F32(f) => Constant::F32(f),
            Num::F64(f) => Constant::F64(f),
            Num::I32(f) => Constant::I32(f),
            Num::I64(f) => Constant::I64(f),
            Num::U32(f) => Constant::U32(f),
            Num::U64(f) => Constant::U64(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Display, IntoStaticStr)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Bang,
    Eq,
    Lt,
    Gt,
    And,
    Or,
    Tilde,
}

#[derive(Debug, Clone, Copy, PartialEq, Display, IntoStaticStr)]
pub enum Ctrl {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Colon,
    Semi,
    Comma,
    Period,
    Dot,
    Quest,
    LArrow,
}

#[derive(Debug, Clone, Copy, PartialEq, Display, IntoStaticStr)]
pub enum Kw {
    Module,
    Class,
    Struct,
    Enum,
    Func,
    Let,
    New,
    If,
    Else,
    Switch,
    Case,
    Break,
    While,
    For,
    In,
    Continue,
    Return,
    Try,
    Catch,
    Finally,
}

// -----------------------------------------------------------------------------
// Trivia
// -----------------------------------------------------------------------------

fn comment_multiline(i: Span) -> IResult<Span> {
    recognize(many_till_balanced1(
        tag("/*"),
        recognize(many0(not(alt((tag("*/"), tag("/*")))))),
        tag("*/"),
    ))(i)
}

pub fn trivia(i: Span) -> IResult<(Trivia, Span)> {
    alt((
        map(comment_multiline, |s| (Trivia::Comment, s)),
        map(recognize(preceded(tag("//"), many0(not(line_ending)))), |s| {
            (Trivia::Comment, s)
        }),
        map(recognize(line_ending), |s| (Trivia::LineEnd, s)),
        map(recognize(multispace1), |s| (Trivia::Whitespace, s)),
    ))(i)
}

// -----------------------------------------------------------------------------
// Numeric
// -----------------------------------------------------------------------------

fn float_literal(i: Span) -> IResult<Span> {
    recognize(separated_pair(digit0, tag("."), digit1))(i)
}

fn sciexp_literal(i: Span) -> IResult<Span> {
    recognize(separated_pair(
        alt((float_literal, digit1)),
        one_of("eE"),
        pair(one_of("-+"), digit1),
    ))(i)
}

fn parse_num<T: num::Num + Default>(i: &Span, radix: u32) -> T
where
    T::FromStrRadixErr: std::fmt::Display,
{
    match T::from_str_radix(i.fragment(), radix) {
        Ok(value) => value,
        Err(error) => {
            diag_report!(i, ERR_NUM_PARSE, i.fragment(), error);
            T::default()
        }
    }
}

fn int_width(suffix: Option<Span>) -> u32 {
    match suffix {
        Some(suffix) => match alt((
            map(tag("i32"), |_: Span| 32),
            map(tag("i64"), |_| 64),
            map(tag("u32"), |_| 32),
            map(tag("u64"), |_| 64),
            map(tag("l"), |_| 64),
        ))(suffix.clone())
        {
            Ok((_, width)) => width,
            Err(err) => {
                let _: NomError = err; // needed for type inference
                diag_report!(suffix, ERR_NUM_SUFFIX, suffix.fragment());
                32
            }
        },
        None => 32,
    }
}

pub fn integer(is: Span) -> IResult<Num> {
    match pair(
        alt((
            map(preceded(tag("0x"), hex_digit0), |s| (16, s)),
            map(preceded(tag("0o"), oct_digit0), |s| (8, s)),
            map(
                preceded(tag("0b"), take_while_m_n(1, 64, |c: char| c == '0' || c == '1')),
                |s| (2, s),
            ),
            map(digit0, |s| (10, s)),
        )),
        opt(identifier),
    )(is)
    {
        Ok((rem, ((radix, value), suffix))) => {
            let num = match int_width(suffix) {
                64 => Num::I64(parse_num(&value, radix)),
                _ => Num::I32(parse_num(&value, radix)),
            };
            Ok((rem, num))
        }
        Err(err) => Err(err),
    }
}

fn float_width(suffix: Option<Span>) -> u32 {
    match suffix {
        Some(suffix) => {
            match alt((map(tag("f32"), |_| 32), map(tag("f64"), |_| 64), map(tag("d"), |_| 64)))(suffix.clone()) {
                Ok((_, width)) => width,
                Err(err) => {
                    let err: NomError = err; // needed for type inference
                    diag_report!(suffix, ERR_NUM_SUFFIX, suffix.fragment());
                    64
                }
            }
        }
        None => 64,
    }
}

pub fn float(is: Span) -> IResult<Num> {
    match pair(alt((sciexp_literal, float_literal)), opt(identifier))(is) {
        Ok((rem, (value, suffix))) => {
            let num = match float_width(suffix) {
                64 => Num::F64(parse_num(&value, 10)),
                _ => Num::F32(parse_num(&value, 10)),
            };
            Ok((rem, num))
        }
        Err(err) => Err(err),
    }
}

// -----------------------------------------------------------------------------
// String
// -----------------------------------------------------------------------------
// Strings are parsed as a sequence of literal portions and interpolated portions
// The interpolated portions are parsed as part of the token stream, delimited by the start and end interpolation tokens
// The literal portions are parsed as a single token
// The entire string may be prefixed with a type specifier, a char.

fn str_char_uni(is: Span) -> IResult<Option<char>> {
    let parse_hex = &take_while_m_n(1, 6, char::is_hex_digit);
    let parse_delimited_hex = delimited(char('{'), parse_hex, char('}'));
    let (i, digits) = alt((preceded(char('u'), parse_delimited_hex), preceded(char('u'), parse_hex)))(is.clone())?;
    if let Ok(hex) = u32::from_str_radix(digits.fragment(), 16) {
        if let Some(c) = char::from_u32(hex) {
            Ok((i, Some(c)))
        } else {
            diag_report!((&is..&i), ERR_CHAR_UTF8, hex);
            Ok((i, None))
        }
    } else {
        diag_report!((&is..&i), ERR_CHAR_UTF8, digits.fragment());
        Ok((i, None))
    }
}

fn str_char_invalid(is: Span) -> IResult<Option<char>> {
    let (i, c) = preceded(char('\\'), anychar)(is.clone())?;
    diag_report!((&is..&i), ERR_CHAR_ESCAPE, c);
    Ok((i, None))
}

fn str_char(i: Span) -> IResult<Option<char>> {
    alt((
        map(tag(r#"\\"#), |_| Some('\\')),
        map(tag(r#"\/"#), |_| Some('/')),
        map(tag(r#"\""#), |_| Some('"')),
        map(tag(r#"\n"#), |_| Some('\n')),
        map(tag(r#"\t"#), |_| Some('\t')),
        map(tag(r#"\r"#), |_| Some('\r')),
        map(tag(r#"\0"#), |_| Some('\0')),
        str_char_uni,
        map(none_of("\\"), Some),
        str_char_invalid,
    ))(i)
}

fn str_chars(mut i: Span) -> IResult<Str> {
    let mut s = String::default();
    while let Ok((i_remaining, c)) = str_char(i.clone()) {
        if let Some(c) = c {
            s.push(c);
        }
        i = i_remaining;
    }
    Ok((i, Str::from_ref(s)))
}

fn string_type(i: &Span, c: Option<char>) -> Literal {
    match c {
        Some(c) => match c {
            'n' => Literal::Name,
            'r' => Literal::Resource,
            't' => Literal::TweakDbId,
            's' => Literal::String,
            _ => {
                diag_report!(i, ERR_LITERAL_TYPE_INVALID, c);
                Literal::String
            }
        },
        None => Literal::String,
    }
}

// a parser accepting a function and returning the result of the function, by consuming the input
pub fn string(i: Span) -> IResult<(Span, Literal, Str)> {
    let (i, (o, (p, s))) = consumed(pair(
        opt(satisfy(AsChar::is_alpha)),
        delimited(tag("\""), str_chars, tag("\"")),
    ))(i)?;
    let p = string_type(&o, p);
    Ok((i, (o, p, s)))
}

// matches a string literal until the first interpolation
pub fn string_inter_start(i: Span) -> IResult<(Span, Literal, Str)> {
    let (i, (o, (p, s))) = consumed(pair(
        opt(satisfy(AsChar::is_alpha)),
        delimited(tag("\""), str_chars, tag(r#"\("#)),
    ))(i)?;
    let p = string_type(&o, p);
    Ok((i, (o, p, s)))
}

// matches a string literal from the end of the first interpolation until the end of the string
pub fn string_inter_end(i: Span) -> IResult<(Span, Str)> {
    consumed(delimited(tag(r#")"#), str_chars, tag("\"")))(i)
}

// matches a string literal from the end of the first interpolation until the start of the next interpolation
pub fn string_inter_part(i: Span) -> IResult<(Span, Str)> {
    consumed(delimited(tag(r#")"#), str_chars, tag(r#"\("#)))(i)
}

// -----------------------------------------------------------------------------
// Operator
// -----------------------------------------------------------------------------
// one of `+-*/!=<>&|~`

pub fn operator(i: Span) -> IResult<(Span, Op)> {
    alt((
        map(tag("="), |s| (s, Op::Add)),
        map(tag("-"), |s| (s, Op::Sub)),
        map(tag("*"), |s| (s, Op::Mul)),
        map(tag("/"), |s| (s, Op::Div)),
        map(tag("!"), |s| (s, Op::Bang)),
        map(tag("="), |s| (s, Op::Eq)),
        map(tag("<"), |s| (s, Op::Lt)),
        map(tag(">"), |s| (s, Op::Gt)),
        map(tag("&"), |s| (s, Op::Add)),
        map(tag("|"), |s| (s, Op::Or)),
        map(tag("~"), |s| (s, Op::Tilde)),
    ))(i)
}

// -----------------------------------------------------------------------------
// Control character
// -----------------------------------------------------------------------------
// one of `()[]{}:;,.?` and ->

pub fn control(i: Span) -> IResult<(Span, Ctrl)> {
    alt((
        map(tag("("), |s| (s, Ctrl::LParen)),
        map(tag(")"), |s| (s, Ctrl::RParen)),
        map(tag("["), |s| (s, Ctrl::LBracket)),
        map(tag("]"), |s| (s, Ctrl::RBracket)),
        map(tag("{"), |s| (s, Ctrl::LBrace)),
        map(tag("}"), |s| (s, Ctrl::RBrace)),
        map(tag(":"), |s| (s, Ctrl::Colon)),
        map(tag(";"), |s| (s, Ctrl::Semi)),
        map(tag(","), |s| (s, Ctrl::Comma)),
        map(tag("."), |s| (s, Ctrl::Dot)),
        map(tag("?"), |s| (s, Ctrl::Quest)),
        map(tag("->"), |s| (s, Ctrl::LArrow)),
    ))(i)
}

// -----------------------------------------------------------------------------
// Identifier
// -----------------------------------------------------------------------------
// An identifier is a sequence of letters, numbers, and underscores, starting with a letter or underscore

pub fn identifier(i: Span) -> IResult<Span> {
    recognize(pair(alpha1, take_while(|c: char| c.is_alphanumeric() || c == '_')))(i)
}

// -----------------------------------------------------------------------------
// Keyword
// -----------------------------------------------------------------------------
// A reserved langauge keyword
// one of module, class, struct, enum, func, let, new, if, else, switch, case, break, while, for, in, continue, return, try, catch, finally

pub fn keyword(i: Span) -> IResult<(Span, Kw)> {
    alt((
        map(tag("module"), |s| (s, Kw::Module)),
        map(tag("class"), |s| (s, Kw::Class)),
        map(tag("struct"), |s| (s, Kw::Struct)),
        map(tag("enum"), |s| (s, Kw::Enum)),
        map(tag("func"), |s| (s, Kw::Func)),
        map(tag("let"), |s| (s, Kw::Let)),
        map(tag("new"), |s| (s, Kw::New)),
        map(tag("if"), |s| (s, Kw::If)),
        map(tag("else"), |s| (s, Kw::Else)),
        map(tag("switch"), |s| (s, Kw::Switch)),
        map(tag("case"), |s| (s, Kw::Case)),
        map(tag("break"), |s| (s, Kw::Break)),
        map(tag("while"), |s| (s, Kw::While)),
        map(tag("for"), |s| (s, Kw::For)),
        map(tag("in"), |s| (s, Kw::In)),
        map(tag("continue"), |s| (s, Kw::Continue)),
        map(tag("return"), |s| (s, Kw::Return)),
        map(tag("try"), |s| (s, Kw::Try)),
        map(tag("catch"), |s| (s, Kw::Catch)),
        map(tag("finally"), |s| (s, Kw::Finally)),
    ))(i)
}

pub fn null(i: Span) -> IResult<Span> {
    tag("null")(i)
}

pub fn this(i: Span) -> IResult<Span> {
    tag("this")(i)
}

pub fn super_(i: Span) -> IResult<Span> {
    tag("super")(i)
}

pub fn boolean(i: Span) -> IResult<(Span, bool)> {
    alt((map(tag("true"), |s| (s, true)), map(tag("false"), |s| (s, false))))(i)
}
