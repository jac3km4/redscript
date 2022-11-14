use std::cell::RefCell;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while, take_while_m_n};
use nom::character::complete::{
    alpha1, anychar, char, digit0, digit1, hex_digit0, line_ending, none_of, oct_digit0, one_of, satisfy
};
use nom::combinator::{consumed, map, not, opt, recognize};
use nom::error::ParseError;
use nom::multi::{many0, many0_count};
use nom::sequence::{delimited, pair, preceded, separated_pair, tuple};
use nom::AsChar;
use redscript::Str;
use strum::{Display, IntoStaticStr};

use crate::validators::*;
use crate::{diag_report, *};

pub trait ParseErr<'a>: ParseError<Span<'a>> {}
pub type IResult<'a, O> = nom::IResult<Span<'a>, O>;
pub type NomError<'a> = nom::Err<Span<'a>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub enum Token<'a> {
    Trivia(Span<'a>, Trivia),
    /// arbitrary numeric literal
    Num(Span<'a>, Num),
    /// a string literal portion
    /// the initial portion can have a type prefix
    /// a interpolated string needs to be parsed recursively: literal + start + token... + end + literal
    Str(Span<'a>, Option<char>, Str),
    /// Start of a string interpolation
    StrIs(Span<'a>, Option<char>, Str),
    /// End of a string interpolation
    StrIe(Span<'a>, Str),
    /// Inbetween part of a string interpolation
    StrIp(Span<'a>, Str),
    /// null
    Null(Span<'a>),
    /// one of true | false
    Bool(Span<'a>, bool),
    /// one of `+-*/!=<>&|~`
    Op(Span<'a>, Op),
    /// one of `()[]{}:;,.`
    Ctrl(Span<'a>, Ctrl),
    Ident(Span<'a>),
    /// Language keywords
    /// one of module, class, struct, enum, func, let, new, if, else, switch, case, break, while, for, in, continue, return, try, catch, finally
    Kw(Span<'a>, Kw),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum Trivia {
    Comment,
    Whitespace,
    LineEnd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, IntoStaticStr)]
pub enum Num {
    Float,
    Int,
    Hex,
    Oct,
    Bin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, IntoStaticStr)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, IntoStaticStr)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, IntoStaticStr)]
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

fn comment_multiline<'a>(i: Span<'a>) -> IResult<'a, Span<'a>> {
    recognize(delimited(
        tag("/*"),
        recognize(many0_count(alt((
            map(comment_multiline, |s| s),
            map(take_until("*/"), |s| s),
            map(take_while(|c| c != '*' && c != '/'), |s| s),
        )))),
        tag("*/"),
    ))(i)
}

pub fn trivia<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Trivia)> {
    alt((
        map(comment_multiline, |s| (s, Trivia::Comment)),
        map(recognize(preceded(tag("//"), many0(not(line_ending)))), |s| {
            (s, Trivia::Comment)
        }),
        map(recognize(line_ending), |s| (s, Trivia::LineEnd)),
        map(recognize(take_while(|c: char| c.is_whitespace())), |s| {
            (s, Trivia::Whitespace)
        }),
    ))(i)
}

// -----------------------------------------------------------------------------
// Numeric
// -----------------------------------------------------------------------------

fn float_literal<'a>(i: Span<'a>) -> IResult<'a, Span<'a>> {
    recognize(separated_pair(digit0, tag("."), digit1))(i)
}

fn sciexp_literal<'a>(i: Span<'a>) -> IResult<'a, Span<'a>> {
    recognize(separated_pair(alt((float_literal, digit1)), one_of("eE"), digit1))(i)
}

pub fn number<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Num)> {
    alt((
        map(sciexp_literal, |s| (s, Num::Float)),
        map(float_literal, |s| (s, Num::Float)),
        map(digit1, |s| (s, Num::Int)),
        map(preceded(tag("0x"), hex_digit0), |s| (s, Num::Hex)),
        map(preceded(tag("0o"), oct_digit0), |s| (s, Num::Oct)),
        map(preceded(tag("0b"), take_while(|c: char| c == '0' || c == '1')), |s| {
            (s, Num::Bin)
        }),
    ))(i)
}

// -----------------------------------------------------------------------------
// String
// -----------------------------------------------------------------------------
// Strings are parsed as a sequence of literal portions and interpolated portions
// The interpolated portions are parsed as part of the token stream, delimited by the start and end interpolation tokens
// The literal portions are parsed as a single token
// The entire string may be prefixed with a type specifier, a char.

fn str_char_uni<'a>(is: Span<'a>) -> IResult<'a, Option<char>> {
    let parse_hex = &take_while_m_n(1, 6, char::is_hex_digit);
    let parse_delimited_hex = delimited(char('{'), parse_hex, char('}'));
    let (i, digits) = alt((preceded(char('u'), parse_delimited_hex), preceded(char('u'), parse_hex)))(is.clone())?;
    match u32::from_str_radix(digits.fragment(), 16) {
        Ok(hex) => match char::from_u32(hex) {
            Some(c) => Ok((i, Some(c))),
            None => {
                diag_report!((&is..&i), ERR_INVALID_UTF8, hex);
                Ok((i, None))
            }
        },
        Err(_) => {
            diag_report!((&is..&i), ERR_INVALID_UTF8, digits.fragment());
            Ok((i, None))
        }
    }
}

fn str_char_invalid<'a>(is: Span<'a>) -> IResult<'a, Option<char>> {
    let (i, c) = preceded(char('\\'), anychar)(is.clone())?;
    diag_report!((&is..&i), ERR_INVALID_ESCAPE, c);
    Ok((i, None))
}

fn str_char<'a>(i: Span<'a>) -> IResult<'a, Option<char>> {
    alt((
        map(tag(r#"\\"#), |_| Some('\\')),
        map(tag(r#"\/"#), |_| Some('/')),
        map(tag(r#"\""#), |_| Some('"')),
        map(tag(r#"\n"#), |_| Some('\n')),
        map(tag(r#"\t"#), |_| Some('\t')),
        map(tag(r#"\r"#), |_| Some('\r')),
        map(tag(r#"\0"#), |_| Some('\0')),
        str_char_uni,
        map(none_of("\\"), |c| Some(c)),
        str_char_invalid,
    ))(i)
}

fn str_chars<'a>(mut i: Span<'a>) -> IResult<'a, Str> {
    let mut s = String::default();
    while let Ok((i_remaining, c)) = str_char(i.clone()) {
        if let Some(c) = c {
            s.push(c);
        }
        i = i_remaining;
    }
    Ok((i, Str::from_ref(s)))
}

// a parser accepting a function and returning the result of the function, by consuming the input
fn string<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Option<char>, Str)> {
    let (i, (o, (p, s))) = consumed(pair(
        opt(satisfy(|c: char| c.is_alpha())),
        delimited(tag("\""), str_chars, tag("\"")),
    ))(i)?;
    Ok((i, (o, p, s)))
}

fn string_inter_start<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Option<char>, Str)> {
    let (i, (o, (p, s))) = consumed(pair(
        opt(satisfy(|c: char| c.is_alpha())),
        delimited(tag("\""), str_chars, tag(r#"\("#)),
    ))(i)?;
    Ok((i, (o, p, s)))
}

fn string_inter_end<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Str)> {
    consumed(delimited(tag(r#")"#), str_chars, tag("\"")))(i)
}

fn string_inter_part<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Str)> {
    consumed(delimited(tag(r#"\("#), str_chars, tag(r#")"#)))(i)
}

// -----------------------------------------------------------------------------
// Operator
// -----------------------------------------------------------------------------
// one of `+-*/!=<>&|~`

fn operator<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Op)> {
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
// one of `()[]{};,.`

fn control<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Ctrl)> {
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
    ))(i)
}

// -----------------------------------------------------------------------------
// Identifier
// -----------------------------------------------------------------------------
// An identifier is a sequence of letters, numbers, and underscores, starting with a letter or underscore

fn identifier<'a>(i: Span<'a>) -> IResult<'a, Span<'a>> {
    recognize(tuple((alpha1, take_while(|c: char| c.is_alphanumeric() || c == '_'))))(i)
}

// -----------------------------------------------------------------------------
// Keyword
// -----------------------------------------------------------------------------
// A reserved langauge keyword
// one of module, class, struct, enum, func, let, new, if, else, switch, case, break, while, for, in, continue, return, try, catch, finally

fn keyword<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, Kw)> {
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

fn null<'a>(i: Span<'a>) -> IResult<'a, Span<'a>> {
    tag("null")(i)
}

fn boolean<'a>(i: Span<'a>) -> IResult<'a, (Span<'a>, bool)> {
    alt((map(tag("true"), |s| (s, true)), map(tag("false"), |s| (s, false))))(i)
}

pub fn token<'a>(i: Span<'a>) -> IResult<'a, Token> {
    alt((
        map(trivia, |(s, t)| Token::Trivia(s, t)),
        map(number, |(s, n)| Token::Num(s, n)),
        map(string, |(s, t, n)| Token::Str(s, t, n)),
        map(string_inter_start, |(s, t, n)| Token::StrIs(s, t, n)),
        map(string_inter_end, |(s, n)| Token::StrIe(s, n)),
        map(string_inter_part, |(s, n)| Token::StrIp(s, n)),
        map(null, |s| Token::Null(s)),
        map(boolean, |(s, b)| Token::Bool(s, b)),
        map(operator, |(s, o)| Token::Op(s, o)),
        map(control, |(s, c)| Token::Ctrl(s, c)),
        map(identifier, |s| Token::Ident(s)),
        map(keyword, |(s, k)| Token::Kw(s, k)),
    ))(i)
}

pub fn tokens<'a>(i: Span<'a>) -> IResult<'a, Vec<Token>> {
    many0(token)(i)
}

pub fn parse_file<'a>(
    input: &'a str,
    file: Str,
    diag: &'a RefCell<Vec<Diagnostic>>,
) -> Result<Vec<Token<'a>>, NomError<'a>> {
    let input = Span::new_extra(input, State(diag, file));
    let (_, tokens) = tokens(input).unwrap();
    Ok(tokens)
}

pub fn parse<'a>(input: &'a str, diag: &'a RefCell<Vec<Diagnostic>>) -> Result<Vec<Token<'a>>, NomError<'a>> {
    parse_file(input, Default::default(), diag)
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn parse_ternary_op() {
        let diag = RefCell::new(Vec::new());
        let expr = parse("3.0 ? 5.0 : 5 + 4", &diag).unwrap();
        let text = format!("{:?}", expr);
        println!("{}", text);
    }
}
