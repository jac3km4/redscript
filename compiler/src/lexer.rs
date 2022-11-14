use std::cell::RefCell;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while, take_while_m_n};
use nom::character::complete::{
    alpha1, anychar, char, digit0, digit1, hex_digit0, line_ending, multispace1, none_of, oct_digit0, one_of, satisfy
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

#[derive(Debug, Clone, PartialEq, Display)]
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
    /// one of `()[]{}:;,.?` and ->
    Ctrl(Span<'a>, Ctrl),
    Ident(Span<'a>),
    /// Language keywords
    /// one of module, class, struct, enum, func, let, new, if, else, switch, case, break, while, for, in, continue, return, try, catch, finally
    Kw(Span<'a>, Kw),
}

#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum Trivia {
    Comment,
    Whitespace,
    LineEnd,
}

#[derive(Debug, Clone, Copy, PartialEq, Display, IntoStaticStr)]
pub enum Num {
    Float(f64),
    Int(u64),
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

fn parse_float(i: &Span) -> f64 {
    match i.fragment().parse() {
        Ok(value) => value,
        Err(error) => {
            diag_report!(i, ERR_PARSE_FLOAT, i.fragment(), error);
            0.0
        }
    }
}

fn parse_int(i: &Span, radix: u32) -> u64 {
    match u64::from_str_radix(i.fragment(), radix) {
        Ok(value) => value,
        Err(error) => {
            diag_report!(i, ERR_PARSE_INT, i.fragment(), error);
            0
        }
    }
}

pub fn number(i: Span) -> IResult<(Num, Span)> {
    alt((
        map(preceded(tag("0x"), hex_digit0), |s| (Num::Int(parse_int(&s, 16)), s)),
        map(preceded(tag("0o"), oct_digit0), |s| (Num::Int(parse_int(&s, 8)), s)),
        map(preceded(tag("0b"), take_while(|c: char| c == '0' || c == '1')), |s| {
            (Num::Int(parse_int(&s, 2)), s)
        }),
        map(sciexp_literal, |s| (Num::Float(parse_float(&s)), s)),
        map(float_literal, |s| (Num::Float(parse_float(&s)), s)),
        map(digit1, |s| (Num::Int(parse_int(&s, 10)), s)),
    ))(i)
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
            diag_report!((&is..&i), ERR_INVALID_UTF8, hex);
            Ok((i, None))
        }
    } else {
        diag_report!((&is..&i), ERR_INVALID_UTF8, digits.fragment());
        Ok((i, None))
    }
}

fn str_char_invalid(is: Span) -> IResult<Option<char>> {
    let (i, c) = preceded(char('\\'), anychar)(is.clone())?;
    diag_report!((&is..&i), ERR_INVALID_ESCAPE, c);
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

// a parser accepting a function and returning the result of the function, by consuming the input
fn string(i: Span) -> IResult<(Span, Option<char>, Str)> {
    let (i, (o, (p, s))) = consumed(pair(
        opt(satisfy(nom::AsChar::is_alpha)),
        delimited(tag("\""), str_chars, tag("\"")),
    ))(i)?;
    Ok((i, (o, p, s)))
}

fn string_inter_start(i: Span) -> IResult<(Span, Option<char>, Str)> {
    let (i, (o, (p, s))) = consumed(pair(
        opt(satisfy(nom::AsChar::is_alpha)),
        delimited(tag("\""), str_chars, tag(r#"\("#)),
    ))(i)?;
    Ok((i, (o, p, s)))
}

fn string_inter_end(i: Span) -> IResult<(Span, Str)> {
    consumed(delimited(tag(r#")"#), str_chars, tag("\"")))(i)
}

fn string_inter_part(i: Span) -> IResult<(Span, Str)> {
    consumed(delimited(tag(r#"\("#), str_chars, tag(r#")"#)))(i)
}

// -----------------------------------------------------------------------------
// Operator
// -----------------------------------------------------------------------------
// one of `+-*/!=<>&|~`

fn operator(i: Span) -> IResult<(Span, Op)> {
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

fn control(i: Span) -> IResult<(Span, Ctrl)> {
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

fn identifier(i: Span) -> IResult<Span> {
    recognize(tuple((alpha1, take_while(|c: char| c.is_alphanumeric() || c == '_'))))(i)
}

// -----------------------------------------------------------------------------
// Keyword
// -----------------------------------------------------------------------------
// A reserved langauge keyword
// one of module, class, struct, enum, func, let, new, if, else, switch, case, break, while, for, in, continue, return, try, catch, finally

fn keyword(i: Span) -> IResult<(Span, Kw)> {
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

fn null(i: Span) -> IResult<Span> {
    tag("null")(i)
}

fn boolean(i: Span) -> IResult<(Span, bool)> {
    alt((map(tag("true"), |s| (s, true)), map(tag("false"), |s| (s, false))))(i)
}

pub fn token(i: Span) -> IResult<Token> {
    alt((
        map(trivia, |(t, s)| Token::Trivia(s, t)),
        map(number, |(n, s)| Token::Num(s, n)),
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

pub fn tokens(i: Span) -> IResult<Vec<Token>> {
    many0(token)(i)
}

pub fn parse_file<'a>(input: &'a str, file: Str, diag: &'a RefCell<Vec<Diagnostic>>) -> Result<Vec<Token>, NomError> {
    let input = Span::new_extra(input, State(diag, file));
    let (_, tokens) = tokens(input).unwrap();
    Ok(tokens)
}

pub fn parse<'a>(input: &'a str, diag: &'a RefCell<Vec<Diagnostic>>) -> Result<Vec<Token>, NomError> {
    parse_file(input, Str::default(), diag)
}

#[cfg(test)]
#[allow(unused_imports, dead_code)]
mod test {
    use super::*;

    #[test]
    fn parse_ternary_op() {
        let diag = RefCell::new(vec![]);
        let tokens = parse("3.0 ? 5.0 : 5 + 4", &diag).unwrap();
        println!("{:?}", tokens);
    }

    fn parse<'a>(input: &'a str, diag: &'a RefCell<Vec<Diagnostic>>) -> Result<Vec<Token<'a>>, NomError<'a>> {
        let input = Span::new_extra(input, State(diag, Str::default()));
        let (_, tokens) = tokens(input).unwrap();
        let text = format!("{:?}", tokens);
        assert!(!text.is_empty());
        Ok(tokens)
    }
}
