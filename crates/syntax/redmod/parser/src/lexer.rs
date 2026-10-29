use std::borrow::Cow;
use std::{fmt, ops};

use chumsky::input::InputRef;
use chumsky::prelude::*;

use crate::Span;

type LexSpan = SimpleSpan;
type LexExtra<'src> = extra::Err<Rich<'src, char, LexSpan>>;

pub fn lex<'src>(
    keep_lf_and_comments: bool,
) -> impl Parser<'src, &'src str, Vec<(Token<'src, LexSpan>, LexSpan)>, LexExtra<'src>> + Clone {
    let num = text::int(10)
        .then_ignore(
            just('.')
                .then_ignore(just('.').not())
                .then(text::digits(10).or_not())
                .or_not(),
        )
        .ignored()
        .or(just('.').ignore_then(text::digits(10)).ignored())
        .then_ignore(
            one_of("eE")
                .ignore_then(one_of("+-").or_not())
                .ignore_then(text::digits(10))
                .or_not(),
        )
        .to_slice()
        .then(text::ascii::ident().or_not())
        .validate(|value, e, emitter| {
            if matches!(value.1, Some("u" | "ul")) {
                emitter.emit(Rich::custom(
                    e.span(),
                    "unsigned literal suffixes are not supported in REDmod",
                ));
            }
            value
        })
        .validate(|(str, suffix): (&str, _), e, emitter| {
            let span = e.span();
            let error = |ty, hint| {
                let hint = if matches!(suffix, Some("u" | "l" | "ul")) && str.contains(['.', 'e', 'E']) {
                    "integer literals cannot have a decimal point or exponent; remove the integer suffix and use a decimal point or `d` suffix for floating-point values"
                } else { hint };
                Rich::custom(span, format!("invalid {ty} literal; {hint}"))
            };
            let result = match suffix {
                Some("f") => str.parse().map(Token::Float).map_err(|_| error("Float", "use a finite Float value")),
                Some(suffix) if !matches!(suffix, "ul" | "u" | "l" | "d") => Err(error("numeric", "unsupported numeric suffix")),
                Some("ul") => str.parse().map(Token::Ulong).map_err(|_| error("Uint64", "use a value between 0 and 18446744073709551615")),
                Some("u") => str.parse().map(Token::Uint).map_err(|_| error("Uint32", "use the `ul` suffix for a larger unsigned integer")),
                Some("l") if str == "9223372036854775808" => Ok(Token::LongMinMagnitude),
                Some("l") => str.parse().map(Token::Long).map_err(|_| error("Int64", "use the `ul` suffix for a larger nonnegative integer")),
                Some("d") => str.parse().map(Token::Double).map_err(|_| error("Double", "use a decimal number with the `d` suffix")),
                _ if str.contains(['.', 'e', 'E']) => str.parse().map(Token::Float).map_err(|_| error("Float", "use the `d` suffix for a Double literal")),
                _ if str == "2147483648" => Ok(Token::IntMinMagnitude),
                _ => str.parse().map(Token::Int).map_err(|_| error("Int32", "use the `l` suffix for Int64 or `ul` for Uint64; use a decimal point or `d` suffix for floating-point values")),
            };
            let result = result.and_then(|token| {
                if matches!(token, Token::Float(n) if !n.is_finite())
                    || matches!(token, Token::Double(n) if !n.is_finite())
                {
                    Err(error("floating-point", "use a finite value within the literal type's range"))
                } else {
                    Ok(token)
                }
            });
            result.unwrap_or_else(|err| {
                emitter.emit(err);
                Token::Int(0)
            })
        });

    // Consume unsupported prefixes as literals too, so they cannot silently
    // become an identifier followed by an ordinary string.
    let str = one_of("nRTsrt")
        .or_not()
        .then(str_contents('"').delimited_by(just('"'), string_end('"')))
        .validate(|(prefix, parts), e, emitter| {
            let message = match prefix {
                Some('s') => Some("interpolated strings are not supported in REDmod"),
                Some('r' | 't') => Some("REDmod requires uppercase R/T literal prefixes"),
                _ => None,
            };
            if let Some(message) = message {
                emitter.emit(Rich::custom(e.span(), message));
            }
            match prefix {
                Some('n') => Token::CName(parts),
                Some('R' | 'r') => Token::ResRef(parts),
                Some('T' | 't') => Token::TdbId(parts),
                _ => Token::Str(parts),
            }
        })
        .or(str_contents('\'')
            .delimited_by(just('\''), string_end('\''))
            .map(Token::CName));

    let word = text::ascii::ident().map(|str| match str {
        "true" => Token::True,
        "false" => Token::False,
        "NULL" => Token::Null,
        "this" => Token::This,
        "super" => Token::Super,
        "case" => Token::Case,
        "default" => Token::Default,
        other => Token::Ident(other),
    });

    let line_comment = just("//")
        .ignored()
        .then_ignore(any().and_is(text::newline().not()).repeated())
        .to_slice()
        .map(|str: &str| {
            if str.starts_with("///") && !str.starts_with("////") {
                Token::DocComment(str)
            } else {
                Token::LineComment(str)
            }
        });

    let block_comment = recursive(|comment| {
        just("/*")
            .ignored()
            .then_ignore(
                comment
                    .padded()
                    .or(any().and_is(just("*/").not()).ignored())
                    .repeated(),
            )
            .then_ignore(just("*/").or(end().validate(|(), e, emitter| {
                emitter.emit(Rich::custom(
                    e.span(),
                    "unterminated block comment; add `*/`",
                ));
                "*/"
            })))
    })
    .to_slice()
    .map(Token::BlockComment);

    let comment = line_comment.or(block_comment);

    let lf = text::newline().to(Token::LineFeed);
    let ws = text::inline_whitespace().at_least(1).to(Token::Whitespace);

    choice((lf, ws, comment, str, num, word, symbol()))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .map_with(|tok, e| (tok, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .map(move |mut toks| {
            if !keep_lf_and_comments {
                toks.retain(|(tok, _)| !tok.is_ws_or_comment());
            }
            toks
        })
}

fn string_end<'src>(delimiter: char) -> impl Parser<'src, &'src str, char, LexExtra<'src>> + Clone {
    just(delimiter).or(end().validate(move |(), e, emitter| {
        emitter.emit(Rich::custom(
            e.span(),
            format!("unterminated string literal; add a closing `{delimiter}`"),
        ));
        delimiter
    }))
}

fn str_contents<'src>(
    delimiter: char,
) -> impl Parser<'src, &'src str, Cow<'src, str>, LexExtra<'src>> + Clone {
    // REDmod delimiters terminate literals even when preceded by a backslash.
    any()
        .filter(move |c| *c != delimiter)
        .repeated()
        .to_slice()
        .map(Cow::Borrowed)
}

fn symbol<'src>() -> impl Parser<'src, &'src str, Token<'src, LexSpan>, LexExtra<'src>> + Clone {
    custom(|inp| {
        let before = inp.save();
        let pos = inp.offset();
        let skipped = |inp: &mut InputRef<'src, '_, _, _>, tok| {
            inp.skip();
            tok
        };
        let res = match inp.next() {
            Some('+') if inp.peek() == Some('=') => skipped(inp, Token::AssignAdd),
            Some('+') => Token::Plus,
            Some('-') if inp.peek() == Some('=') => skipped(inp, Token::AssignSub),
            Some('-') if inp.peek() == Some('>') => skipped(inp, Token::Arrow),
            Some('-') => Token::Minus,
            Some('*') if inp.peek() == Some('=') => skipped(inp, Token::AssignMul),
            Some('*') => Token::Star,
            Some('/') if inp.peek() == Some('=') => skipped(inp, Token::AssignDiv),
            Some('/') => Token::Slash,
            Some('%') => Token::Percent,
            Some('<') if inp.peek() == Some('=') => skipped(inp, Token::Le),
            Some('<') => Token::LAngle,
            Some('>') if inp.peek() == Some('=') => skipped(inp, Token::Ge),
            Some('>') => Token::RAngle,
            Some('!') if inp.peek() == Some('=') => skipped(inp, Token::Ne),
            Some('!') => Token::Not,
            Some('~') => Token::BitNot,
            Some('.') => Token::Period,
            Some(',') => Token::Comma,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some('[') => Token::LBracket,
            Some(']') => Token::RBracket,
            Some('&') if inp.peek() == Some('&') => skipped(inp, Token::And),
            Some('&') if inp.peek() == Some('=') => skipped(inp, Token::AssignBitAnd),
            Some('&') => Token::BitAnd,
            Some('|') if inp.peek() == Some('|') => skipped(inp, Token::Or),
            Some('|') if inp.peek() == Some('=') => skipped(inp, Token::AssignBitOr),
            Some('|') => Token::BitOr,
            Some('^') => Token::BitXor,
            Some(':') => Token::Colon,
            Some(';') => Token::Semicolon,
            Some('=') if inp.peek() == Some('=') => skipped(inp, Token::Eq),
            Some('=') => Token::Assign,
            Some('?') => Token::Question,
            Some('@') => Token::At,
            found => {
                let span = inp.span(pos..inp.offset());
                inp.rewind(before);
                return Err(Rich::custom(
                    span,
                    match found {
                        Some(c) => format!("invalid symbol `{}`", c.escape_debug()),
                        None => "unexpected end of input; expected a token".into(),
                    },
                ));
            }
        };
        Ok(res)
    })
    .or(any().validate(|c: char, e, emitter| {
        emitter.emit(Rich::custom(
            e.span(),
            format!("invalid symbol `{}`", c.escape_debug()),
        ));
        Token::Whitespace
    }))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src, S = Span> {
    Group(Box<[(Self, S)]>),
    Ident(&'src str),
    Int(i32),
    Uint(u32),
    Ulong(u64),
    Long(i64),
    // These magnitudes are only valid immediately after unary minus.
    IntMinMagnitude,
    LongMinMagnitude,
    Float(f32),
    Double(f64),
    Str(Cow<'src, str>),
    CName(Cow<'src, str>),
    ResRef(Cow<'src, str>),
    TdbId(Cow<'src, str>),
    StrFrag(Cow<'src, str>),
    InterpStr(Box<[(Self, S)]>),

    LineComment(&'src str),
    DocComment(&'src str),
    BlockComment(&'src str),
    Whitespace,
    LineFeed,

    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignBitOr,
    AssignBitAnd,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    Ne,
    Le,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Not,
    BitNot,

    Period,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LAngle,
    RAngle,
    Arrow,
    Colon,
    Semicolon,
    Assign,
    Question,
    At,

    True,
    False,
    Null,
    This,
    Super,
    Case,
    Default,
}

impl<'src, S1> Token<'src, S1> {
    pub fn is_ws_or_comment(&self) -> bool {
        matches!(
            self,
            Self::LineComment(_) | Self::BlockComment(_) | Self::LineFeed | Self::Whitespace
        )
    }

    pub fn map_span<S2>(self, base: S2, f: impl Fn(S1) -> S2 + Clone) -> Token<'src, S2>
    where
        S2: Copy + ops::Add<u8, Output = S2> + ops::Add<S2, Output = S2>,
    {
        match self {
            Self::Group(s) => Token::Group(
                s.into_vec()
                    .into_iter()
                    .map(|(tok, nested)| {
                        let span = f(nested);
                        (tok.map_span(span, f.clone()), base + 1u8 + span)
                    })
                    .collect(),
            ),
            Self::Ident(s) => Token::Ident(s),
            Self::Int(n) => Token::Int(n),
            Self::Uint(n) => Token::Uint(n),
            Self::Ulong(n) => Token::Ulong(n),
            Self::Long(n) => Token::Long(n),
            Self::IntMinMagnitude => Token::IntMinMagnitude,
            Self::LongMinMagnitude => Token::LongMinMagnitude,
            Self::Float(n) => Token::Float(n),
            Self::Double(n) => Token::Double(n),
            Self::Str(s) => Token::Str(s),
            Self::CName(s) => Token::CName(s),
            Self::ResRef(s) => Token::ResRef(s),
            Self::TdbId(s) => Token::TdbId(s),
            Self::StrFrag(s) => Token::StrFrag(s),
            Self::InterpStr(s) => Token::InterpStr(
                s.into_vec()
                    .into_iter()
                    .map(|(tok, nested)| {
                        let span = f(nested);
                        (tok.map_span(span, f.clone()), base + span)
                    })
                    .collect(),
            ),
            Self::LineComment(s) => Token::LineComment(s),
            Self::DocComment(s) => Token::DocComment(s),
            Self::BlockComment(s) => Token::BlockComment(s),
            Self::Whitespace => Token::Whitespace,
            Self::LineFeed => Token::LineFeed,
            Self::AssignAdd => Token::AssignAdd,
            Self::AssignSub => Token::AssignSub,
            Self::AssignMul => Token::AssignMul,
            Self::AssignDiv => Token::AssignDiv,
            Self::AssignBitOr => Token::AssignBitOr,
            Self::AssignBitAnd => Token::AssignBitAnd,
            Self::Plus => Token::Plus,
            Self::Minus => Token::Minus,
            Self::Star => Token::Star,
            Self::Slash => Token::Slash,
            Self::Percent => Token::Percent,
            Self::Eq => Token::Eq,
            Self::Ne => Token::Ne,
            Self::Le => Token::Le,
            Self::Ge => Token::Ge,
            Self::And => Token::And,
            Self::Or => Token::Or,
            Self::BitAnd => Token::BitAnd,
            Self::BitOr => Token::BitOr,
            Self::BitXor => Token::BitXor,
            Self::Not => Token::Not,
            Self::BitNot => Token::BitNot,
            Self::Period => Token::Period,
            Self::Comma => Token::Comma,
            Self::LParen => Token::LParen,
            Self::RParen => Token::RParen,
            Self::LBrace => Token::LBrace,
            Self::RBrace => Token::RBrace,
            Self::LBracket => Token::LBracket,
            Self::RBracket => Token::RBracket,
            Self::LAngle => Token::LAngle,
            Self::RAngle => Token::RAngle,
            Self::Arrow => Token::Arrow,
            Self::Colon => Token::Colon,
            Self::Semicolon => Token::Semicolon,
            Self::Assign => Token::Assign,
            Self::Question => Token::Question,
            Self::At => Token::At,
            Self::True => Token::True,
            Self::False => Token::False,
            Self::Null => Token::Null,
            Self::This => Token::This,
            Self::Super => Token::Super,
            Self::Case => Token::Case,
            Self::Default => Token::Default,
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(s) | Self::LineComment(s) | Self::DocComment(s) | Self::BlockComment(s) => {
                write!(f, "{s}")
            }
            Self::Int(n) => write!(f, "{n}"),
            Self::Uint(n) => write!(f, "{n}u"),
            Self::Ulong(n) => write!(f, "{n}ul"),
            Self::Long(n) => write!(f, "{n}l"),
            Self::IntMinMagnitude => write!(f, "2147483648"),
            Self::LongMinMagnitude => write!(f, "9223372036854775808l"),
            Self::Float(n) => write!(f, "{n}f"),
            Self::Double(n) => write!(f, "{n}d"),
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::CName(s) => write!(f, "n\"{s}\""),
            Self::ResRef(s) => write!(f, "R\"{s}\""),
            Self::TdbId(s) => write!(f, "T\"{s}\""),
            Self::InterpStr(s) => {
                write!(f, "s\"")?;
                for (tok, _) in s {
                    write!(f, "{tok}")?;
                }
                write!(f, "\"")
            }
            Self::Whitespace => write!(f, " "),
            Self::LineFeed => writeln!(f),
            Self::Group(s) => {
                for (tok, _) in s {
                    write!(f, "\\({tok})")?;
                }
                Ok(())
            }
            Self::StrFrag(s) => write!(f, "{s}"),
            Self::AssignAdd => write!(f, "+="),
            Self::AssignSub => write!(f, "-="),
            Self::AssignMul => write!(f, "*="),
            Self::AssignDiv => write!(f, "/="),
            Self::AssignBitOr => write!(f, "|="),
            Self::AssignBitAnd => write!(f, "&="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Le => write!(f, "<="),
            Self::Ge => write!(f, ">="),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::Not => write!(f, "!"),
            Self::BitNot => write!(f, "~"),
            Self::Period => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::LAngle => write!(f, "<"),
            Self::RAngle => write!(f, ">"),
            Self::Arrow => write!(f, "->"),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),
            Self::Assign => write!(f, "="),
            Self::Question => write!(f, "?"),
            Self::At => write!(f, "@"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "NULL"),
            Self::This => write!(f, "this"),
            Self::Super => write!(f, "super"),
            Self::Case => write!(f, "case"),
            Self::Default => write!(f, "default"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokens(src: &str) -> Vec<(Token<'_, LexSpan>, LexSpan)> {
        let (tokens, errors) = lex(false).parse(src).into_output_errors();
        assert!(errors.is_empty(), "{errors:?}");
        tokens.unwrap()
    }

    #[test]
    fn redmod_literals() {
        let result =
            tokens(r#"NULL T"Items.Foo" R"base\\path" n"Name" 'Name with "quotes"' "text" ''"#);
        let result: Vec<_> = result.into_iter().map(|(token, _)| token).collect();
        assert_eq!(
            result,
            vec![
                Token::Null,
                Token::TdbId("Items.Foo".into()),
                Token::ResRef(r"base\\path".into()),
                Token::CName("Name".into()),
                Token::CName("Name with \"quotes\"".into()),
                Token::Str("text".into()),
                Token::CName("".into()),
            ]
        );
    }

    #[test]
    fn raw_literal_values_are_borrowed() {
        for (src, expected) in [
            (r#""assets\widgets\button""#, r"assets\widgets\button"),
            (r#""assets\""#, r"assets\"),
            (
                r#""\n\r\t\\\u{22}\u{D800}\x22""#,
                r"\n\r\t\\\u{22}\u{D800}\x22",
            ),
            ("\"first\n  second\"", "first\n  second"),
            ("\"\"", ""),
        ] {
            let result = tokens(src);
            let Token::Str(Cow::Borrowed(actual)) = &result[0].0 else {
                panic!("{:?}", result[0]);
            };
            assert_eq!(*actual, expected);
            assert_eq!(actual.as_ptr(), src[1..].as_ptr());
            assert_eq!(result[0].1, LexSpan::from(0..src.len()));
        }
        for (src, expected) in [
            (r#"'raw\name\'"#, r"raw\name\"),
            (r#"n"raw\name\""#, r"raw\name\"),
        ] {
            assert_eq!(tokens(src)[0].0, Token::CName(Cow::Borrowed(expected)));
        }
        assert_eq!(
            tokens(r#"R"raw\path""#)[0].0,
            Token::ResRef(Cow::Borrowed(r"raw\path"))
        );
        assert_eq!(
            tokens(r#"T"raw\id""#)[0].0,
            Token::TdbId(Cow::Borrowed(r"raw\id"))
        );
    }

    #[test]
    fn escaped_delimiters_do_not_escape_quotes() {
        for src in [r#""say\"hello""#, r#"'say\'hello'"#, r#"n"say\"hello""#] {
            let (_, errors) = lex(false).parse(src).into_output_errors();
            assert!(!errors.is_empty(), "accepted {src:?}");
        }
    }

    #[test]
    fn diagnoses_community_literals() {
        for (src, message) in [
            ("1u", "unsigned literal suffixes"),
            ("1ul", "unsigned literal suffixes"),
            (r#"t"é""#, "uppercase R/T"),
            (r#"r"é""#, "uppercase R/T"),
            (r#"s"é""#, "interpolated strings"),
            (r#"s"value: \(x)""#, "interpolated strings"),
        ] {
            let (_, errors) = lex(false).parse(src).into_output_errors();
            assert!(
                errors.iter().any(|error| {
                    error.to_string().contains(message)
                        && *error.span() == LexSpan::from(0..src.len())
                }),
                "{src:?}: {errors:?}"
            );
        }
    }

    #[test]
    fn preserves_identifier_keywords() {
        let result = tokens("function event var optional func cb let null NULL NULLish");
        let expected = [
            "function", "event", "var", "optional", "func", "cb", "let", "null",
        ];
        for ((token, _), word) in result.iter().zip(expected) {
            assert_eq!(*token, Token::Ident(word));
        }
        assert_eq!(result[8].0, Token::Null);
        assert_eq!(result[9].0, Token::Ident("NULLish"));
    }

    #[test]
    fn spans_are_original_byte_offsets() {
        let result = tokens("'é\"' T\"猫\" \"\\u{1F600}\" NULL");
        assert_eq!(
            result,
            vec![
                (Token::CName("é\"".into()), (0..5).into()),
                (Token::TdbId("猫".into()), (6..12).into()),
                (Token::Str(r"\u{1F600}".into()), (13..24).into()),
                (Token::Null, (25..29).into()),
            ]
        );
    }

    #[test]
    fn comments_and_whitespace_are_not_rewritten() {
        let src = "// NULL t\"é\" \\\"\n/// function '猫'\n/* r\"x\" /* NULL */ s\"x\" */\nNULL";
        let (result, errors) = lex(true).parse(src).into_output_errors();
        assert!(errors.is_empty(), "{errors:?}");
        let result = result.unwrap();
        assert_eq!(result.len(), 7);
        assert!(matches!(result[0].0, Token::LineComment(_)));
        assert!(matches!(result[2].0, Token::DocComment(_)));
        assert!(matches!(result[4].0, Token::BlockComment(_)));
        for (token, span) in &result[..6] {
            let original = &src[span.start..span.end];
            match token {
                Token::LineComment(text) | Token::DocComment(text) | Token::BlockComment(text) => {
                    assert_eq!(*text, original);
                }
                Token::LineFeed => assert_eq!(original, "\n"),
                _ => panic!("unexpected token: {token:?}"),
            }
        }
        assert_eq!(tokens(src).len(), 2);
        let (result, errors) = lex(true).parse(" \tNULL").into_output_errors();
        assert!(errors.is_empty());
        assert_eq!(result.unwrap()[0], (Token::Whitespace, (0..2).into()));
    }

    #[test]
    fn exponent_only_float_literals() {
        let result = tokens("1e3 2E-2 3e+2 4e2d");
        let result: Vec<_> = result.into_iter().map(|(token, _)| token).collect();
        assert_eq!(
            result,
            vec![
                Token::Float(1000.0),
                Token::Float(0.02),
                Token::Float(300.0),
                Token::Double(400.0)
            ]
        );
    }

    #[test]
    fn float_spellings_and_invalid_numbers() {
        assert_eq!(Token::Float(1.0).to_string(), "1f");
        let result: Vec<_> = tokens(".125f 1f 1.f .25 1e2f")
            .into_iter()
            .map(|(token, _)| token)
            .collect();
        assert_eq!(
            result,
            vec![
                Token::Float(0.125),
                Token::Float(1.0),
                Token::Float(1.0),
                Token::Float(0.25),
                Token::Float(100.0)
            ]
        );
        let result = tokens("1..2");
        assert_eq!(result[0], (Token::Int(1), (0..1).into()));
        assert_eq!(result[1], (Token::Period, (1..2).into()));
        for src in [
            "2147483649",
            "9223372036854775809l",
            "1e100f",
            "1e999d",
            "1.2l",
            ".5ul",
            "1ff",
            "1fd",
            "1z",
            "1e+",
        ] {
            assert!(!lex(false).parse(src).into_errors().is_empty(), "{src}");
        }
    }

    #[test]
    fn exponents_allow_leading_zeroes() {
        for (source, expected) in [
            ("1.0e01", Token::Float(10.0)),
            (".5E-01f", Token::Float(0.05)),
            ("1e00", Token::Float(1.0)),
            ("1.0E+001d", Token::Double(10.0)),
        ] {
            for keep_trivia in [false, true] {
                let (result, errors) = lex(keep_trivia).parse(source).into_output_errors();
                assert!(errors.is_empty(), "{source}: {errors:?}");
                assert_eq!(
                    result.unwrap(),
                    vec![(expected.clone(), (0..source.len()).into())]
                );
            }
        }
    }

    #[test]
    fn line_comments_stop_at_lexer_newlines() {
        for newline in [
            "\n", "\r\n", "\r", "\u{b}", "\u{c}", "\u{85}", "\u{2028}", "\u{2029}",
        ] {
            for comment in ["// comment", "/// docs"] {
                let source = format!("{comment}{newline}NULL");
                let (result, errors) = lex(true).parse(&source).into_output_errors();
                assert!(errors.is_empty(), "{source:?}: {errors:?}");
                let result = result.unwrap();
                assert_eq!(result.len(), 3, "{source:?}: {result:?}");
                assert_eq!(result[0].1, (0..comment.len()).into());
                assert!(
                    matches!(result[0].0, Token::LineComment(text) | Token::DocComment(text) if text == comment)
                );
                assert_eq!(
                    result[1],
                    (
                        Token::LineFeed,
                        (comment.len()..comment.len() + newline.len()).into()
                    )
                );
                assert_eq!(result[2].0, Token::Null);
            }
        }
    }

    #[test]
    fn signed_minimum_magnitudes_keep_minus_separate() {
        for (literal, expected) in [
            ("2147483648", Token::IntMinMagnitude),
            ("9223372036854775808l", Token::LongMinMagnitude),
        ] {
            let span = Span::new(0, 0, redscript_redmod_ast::FileId::from_i32(0));
            let mapped = expected.clone().map_span(span, |_| span);
            assert_eq!(mapped.to_string(), literal);
            assert_eq!(
                tokens(literal),
                vec![(expected.clone(), (0..literal.len()).into())]
            );
            let source = format!("-{literal}");
            assert_eq!(
                tokens(&source),
                vec![
                    (Token::Minus, (0..1).into()),
                    (expected, (1..source.len()).into()),
                ]
            );
        }
    }

    #[test]
    fn signed_and_floating_point_literals() {
        let result = tokens("1 2l 3.5 4d -5");
        let result: Vec<_> = result.into_iter().map(|(token, _)| token).collect();
        assert_eq!(
            result,
            vec![
                Token::Int(1),
                Token::Long(2),
                Token::Float(3.5),
                Token::Double(4.0),
                Token::Minus,
                Token::Int(5),
            ]
        );
    }
}
