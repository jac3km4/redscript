use std::borrow::Cow;
use std::{fmt, ops};

use chumsky::input::InputRef;
use chumsky::prelude::*;

use crate::Span;
use crate::shared::StrParts;

type LexSpan = SimpleSpan;
type LexExtra<'src> = extra::Err<Rich<'src, char, LexSpan>>;

pub fn lex<'src>(
    keep_lf_and_comments: bool,
) -> impl Parser<'src, &'src str, Vec<(Token<'src, LexSpan>, LexSpan)>, LexExtra<'src>> + Clone {
    let num = text::int(10)
        .then_ignore(just('.').then(text::digits(10).or_not()).or_not())
        .then_ignore(
            one_of("eE")
                .ignore_then(one_of("+-").or_not())
                .ignore_then(text::digits(10))
                .or_not(),
        )
        .to_slice()
        .then(choice([just("ul"), just("u"), just("l"), just("d")]).or_not())
        .validate(|(str, suffix): (&str, _), e, emitter| {
            let span = e.span();
            let error = |ty, hint| {
                let hint = if matches!(suffix, Some("u" | "l" | "ul"))
                    && str.contains(['.', 'e', 'E'])
                {
                    "integer literals cannot have a decimal point or exponent; remove the integer suffix and use a decimal point or `d` suffix for floating-point values"
                } else {
                    hint
                };
                Rich::custom(span, format!("invalid {ty} literal; {hint}"))
            };
            let result = match suffix {
                Some("ul") => str.parse().map(Token::Ulong).map_err(|_| {
                    error("Uint64", "use a value between 0 and 18446744073709551615")
                }),
                Some("u") => str.parse().map(Token::Uint).map_err(|_| {
                    error("Uint32", "use the `ul` suffix for a larger unsigned integer")
                }),
                Some("l") if str == "9223372036854775808" => Ok(Token::LongMinMagnitude),
                Some("l") => str.parse().map(Token::Long).map_err(|_| {
                    error("Int64", "use the `ul` suffix for a larger nonnegative integer")
                }),
                Some("d") => str.parse().map(Token::Double).map_err(|_| {
                    error("Double", "use a decimal number with the `d` suffix")
                }),
                _ if str.contains('.') => str.parse().map(Token::Float).map_err(|_| {
                    error("Float", "use the `d` suffix for a Double literal")
                }),
                _ if str == "2147483648" => Ok(Token::IntMinMagnitude),
                _ => str.parse().map(Token::Int).map_err(|_| {
                    error("Int32", "use the `l` suffix for Int64 or `ul` for Uint64; use a decimal point or `d` suffix for floating-point values")
                }),
            };
            result.unwrap_or_else(|err| {
                emitter.emit(err);
                Token::Int(0)
            })
        });

    let str = one_of("nrt")
        .or_not()
        .then(
            str_elem(false)
                .clone()
                .repeated()
                .collect::<StrParts<'_>>()
                .delimited_by(just('"'), string_end()),
        )
        .map(|(prefix, parts)| match prefix {
            Some('n') => Token::CName(parts.str),
            Some('r') => Token::ResRef(parts.str),
            Some('t') => Token::TdbId(parts.str),
            _ => Token::Str(parts.str),
        });

    let word = text::ascii::ident().map(|str| match str {
        "true" => Token::True,
        "false" => Token::False,
        "null" => Token::Null,
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

    recursive(|this| {
        let lf = text::newline().to(Token::LineFeed);
        let ws = text::inline_whitespace().at_least(1).to(Token::Whitespace);

        choice((lf, ws, comment, str, interp_str(this), num, word, symbol()))
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
    })
}

fn string_end<'src>() -> impl Parser<'src, &'src str, char, LexExtra<'src>> + Clone {
    just('"').or(end().validate(|(), e, emitter| {
        emitter.emit(Rich::custom(
            e.span(),
            "unterminated string literal; add a closing `\"`",
        ));
        '"'
    }))
}

fn str_elem<'src>(
    interpolated: bool,
) -> impl Parser<'src, &'src str, Cow<'src, str>, LexExtra<'src>> + Clone {
    let unicode_escape = any()
        .filter(char::is_ascii_hexdigit)
        .repeated()
        .at_least(1)
        .at_most(6)
        .to_slice()
        .delimited_by(just('{'), just('}'))
        .validate(|str, e, emitter| {
            // The parser limits the value to six hex digits, so it always fits in u32.
            let n = u32::from_str_radix(str, 16).unwrap();
            let c = char::from_u32(n).unwrap_or_else(|| {
                emitter.emit(Rich::custom(
                    e.span(),
                    "invalid Unicode escape; use a Unicode scalar value (not a surrogate, at most 10FFFF)",
                ));
                char::REPLACEMENT_CHARACTER
            });
            Cow::Owned(c.into())
        })
        .or(none_of("\"\\}")
            .repeated()
            .then_ignore(just('}').or_not())
            .validate(|(), e, emitter| {
                emitter.emit(Rich::custom(
                    e.span(),
                    "invalid Unicode escape; use `\\u{HEX}` with 1 to 6 hex digits and a Unicode scalar value (not a surrogate, at most 10FFFF)",
                ));
                Cow::Owned(char::REPLACEMENT_CHARACTER.into())
            }));

    choice((
        none_of("\"\\")
            .repeated()
            .at_least(1)
            .to_slice()
            .map(Cow::Borrowed),
        just('\\').ignore_then(choice((
            just('n').to("\n".into()),
            just('r').to("\r".into()),
            just('t').to("\t".into()),
            just('\'').to("'".into()),
            just('"').to("\"".into()),
            just('\\').to("\\".into()),
            just('u').ignore_then(unicode_escape),
            any()
                .filter(move |c: &char| *c != 'u' && (!interpolated || *c != '('))
                .validate(|c, e, emitter| {
                    emitter.emit(Rich::custom(
                        e.span(),
                        format!(
                            "invalid escape `\\{}`; use \\n, \\r, \\t, \\', \\\", \\\\ or \\u{{HEX}}; escape a literal backslash as \\\\",
                            c.escape_default()
                        ),
                    ));
                    Cow::Owned(c.into())
                }),
            end().validate(|(), e, emitter| {
                emitter.emit(Rich::custom(
                    e.span(),
                    "incomplete escape; add an escape character such as `n`, or escape a literal backslash as `\\\\`",
                ));
                Cow::Borrowed("")
            }),
        ))),
    ))
}

fn interp_str<'src>(
    toks: impl Parser<'src, &'src str, Vec<(Token<'src, LexSpan>, LexSpan)>, LexExtra<'src>> + Clone,
) -> impl Parser<'src, &'src str, Token<'src, LexSpan>, LexExtra<'src>> + Clone {
    let balanced_parens = recursive(|p| {
        just('(')
            .ignore_then(p.or(any().and_is(just(')').not()).ignored()).repeated())
            .then_ignore(just(')'))
    })
    .to_slice()
    .validate(move |src, e, emitter| {
        let offset: LexSpan = e.span();
        let (tokens, errors) = toks.parse(src).into_output_errors();
        for error in errors {
            let span = error.span();
            emitter.emit(Rich::custom(
                LexSpan::from(offset.start + span.start..offset.start + span.end),
                error.reason().to_string(),
            ));
        }
        // Token spans stay slice-relative for Token::map_span; only diagnostics are rebased here.
        tokens.unwrap_or_default()
    });

    just('s').ignore_then(
        choice((
            str_elem(true)
                .repeated()
                .at_least(1)
                .collect::<StrParts<'_>>()
                .map(|ps| Token::StrFrag(ps.str)),
            just("\\")
                .ignore_then(balanced_parens)
                .map(|tok| Token::Group(tok.into())),
        ))
        .map_with(|tok, e| (tok, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just('"'), string_end())
        .map(|ps| Token::InterpStr(ps.into())),
    )
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
        // Treat the invalid character as trivia during recovery.
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
    // These magnitudes are only representable under unary minus, which the parser checks.
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
        self.map_span_with_offset(base, None, f)
    }

    fn map_span_with_offset<S2>(
        self,
        base: S2,
        offset: Option<S2>,
        f: impl Fn(S1) -> S2 + Clone,
    ) -> Token<'src, S2>
    where
        S2: Copy + ops::Add<u8, Output = S2> + ops::Add<S2, Output = S2>,
    {
        match self {
            Self::Group(s) => Token::Group(
                s.into_vec()
                    .into_iter()
                    .map(|(tok, nested)| {
                        // The interpolation lexer lexes the parenthesized slice after the backslash.
                        let offset = base + 1u8;
                        let span = offset + f(nested);
                        (
                            tok.map_span_with_offset(span, Some(offset), f.clone()),
                            span,
                        )
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
                        // String parts share their containing input's coordinates.
                        let span = f(nested);
                        let span = offset.map_or(span, |offset| offset + span);
                        (tok.map_span_with_offset(span, offset, f.clone()), span)
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
            Self::Float(n) => write!(f, "{n}"),
            Self::Double(n) => write!(f, "{n}d"),
            Self::Str(s) => write!(f, "\"{}\"", s.escape_default()),
            Self::CName(s) => write!(f, "n\"{}\"", s.escape_default()),
            Self::ResRef(s) => write!(f, "r\"{}\"", s.escape_default()),
            Self::TdbId(s) => write!(f, "t\"{}\"", s.escape_default()),
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
                write!(f, "\\(")?;
                let contents =
                    if let [(Self::LParen, _), contents @ .., (Self::RParen, _)] = s.as_ref() {
                        contents
                    } else {
                        s.as_ref()
                    };
                for (i, (tok, _)) in contents.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{tok}")?;
                }
                write!(f, ")")
            }
            Self::StrFrag(s) => write!(f, "{}", s.escape_default()),
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
            Self::Null => write!(f, "null"),
            Self::This => write!(f, "this"),
            Self::Super => write!(f, "super"),
            Self::Case => write!(f, "case"),
            Self::Default => write!(f, "default"),
        }
    }
}

#[cfg(test)]
mod tests {
    use redscript_community_ast::FileId;

    use super::*;

    #[test]
    fn valid_literals_escapes_and_comments_are_unchanged() {
        let src = r#"2147483647 4294967295u 9223372036854775807l 18446744073709551615ul
            1.5 2.d "\n\r\t\'\"\\\u{E9}\u{10FFFF}" n"name" r"res" t"id"
            /* outer /* nested */ comment */ // line
            s"value: \(foo + 1)""#;
        for keep_comments in [false, true] {
            let (tokens, errors) = lex(keep_comments).parse(src).into_output_errors();
            assert!(errors.is_empty(), "{errors:?}");
            let tokens = tokens.unwrap();
            assert!(
                tokens
                    .iter()
                    .any(|(tok, _)| matches!(tok, Token::Ulong(u64::MAX)))
            );
            assert!(
                tokens
                    .iter()
                    .any(|(tok, _)| matches!(tok, Token::Str(s) if s == "\n\r\t'\"\\é\u{10FFFF}"))
            );
            assert_eq!(
                tokens
                    .iter()
                    .any(|(tok, _)| matches!(tok, Token::BlockComment(_))),
                keep_comments
            );
        }
    }

    #[test]
    fn line_comments_stop_at_lexer_newlines() {
        let file = FileId::from_i32(7);
        for newline in [
            "\n", "\r\n", "\r", "\u{B}", "\u{C}", "\u{85}", "\u{2028}", "\u{2029}",
        ] {
            for comment in ["// comment", "/// doc", "//// ordinary"] {
                let src = format!("{comment}{newline}42");
                let end = comment.len() as u32;
                let next = end + newline.len() as u32;
                let expected = if comment == "/// doc" {
                    Token::DocComment(comment)
                } else {
                    Token::LineComment(comment)
                };
                let (tokens, errors) = crate::lex_with_lf_and_comments(&src, file);
                assert!(errors.is_empty(), "{src:?}: {errors:?}");
                assert_eq!(
                    tokens.unwrap(),
                    vec![
                        (expected, Span::new(0, end, file)),
                        (Token::LineFeed, Span::new(end, next, file)),
                        (Token::Int(42), Span::new(next, next + 2, file)),
                    ],
                    "{src:?}"
                );
            }
            let src = format!("// comment{newline}42");
            let start = src.len() as u32 - 2;
            let (tokens, errors) = crate::lex(&src, file);
            assert!(errors.is_empty(), "{src:?}: {errors:?}");
            assert_eq!(
                tokens.unwrap(),
                vec![(Token::Int(42), Span::new(start, start + 2, file))],
                "{src:?}"
            );
            let (expr, errors) = crate::parse_expr(&src, file);
            assert!(errors.is_empty(), "{src:?}: {errors:?}");
            assert!(expr.is_some(), "{src:?}");
        }
    }

    #[test]
    fn exponents_allow_leading_zeroes() {
        let file = FileId::from_i32(7);
        for (src, expected) in [
            ("1.0e01", Token::Float(10.0)),
            ("1.0e-01", Token::Float(0.1)),
            ("1.0e00d", Token::Double(1.0)),
            ("1.0E+001d", Token::Double(10.0)),
        ] {
            for lex in [crate::lex, crate::lex_with_lf_and_comments] {
                let (tokens, errors) = lex(src, file);
                assert!(errors.is_empty(), "{src}: {errors:?}");
                assert_eq!(
                    tokens.unwrap(),
                    vec![(expected.clone(), Span::new(0, src.len() as u32, file))],
                    "{src}"
                );
            }
            let (expr, errors) = crate::parse_expr(src, file);
            assert!(errors.is_empty(), "{src}: {errors:?}");
            assert!(expr.is_some(), "{src}");
        }
    }

    #[test]
    fn signed_minimum_magnitudes_keep_minus_separate() {
        let file = FileId::from_i32(7);
        for (literal, magnitude) in [
            ("2147483648", Token::IntMinMagnitude),
            ("9223372036854775808l", Token::LongMinMagnitude),
        ] {
            for prefix in ["-", "x-"] {
                let source = format!("{prefix}{literal}");
                let start = prefix.len() as u32;
                let end = source.len() as u32;
                for lex in [crate::lex, crate::lex_with_lf_and_comments] {
                    let (tokens, errors) = lex(&source, file);
                    assert!(errors.is_empty(), "{source}: {errors:?}");
                    let mut expected = vec![];
                    if prefix.starts_with('x') {
                        expected.push((Token::Ident("x"), Span::new(0, 1, file)));
                    }
                    expected.push((Token::Minus, Span::new(start - 1, start, file)));
                    expected.push((magnitude.clone(), Span::new(start, end, file)));
                    assert_eq!(tokens.unwrap(), expected, "{source}");
                }
                assert_eq!(magnitude.to_string(), literal);
            }
        }
    }

    #[test]
    fn invalid_numbers_name_the_literal_type_and_hint() {
        for (src, ty, hint) in [
            ("2147483649", "Int32", "`l`"),
            ("4294967296u", "Uint32", "`ul`"),
            ("9223372036854775809l", "Int64", "`ul`"),
            ("18446744073709551616ul", "Uint64", "18446744073709551615"),
            ("1.5u", "Uint32", "remove the integer suffix"),
            ("1e3", "Int32", "decimal point"),
        ] {
            let (_, errors) = lex(false).parse(src).into_output_errors();
            assert!(
                errors.iter().any(|err| {
                    let message = err.to_string();
                    message.contains(ty) && message.contains(hint)
                }),
                "{src}: {errors:?}"
            );
        }
    }

    #[test]
    fn invalid_symbols_have_the_actual_character_and_byte_span() {
        for c in ['$', 'é', '💥', '\0'] {
            let src = format!("ok {c} next");
            let (tokens, errors) = lex(false).parse(&src).into_output_errors();
            let err = errors
                .iter()
                .find(|err| err.to_string().contains("invalid symbol"))
                .expect("invalid symbol diagnostic");
            assert_eq!(*err.span(), LexSpan::from(3..3 + c.len_utf8()));
            assert!(err.to_string().contains(&c.escape_debug().to_string()));
            assert!(
                tokens
                    .unwrap()
                    .iter()
                    .any(|(tok, _)| *tok == Token::Ident("next"))
            );
        }
    }

    #[test]
    fn unterminated_literals_have_closing_delimiter_guidance() {
        for (src, message, delimiter) in [
            ("\"unfinished", "unterminated string", "closing"),
            ("n\"unfinished", "unterminated string", "closing"),
            ("s\"unfinished", "unterminated string", "closing"),
            ("/* unfinished", "unterminated block comment", "*/"),
            ("/* outer /* inner */", "unterminated block comment", "*/"),
        ] {
            let (_, errors) = lex(false).parse(src).into_output_errors();
            assert!(
                errors.iter().any(|err| {
                    let text = err.to_string();
                    text.contains(message) && text.contains(delimiter)
                }),
                "{src}: {errors:?}"
            );
        }
    }

    #[test]
    fn invalid_escapes_have_guidance() {
        for (src, message, hint) in [
            (r#""\q""#, "invalid escape", "backslash"),
            (r#"s"\q""#, "invalid escape", "backslash"),
            (r#""\u{}""#, "invalid Unicode escape", "hex digits"),
            (r#""\u{xyz}""#, "invalid Unicode escape", "hex digits"),
            (r#""\u{1234567}""#, "invalid Unicode escape", "hex digits"),
            (r#""\u1234""#, "invalid Unicode escape", "hex digits"),
            ("\"unfinished\\", "incomplete escape", "backslash"),
            (r#""\u{D800}""#, "invalid Unicode escape", "scalar"),
            (r#""\u{110000}""#, "invalid Unicode escape", "10FFFF"),
        ] {
            let (_, errors) = lex(false).parse(src).into_output_errors();
            assert!(
                errors.iter().any(|err| {
                    let text = err.to_string();
                    text.contains(message) && text.contains(hint)
                }),
                "{src}: {errors:?}"
            );
        }
    }

    #[test]
    fn string_token_display_escapes_contents() {
        let contents = "line\n\r\t\0\"\\é";
        for (token, prefix) in [
            (Token::Str(contents.into()), ""),
            (Token::CName(contents.into()), "n"),
            (Token::ResRef(contents.into()), "r"),
            (Token::TdbId(contents.into()), "t"),
        ] {
            assert_eq!(
                token.to_string(),
                format!("{prefix}\"{}\"", contents.escape_default())
            );
        }
        let fragment: Token<'_> = Token::StrFrag(contents.into());
        assert_eq!(fragment.to_string(), contents.escape_default().to_string());
    }

    #[test]
    fn interpolation_spans_use_the_containing_input_coordinates() {
        let src = r#"prefix s"text \(value)""#;
        let (tokens, errors) = lex(false).parse(src).into_output_errors();
        assert!(errors.is_empty(), "{errors:?}");
        let tokens = tokens.unwrap();
        let Token::InterpStr(parts) = &tokens[1].0 else {
            panic!("expected interpolated string");
        };
        assert_eq!(tokens[1].1, LexSpan::from(7..23));
        assert_eq!(parts[0], (Token::StrFrag("text ".into()), (9..14).into()));
        assert_eq!(parts[1].1, LexSpan::from(14..22));
        let Token::Group(group) = &parts[1].0 else {
            panic!("expected interpolation group");
        };
        // Only nested_in starts a new coordinate system, including its parentheses.
        assert_eq!(
            group.as_ref(),
            &[
                (Token::LParen, (0..1).into()),
                (Token::Ident("value"), (1..6).into()),
                (Token::RParen, (6..7).into()),
            ]
        );
    }

    #[test]
    fn nested_interpolation_spans_are_absolute() {
        use redscript_community_ast::{Expr, StrPart};

        let file = FileId::from_i32(7);
        let inner = r#"s"inner \(leaf) end""#;
        let middle = format!(r#"s"middle \({inner}) tail""#);
        let outer = format!(r#"s"outer \({middle}) after \(other) done""#);
        for prefix in ["", "/* é prefix */ "] {
            let src = format!("{prefix}{outer}");
            let span = |text: &str| {
                let start = src.find(text).unwrap() as u32;
                Span::new(start, start + text.len() as u32, file)
            };
            let group = |body: Token<'static, Span>, text: &str| {
                Token::Group(
                    vec![
                        (Token::LParen, {
                            let start = span(text).start;
                            Span::new(start - 1, start, file)
                        }),
                        (body, span(text)),
                        (Token::RParen, {
                            let end = span(text).end;
                            Span::new(end, end + 1, file)
                        }),
                    ]
                    .into(),
                )
            };
            let inner_token = Token::InterpStr(
                vec![
                    (Token::StrFrag("inner ".into()), span("inner ")),
                    (group(Token::Ident("leaf"), "leaf"), span(r"\(leaf)")),
                    (Token::StrFrag(" end".into()), span(" end")),
                ]
                .into(),
            );
            let middle_token = Token::InterpStr(
                vec![
                    (Token::StrFrag("middle ".into()), span("middle ")),
                    (group(inner_token, inner), span(&format!(r"\({inner})"))),
                    (Token::StrFrag(" tail".into()), span(" tail")),
                ]
                .into(),
            );
            let expected = Token::InterpStr(
                vec![
                    (Token::StrFrag("outer ".into()), span("outer ")),
                    (group(middle_token, &middle), span(&format!(r"\({middle})"))),
                    (Token::StrFrag(" after ".into()), span(" after ")),
                    (group(Token::Ident("other"), "other"), span(r"\(other)")),
                    (Token::StrFrag(" done".into()), span(" done")),
                ]
                .into(),
            );
            for lex in [crate::lex, crate::lex_with_lf_and_comments] {
                let (tokens, errors) = lex(&src, file);
                assert!(errors.is_empty(), "{src}: {errors:?}");
                let tokens = tokens.unwrap();
                assert_eq!(tokens.last().unwrap(), &(expected.clone(), span(&outer)));
            }

            let (expr, errors) = crate::parse_expr(&src, file);
            assert!(errors.is_empty(), "{src}: {errors:?}");
            let Expr::InterpolatedString(outer_parts) = expr.unwrap() else {
                panic!("expected outer string");
            };
            let StrPart::Expr((Expr::InterpolatedString(middle_parts), middle_span)) =
                &outer_parts[1]
            else {
                panic!("expected middle string");
            };
            assert_eq!(*middle_span, span(&middle));
            let StrPart::Expr((Expr::InterpolatedString(inner_parts), inner_span)) =
                &middle_parts[1]
            else {
                panic!("expected inner string");
            };
            assert_eq!(*inner_span, span(inner));
            assert_eq!(
                inner_parts[1],
                StrPart::Expr((Expr::Ident("leaf"), span("leaf")))
            );
            assert_eq!(
                outer_parts[3],
                StrPart::Expr((Expr::Ident("other"), span("other")))
            );
        }
    }

    #[test]
    fn interpolation_display_keeps_expression_in_one_group() {
        let src = r#"s"line\n\"\\ \(foo + "a\tb")""#;
        let (tokens, errors) = crate::lex(src, FileId::from_i32(0));
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(tokens.unwrap()[0].0.to_string(), src);
    }

    #[test]
    fn unexpected_string_tokens_are_safe_in_diagnostics() {
        for literal in [
            r#""a\n\r\t\u{0}\"\\b""#,
            r#"n"a\n\"b""#,
            r#"r"a\n\"b""#,
            r#"t"a\n\"b""#,
            r#"s"a\n\"\(foo + "b\tc")""#,
        ] {
            let src = format!("1 {literal}");
            let (_, errors) = crate::parse_expr(&src, FileId::from_i32(0));
            assert!(!errors.is_empty(), "{src}");
            let message = errors[0].to_string();
            assert!(message.contains("\\n"), "{message}");
            assert!(!message.chars().any(char::is_control), "{message:?}");
        }
    }
}
