use std::borrow::Cow;
use std::{fmt, mem, ops};

use chumsky::container::Container;
use chumsky::input::InputRef;
use chumsky::prelude::*;
use redscript_ast::Dialect;

use crate::{Branch, Span};

type LexSpan = SimpleSpan;
type LexExtra<'src> = extra::Err<Rich<'src, char, LexSpan>>;

/// Lexes the source. `compat` enables the 0.5.x compatibility mode of the community dialect.
pub fn lex<'src>(
    keep_lf_and_comments: bool,
    dialect: Dialect,
    compat: bool,
) -> impl Parser<'src, &'src str, Vec<(Token<'src, LexSpan>, LexSpan)>, LexExtra<'src>> + Clone {
    let num = text::int(10)
        .then_ignore(just('.').then(text::digits(10).or_not()).or_not())
        .then_ignore(
            one_of("eE")
                .ignore_then(one_of("+-").or_not())
                .ignore_then(text::int(10))
                .or_not(),
        )
        .to_slice()
        .then(choice([just("ul"), just("u"), just("l"), just("d")]).or_not())
        .try_map(|(str, suffix): (&str, _), span| match suffix {
            Some("ul") => Ok(Token::Ulong(
                str.parse().map_err(|e| Rich::custom(span, e))?,
            )),
            Some("u") => Ok(Token::Uint(str.parse().map_err(|e| Rich::custom(span, e))?)),
            Some("l") => Ok(Token::Long(str.parse().map_err(|e| Rich::custom(span, e))?)),
            Some("d") => Ok(Token::Double(
                str.parse().map_err(|e| Rich::custom(span, e))?,
            )),
            _ if str.contains('.') => Ok(Token::Float(
                str.parse().map_err(|e| Rich::custom(span, e))?,
            )),
            _ => Ok(Token::Int(str.parse().map_err(|e| Rich::custom(span, e))?)),
        });

    // 0.5.x compatibility: leading zeros (`007`), leading-dot floats (`.5`), and the suffixes
    // `u`, `l`, `d` in any combination of an optional `u` followed by an optional `l` or `d`
    // (`1ud` is a double, `1.5u` and `1.5l` are floats)
    let compat_num = text::digits(10)
        .then(just('.').then(text::digits(10).or_not()).or_not())
        .ignored()
        .or(just('.').then(text::digits(10)).ignored())
        .then_ignore(
            one_of("eE")
                .ignore_then(one_of("+-").or_not())
                .ignore_then(text::int(10))
                .or_not(),
        )
        .to_slice()
        .then(just('u').or_not())
        .then(one_of("ld").or_not())
        .try_map(|((str, unsigned), suffix): ((&str, _), _), span| {
            match (unsigned.is_some(), suffix) {
                (_, Some('d')) => Ok(Token::Double(
                    str.parse().map_err(|e| Rich::custom(span, e))?,
                )),
                _ if str.contains('.') => Ok(Token::Float(
                    str.parse().map_err(|e| Rich::custom(span, e))?,
                )),
                (true, Some(_)) => Ok(Token::Ulong(
                    str.parse().map_err(|e| Rich::custom(span, e))?,
                )),
                (true, None) => Ok(Token::Uint(str.parse().map_err(|e| Rich::custom(span, e))?)),
                (false, Some(_)) => {
                    Ok(Token::Long(str.parse().map_err(|e| Rich::custom(span, e))?))
                }
                (false, None) => Ok(Token::Int(str.parse().map_err(|e| Rich::custom(span, e))?)),
            }
        });

    // 0.5.x compatibility: the minimum signed literals (`-2147483648`, `-9223372036854775808l`)
    // are lexed as a single negative literal, since their magnitude does not fit the type;
    // any other `-` is left to the symbol lexer
    let signed_min = just('-')
        .then(text::digits(10))
        .to_slice()
        .then(just('l').or_not())
        .then_ignore(one_of(".uldeE0123456789").not())
        .try_map(|(str, long): (&str, _), span| {
            let magnitude = &str[1..];
            let not_min = || Rich::custom(span, "not a minimum signed literal");
            if long.is_some() {
                match (magnitude.parse::<i64>(), str.parse::<i64>()) {
                    (Err(_), Ok(n)) => Ok(Token::Long(n)),
                    _ => Err(not_min()),
                }
            } else {
                match (magnitude.parse::<i32>(), str.parse::<i32>()) {
                    (Err(_), Ok(n)) => Ok(Token::Int(n)),
                    _ => Err(not_min()),
                }
            }
        });

    let str = one_of("nrt")
        .or_not()
        .then(
            str_elem(Dialect::Community)
                .repeated()
                .collect::<StrParts<'_>>()
                .delimited_by(just('"'), just('"')),
        )
        .map(|(prefix, parts)| match prefix {
            Some('n') => Token::CName(parts.str),
            Some('r') => Token::ResRef(parts.str),
            Some('t') => Token::TdbId(parts.str),
            _ => Token::Str(parts.str),
        });

    // REDmod: `n"..."`, `T"..."`, `R"..."` (lowercase `t` and `r` accepted too)
    let redmod_str = one_of("nTRtr")
        .or_not()
        .then(
            str_elem(Dialect::Redmod)
                .repeated()
                .collect::<StrParts<'_>>()
                .delimited_by(just('"'), just('"')),
        )
        .map(|(prefix, parts)| match prefix {
            Some('n') => Token::CName(parts.str),
            Some('r' | 'R') => Token::ResRef(parts.str),
            Some('t' | 'T') => Token::TdbId(parts.str),
            _ => Token::Str(parts.str),
        });

    // REDmod: `'Name'` CName literal, no escapes
    let redmod_cname = none_of("'\r\n")
        .repeated()
        .to_slice()
        .delimited_by(just('\''), just('\''))
        .map(|str: &str| Token::CName(Cow::Borrowed(str)));

    let word = text::ascii::ident().map(move |str| match str {
        "true" => Token::True,
        "false" => Token::False,
        "null" => Token::Null,
        "NULL" if dialect == Dialect::Redmod => Token::Null,
        "this" => Token::This,
        "super" => Token::Super,
        "case" => Token::Case,
        "default" => Token::Default,
        other => Token::Ident(other),
    });

    let line_comment = just("//")
        .ignored()
        .then_ignore(any().and_is(just('\n').not()).repeated())
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
            .then_ignore(just("*/"))
    })
    .to_slice()
    .map(Token::BlockComment);

    let comment = line_comment.or(block_comment);

    recursive(|this| {
        let lf = text::newline().to(Token::LineFeed);
        let ws = text::inline_whitespace().at_least(1).to(Token::Whitespace);

        let tok = match dialect {
            Dialect::Community if compat => choice((
                lf,
                ws,
                comment,
                str,
                interp_str(this, compat),
                signed_min,
                compat_num,
                symbol(),
                word,
            ))
            .left_branch()
            .left_branch(),
            Dialect::Community => choice((
                lf,
                ws,
                comment,
                str,
                interp_str(this, compat),
                num,
                symbol(),
                word,
            ))
            .right_branch()
            .left_branch(),
            Dialect::Redmod => choice((
                lf,
                ws,
                comment,
                redmod_str,
                redmod_cname,
                num,
                symbol(),
                word,
            ))
            .right_branch(),
        };
        tok.recover_with(skip_then_retry_until(any().ignored(), end()))
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

fn str_elem<'src>(
    dialect: Dialect,
) -> impl Parser<'src, &'src str, Cow<'src, str>, LexExtra<'src>> + Clone {
    let unicode_escape = any()
        .filter(char::is_ascii_hexdigit)
        .repeated()
        .at_least(1)
        .at_most(6)
        .to_slice()
        .delimited_by(just('{'), just('}'))
        .try_map(|str, span| {
            let n = u32::from_str_radix(str, 16).map_err(|err| Rich::custom(span, err))?;
            let c = std::char::from_u32(n)
                .ok_or_else(|| Rich::custom(span, "invalid unicode escape"))?;
            Ok(Cow::Owned(c.into()))
        });

    let plain = none_of("\"\\")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Cow::Borrowed);

    match dialect {
        Dialect::Community => choice((
            plain,
            just('\\').ignore_then(choice((
                just('n').to("\n".into()),
                just('r').to("\r".into()),
                just('t').to("\t".into()),
                just('\'').to("'".into()),
                just('"').to("\"".into()),
                just('\\').to("\\".into()),
                just('u').ignore_then(unicode_escape),
            ))),
        ))
        .left_branch(),
        // REDmod only supports `\n \r \t \\ \u{..}`
        Dialect::Redmod => choice((
            plain,
            just('\\').ignore_then(choice((
                just('n').to("\n".into()),
                just('r').to("\r".into()),
                just('t').to("\t".into()),
                just('\\').to("\\".into()),
                just('u').ignore_then(unicode_escape),
            ))),
        ))
        .right_branch(),
    }
}

fn interp_str<'src>(
    toks: impl Parser<'src, &'src str, Vec<(Token<'src, LexSpan>, LexSpan)>, LexExtra<'src>> + Clone,
    compat: bool,
) -> impl Parser<'src, &'src str, Token<'src, LexSpan>, LexExtra<'src>> + Clone {
    // 0.5.x compatibility: string literals inside an interpolation are skipped as a whole,
    // so that they can contain unbalanced parentheses (`s"\(f(")"))"`)
    let str_lit = just('"')
        .then(
            none_of("\"\\")
                .ignored()
                .or(just('\\').then(any()).ignored())
                .repeated(),
        )
        .then(just('"'))
        .ignored();
    let balanced_parens = recursive(|p| {
        let elem = if compat {
            p.or(str_lit)
                .or(any().and_is(just(')').not()).ignored())
                .left_branch()
        } else {
            p.or(any().and_is(just(')').not()).ignored()).right_branch()
        };
        just('(')
            .ignore_then(elem.repeated())
            .then_ignore(just(')'))
    })
    .to_slice();

    just('s').ignore_then(
        choice((
            str_elem(Dialect::Community)
                .repeated()
                .at_least(1)
                .collect::<StrParts<'_>>()
                .map(|ps| Token::StrFrag(ps.str)),
            just("\\")
                .ignore_then(toks.nested_in(balanced_parens))
                .map(|tok| Token::Group(tok.into())),
        ))
        .map_with(|tok, e| (tok, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just('"'), just('"'))
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
            _ => {
                inp.rewind(before);
                return Err(Rich::custom(inp.span(pos..pos), "invalid symbol"));
            }
        };
        Ok(res)
    })
}

/// 0.5.x compatibility: `///` comments are ordinary comments that can appear anywhere. The
/// ones that document an item (in a module or class body, right before a declaration) are kept
/// as doc comments, the others become line comments. Their leading `/` is dropped so that they
/// do not read as misplaced doc comments once printed (`/// x` becomes `// x`).
pub fn demote_doc_comments(tokens: &mut [(Token<'_, LexSpan>, LexSpan)]) {
    // whether each enclosing `{` opens a class or struct body
    let mut scopes: Vec<bool> = vec![];
    for i in 0..tokens.len() {
        match tokens[i].0 {
            Token::LBrace => {
                let aggregate = significant_before(&tokens[..i])
                    .find_map(|tok| match tok {
                        Token::Ident("class" | "struct") => Some(true),
                        Token::LParen
                        | Token::RParen
                        | Token::LBrace
                        | Token::RBrace
                        | Token::Semicolon
                        | Token::Assign
                        | Token::Arrow => Some(false),
                        _ => None,
                    })
                    .unwrap_or(false);
                scopes.push(aggregate);
            }
            Token::RBrace => {
                scopes.pop();
            }
            Token::DocComment(str) => {
                let item_scope = scopes.last().copied().unwrap_or(true);
                let next = tokens[i + 1..]
                    .iter()
                    .map(|(tok, _)| tok)
                    .find(|tok| !tok.is_ws_or_comment() && !matches!(tok, Token::DocComment(_)));
                if !(item_scope && next.is_some_and(starts_item)) {
                    tokens[i].0 = Token::LineComment(&str[1..]);
                }
            }
            _ => {}
        }
    }
}

fn significant_before<'a, 'src>(
    tokens: &'a [(Token<'src, LexSpan>, LexSpan)],
) -> impl Iterator<Item = &'a Token<'src, LexSpan>> {
    tokens
        .iter()
        .rev()
        .map(|(tok, _)| tok)
        .filter(|tok| !tok.is_ws_or_comment() && !matches!(tok, Token::DocComment(_)))
}

fn starts_item(tok: &Token<'_, LexSpan>) -> bool {
    matches!(
        tok,
        Token::At
            | Token::Ident(
                "public"
                    | "private"
                    | "protected"
                    | "abstract"
                    | "cb"
                    | "const"
                    | "exec"
                    | "final"
                    | "importonly"
                    | "native"
                    | "persistent"
                    | "quest"
                    | "static"
                    | "opt"
                    | "out"
                    | "func"
                    | "class"
                    | "struct"
                    | "enum"
                    | "let"
                    | "import"
            )
    )
}

/// 0.5.x compatibility: splits a `>=` token into `>` and `=` when it closes the type arguments
/// of a type annotation, as in `let x: array<Int32>= [];`.
pub fn split_generic_ge(tokens: &mut Vec<(Token<'_, LexSpan>, LexSpan)>) {
    let mut i = 0;
    while i < tokens.len() {
        if matches!(tokens[i].0, Token::Ge) && closes_type_annotation(&tokens[..i]) {
            let span = tokens[i].1;
            let mid = span.start + 1;
            tokens[i] = (Token::RAngle, LexSpan::from(span.start..mid));
            tokens.insert(i + 1, (Token::Assign, LexSpan::from(mid..span.end)));
            i += 1;
        }
        i += 1;
    }
}

/// Whether the tokens end with an unclosed `name: Type<...` type annotation.
fn closes_type_annotation(tokens: &[(Token<'_, LexSpan>, LexSpan)]) -> bool {
    let mut it = significant_before(tokens);
    let mut depth = 1u32;
    while depth > 0 {
        match it.next() {
            Some(Token::RAngle) => depth += 1,
            Some(Token::LAngle) => depth -= 1,
            Some(Token::Ident(_) | Token::Comma | Token::LBracket | Token::RBracket) => {}
            _ => return false,
        }
    }
    matches!(it.next(), Some(Token::Ident(_))) && matches!(it.next(), Some(Token::Colon))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src, S = Span> {
    Group(Box<[(Self, S)]>),
    Ident(&'src str),
    Int(i32),
    Uint(u32),
    Ulong(u64),
    Long(i64),
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
            Self::Float(n) => write!(f, "{n}"),
            Self::Double(n) => write!(f, "{n}d"),
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::CName(s) => write!(f, "n\"{s}\""),
            Self::ResRef(s) => write!(f, "r\"{s}\""),
            Self::TdbId(s) => write!(f, "t\"{s}\""),
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
            Self::Null => write!(f, "null"),
            Self::This => write!(f, "this"),
            Self::Super => write!(f, "super"),
            Self::Case => write!(f, "case"),
            Self::Default => write!(f, "default"),
        }
    }
}

#[derive(Debug, Default)]
struct StrParts<'src> {
    str: Cow<'src, str>,
}

impl<'src> Container<Cow<'src, str>> for StrParts<'src> {
    fn push(&mut self, c: Cow<'src, str>) {
        if self.str.is_empty() {
            self.str = c;
        } else {
            self.str = Cow::Owned(mem::take(&mut self.str).into_owned() + &c);
        }
    }
}
