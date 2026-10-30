use std::collections::BTreeMap;

use chumsky::input::{SpannedInput, Stream};
use chumsky::prelude::*;
use redscript_redmod_ast::{
    Block, Declaration, Expr, FileId, Item, Module, SourceType, Span, Spanned, Statement, Type,
};

use crate::lexer::Token;

mod expr;
mod item;
mod native;

pub(super) type ParserInput<'tok, 'src> =
    SpannedInput<Token<'src>, Span, chumsky::input::BoxedStream<'tok, Spanned<Token<'src>>>>;
pub(super) type ParserExtra<'tok, 'src> =
    extra::Full<Rich<'tok, Token<'src>, Span>, (), DeclarationDocs<'src>>;

/// Documentation travels alongside the grammar input, not as syntax tokens.
/// This lets comments be trivia everywhere without recovery swallowing sibling docs.
#[derive(Clone, Default)]
pub struct DeclarationDocs<'src>(BTreeMap<u32, (u32, Vec<&'src str>)>);

impl<'src> DeclarationDocs<'src> {
    pub(super) fn from_tokens(tokens: &[(Token<'src>, Span)]) -> Self {
        let mut docs = Self::default();
        let mut pending = Vec::new();
        let mut start = 0;
        for (token, span) in tokens {
            if let Token::DocComment(text) = token {
                if pending.is_empty() {
                    start = span.start;
                }
                pending.push(text.trim_end_matches('\r'));
            } else if !pending.is_empty() {
                docs.0
                    .insert(span.start, (start, std::mem::take(&mut pending)));
            }
        }
        docs
    }

    pub(super) fn at(&self, start: u32) -> Vec<&'src str> {
        self.0
            .get(&start)
            .map(|(_, docs)| docs.clone())
            .unwrap_or_default()
    }

    fn declaration_span(&self, span: Span) -> Span {
        Span::new(
            self.0
                .get(&span.start)
                .map_or(span.start, |(start, _)| *start),
            span.end,
            span.file,
        )
    }
}

pub(super) fn input<'tok, 'src: 'tok>(
    tokens: &'tok [(Token<'src>, Span)],
    file: FileId,
    eof: u32,
) -> ParserInput<'tok, 'src> {
    Stream::from_iter(
        tokens
            .iter()
            .filter(|(token, _)| !matches!(token, Token::DocComment(_)))
            .cloned(),
    )
    .boxed()
    .spanned(Span::new(eof, eof, file))
}

pub trait Parse<'tok, 'src: 'tok, A>:
    Parser<'tok, ParserInput<'tok, 'src>, A, ParserExtra<'tok, 'src>> + Clone
{
    /// Erasing combinator types makes debug compilation significantly faster.
    #[cfg(debug_assertions)]
    fn erased<'a>(self) -> Boxed<'tok, 'a, ParserInput<'tok, 'src>, A, ParserExtra<'tok, 'src>>
    where
        Self: 'tok + 'a,
    {
        Parser::boxed(self)
    }

    #[cfg(not(debug_assertions))]
    #[inline]
    fn erased(self) -> impl Parse<'tok, 'src, A> {
        self
    }
}

impl<'tok, 'src: 'tok, A, P> Parse<'tok, 'src, A> for P where
    P: Parser<'tok, ParserInput<'tok, 'src>, A, ParserExtra<'tok, 'src>> + Clone
{
}

pub fn item_decl<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Declaration<'src>> {
    native::declaration()
}

pub fn item<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Item<'src>> {
    item_decl().map(|decl| decl.item)
}

pub fn stmt<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Statement<'src>> {
    native::statement()
}

fn expr_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Spanned<Expr<'src>>> {
    recursive(expr::expr_with_span_rec)
}

pub fn expr<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Expr<'src>> {
    expr_with_span().map(|(expr, _)| expr)
}

pub fn module<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Module<'src>> {
    native::module()
}

fn block_rec<'tok, 'src: 'tok>(
    stmt: impl Parse<'tok, 'src, Statement<'src>> + 'tok,
) -> impl Parse<'tok, 'src, Block<'src>> {
    recovered_stmt(stmt)
        .map_with(|stmt, e| (stmt, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(
            just(Token::LBrace),
            just(Token::RBrace).or(any()
                .filter(is_declaration_prefix)
                .ignored()
                .rewind()
                .or(end())
                .validate(|(), e, errors| {
                    let span: Span = e.span();
                    let docs: &DeclarationDocs<'src> = e.ctx();
                    let start = docs.declaration_span(span).start;
                    errors.emit(Rich::custom(
                        Span::new(start, start, span.file),
                        "unclosed block; expected '}'",
                    ));
                    Token::RBrace
                })),
        )
        .recover_with(via_parser(nested_delimiters(
            Token::LBrace,
            Token::RBrace,
            [
                (Token::LParen, Token::RParen),
                (Token::LBracket, Token::RBracket),
            ],
            |span| vec![(Statement::Expr((Expr::Error, span).into()), span)],
        )))
        .labelled("block")
        .erased()
}

fn is_declaration_prefix(token: &Token<'_>) -> bool {
    matches!(
        token,
        Token::At
            | Token::Ident(
                "function"
                    | "event"
                    | "class"
                    | "struct"
                    | "enum"
                    | "public"
                    | "private"
                    | "protected"
                    | "override"
                    | "abstract"
                    | "const"
                    | "exec"
                    | "final"
                    | "static"
                    | "import"
                    | "importonly"
                    | "persistent"
                    | "quest"
                    | "editable"
            )
    )
}

/// Skip one malformed region, leaving a following sibling or closing brace untouched.
fn synchronize<'tok, 'src: 'tok>(declaration: bool, top_level: bool) -> impl Parse<'tok, 'src, ()> {
    custom(move |input| {
        let start = input.offset();
        let mut braces = 0usize;
        let mut parens = 0usize;
        let mut brackets = 0usize;
        while let Some(token) = input.peek() {
            let boundary = is_declaration_prefix(&token)
                || matches!(token, Token::Ident("var" | "let"))
                || (!declaration
                    && matches!(
                        token,
                        Token::Ident(
                            "var"
                                | "let"
                                | "return"
                                | "if"
                                | "while"
                                | "for"
                                | "switch"
                                | "break"
                                | "continue"
                        ) | Token::Case
                            | Token::Default
                    ));
            if braces == 0
                && !declaration
                && (is_declaration_prefix(&token) || matches!(token, Token::Case | Token::Default))
            {
                break;
            }
            if braces == 0
                && ((input.offset() != start && boundary) || (token == Token::RBrace && !top_level))
            {
                break;
            }
            input.next();
            match token {
                Token::LBrace => braces += 1,
                Token::RBrace if braces > 0 => {
                    braces -= 1;
                    if braces == 0 {
                        break;
                    }
                }
                Token::LParen => parens += 1,
                Token::RParen => parens = parens.saturating_sub(1),
                Token::LBracket => brackets += 1,
                Token::RBracket => brackets = brackets.saturating_sub(1),
                Token::Semicolon if braces == 0 && parens == 0 && brackets == 0 => break,
                _ => {}
            }
        }
        if input.offset() == start {
            Err(Rich::custom(
                input.span(start..start),
                "expected a recoverable region",
            ))
        } else {
            Ok(())
        }
    })
    .erased()
}

fn recovered_stmt<'tok, 'src: 'tok>(
    stmt: impl Parse<'tok, 'src, Statement<'src>> + 'tok,
) -> impl Parse<'tok, 'src, Statement<'src>> {
    // A documented declaration after an incomplete body belongs to its parent.
    // Do not let permissive expression identifiers consume its modifiers first.
    select! { token = e if {
        let docs: &DeclarationDocs<'src> = e.ctx();
        let span: Span = e.span();
        is_declaration_prefix(&token) && docs.0.contains_key(&span.start)
    } => () }
    .not()
    .ignore_then(stmt)
    .recover_with(via_parser(
        synchronize(false, false)
            .map_with(|(), e| Statement::Expr(Box::new((Expr::Error, e.span())))),
    ))
    .erased()
}

fn declarations<'tok, 'src: 'tok>(
    decl: impl Parse<'tok, 'src, Declaration<'src>> + 'tok,
    member: bool,
) -> impl Parse<'tok, 'src, Vec<Spanned<Declaration<'src>>>> {
    decl.map_with(|decl, e| Some((decl, e.ctx().declaration_span(e.span()))))
        .recover_with(via_parser(synchronize(true, !member).to(None)))
        .repeated()
        .collect::<Vec<_>>()
        .map(|items| items.into_iter().flatten().collect())
        .erased()
}

fn ident<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, &'src str> {
    select! { Token::Ident(ident) => ident }.labelled("identifier")
}

fn extended_ident<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, &'src str> {
    ident()
}

fn ident_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Spanned<&'src str>> {
    ident().map_with(|ident, e| (ident, e.span()))
}

fn type_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Spanned<SourceType<'src>>> {
    recursive(|this| {
        let mixed_ref = just(Token::Ident("mixedRef"))
            .then(
                this.clone()
                    .delimited_by(just(Token::LAngle), just(Token::RAngle))
                    .or_not(),
            )
            .map(|(_, arg)| Type::Named {
                name: "mixedRef",
                args: arg.into_iter().collect(),
            });
        let constructor = select! {
            Token::Ident("array") => "array",
            Token::Ident("ref") => "ref",
            Token::Ident("wref") => "wref",
            Token::Ident("weak") => "weak",
            Token::Ident("script_ref") => "script_ref",
        }
        .then(this.delimited_by(just(Token::LAngle), just(Token::RAngle)))
        .map(|(name, elem)| {
            if name == "array" {
                Type::Array(Box::new(elem))
            } else {
                Type::Named {
                    name,
                    args: [elem].into(),
                }
            }
        });
        let plain = select! {
            Token::Ident(name) if !matches!(name, "array" | "ref" | "wref" | "weak" | "script_ref" | "mixedRef") =>
                Type::plain(name),
        };
        let named = mixed_ref
            .or(constructor)
            .or(plain)
            .map_with(|typ, e| (typ, e.span()));
        named
            .foldl_with(
                select! { Token::Int(size) => size }
                    .try_map(|size, span| {
                        usize::try_from(size)
                            .map_err(|_| Rich::custom(span, "invalid static array size"))
                    })
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .repeated(),
                |elem, size, e| (Type::StaticArray(Box::new(elem), size), e.span()),
            )
            .labelled("type")
    })
}
