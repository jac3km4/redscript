use chumsky::prelude::*;
use redscript_redmod_ast::{Annotation, Field, SourceExpr, Span, Visibility};

use super::{Parse, ident_with_span, type_with_span};
use crate::Token;

pub(super) fn field<'tok, 'src: 'tok>(
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, Field<'src>> {
    one_of([Token::Ident("var"), Token::Ident("let")])
        .ignore_then(
            ident_with_span()
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::Colon))
        .then(type_with_span())
        .then(just(Token::Assign).ignore_then(expr).or_not())
        .then_ignore(just(Token::Semicolon))
        .map(|((names, typ), default)| Field {
            names,
            typ,
            default,
        })
        .erased()
}

pub(super) fn visibility<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Visibility> {
    select! {
        Token::Ident("public") => Visibility::Public,
        Token::Ident("private") => Visibility::Private,
        Token::Ident("protected") => Visibility::Protected,
    }
    .labelled("item visibility")
}

pub(super) fn annotation<'tok, 'src: 'tok>(
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, Annotation<'src>> {
    just(Token::At)
        .ignore_then(ident_with_span())
        .then(
            expr.separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .map(|(name, args)| Annotation { name, args })
        .erased()
}
