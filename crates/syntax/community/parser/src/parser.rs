use chumsky::input::SpannedInput;
use chumsky::prelude::*;

use crate::lexer::Token;

mod expr;
mod item;
mod stmt;

use expr::expr_with_span_rec;
use item::item_rec;
use redscript_community_ast::{
    Block, Expr, FileId, Module, Path, SourceBlock, SourceExpr, SourceItem, SourceItemDecl,
    SourceModule, SourceStmt, SourceType, SourceTypeParam, Span, Spanned, Stmt, Type, TypeParam,
    Variance,
};

use self::item::item_decl_rec;
use self::stmt::stmt_rec;

pub(super) type ParserInput<'tok, 'src> =
    SpannedInput<Token<'src>, Span, &'tok [(Token<'src>, Span)]>;

pub(super) type ParserExtra<'tok, 'src> = extra::Full<Rich<'tok, Token<'src>, Span>, (), FileId>;

pub trait Parse<'tok, 'src: 'tok, A>:
    Parser<'tok, ParserInput<'tok, 'src>, A, ParserExtra<'tok, 'src>> + Clone
{
    /// Wraps the parser in a [`Boxed`] erasing the types when in debug mode. Makes compilation
    /// significantly faster for complex parsers.
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

pub fn item_decl<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceItemDecl<'src>> {
    all_parsers().0
}

pub fn item<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceItem<'src>> {
    all_parsers().1
}

pub fn stmt<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceStmt<'src>> {
    block_stmt_expr_parsers().1
}

fn expr_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> {
    block_stmt_expr_parsers().2
}

pub fn expr<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceExpr<'src>> {
    expr_with_span().map(|(expr, _)| expr)
}

pub(super) fn all_parsers<'tok, 'src: 'tok>() -> (
    impl Parse<'tok, 'src, SourceItemDecl<'src>>,
    impl Parse<'tok, 'src, SourceItem<'src>>,
    impl Parse<'tok, 'src, SourceBlock<'src>>,
    impl Parse<'tok, 'src, SourceStmt<'src>>,
    impl Parse<'tok, 'src, (SourceExpr<'src>, Span)>,
) {
    let mut decl = Recursive::declare();
    let (block, stmt, expr) = block_stmt_expr_parsers();
    decl.define(item_decl_rec(decl.clone(), block.clone(), expr.clone()));
    (
        decl.clone(),
        item_rec(decl, block.clone(), expr.clone()),
        block,
        stmt,
        expr,
    )
}

fn block_stmt_expr_parsers<'tok, 'src: 'tok>() -> (
    impl Parse<'tok, 'src, SourceBlock<'src>>,
    impl Parse<'tok, 'src, SourceStmt<'src>>,
    impl Parse<'tok, 'src, (SourceExpr<'src>, Span)>,
) {
    let mut stmt = Recursive::declare();
    let mut expr = Recursive::declare();
    let block = block_rec(stmt.clone());
    stmt.define(stmt_rec(expr.clone(), stmt.clone(), block.clone()));
    expr.define(expr_with_span_rec(expr.clone(), block.clone()));
    (block, stmt, expr)
}

fn block_rec<'tok, 'src: 'tok>(
    stmt: impl Parse<'tok, 'src, SourceStmt<'src>> + 'tok,
) -> impl Parse<'tok, 'src, SourceBlock<'src>> {
    stmt.map_with(|stmt, e| (stmt, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map(Block::new)
        .recover_with(via_parser(nested_delimiters(
            Token::LBrace,
            Token::RBrace,
            [
                (Token::LParen, Token::RParen),
                (Token::LBracket, Token::RBracket),
            ],
            |span| Block::single((Stmt::Expr((Expr::Error, span).into()), span)),
        )))
        .labelled("block")
        .erased()
}

pub fn module<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceModule<'src>> {
    just(Token::Ident("module"))
        .ignore_then(
            ident()
                .separated_by(just(Token::Period))
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .or_not()
        .then(
            item_decl()
                .map_with(|i, e| (i, e.span()))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(path, items)| Module::new(path.map(Path::new), items))
        .erased()
}

fn ident<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, &'src str> {
    select! {
        Token::Ident(ident) => ident,
    }
    .labelled("identifier")
}

fn extended_ident<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, &'src str> {
    select! {
        Token::Ident(ident) => ident,
        Token::True => "true",
        Token::False => "false",
        Token::Default => "default",
    }
    .labelled("identifier")
}

fn extended_ident_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Spanned<&'src str>> {
    extended_ident().map_with(|ident, e| (ident, e.span()))
}

fn ident_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Spanned<&'src str>> {
    ident().map_with(|ident, e| (ident, e.span()))
}

fn type_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Spanned<SourceType<'src>>> {
    recursive(|this| {
        let array_size = select! {  Token::Int(i) => i };
        choice((
            this.clone()
                .then(just(Token::Semicolon).ignore_then(array_size).or_not())
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .map(|(ex, size)| match size {
                    Some(size) => Type::StaticArray(Box::new(ex), size as _),
                    None => Type::Array(Box::new(ex)),
                }),
            ident()
                .then(
                    this.clone()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LAngle), just(Token::RAngle))
                        .or_not(),
                )
                .map(|(name, args)| Type::Named {
                    name,
                    args: args.unwrap_or_default().into(),
                }),
            this.clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .then_ignore(just(Token::Arrow))
                .then(this)
                .map(|(args, ret)| Type::Fn {
                    params: args.into(),
                    return_type: Box::new(ret),
                }),
        ))
        .labelled("type")
        .map_with(|typ, e| (typ, e.span()))
    })
}

fn type_param<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceTypeParam<'src>> {
    let variance = select! {
        Token::Plus => Variance::Covariant,
        Token::Minus => Variance::Contravariant,
    };

    variance
        .or_not()
        .then(ident_with_span())
        .then(
            just(Token::Ident("extends"))
                .ignore_then(type_with_span())
                .or_not(),
        )
        .map(|((variance, name), extends)| {
            TypeParam::new(
                variance.unwrap_or(Variance::Invariant),
                name,
                extends.map(Box::new),
            )
        })
        .erased()
}

fn type_params<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Vec<SourceTypeParam<'src>>> {
    type_param()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LAngle), just(Token::RAngle))
}

#[cfg(test)]
mod tests {
    use redscript_community_ast::{
        Aggregate, Constant, Function, FunctionBody, Import, Item, ItemDecl, ItemQualifiers,
        Visibility,
    };
    use similar_asserts::assert_eq;

    use super::*;
    use crate::parse_module;

    #[test]
    fn mod_with_imports() {
        let code = r#"
        module Dummy
        import Std.*
        import Something.{Test1, Test2}
        import Exact.Path
        "#;

        let res = parse_module(code, FileId::from_i32(0))
            .0
            .unwrap()
            .unwrapped();
        assert_eq!(
            res,
            Module::new(
                Some(Path::new(["Dummy"])),
                [
                    ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::empty(),
                        [],
                        Item::Import(Import::All(Path::new(["Std"]))),
                    ),
                    ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::empty(),
                        [],
                        Item::Import(Import::Select(
                            Path::new(["Something"]),
                            ["Test1", "Test2"].into(),
                        )),
                    ),
                    ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::empty(),
                        [],
                        Item::Import(Import::Exact(Path::new(["Exact", "Path"]))),
                    ),
                ],
            )
        );
    }

    #[test]
    fn items() {
        let code = r#"
        public static func Dummy()

        /// A doc comment
        native struct Test {}

        func Inline() -> Int32 = 1
        "#;

        let res = parse_module(code, FileId::from_i32(0))
            .0
            .unwrap()
            .unwrapped();
        assert_eq!(
            res,
            Module::new(
                None,
                [
                    ItemDecl::new(
                        [],
                        Some(Visibility::Public),
                        ItemQualifiers::STATIC,
                        [],
                        Item::Function(Function::new("Dummy", [], [], None, None)),
                    ),
                    ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::NATIVE,
                        ["/// A doc comment"],
                        Item::Struct(Aggregate::new("Test", [], None, [])),
                    ),
                    ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::empty(),
                        [],
                        Item::Function(Function::new(
                            "Inline",
                            [],
                            [],
                            Some(Type::plain("Int32").into()),
                            Some(FunctionBody::Inline(
                                Expr::Constant(Constant::I32(1)).into()
                            )),
                        )),
                    ),
                ],
            )
        );
    }
}
