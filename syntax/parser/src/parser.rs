use std::borrow::Cow;

use chumsky::input::SpannedInput;
use chumsky::prelude::*;

use crate::lexer::Token;
use crate::{Branch, ParseOptions};

mod expr;
mod item;
mod stmt;

use expr::expr_with_span_rec;
use item::item_rec;
use redscript_ast::{
    Block, Dialect, Expr, FileId, Module, Path, SourceBlock, SourceExpr, SourceItem,
    SourceItemDecl, SourceModule, SourceStmt, SourceType, SourceTypeParam, Span, Spanned, Stmt,
    Type, TypeParam, Variance,
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
    item_decl_with(Dialect::Community)
}

pub fn item_decl_with<'tok, 'src: 'tok>(
    dialect: Dialect,
) -> impl Parse<'tok, 'src, SourceItemDecl<'src>> {
    item_decl_with_options(dialect.into())
}

pub fn item_decl_with_options<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> impl Parse<'tok, 'src, SourceItemDecl<'src>> {
    all_parsers(opts).0
}

pub fn item<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceItem<'src>> {
    item_with(Dialect::Community)
}

pub fn item_with<'tok, 'src: 'tok>(dialect: Dialect) -> impl Parse<'tok, 'src, SourceItem<'src>> {
    all_parsers(dialect.into()).1
}

pub fn stmt<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceStmt<'src>> {
    stmt_with(Dialect::Community)
}

pub fn stmt_with<'tok, 'src: 'tok>(dialect: Dialect) -> impl Parse<'tok, 'src, SourceStmt<'src>> {
    stmt_with_options(dialect.into())
}

pub fn stmt_with_options<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> impl Parse<'tok, 'src, SourceStmt<'src>> {
    block_stmt_expr_parsers(opts).1
}

fn expr_with_span<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> {
    block_stmt_expr_parsers(opts).2
}

pub fn expr<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceExpr<'src>> {
    expr_with(Dialect::Community)
}

pub fn expr_with<'tok, 'src: 'tok>(dialect: Dialect) -> impl Parse<'tok, 'src, SourceExpr<'src>> {
    expr_with_options(dialect.into())
}

pub fn expr_with_options<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> impl Parse<'tok, 'src, SourceExpr<'src>> {
    expr_with_span(opts).map(|(expr, _)| expr)
}

pub(super) fn all_parsers<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> (
    impl Parse<'tok, 'src, SourceItemDecl<'src>>,
    impl Parse<'tok, 'src, SourceItem<'src>>,
    impl Parse<'tok, 'src, SourceBlock<'src>>,
    impl Parse<'tok, 'src, SourceStmt<'src>>,
    impl Parse<'tok, 'src, (SourceExpr<'src>, Span)>,
) {
    let mut decl = Recursive::declare();
    let (block, stmt, expr) = block_stmt_expr_parsers(opts);
    decl.define(item_decl_rec(
        opts,
        decl.clone(),
        block.clone(),
        expr.clone(),
    ));
    (
        decl.clone(),
        item_rec(opts, decl, block.clone(), expr.clone()),
        block,
        stmt,
        expr,
    )
}

fn block_stmt_expr_parsers<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> (
    impl Parse<'tok, 'src, SourceBlock<'src>>,
    impl Parse<'tok, 'src, SourceStmt<'src>>,
    impl Parse<'tok, 'src, (SourceExpr<'src>, Span)>,
) {
    let mut stmt = Recursive::declare();
    let mut expr = Recursive::declare();
    let block = block_rec(stmt.clone());
    stmt.define(stmt_rec(opts, expr.clone(), stmt.clone(), block.clone()));
    expr.define(expr_with_span_rec(opts, expr.clone(), block.clone()));
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
    module_with(Dialect::Community)
}

pub fn module_with<'tok, 'src: 'tok>(
    dialect: Dialect,
) -> impl Parse<'tok, 'src, SourceModule<'src>> {
    module_with_options(dialect.into())
}

pub fn module_with_options<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> impl Parse<'tok, 'src, SourceModule<'src>> {
    let dialect = opts.dialect;
    let community_header = just(Token::Ident("module"))
        .ignore_then(
            ident()
                .separated_by(just(Token::Period))
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(Path::new);
    // REDmod: `@module "A.B"`
    let redmod_header = just(Token::At)
        .then(just(Token::Ident("module")))
        .ignore_then(module_path());
    let header = match dialect {
        Dialect::Community => community_header.left_branch(),
        Dialect::Redmod => redmod_header.right_branch(),
    };

    header
        .or_not()
        .then(
            item_decl_with_options(opts)
                .map_with(|i, e| (i, e.span()))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(move |(path, items)| Module::new(path, items).with_dialect(dialect))
        .erased()
}

/// A dotted module path inside a string literal, e.g. `"A.B"` (REDmod only).
pub(super) fn module_path<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Path<'src>> {
    select! { Token::Str(str) => str }
        .try_map(|str, span| match str {
            Cow::Borrowed(str) => Ok(Path::new(str.split('.').collect::<Vec<_>>())),
            Cow::Owned(_) => Err(Rich::custom(span, "module path must not contain escapes")),
        })
        .labelled("module path")
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

/// 0.5.x compatibility: any word is accepted as a declaration or member name, including the
/// words that are lexed as keywords (0.5.x has no reserved words in its `ident` rule).
fn compat_ident<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, &'src str> {
    select! {
        Token::Ident(ident) => ident,
        Token::True => "true",
        Token::False => "false",
        Token::Null => "null",
        Token::This => "this",
        Token::Super => "super",
        Token::Case => "case",
        Token::Default => "default",
    }
    .labelled("identifier")
}

/// A declaration name: an identifier, or any word in the 0.5.x compatibility mode.
fn name<'tok, 'src: 'tok>(opts: ParseOptions) -> impl Parse<'tok, 'src, &'src str> {
    if opts.is_compat() {
        compat_ident().left_branch()
    } else {
        ident().right_branch()
    }
}

/// Like [`name`], with the span of the name.
fn name_with_span<'tok, 'src: 'tok>(
    opts: ParseOptions,
) -> impl Parse<'tok, 'src, Spanned<&'src str>> {
    name(opts).map_with(|ident, e| (ident, e.span()))
}

/// A member or field name: [`extended_ident`], or any word in the 0.5.x compatibility mode.
fn member_name<'tok, 'src: 'tok>(opts: ParseOptions) -> impl Parse<'tok, 'src, &'src str> {
    if opts.is_compat() {
        compat_ident().left_branch()
    } else {
        extended_ident().right_branch()
    }
}

fn ident_with_span<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Spanned<&'src str>> {
    ident().map_with(|ident, e| (ident, e.span()))
}

fn type_with_span<'tok, 'src: 'tok>(
    dialect: Dialect,
) -> impl Parse<'tok, 'src, Spanned<SourceType<'src>>> {
    recursive(|this| {
        let array_size = select! {  Token::Int(i) => i };
        let base = choice((
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
                .map(move |(name, args)| {
                    let mut args = args.unwrap_or_default();
                    // REDmod: `array<T>` is normalized to the array type
                    if dialect == Dialect::Redmod && name == "array" && args.len() == 1 {
                        return Type::Array(Box::new(args.remove(0)));
                    }
                    Type::Named {
                        name,
                        args: args.into(),
                    }
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
        .map_with(|typ, e| (typ, e.span()));
        match dialect {
            Dialect::Community => base.left_branch(),
            // REDmod: fixed-size array `Int32[3]`
            Dialect::Redmod => base
                .foldl_with(
                    array_size
                        .delimited_by(just(Token::LBracket), just(Token::RBracket))
                        .repeated(),
                    |inner, size, e| (Type::StaticArray(Box::new(inner), size as _), e.span()),
                )
                .right_branch(),
        }
    })
}

fn type_param<'tok, 'src: 'tok>(dialect: Dialect) -> impl Parse<'tok, 'src, SourceTypeParam<'src>> {
    let variance = select! {
        Token::Plus => Variance::Covariant,
        Token::Minus => Variance::Contravariant,
    };

    variance
        .or_not()
        .then(ident_with_span())
        .then(
            just(Token::Ident("extends"))
                .ignore_then(type_with_span(dialect))
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

fn type_params<'tok, 'src: 'tok>(
    dialect: Dialect,
) -> impl Parse<'tok, 'src, Vec<SourceTypeParam<'src>>> {
    type_param(dialect)
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LAngle), just(Token::RAngle))
}

#[cfg(test)]
mod tests {
    use redscript_ast::{
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
