use std::ops::BitOr;

use chumsky::container::Container;
use chumsky::prelude::*;
use redscript_ast::{
    Aggregate, Annotation, Dialect, Enum, EnumVariant, Field, Function, FunctionBody, Import, Item,
    ItemDecl, ItemQualifiers, Param, ParamQualifiers, Path, SourceAnnotation, SourceBlock,
    SourceEnum, SourceExpr, SourceField, SourceFunction, SourceItem, SourceItemDecl, Span, Type,
    Visibility,
};

use super::{
    Parse, ident, ident_with_span, member_name, module_path, name, name_with_span, type_params,
    type_with_span,
};
use crate::lexer::Token;
use crate::{Branch, ParseOptions};

pub fn item_decl_rec<'tok, 'src: 'tok>(
    opts: ParseOptions,
    item_decl: impl Parse<'tok, 'src, SourceItemDecl<'src>> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceItemDecl<'src>> {
    let dialect = opts.dialect;
    let annotations = annotation(dialect, expr.clone())
        .map_with(|a, e| (a, e.span()))
        .repeated()
        .collect::<Vec<_>>();
    let qualifiers = if opts.is_compat() {
        // 0.5.x compatibility: the visibility can appear anywhere among the qualifiers,
        // duplicates are tolerated, and the parameter qualifiers `opt` and `out` are ignored
        compat_qualifier()
            .repeated()
            .collect::<CompatQualifiers>()
            .map(|q| (q.visibility, q.qualifiers))
            .left_branch()
    } else {
        visibility()
            .or_not()
            .then(
                item_qualifier(dialect)
                    .repeated()
                    .collect::<BitCollection<_>>()
                    .map(|q| q.value),
            )
            .right_branch()
    };
    let docs_and_annotations = if opts.is_compat() {
        // 0.5.x compatibility: doc comments can also follow the annotations
        doc_comment()
            .map(Ok)
            .or(annotation(dialect, expr.clone()).map_with(|a, e| Err((a, e.span()))))
            .repeated()
            .collect::<Vec<_>>()
            .map(|elems| {
                let (docs, annotations): (Vec<_>, Vec<_>) =
                    elems.into_iter().partition(Result::is_ok);
                (
                    docs.into_iter().filter_map(Result::ok).collect::<Vec<_>>(),
                    annotations
                        .into_iter()
                        .filter_map(Result::err)
                        .collect::<Vec<_>>(),
                )
            })
            .left_branch()
    } else {
        doc_comment()
            .repeated()
            .collect::<Vec<_>>()
            .then(annotations)
            .right_branch()
    };
    docs_and_annotations
        .then(qualifiers)
        .then(item_with_qualifiers(opts, item_decl, block, expr))
        .map(
            |(((doc, annotations), (visibility, qualifiers)), (extra, item))| {
                ItemDecl::new(annotations, visibility, qualifiers | extra, doc, item)
            },
        )
        .labelled("declaration")
        .erased()
}

pub fn item_rec<'tok, 'src: 'tok>(
    opts: ParseOptions,
    item_decl: impl Parse<'tok, 'src, SourceItemDecl<'src>> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceItem<'src>> {
    item_with_qualifiers(opts, item_decl, block, expr)
        .map(|(_, item)| item)
        .erased()
}

/// Parses an item along with any qualifiers implied by the item syntax itself
/// (e.g. REDmod `event` handlers imply `ItemQualifiers::CALLBACK`).
fn item_with_qualifiers<'tok, 'src: 'tok>(
    opts: ParseOptions,
    item_decl: impl Parse<'tok, 'src, SourceItemDecl<'src>> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, (ItemQualifiers, SourceItem<'src>)> {
    let dialect = opts.dialect;
    match dialect {
        Dialect::Community => choice((
            import().map(Item::Import),
            aggregate(dialect, item_decl),
            enum_(opts).map(Item::Enum),
            function(opts, block, expr.clone()).map(Item::Function),
            field(opts, expr).map(Item::Let),
        ))
        .map(|item| (ItemQualifiers::empty(), item))
        .labelled("item")
        .left_branch(),
        Dialect::Redmod => choice((
            using(),
            aggregate(dialect, item_decl),
            enum_(opts).map(Item::Enum),
            function(opts, block.clone(), expr.clone()).map(Item::Function),
            field(opts, expr.clone()).map(Item::Let),
        ))
        .map(|item| (ItemQualifiers::empty(), item))
        .or(event(opts, block, expr).map(|f| (ItemQualifiers::CALLBACK, Item::Function(f))))
        .labelled("item")
        .right_branch(),
    }
}

/// REDmod `@using "A.B"`
fn using<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceItem<'src>> {
    just(Token::At)
        .then(just(Token::Ident("using")))
        .ignore_then(module_path())
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|path| Item::Import(Import::All(path)))
        .erased()
}

/// REDmod `event OnX(params) { ... }`, a callback implicitly returning `Bool`
fn event<'tok, 'src: 'tok>(
    opts: ParseOptions,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceFunction<'src>> {
    just(Token::Ident("event"))
        .ignore_then(ident_with_span())
        .then(params(opts))
        .then(function_body(block, expr).or_not())
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|((name, params), body)| {
            let ret = (Type::plain("Bool"), name.1);
            Function::new(name, [], params, Some(Box::new(ret)), body)
        })
        .erased()
}

type SpannedParam<'src> = (redscript_ast::SourceParam<'src>, Span);

fn params<'tok, 'src: 'tok>(opts: ParseOptions) -> impl Parse<'tok, 'src, Vec<SpannedParam<'src>>> {
    let dialect = opts.dialect;
    let community_qualifiers = select! {
        Token::Ident("opt") => ParamQualifiers::OPTIONAL,
        Token::Ident("out") => ParamQualifiers::OUT,
        Token::Ident("const") => ParamQualifiers::CONST,
    }
    .labelled("parameter qualifiers");
    let param_qualifiers = match dialect {
        Dialect::Community => community_qualifiers.left_branch(),
        // REDmod spells `opt` as `optional`
        Dialect::Redmod => community_qualifiers
            .or(
                select! { Token::Ident("optional") => ParamQualifiers::OPTIONAL }
                    .labelled("parameter qualifiers"),
            )
            .right_branch(),
    };

    param_qualifiers
        .repeated()
        .collect::<BitCollection<_>>()
        .then(name(opts))
        .then(just(Token::Colon).ignore_then(type_with_span(dialect)))
        .map_with(|((qualifiers, name), typ), e| {
            (Param::new(name, Some(typ), qualifiers.value), e.span())
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .erased()
}

fn function_body<'tok, 'src: 'tok>(
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, FunctionBody<'src, redscript_ast::WithSpan>> {
    choice((
        block.map(FunctionBody::Block),
        just(Token::Assign)
            .ignore_then(expr)
            .map(|e| FunctionBody::Inline(e.into())),
    ))
}

fn function<'tok, 'src: 'tok>(
    opts: ParseOptions,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceFunction<'src>> {
    let dialect = opts.dialect;
    let ty = type_with_span(dialect);
    let params = params(opts);
    let function_body = function_body(block, expr);

    let keyword = match dialect {
        Dialect::Community => just(Token::Ident("func")).ignored().left_branch(),
        // REDmod spells `func` as `function`
        Dialect::Redmod => just(Token::Ident("func"))
            .or(just(Token::Ident("function")))
            .ignored()
            .right_branch(),
    };
    let return_arrow = match dialect {
        Dialect::Community => just(Token::Arrow).left_branch(),
        // REDmod return types are introduced by either `->` or `:`
        Dialect::Redmod => just(Token::Arrow).or(just(Token::Colon)).right_branch(),
    };

    keyword
        .ignore_then(name_with_span(opts))
        .then(type_params(dialect).or_not())
        .then(params)
        .then(return_arrow.ignore_then(ty).or_not())
        .then(function_body.or_not())
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|((((name, type_params), params), ret_ty), body)| {
            Function::new(
                name,
                type_params.unwrap_or_default(),
                params,
                ret_ty.map(Box::new),
                body,
            )
        })
        .erased()
}

fn field<'tok, 'src: 'tok>(
    opts: ParseOptions,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceField<'src>> {
    let dialect = opts.dialect;
    let keyword = match dialect {
        Dialect::Community => just(Token::Ident("let")).left_branch(),
        // `var x : T;` is equivalent to `let x: T;`
        Dialect::Redmod => just(Token::Ident("let"))
            .or(just(Token::Ident("var")))
            .right_branch(),
    };

    keyword
        .ignore_then(member_name(opts).map_with(|name, e| (name, e.span())))
        .then(just(Token::Colon).ignore_then(type_with_span(dialect)))
        .then(just(Token::Assign).ignore_then(expr).or_not())
        .then_ignore(just(Token::Semicolon))
        .map(|((name, ty), default)| Field::new(name, ty.into(), default.map(Box::new)))
        .erased()
}

fn enum_<'tok, 'src: 'tok>(opts: ParseOptions) -> impl Parse<'tok, 'src, SourceEnum<'src>> {
    let value = if opts.is_compat() {
        // 0.5.x compatibility: enum values can be 64-bit literals (`A = 10l`)
        select! {
            Token::Int(i) => i64::from(i),
            Token::Long(i) => i,
        }
        .left_branch()
    } else {
        select! { Token::Int(i) => i64::from(i) }.right_branch()
    };
    let int = just(Token::Minus).or_not().then(value);

    let variants = member_name(opts)
        .then(just(Token::Assign).ignore_then(int).or_not())
        .map_with(|(name, value), e| {
            let val = value.map(|(minus, val)| {
                if minus.is_some() {
                    val.wrapping_neg()
                } else {
                    val
                }
            });
            (EnumVariant::new(name, val), e.span())
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace));

    just(Token::Ident("enum"))
        .ignore_then(ident_with_span())
        .then(variants)
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|(name, variants)| Enum::new(name, variants))
        .erased()
}

fn aggregate<'tok, 'src: 'tok>(
    dialect: Dialect,
    item_decl: impl Parse<'tok, 'src, SourceItemDecl<'src>> + 'tok,
) -> impl Parse<'tok, 'src, SourceItem<'src>> {
    let is_struct = select! {
        Token::Ident("class") => false,
        Token::Ident("struct") => true,
    };

    let items = item_decl
        .map_with(|i, e| (i, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace));

    is_struct
        .then(ident_with_span())
        .then(type_params(dialect).or_not())
        .then(
            just(Token::Ident("extends"))
                .ignore_then(type_with_span(dialect))
                .or_not(),
        )
        .then(items)
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|((((is_struct, name), type_params), extends), items)| {
            let aggregate = Aggregate::new(
                name,
                type_params.unwrap_or_default(),
                extends.map(Box::new),
                items,
            );
            if is_struct {
                Item::Struct(aggregate)
            } else {
                Item::Class(aggregate)
            }
        })
        .erased()
}

fn import<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Import<'src>> {
    let ident = ident();
    let import_selector = ident
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map(Some)
        .or(just(Token::Star).to(None));

    just(Token::Ident("import"))
        .ignore_then(
            ident
                .separated_by(just(Token::Period))
                .at_least(1)
                .collect::<Vec<_>>()
                .map(Path::new)
                .then(just(Token::Period).ignore_then(import_selector).or_not()),
        )
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|(path, last)| match last {
            Some(Some(idents)) => Import::Select(path, idents.into()),
            Some(None) => Import::All(path),
            None => Import::Exact(path),
        })
        .erased()
}

fn visibility<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Visibility> {
    select! {
        Token::Ident("public") => Visibility::Public,
        Token::Ident("private") => Visibility::Private,
        Token::Ident("protected") => Visibility::Protected,
    }
    .labelled("item visibility")
}

fn annotation<'tok, 'src: 'tok>(
    dialect: Dialect,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceAnnotation<'src>> {
    let _ = dialect;
    just(Token::At)
        .ignore_then(ident())
        .then(
            expr.separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .map(|(name, args)| Annotation::new(name, args))
        .erased()
}

fn item_qualifier<'tok, 'src: 'tok>(dialect: Dialect) -> impl Parse<'tok, 'src, ItemQualifiers> {
    let community = select! {
        Token::Ident("abstract") => ItemQualifiers::ABSTRACT,
        Token::Ident("cb") => ItemQualifiers::CALLBACK,
        Token::Ident("const") => ItemQualifiers::CONST,
        Token::Ident("exec") => ItemQualifiers::EXEC,
        Token::Ident("final") => ItemQualifiers::FINAL,
        Token::Ident("importonly") => ItemQualifiers::IMPORT_ONLY,
        Token::Ident("native") => ItemQualifiers::NATIVE,
        Token::Ident("persistent") => ItemQualifiers::PERSISTENT,
        Token::Ident("quest") => ItemQualifiers::QUEST,
        Token::Ident("static") => ItemQualifiers::STATIC

    }
    .labelled("item qualifier");

    match dialect {
        Dialect::Community => community.left_branch(),
        Dialect::Redmod => community
            .or(select! {
                Token::Ident("override") => ItemQualifiers::OVERRIDE,
                Token::Ident("virtual") => ItemQualifiers::VIRTUAL,
                // `import function` and `import class` are native declarations
                Token::Ident("import") => ItemQualifiers::NATIVE,
            }
            .labelled("item qualifier"))
            .right_branch(),
    }
}

fn doc_comment<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, &'src str> {
    select! {
        Token::DocComment(str) => str,
    }
    .labelled("doc comment")
}

/// A qualifier accepted in the 0.5.x compatibility mode.
#[derive(Debug, Clone, Copy)]
enum CompatQualifier {
    Visibility(Visibility),
    Item(ItemQualifiers),
    /// A parameter qualifier (`opt`, `out`) on an item, which 0.5.x accepts and ignores.
    Ignored,
}

fn compat_qualifier<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, CompatQualifier> {
    visibility()
        .map(CompatQualifier::Visibility)
        .or(item_qualifier(Dialect::Community).map(CompatQualifier::Item))
        .or(select! {
            Token::Ident("opt") => CompatQualifier::Ignored,
            Token::Ident("out") => CompatQualifier::Ignored,
        })
        .labelled("item qualifier")
}

/// Qualifiers in any order; the first visibility wins, like in 0.5.x.
#[derive(Debug, Default)]
struct CompatQualifiers {
    visibility: Option<Visibility>,
    qualifiers: ItemQualifiers,
}

impl Container<CompatQualifier> for CompatQualifiers {
    fn push(&mut self, item: CompatQualifier) {
        match item {
            CompatQualifier::Visibility(visibility) => {
                self.visibility.get_or_insert(visibility);
            }
            CompatQualifier::Item(qualifiers) => self.qualifiers |= qualifiers,
            CompatQualifier::Ignored => {}
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct BitCollection<A> {
    value: A,
}

impl<A: Default + Copy + BitOr<Output = A>> Container<A> for BitCollection<A> {
    #[inline]
    fn push(&mut self, item: A) {
        self.value = self.value | item;
    }
}

#[cfg(test)]
mod tests {
    use redscript_ast::{
        Block, Constant, Expr, FileId, FunctionBody, Stmt, Type, TypeParam, Variance,
    };
    use similar_asserts::assert_eq;

    use super::*;
    use crate::{parse_item, parse_item_decl};

    #[test]
    fn class() {
        let code = r#"
        class Test {
            public final func Method(opt arg: Int32) -> Int32 {
                return arg;
            }
        }
        "#;

        assert_eq!(
            parse_item(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Item::Class(Aggregate::new(
                "Test",
                [],
                None,
                [ItemDecl::new(
                    [],
                    Some(Visibility::Public),
                    ItemQualifiers::FINAL,
                    [],
                    Item::Function(Function::new(
                        "Method",
                        [],
                        [Param::new(
                            "arg",
                            Some(Type::plain("Int32")),
                            ParamQualifiers::OPTIONAL
                        )],
                        Some(Type::plain("Int32").into()),
                        Some(FunctionBody::Block(Block::single(Stmt::Return(Some(
                            Expr::Ident("arg").into()
                        )))))
                    ))
                )]
            ))
        );
    }

    #[test]
    fn enum_() {
        let code = r#"
        enum Test {
            A = 1,
            B = 2,
            C,
        }
        "#;

        assert_eq!(
            parse_item(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Item::Enum(Enum::new(
                "Test",
                [
                    EnumVariant::new("A", Some(1)),
                    EnumVariant::new("B", Some(2)),
                    EnumVariant::new("C", None::<i64>),
                ]
            ))
        );
    }

    #[test]
    fn struct_() {
        let code = r#"
        struct Test {
            let a: Int32;
            let b: Int32 = 3;
        }
        "#;

        assert_eq!(
            parse_item(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Item::Struct(Aggregate::new(
                "Test",
                [],
                None,
                [
                    ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::empty(),
                        [],
                        Item::Let(Field::new("a", Type::plain("Int32").into(), None))
                    ),
                    ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::empty(),
                        [],
                        Item::Let(Field::new(
                            "b",
                            Type::plain("Int32").into(),
                            Some(Expr::Constant(Constant::I32(3)).into())
                        ))
                    ),
                ]
            ))
        );
    }

    #[test]
    fn func() {
        let code = r#"
        func Test(arg1: Int32, arg2: Int64) -> Int32 {
            return arg1;
        }
        "#;

        assert_eq!(
            parse_item(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Item::Function(Function::new(
                "Test",
                [],
                [
                    Param::new("arg1", Some(Type::plain("Int32")), ParamQualifiers::empty()),
                    Param::new("arg2", Some(Type::plain("Int64")), ParamQualifiers::empty()),
                ],
                Some(Type::plain("Int32").into()),
                Some(FunctionBody::Block(Block::single(Stmt::Return(Some(
                    Expr::Ident("arg1").into()
                )))))
            ))
        );
    }

    #[test]
    fn annotations() {
        let code = r#"
        @if(true)
        @replaceGlobal()
        func Test()
        "#;

        assert_eq!(
            parse_item_decl(code, FileId::from_i32(0))
                .0
                .unwrap()
                .unwrapped(),
            ItemDecl::new(
                [
                    Annotation::new("if", [Expr::Constant(Constant::Bool(true))]),
                    Annotation::new("replaceGlobal", [])
                ],
                None,
                ItemQualifiers::empty(),
                [],
                Item::Function(Function::new("Test", [], [], None, None))
            )
        );
    }

    #[test]
    fn class_with_type_params() {
        let code = r#"
        class Test<-A, +B extends C> {
            public final func Method<D extends E>(opt arg: Int32) -> Int32 {
                return arg;
            }
        }
        "#;

        assert_eq!(
            parse_item(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Item::Class(Aggregate::new(
                "Test",
                [
                    TypeParam::new(Variance::Contravariant, "A", None),
                    TypeParam::new(Variance::Covariant, "B", Some(Type::plain("C").into()))
                ],
                None,
                [ItemDecl::new(
                    [],
                    Some(Visibility::Public),
                    ItemQualifiers::FINAL,
                    [],
                    Item::Function(Function::new(
                        "Method",
                        [TypeParam::new(
                            Variance::Invariant,
                            "D",
                            Some(Type::plain("E").into())
                        )],
                        [Param::new(
                            "arg",
                            Some(Type::plain("Int32")),
                            ParamQualifiers::OPTIONAL
                        )],
                        Some(Type::plain("Int32").into()),
                        Some(FunctionBody::Block(Block::single(Stmt::Return(Some(
                            Expr::Ident("arg").into()
                        )))))
                    ))
                )]
            ))
        );
    }

    #[test]
    fn doc_comments() {
        let code = r#"
        /// This is a test class
        class Test {
            /// This is a test function
            func Method() {
                return;
            }
        }
        "#;

        assert_eq!(
            parse_item_decl(code, FileId::from_i32(0))
                .0
                .unwrap()
                .unwrapped(),
            ItemDecl::new(
                [],
                None,
                ItemQualifiers::empty(),
                ["/// This is a test class"],
                Item::Class(Aggregate::new(
                    "Test",
                    [],
                    None,
                    [ItemDecl::new(
                        [],
                        None,
                        ItemQualifiers::empty(),
                        ["/// This is a test function"],
                        Item::Function(Function::new(
                            "Method",
                            [],
                            [],
                            None,
                            Some(FunctionBody::Block(Block::single(Stmt::Return(None))))
                        ))
                    )]
                ))
            )
        );
    }
}
