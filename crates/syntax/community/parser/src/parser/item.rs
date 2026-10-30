use chumsky::prelude::*;
use redscript_community_ast::{
    Aggregate, Annotation, Enum, EnumVariant, Field, Function, FunctionBody, Import, Item,
    ItemDecl, ItemQualifiers, Param, ParamQualifiers, Path, SourceAnnotation, SourceBlock,
    SourceEnum, SourceExpr, SourceField, SourceFunction, SourceItem, SourceItemDecl, Span,
    Visibility,
};

use super::{
    Parse, extended_ident, extended_ident_with_span, ident, ident_with_span, type_params,
    type_with_span,
};
use crate::lexer::Token;
use crate::shared::BitCollection;

pub fn item_decl_rec<'tok, 'src: 'tok>(
    item_decl: impl Parse<'tok, 'src, SourceItemDecl<'src>> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceItemDecl<'src>> {
    let annotations = annotation(expr.clone())
        .map_with(|a, e| (a, e.span()))
        .repeated()
        .collect::<Vec<_>>();
    doc_comment()
        .repeated()
        .collect::<Vec<_>>()
        .then(annotations)
        .then(visibility().or_not())
        .then(item_qualifier().repeated().collect::<BitCollection<_>>())
        .then(item_rec(item_decl, block, expr))
        .map(|((((doc, annotations), visibility), qualifiers), item)| {
            ItemDecl::new(annotations, visibility, qualifiers.value, doc, item)
        })
        .labelled("declaration")
        .erased()
}

pub fn item_rec<'tok, 'src: 'tok>(
    item_decl: impl Parse<'tok, 'src, SourceItemDecl<'src>> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceItem<'src>> {
    choice((
        import().map(Item::Import),
        aggregate(item_decl),
        enum_().map(Item::Enum),
        function(block, expr.clone()).map(Item::Function),
        field(expr).map(Item::Let),
    ))
    .labelled("item")
    .erased()
}

fn function<'tok, 'src: 'tok>(
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceFunction<'src>> {
    let param_qualifiers = select! {
        Token::Ident("opt") => ParamQualifiers::OPTIONAL,
        Token::Ident("out") => ParamQualifiers::OUT,
        Token::Ident("const") => ParamQualifiers::CONST,
    }
    .labelled("parameter qualifiers");

    let ty = type_with_span();

    let params = param_qualifiers
        .repeated()
        .collect::<BitCollection<_>>()
        .then(ident())
        .then(just(Token::Colon).ignore_then(ty.clone()))
        .map_with(|((qualifiers, name), typ), e| {
            (Param::new(name, Some(typ), qualifiers.value), e.span())
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen));

    let function_body = choice((
        block.map(FunctionBody::Block),
        just(Token::Assign)
            .ignore_then(expr)
            .map(|e| FunctionBody::Inline(e.into())),
    ));

    just(Token::Ident("func"))
        .ignore_then(ident_with_span())
        .then(type_params().or_not())
        .then(params)
        .then(just(Token::Arrow).ignore_then(ty).or_not())
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
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceField<'src>> {
    just(Token::Ident("let"))
        .ignore_then(extended_ident_with_span())
        .then(just(Token::Colon).ignore_then(type_with_span()))
        .then(just(Token::Assign).ignore_then(expr).or_not())
        .then_ignore(just(Token::Semicolon))
        .map(|((name, ty), default)| Field::new(name, ty.into(), default.map(Box::new)))
        .erased()
}

fn enum_<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, SourceEnum<'src>> {
    let int = just(Token::Minus).or_not().then(select! {
        Token::Int(i) => i64::from(i),
        Token::Long(i) => i,
    });

    let variants = extended_ident()
        .then(just(Token::Assign).ignore_then(int).or_not())
        .map_with(|(name, value), e| {
            let val = value.map(|(minus, val)| if minus.is_some() { -val } else { val });
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
        .then(type_params().or_not())
        .then(
            just(Token::Ident("extends"))
                .ignore_then(type_with_span())
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
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, SourceAnnotation<'src>> {
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

fn item_qualifier<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, ItemQualifiers> {
    select! {
        Token::Ident("abstract") => ItemQualifiers::ABSTRACT,
        Token::Ident("cb") => ItemQualifiers::CALLBACK,
        Token::Ident("const") => ItemQualifiers::CONST,
        Token::Ident("editable") => ItemQualifiers::EDITABLE,
        Token::Ident("exec") => ItemQualifiers::EXEC,
        Token::Ident("final") => ItemQualifiers::FINAL,
        Token::Ident("importonly") => ItemQualifiers::IMPORT_ONLY,
        Token::Ident("instanceeditable") => ItemQualifiers::INSTANCE_EDITABLE,
        Token::Ident("inlined") => ItemQualifiers::INLINE,
        Token::Ident("native") => ItemQualifiers::NATIVE,
        Token::Ident("persistent") => ItemQualifiers::PERSISTENT,
        Token::Ident("quest") => ItemQualifiers::QUEST,
        Token::Ident("replicated") => ItemQualifiers::REPLICATED,
        Token::Ident("static") => ItemQualifiers::STATIC,
        Token::Ident("testonly") => ItemQualifiers::TEST_ONLY,

    }
    .labelled("item qualifier")
}

fn doc_comment<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, &'src str> {
    select! {
        Token::DocComment(str) => str,
    }
    .labelled("doc comment")
}

#[cfg(test)]
mod tests {
    use redscript_community_ast::{
        Block, Constant, Expr, FileId, FunctionBody, Stmt, Type, TypeParam, Variance,
    };
    use similar_asserts::assert_eq;

    use super::*;
    use crate::{parse_item, parse_item_decl};

    #[test]
    fn shared_field_qualifiers() {
        for (spelling, qualifier) in [
            ("editable", ItemQualifiers::EDITABLE),
            ("instanceeditable", ItemQualifiers::INSTANCE_EDITABLE),
            ("inlined", ItemQualifiers::INLINE),
            ("replicated", ItemQualifiers::REPLICATED),
            ("testonly", ItemQualifiers::TEST_ONLY),
            ("const", ItemQualifiers::CONST),
        ] {
            let code = format!("{spelling} let value: Int32;");
            let (decl, errors) = parse_item_decl(&code, FileId::from_i32(0));
            assert!(errors.is_empty(), "{errors:?}");
            assert_eq!(decl.unwrap().qualifiers, qualifier);
        }
    }

    #[test]
    fn grouped_parameters_are_rejected() {
        for code in [
            "func Invalid(first, second: Int32) {}",
            "func Invalid(opt out first, second: Int32) {}",
            "func Invalid(first: Int32, second) {}",
        ] {
            let (_, errors) = parse_item(code, FileId::from_i32(0));
            assert!(!errors.is_empty(), "{code}");
        }
    }

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
    fn enum_integer_initializers() {
        for (literal, expected) in [
            ("2147483648l", 2_147_483_648_i64),
            ("-2147483649l", -2_147_483_649),
            ("42", 42),
            ("-42", -42),
        ] {
            let code = format!("enum Test {{ A = {literal} }}");
            let (item, errors) = parse_item(&code, FileId::from_i32(0));

            assert!(errors.is_empty(), "{literal}: {errors:?}");
            assert_eq!(
                item.unwrap().unwrapped(),
                Item::Enum(Enum::new("Test", [EnumVariant::new("A", Some(expected))]))
            );
        }
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
