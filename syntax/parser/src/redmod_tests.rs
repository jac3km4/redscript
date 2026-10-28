//! Tests for the REDmod dialect, and for making sure the community dialect rejects it.

use redscript_ast::{
    Aggregate, Annotation, BinOp, Block, Constant, Dialect, Expr, Field, FileId, Function,
    FunctionBody, Import, Item, ItemDecl, ItemQualifiers, Module, Param, ParamQualifiers, Path,
    Stmt, Type, UnOp, Visibility,
};
use similar_asserts::assert_eq;

use super::*;

fn file() -> FileId {
    FileId::from_i32(0)
}
const RM: Dialect = Dialect::Redmod;
const CM: Dialect = Dialect::Community;

fn tokens(src: &str, dialect: Dialect) -> (Vec<Token<'_>>, usize) {
    let (toks, errs) = lex_with_dialect(src, file(), dialect);
    let toks = toks
        .unwrap_or_default()
        .into_iter()
        .map(|(t, _)| t)
        .collect();
    (toks, errs.len())
}

fn expr(src: &str) -> Expr<'_> {
    let (res, errs) = parse_expr_with(src, file(), RM);
    assert!(errs.is_empty(), "{errs:?}");
    res.unwrap().unwrapped()
}

fn stmt(src: &str) -> Stmt<'_> {
    let (res, errs) = parse_stmt_with(src, file(), RM);
    assert!(errs.is_empty(), "{errs:?}");
    res.unwrap().unwrapped()
}

fn decl(src: &str) -> ItemDecl<'_> {
    let (res, errs) = parse_item_decl_with(src, file(), RM);
    assert!(errs.is_empty(), "{errs:?}");
    res.unwrap().unwrapped()
}

fn int(n: i32) -> Expr<'static> {
    Expr::Constant(Constant::I32(n))
}

fn cast<'a>(e: Expr<'a>, ty: Type<'a>) -> Expr<'a> {
    Expr::DynCast {
        expr: Box::new(e),
        typ: Box::new(ty),
    }
}

fn plain_fn<'a>(name: &'a str, ret: Option<Type<'a>>, body: Option<FunctionBody<'a>>) -> Item<'a> {
    Item::Function(Function::new(name, [], [], ret.map(Box::new), body))
}

#[test]
fn lex_prefixed_strings() {
    let (toks, errs) = tokens(r#"n"a" T"b" R"c" t"d" r"e" "f""#, RM);
    assert_eq!(errs, 0);
    assert_eq!(
        toks,
        [
            Token::CName("a".into()),
            Token::TdbId("b".into()),
            Token::ResRef("c".into()),
            Token::TdbId("d".into()),
            Token::ResRef("e".into()),
            Token::Str("f".into()),
        ]
    );
}

#[test]
fn lex_single_quoted_cname() {
    let (toks, errs) = tokens(r#"'Some Name' '<Rich color="'"#, RM);
    assert_eq!(errs, 0);
    assert_eq!(
        toks,
        [
            Token::CName("Some Name".into()),
            Token::CName("<Rich color=\"".into())
        ]
    );
    // no newline allowed
    assert_ne!(tokens("'a\nb'", RM).1, 0);
    // community does not know about them
    assert_ne!(tokens("'Name'", CM).1, 0);
}

#[test]
fn lex_null() {
    assert_eq!(tokens("NULL", RM).0, [Token::Null]);
    assert_eq!(tokens("NULL", CM).0, [Token::Ident("NULL")]);
}

#[test]
fn lex_escapes() {
    let (toks, errs) = tokens(r#""a\n\r\t\\\u{22}b""#, RM);
    assert_eq!(errs, 0);
    assert_eq!(toks, [Token::Str("a\n\r\t\\\"b".into())]);

    assert_ne!(tokens(r#""\"""#, RM).1, 0);
    assert_ne!(tokens(r#""\'""#, RM).1, 0);
    assert_ne!(tokens(r#""\x22""#, RM).1, 0);
    // community still accepts them
    assert_eq!(tokens(r#""\"""#, CM).1, 0);
}

#[test]
fn lex_no_interpolation() {
    let (toks, _) = tokens(r#"s"a""#, RM);
    assert!(!matches!(toks.first(), Some(Token::InterpStr(_))));
    let (toks, errs) = tokens(r#"s"a""#, CM);
    assert_eq!(errs, 0);
    assert!(matches!(toks.first(), Some(Token::InterpStr(_))));
}

#[test]
fn function_keyword_and_return_types() {
    for code in [
        "function Test(a : Int32) -> Int32 { return a; }",
        "function Test(a : Int32) : Int32 { return a; }",
        "func Test(a : Int32) : Int32 { return a; }",
    ] {
        assert_eq!(
            decl(code),
            ItemDecl::new(
                [],
                None,
                ItemQualifiers::empty(),
                [],
                Item::Function(Function::new(
                    "Test",
                    [],
                    [Param::new(
                        "a",
                        Some(Type::plain("Int32")),
                        ParamQualifiers::empty()
                    )],
                    Some(Type::plain("Int32").into()),
                    Some(FunctionBody::Block(Block::single(Stmt::Return(Some(
                        Expr::Ident("a").into()
                    )))))
                ))
            )
        );
    }
}

#[test]
fn community_rejects_function_keyword() {
    assert!(!parse_item_decl("function Test() {}", file()).1.is_empty());
    assert!(
        !parse_item_decl("func Test() : Int32 {}", file())
            .1
            .is_empty()
    );
}

#[test]
fn event_handler() {
    assert_eq!(
        decl("protected event OnX(evt : Foo, optional n : Int32) { return; }"),
        ItemDecl::new(
            [],
            Some(Visibility::Protected),
            ItemQualifiers::CALLBACK,
            [],
            Item::Function(Function::new(
                "OnX",
                [],
                [
                    Param::new("evt", Some(Type::plain("Foo")), ParamQualifiers::empty()),
                    Param::new("n", Some(Type::plain("Int32")), ParamQualifiers::OPTIONAL),
                ],
                Some(Type::plain("Bool").into()),
                Some(FunctionBody::Block(Block::single(Stmt::Return(None))))
            ))
        )
    );
    // the span of the implicit return type is the name span
    let (res, _) = parse_item_decl_with("event OnX() {}", file(), RM);
    let Item::Function(f) = res.unwrap().item else {
        panic!("expected function");
    };
    let (_, name_span) = f.name;
    assert_eq!(f.return_type.unwrap().1, name_span);

    assert!(!parse_item_decl("event OnX() {}", file()).1.is_empty());
}

#[test]
fn optional_param_community_rejects() {
    assert!(
        !parse_item_decl("func A(optional x: Int32) {}", file())
            .1
            .is_empty()
    );
}

#[test]
fn import_qualifier_is_native() {
    assert_eq!(
        decl("import function ScriptLog(const text : ref<string>);"),
        ItemDecl::new(
            [],
            None,
            ItemQualifiers::NATIVE,
            [],
            Item::Function(Function::new(
                "ScriptLog",
                [],
                [Param::new(
                    "text",
                    Some(Type::Named {
                        name: "ref",
                        args: [Type::plain("string")].into()
                    }),
                    ParamQualifiers::CONST
                )],
                None,
                None
            ))
        )
    );
    assert_eq!(
        decl("import class X extends Y {}").qualifiers,
        ItemQualifiers::NATIVE
    );
    assert_eq!(
        decl("importonly class X {}").qualifiers,
        ItemQualifiers::IMPORT_ONLY
    );

    // `import A.B` statements are not imports in REDmod
    assert!(!parse_item_decl_with("import A.B", file(), RM).1.is_empty());
    // and `import function` is not valid in community
    assert!(!parse_item_decl("import function A();", file()).1.is_empty());
}

#[test]
fn override_qualifier() {
    assert_eq!(
        decl("private override function OnAttach() -> Void {}"),
        ItemDecl::new(
            [],
            Some(Visibility::Private),
            ItemQualifiers::OVERRIDE,
            [],
            plain_fn(
                "OnAttach",
                Some(Type::plain("Void")),
                Some(FunctionBody::Block(Block::new([])))
            )
        )
    );
    assert!(!parse_item_decl("override func A() {}", file()).1.is_empty());
}

#[test]
fn virtual_qualifier() {
    assert_eq!(
        decl("public const virtual function Call() -> Void;").qualifiers,
        ItemQualifiers::CONST | ItemQualifiers::VIRTUAL
    );
    assert!(!parse_item_decl("virtual func A() {}", file()).1.is_empty());
}

#[test]
fn var_declarations() {
    assert_eq!(
        stmt("var x : Int32 = 1;"),
        Stmt::Let {
            name: "x",
            typ: Some(Type::plain("Int32").into()),
            value: Some(int(1).into())
        }
    );
    assert_eq!(
        decl("class A { var x : Int32; }"),
        ItemDecl::new(
            [],
            None,
            ItemQualifiers::empty(),
            [],
            Item::Class(Aggregate::new(
                "A",
                [],
                None,
                [ItemDecl::new(
                    [],
                    None,
                    ItemQualifiers::empty(),
                    [],
                    Item::Let(Field::new("x", Type::plain("Int32").into(), None))
                )]
            ))
        )
    );
    assert!(!parse_stmt("var x : Int32;", file()).1.is_empty());
}

#[test]
fn array_types() {
    assert_eq!(
        stmt("let a : array<Int32>;"),
        Stmt::Let {
            name: "a",
            typ: Some(Type::Array(Type::plain("Int32").into()).into()),
            value: None
        }
    );
    assert_eq!(
        stmt("let b : Int32[3];"),
        Stmt::Let {
            name: "b",
            typ: Some(Type::StaticArray(Type::plain("Int32").into(), 3).into()),
            value: None
        }
    );
    assert_eq!(
        stmt("let c : array<ref<Foo>>;"),
        Stmt::Let {
            name: "c",
            typ: Some(
                Type::Array(
                    Type::Named {
                        name: "ref",
                        args: [Type::plain("Foo")].into()
                    }
                    .into()
                )
                .into()
            ),
            value: None
        }
    );
    // community keeps `array<T>` as a plain named type
    assert_eq!(
        parse_stmt("let a: array<Int32>;", file())
            .0
            .unwrap()
            .unwrapped(),
        Stmt::Let {
            name: "a",
            typ: Some(
                Type::Named {
                    name: "array",
                    args: [Type::plain("Int32")].into()
                }
                .into()
            ),
            value: None
        }
    );
    assert!(!parse_stmt("let b: Int32[3];", file()).1.is_empty());
}

#[test]
fn new_without_parens() {
    let expected = Expr::New {
        typ: Type::plain("Foo").into(),
        args: [].into(),
    };
    assert_eq!(expr("new Foo"), expected);
    assert_eq!(expr("new Foo()"), expected);
    assert!(!parse_expr("new Foo", file()).1.is_empty());
}

#[test]
fn null_literal() {
    assert_eq!(expr("NULL"), Expr::Null);
    assert_eq!(
        parse_expr("NULL", file()).0.unwrap().unwrapped(),
        Expr::Ident("NULL")
    );
}

#[test]
fn prefixed_literals() {
    assert_eq!(
        expr(r#"T"Items.Foo""#),
        Expr::Constant(Constant::TweakDbId("Items.Foo".into()))
    );
    assert_eq!(
        expr(r#"R"base\\a""#),
        Expr::Constant(Constant::Resource("base\\a".into()))
    );
    assert_eq!(
        expr("'Name'"),
        Expr::Constant(Constant::CName("Name".into()))
    );
}

#[test]
fn cast_expressions() {
    // plain `(a)` stays a parenthesized expression
    assert_eq!(expr("(a)"), Expr::Ident("a"));
    // `(a) + b` is a binary operation
    assert_eq!(
        expr("(a) + b"),
        Expr::BinOp {
            lhs: Box::new(Expr::Ident("a")),
            op: BinOp::Add,
            rhs: Box::new(Expr::Ident("b")),
        }
    );
    // lowercase names are never types, so this is a call on a parenthesized expression
    assert_eq!(
        expr("(a)(b)"),
        Expr::Call {
            expr: Box::new(Expr::Ident("a")),
            type_args: [].into(),
            args: [Expr::Ident("b")].into()
        }
    );
    assert_eq!(
        expr("(Int32)x"),
        cast(Expr::Ident("x"), Type::plain("Int32"))
    );
    assert_eq!(
        expr("(Int32)-x"),
        cast(
            Expr::UnOp {
                op: UnOp::Neg,
                expr: Box::new(Expr::Ident("x"))
            },
            Type::plain("Int32")
        )
    );
    assert_eq!(
        expr("((EInputKey)(n))"),
        cast(Expr::Ident("n"), Type::plain("EInputKey"))
    );
    assert_eq!(
        expr("(Int32)x.y + 1"),
        Expr::BinOp {
            lhs: Box::new(cast(
                Expr::Member {
                    expr: Box::new(Expr::Ident("x")),
                    member: "y"
                },
                Type::plain("Int32")
            )),
            op: BinOp::Add,
            rhs: Box::new(int(1)),
        }
    );
    // `as` after a parenthesized uppercase name is not a cast operand
    assert_eq!(expr("(A) as B"), cast(Expr::Ident("A"), Type::plain("B")));
    // community has no C-style casts
    assert!(!parse_expr("(Int32)x", file()).1.is_empty());
}

#[test]
fn cast_vs_subtraction() {
    // non-primitive names followed by a minus are subtractions
    assert_eq!(
        expr("(MAX) - 1"),
        Expr::BinOp {
            lhs: Box::new(Expr::Ident("MAX")),
            op: BinOp::Sub,
            rhs: Box::new(int(1)),
        }
    );
    // primitive casts accept a negated operand
    assert_eq!(
        expr("(Int32)-x"),
        cast(
            Expr::UnOp {
                op: UnOp::Neg,
                expr: Box::new(Expr::Ident("x"))
            },
            Type::plain("Int32")
        )
    );
    // parenthesized operand is always fine
    assert_eq!(
        expr("(EInputKey)(-1)"),
        cast(
            Expr::UnOp {
                op: UnOp::Neg,
                expr: Box::new(int(1))
            },
            Type::plain("EInputKey")
        )
    );
}

#[test]
fn c_style_for() {
    assert_eq!(
        stmt("for (i = 0; i < 3; i += 1) { return; }"),
        Stmt::For {
            init: Some(
                Stmt::Expr(
                    Expr::Assign {
                        lhs: Box::new(Expr::Ident("i")),
                        rhs: Box::new(int(0))
                    }
                    .into()
                )
                .into()
            ),
            cond: Some(
                Expr::BinOp {
                    lhs: Box::new(Expr::Ident("i")),
                    op: BinOp::Lt,
                    rhs: Box::new(int(3))
                }
                .into()
            ),
            step: Some(
                Expr::BinOp {
                    lhs: Box::new(Expr::Ident("i")),
                    op: BinOp::AssignAdd,
                    rhs: Box::new(int(1))
                }
                .into()
            ),
            body: Block::single(Stmt::Return(None)),
        }
    );
    assert_eq!(
        stmt("for (var i : Int32 = 0; ; ) {}"),
        Stmt::For {
            init: Some(
                Stmt::Let {
                    name: "i",
                    typ: Some(Type::plain("Int32").into()),
                    value: Some(int(0).into())
                }
                .into()
            ),
            cond: None,
            step: None,
            body: Block::new([]),
        }
    );
    assert_eq!(
        stmt("for (;;) {}"),
        Stmt::For {
            init: None,
            cond: None,
            step: None,
            body: Block::new([])
        }
    );
    // for-in is still accepted
    assert!(matches!(stmt("for x in xs {}"), Stmt::ForIn { .. }));
    // community rejects C-style loops
    assert!(
        !parse_stmt("for (i = 0; i < 3; i += 1) {}", file())
            .1
            .is_empty()
    );
}

#[test]
fn module_and_using() {
    let code = r#"
    @module "My.Mod"
    @using "Other.Mod"
    @wrapMethod(Foo)
    function Bar() -> Void {}
    "#;
    let (res, errs) = parse_module_with(code, file(), RM);
    assert!(errs.is_empty(), "{errs:?}");
    assert_eq!(
        res.unwrap().unwrapped(),
        Module::new(
            Some(Path::new(["My", "Mod"])),
            [
                ItemDecl::new(
                    [],
                    None,
                    ItemQualifiers::empty(),
                    [],
                    Item::Import(Import::All(Path::new(["Other", "Mod"])))
                ),
                ItemDecl::new(
                    [Annotation::new("wrapMethod", [Expr::Ident("Foo")])],
                    None,
                    ItemQualifiers::empty(),
                    [],
                    plain_fn(
                        "Bar",
                        Some(Type::plain("Void")),
                        Some(FunctionBody::Block(Block::new([])))
                    )
                ),
            ]
        )
        .with_dialect(RM)
    );
    assert!(!parse_module("@module \"My.Mod\"", file()).1.is_empty());
}

#[test]
fn annotations_with_strings() {
    assert_eq!(
        decl(r#"@prefix("C.M") function F() -> Bool { return true; }"#).annotations[0],
        Annotation::new("prefix", [Expr::Constant(Constant::String("C.M".into()))])
    );
}
