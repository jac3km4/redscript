use redscript_redmod_ast::{
    BinOp, CastStyle, Constant, Expr, FileId, FunctionKind, Item, Modifier, ParamModifier,
    Statement, Type,
};
use redscript_redmod_parser::{parse_expr, parse_item_decl, parse_module, parse_stmt};

fn file() -> FileId {
    FileId::from_i32(0)
}

fn expression(code: &str) -> Expr<'_> {
    let (expr, errors) = parse_expr(code, file());
    assert!(errors.is_empty(), "{code}: {errors:?}");
    expr.unwrap()
}

#[test]
fn signed_minima_preserve_unary_precedence_and_spans() {
    for (literal, expected) in [
        ("2147483648", Constant::I32(i32::MIN)),
        ("9223372036854775808l", Constant::I64(i64::MIN)),
    ] {
        for separator in ["", " ", " /* minimum */ ", " /// docs\r\n"] {
            let source = format!("-{separator}{literal}");
            let (tokens, errors) = redscript_redmod_parser::lex(&source, file());
            assert!(errors.is_empty(), "{source}: {errors:?}");
            let tokens = tokens.unwrap();
            let (expr, errors) = redscript_redmod_parser::parse(
                redscript_redmod_parser::parser::expr(),
                &tokens,
                file(),
            );
            assert!(errors.is_empty(), "{source}: {errors:?}");
            assert_eq!(expr.unwrap(), Expr::Constant(expected.clone()));
            assert_eq!(expression(&source), Expr::Constant(expected.clone()));
            let declaration = format!("var value: Int64 = {source};");
            let (decl, errors) = parse_item_decl(&declaration, file());
            assert!(errors.is_empty(), "{errors:?}");
            let Item::Field(field) = decl.unwrap().item else {
                panic!()
            };
            let (expr, span) = field.default.unwrap();
            assert_eq!(expr, Expr::Constant(expected.clone()));
            assert_eq!(&declaration[span.start as usize..span.end as usize], source);
            assert_eq!(span.file, file());
        }
        let source = format!("x--{literal}");
        let Expr::BinOp {
            op: BinOp::Sub,
            rhs,
            ..
        } = expression(&source)
        else {
            panic!()
        };
        assert_eq!(rhs.0, Expr::Constant(expected.clone()));
        let source = format!("!-{literal}");
        let Expr::UnOp {
            op: redscript_redmod_ast::UnOp::Not,
            expr,
        } = expression(&source)
        else {
            panic!()
        };
        assert_eq!(expr.0, Expr::Constant(expected.clone()));
        let source = format!("(Int64)-{literal}");
        let Expr::Cast { expr, .. } = expression(&source) else {
            panic!()
        };
        assert_eq!(expr.0, Expr::Constant(expected));
    }
    assert!(matches!(
        expression("-42"),
        Expr::UnOp {
            op: redscript_redmod_ast::UnOp::Neg,
            ..
        }
    ));
    for source in [
        "2147483648",
        "2147483649",
        "9223372036854775808l",
        "9223372036854775809l",
        "-2147483649",
        "-9223372036854775809l",
        "x-2147483648",
        "x-9223372036854775808l",
        "-(2147483648)",
        "~2147483648",
        "-2147483648.Member",
        "-9223372036854775808l[0]",
    ] {
        assert!(!parse_expr(source, file()).1.is_empty(), "{source}");
    }
}

#[test]
fn native_enums_remain_an_int32_subset() {
    let source = "enum E: Int64 { Min = - /* minimum */ 2147483648, Max = 2147483647, Next, }";
    let (decl, errors) = parse_item_decl(source, file());
    assert!(errors.is_empty(), "{errors:?}");
    let Item::Enum { variants, .. } = decl.unwrap().item else {
        panic!()
    };
    for (index, expected) in [i32::MIN, i32::MAX].into_iter().enumerate() {
        let (value, span) = variants[index].0.value.unwrap();
        assert_eq!(value, expected);
        let spelling = &source[span.start as usize..span.end as usize];
        assert_eq!(
            spelling,
            if index == 0 {
                "- /* minimum */ 2147483648"
            } else {
                "2147483647"
            }
        );
    }
    assert!(variants[2].0.value.is_none());
    for value in [
        "2147483648",
        "-2147483649",
        "2147483648l",
        "1l",
        "-9223372036854775808l",
    ] {
        let source = format!("enum E: Int64 {{ A = {value} }}");
        assert!(!parse_item_decl(&source, file()).1.is_empty(), "{source}");
    }
}

#[test]
fn native_precedence_associativity_and_byte_spans() {
    let Expr::Assign { rhs, .. } = expression("a = b = c") else {
        panic!()
    };
    assert!(matches!(rhs.0, Expr::Assign { .. }));
    let Expr::BinOp {
        op: BinOp::AssignAdd,
        rhs,
        ..
    } = expression("a += b *= c")
    else {
        panic!()
    };
    assert!(matches!(
        rhs.0,
        Expr::BinOp {
            op: BinOp::AssignMul,
            ..
        }
    ));
    let Expr::BinOp {
        op: BinOp::Sub,
        lhs,
        ..
    } = expression("a - b - c")
    else {
        panic!()
    };
    assert!(matches!(lhs.0, Expr::BinOp { op: BinOp::Sub, .. }));
    let Expr::BinOp {
        op: BinOp::Add,
        rhs,
        ..
    } = expression("a + b * c")
    else {
        panic!()
    };
    assert!(matches!(rhs.0, Expr::BinOp { op: BinOp::Mul, .. }));
    assert_eq!((rhs.1.start, rhs.1.end), (4, 9));
    let Expr::BinOp {
        op: BinOp::Or, rhs, ..
    } = expression("a || b && c")
    else {
        panic!()
    };
    assert!(matches!(rhs.0, Expr::BinOp { op: BinOp::And, .. }));
}

#[test]
fn compound_assignments_bind_below_conditionals() {
    for (symbol, expected_op) in [
        ("+=", BinOp::AssignAdd),
        ("-=", BinOp::AssignSub),
        ("*=", BinOp::AssignMul),
        ("/=", BinOp::AssignDiv),
        ("|=", BinOp::AssignBitOr),
        ("&=", BinOp::AssignBitAnd),
    ] {
        let code = format!("x {symbol} flag ? 1 : 2");
        let Expr::BinOp { lhs, op, rhs } = expression(&code) else {
            panic!("expected compound assignment: {code}")
        };
        assert_eq!(lhs.0, Expr::Ident("x"));
        assert_eq!(op, expected_op);
        let Expr::Conditional { cond, then, else_ } = &rhs.0 else {
            panic!("expected conditional RHS: {rhs:?}")
        };
        assert_eq!(cond.0, Expr::Ident("flag"));
        assert_eq!(then.0, Expr::Constant(Constant::I32(1)));
        assert_eq!(else_.0, Expr::Constant(Constant::I32(2)));
        assert_eq!(
            &code[rhs.1.start as usize..rhs.1.end as usize],
            "flag ? 1 : 2"
        );
    }
}

#[test]
fn mixed_assignments_are_right_associative_below_ordinary_operators() {
    let Expr::BinOp {
        lhs,
        op: BinOp::AssignAdd,
        rhs,
    } = expression("x += y = z")
    else {
        panic!("expected outer compound assignment")
    };
    assert_eq!(lhs.0, Expr::Ident("x"));
    let Expr::Assign { lhs, rhs } = rhs.0 else {
        panic!("expected nested assignment")
    };
    assert_eq!(lhs.0, Expr::Ident("y"));
    assert_eq!(rhs.0, Expr::Ident("z"));

    let Expr::Assign { lhs, rhs } = expression("x = y += flag ? 1 : 2") else {
        panic!("expected outer assignment")
    };
    assert_eq!(lhs.0, Expr::Ident("x"));
    let Expr::BinOp {
        lhs,
        op: BinOp::AssignAdd,
        rhs,
    } = rhs.0
    else {
        panic!("expected nested compound assignment")
    };
    assert_eq!(lhs.0, Expr::Ident("y"));
    assert!(matches!(rhs.0, Expr::Conditional { cond, .. } if cond.0 == Expr::Ident("flag")));

    let Expr::BinOp {
        op: BinOp::AssignMul,
        rhs,
        ..
    } = expression("x *= a + b * c")
    else {
        panic!("expected outer compound assignment")
    };
    let Expr::BinOp {
        lhs,
        op: BinOp::Add,
        rhs,
    } = rhs.0
    else {
        panic!("expected addition below multiplication")
    };
    assert_eq!(lhs.0, Expr::Ident("a"));
    assert!(matches!(rhs.0, Expr::BinOp { op: BinOp::Mul, .. }));

    let Expr::Conditional { cond, then, else_ } =
        expression("a + b * c > d && flag ? x += y : z = w")
    else {
        panic!("expected outer conditional")
    };
    assert!(matches!(cond.0, Expr::BinOp { op: BinOp::And, .. }));
    assert!(matches!(
        then.0,
        Expr::BinOp {
            op: BinOp::AssignAdd,
            ..
        }
    ));
    assert!(matches!(else_.0, Expr::Assign { .. }));
}

#[test]
fn casts_are_native_and_retain_written_types() {
    for (code, style, spelling) in [
        ("(Float)value", CastStyle::CStyle, "Float"),
        ("(weak<T>)value", CastStyle::CStyle, "weak<T>"),
        ("Cast<ref<T>>(value)", CastStyle::Intrinsic, "ref<T>"),
    ] {
        let Expr::Cast {
            typ,
            expr,
            style: actual,
        } = expression(code)
        else {
            panic!()
        };
        assert_eq!(actual, style);
        assert_eq!(&code[typ.1.start as usize..typ.1.end as usize], spelling);
        assert_eq!(expr.0, Expr::Ident("value"));
    }
    assert!(
        matches!(expression("(A)(B)x"), Expr::Cast { expr, .. } if matches!(expr.0, Expr::Cast { .. }))
    );
    assert!(matches!(
        expression("(x)+y"),
        Expr::BinOp { op: BinOp::Add, .. }
    ));
    assert!(
        matches!(expression("(x)-y"), Expr::Cast { expr, .. } if matches!(expr.0, Expr::UnOp { .. }))
    );
    assert!(matches!(expression("x as ref<T>"), Expr::DynCast { .. }));
}

#[test]
fn grouped_parameters_locals_and_native_type_spellings() {
    let (decl, errors) = parse_item_decl(
        "function Run(optional const first, second: weak<T>, out third: name) {}",
        file(),
    );
    assert!(errors.is_empty(), "{errors:?}");
    let Item::Function { params, .. } = decl.unwrap().item else {
        panic!()
    };
    assert_eq!(params.len(), 2);
    assert_eq!(
        params[0].0.names.iter().map(|n| n.0).collect::<Vec<_>>(),
        ["first", "second"]
    );
    assert_eq!(
        params[0]
            .0
            .modifiers
            .iter()
            .map(|m| m.0)
            .collect::<Vec<_>>(),
        [ParamModifier::Optional, ParamModifier::Const]
    );
    assert!(matches!(&params[0].0.typ.0, Type::Named { name: "weak", args } if args.len() == 1));
    assert_eq!(params[1].0.typ.0, Type::plain("name"));
    let (stmt, errors) = parse_stmt("const var first, second: Int32[3] = value;", file());
    assert!(errors.is_empty(), "{errors:?}");
    let Statement::Local(local) = stmt.unwrap() else {
        panic!()
    };
    assert!(local.const_.is_some());
    assert_eq!(local.names.len(), 2);
    assert!(matches!(local.typ.0, Type::StaticArray(_, 3)));
}

#[test]
fn omissions_sequences_blocks_and_switch_order_are_not_desugared() {
    let Expr::Call { args, .. } = expression("Use(, value, ,)") else {
        panic!()
    };
    assert_eq!(args.len(), 4);
    for i in [0, 2, 3] {
        assert_eq!(args[i].0, Expr::Omitted);
        assert_eq!(args[i].1.start, args[i].1.end);
    }
    let (stmt, errors) = parse_stmt(
        "for (var i, j: Int32 = 0; ; i += 1, j += 1) { do { continue; } while (false); }",
        file(),
    );
    assert!(errors.is_empty(), "{errors:?}");
    let Statement::For {
        init: Some(init),
        condition: None,
        step: Some(step),
        body,
    } = stmt.unwrap()
    else {
        panic!()
    };
    assert!(matches!(init.0, Statement::Local(_)));
    assert!(matches!(step.0, Expr::Sequence(_)));
    assert!(matches!(body[0].0, Statement::DoWhile { .. }));
    let (stmt, errors) = parse_stmt(
        "switch (value) { default: Ping(); case 1: { break; } default: return; }",
        file(),
    );
    assert!(errors.is_empty(), "{errors:?}");
    let Statement::Switch { labels, .. } = stmt.unwrap() else {
        panic!()
    };
    assert_eq!(labels.len(), 3);
    assert!(
        labels[0].0.value.is_none() && labels[1].0.value.is_some() && labels[2].0.value.is_none()
    );
    assert!(matches!(labels[1].0.body[0].0, Statement::Block(_)));
}

#[test]
fn all_retained_declaration_forms_are_first_class() {
    let code = r#"
        @module "Native.Example"
        @using "Native.Other"
        [category = "Authored", visible = true]
        @custom(1)
        public export virtual constexpr mutable class Host {
            default value = 1;
            hint value = "Help";
            import enum Choice: Uint8 { First = -1, Second }
            import operator+(a, b: Choice): const Choice;
            import implicit cast(value: Choice): Int32;
            override event OnReady() {}
        }
    "#;
    let (module, errors) = parse_module(code, file());
    assert!(errors.is_empty(), "{errors:?}");
    let module = module.unwrap();
    assert_eq!(module.path.unwrap().segments, ["Native", "Example"]);
    let decl = &module.items[1].0;
    assert_eq!(decl.metadata.len(), 2);
    assert_eq!(decl.annotations[0].0.name.0, "custom");
    assert_eq!(
        decl.modifiers.iter().map(|m| m.0).collect::<Vec<_>>(),
        [
            Modifier::Export,
            Modifier::Virtual,
            Modifier::Constexpr,
            Modifier::Mutable
        ]
    );
    let Item::Aggregate { items, .. } = &decl.item else {
        panic!()
    };
    assert!(
        matches!(&items[2].0.item, Item::Enum { storage: Some(_), variants, .. } if variants[0].0.value.as_ref().unwrap().0 == -1)
    );
    assert!(matches!(
        items[3].0.item,
        Item::Function {
            kind: FunctionKind::Operator(_),
            const_return: Some(_),
            ..
        }
    ));
    assert!(matches!(
        items[4].0.item,
        Item::Function {
            kind: FunctionKind::Cast {
                implicit: Some(_),
                ..
            },
            ..
        }
    ));
}

#[test]
fn literals_postfix_and_allocation_preserve_existing_lexer_behavior() {
    assert_eq!(expression(".25f"), Expr::Constant(Constant::F32(0.25)));
    assert_eq!(
        expression(r#""authored\path""#),
        Expr::Constant(Constant::String("authored\\path".into()))
    );
    assert!(matches!(expression("new Target"), Expr::New { .. }));
    assert!(matches!(expression("[1, 2][0].value"), Expr::Member { .. }));
}

#[test]
fn malformed_constructs_do_not_become_valid_through_recovery() {
    for code in [
        "new Target()",
        "new Target(1)",
        "[]",
        "Foo<T>(x)",
        "Cast<T>()",
        "Cast<T>(x, y)",
        "(value: Int32) -> value",
        "a.",
        "\"unterminated",
    ] {
        assert!(!parse_expr(code, file()).1.is_empty(), "{code}");
    }
    for code in [
        "var value = 1;",
        "let value;",
        "if true {}",
        "while true {}",
        "switch value {}",
        "if (let x = y) {}",
        "for value in values {}",
        "for (i = 0 i < 3; i += 1) {}",
    ] {
        assert!(!parse_stmt(code, file()).1.is_empty(), "{code}");
    }
    for code in [
        "var value: ref;",
        "var value: ref<>;",
        "var value: ref<T, U>;",
        "class Generic<T> {}",
        "function Generic<T>() {}",
        "@module \"invalid-path\"",
        "@module \"has\\nEscape\"",
    ] {
        assert!(!parse_module(code, file()).1.is_empty(), "{code}");
    }
}
