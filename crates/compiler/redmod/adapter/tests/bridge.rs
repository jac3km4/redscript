use redscript_community_ast::{self as ast, FileId};
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner, ir};
use redscript_redmod_adapter::{
    CapabilityError, Error, ParsedModule, parse_file, parse_file_with_metadata,
};

fn file() -> FileId {
    ast::SourceMap::new().push_back("bridge.script", "")
}

fn parsed(source: &str) -> ParsedModule<'_> {
    let (module, errors) = parse_file_with_metadata(source, file());
    assert!(errors.is_empty(), "{source}: {errors:?}");
    module.unwrap()
}

fn body<'a, 'src>(module: &'a ParsedModule<'src>) -> &'a ast::SourceBlock<'src> {
    let ast::Item::Function(function) = &module.module.items[0].0.item else {
        panic!()
    };
    let Some(ast::FunctionBody::Block(body)) = &function.body else {
        panic!()
    };
    body
}

#[test]
fn native_capabilities_fail_closed_with_typed_actionable_errors() {
    for (source, expected) in [
        (
            "function Run() { Use(, 1); }",
            CapabilityError::OmittedArgument,
        ),
        (
            "function Run() { x = y = 1; }",
            CapabilityError::AssignmentValue,
        ),
        (
            "function Run() { return x = 1; }",
            CapabilityError::AssignmentValue,
        ),
        (
            "function Run() { Use(x += 1); }",
            CapabilityError::AssignmentValue,
        ),
        (
            "function Run() { const var x: Int32 = 1; }",
            CapabilityError::ConstLocal,
        ),
        (
            "function Run() { var x, y: Int32 = Make(); }",
            CapabilityError::InitializedLocalGroup,
        ),
        (
            "class Host { var x, y: Int32 = Make(); }",
            CapabilityError::FieldGroup,
        ),
        ("enum Choice: Uint8 { A, B }", CapabilityError::EnumStorage),
        (
            "operator +(a: Int32, b: Int32): Int32;",
            CapabilityError::OperatorDeclaration,
        ),
        (
            "implicit cast(value: Int32): Float;",
            CapabilityError::ConversionDeclaration,
        ),
        (
            "[category = \"Test\"] class Host {}",
            CapabilityError::Metadata,
        ),
        ("function Run(): const Int32;", CapabilityError::ConstReturn),
        (
            "function Run(skip value: Bool);",
            CapabilityError::SkipParameter,
        ),
        (
            "class Host { default inherited = 1; }",
            CapabilityError::DefaultDirective,
        ),
        (
            "class Host { hint missing = \"text\"; }",
            CapabilityError::HintDirective,
        ),
        (
            "export function Run() {}",
            CapabilityError::Modifier(redscript_redmod_ast::Modifier::Export),
        ),
        (
            "function Run() { switch (1) { default: break; case 1: break; } }",
            CapabilityError::SwitchLabelOrder,
        ),
        (
            "function Run() { switch (1) { default: break; default: break; } }",
            CapabilityError::SwitchLabelOrder,
        ),
    ] {
        let (native, syntax_errors) = redscript_redmod_parser::parse_module(source, file());
        assert!(
            native.is_some() && syntax_errors.is_empty(),
            "{source}: {syntax_errors:?}"
        );
        let (module, errors) = parse_file_with_metadata(source, file());
        assert!(module.is_none(), "{source}");
        assert_eq!(errors.len(), 1, "{source}: {errors:?}");
        assert!(
            matches!(errors[0], Error::Capability { kind, .. } if kind == expected),
            "{source}: {errors:?}"
        );
        assert_eq!(errors[0].code(), "REDMOD_CAPABILITY");
        assert_eq!(errors[0].span().file, file());
        assert!(!errors[0].to_string().is_empty());
        assert!(parse_file(source, file()).0.is_none());
    }
}

#[test]
fn syntax_failures_do_not_expose_recovered_shared_modules() {
    let (module, errors) = parse_file_with_metadata("function Run() { Use(1 + ); }", file());
    assert!(module.is_none());
    assert!(!errors.is_empty());
    assert!(errors.iter().all(|error| matches!(error, Error::Parse(_))));
}

#[test]
fn native_only_declarations_and_expressions_are_checked_without_placeholders() {
    let span = ast::Span::new(0, 1, file());
    for (item, expected) in [
        (
            redscript_redmod_ast::Item::Function {
                kind: redscript_redmod_ast::FunctionKind::Operator((
                    redscript_redmod_ast::Operator::Add,
                    span,
                )),
                params: vec![],
                return_type: None,
                const_return: None,
                body: None,
            },
            CapabilityError::OperatorDeclaration,
        ),
        (
            redscript_redmod_ast::Item::Function {
                kind: redscript_redmod_ast::FunctionKind::Cast {
                    implicit: Some(span),
                    span,
                },
                params: vec![],
                return_type: None,
                const_return: None,
                body: None,
            },
            CapabilityError::ConversionDeclaration,
        ),
    ] {
        let module = redscript_redmod_ast::Module {
            path: None,
            items: vec![(
                redscript_redmod_ast::Declaration {
                    annotations: vec![],
                    visibility: None,
                    modifiers: vec![],
                    metadata: vec![],
                    docs: vec![],
                    item,
                },
                span,
            )],
        };
        assert!(
            matches!(redscript_redmod_adapter::bridge::lower(module), Err((kind, _)) if kind == expected)
        );
    }
    for (expr, expected) in [
        (
            redscript_redmod_ast::Expr::Sequence(
                vec![(redscript_redmod_ast::Expr::Ident("x"), span)].into(),
            ),
            CapabilityError::SequenceExpression,
        ),
        (
            redscript_redmod_ast::Expr::Error,
            CapabilityError::RecoveredExpression,
        ),
    ] {
        let mut module = redscript_redmod_parser::parse_module("function Run() {}", file())
            .0
            .unwrap();
        let redscript_redmod_ast::Item::Function {
            body: Some(body), ..
        } = &mut module.items[0].0.item
        else {
            panic!()
        };
        body.push((
            redscript_redmod_ast::Statement::Expr(Box::new((expr, span))),
            span,
        ));
        assert!(
            matches!(redscript_redmod_adapter::bridge::lower(module), Err((kind, _)) if kind == expected)
        );
    }
}

#[test]
fn casts_names_and_weak_types_preserve_source_spans() {
    let source = "function Run(value: weak<Host>, key: name) { Use((Int32)1, Cast<Float>(2)); value as Host; new Host; }";
    let module = parsed(source);
    let ast::Item::Function(function) = &module.module.items[0].0.item else {
        panic!()
    };
    assert!(matches!(
        &function.params[0].0.typ.as_ref().unwrap().0,
        ast::Type::Named { name: "wref", .. }
    ));
    assert!(matches!(
        &function.params[1].0.typ.as_ref().unwrap().0,
        ast::Type::Named { name: "CName", .. }
    ));
    let ast::Stmt::Expr(call) = &body(&module).stmts[0].0 else {
        panic!()
    };
    let ast::Expr::Call { args, .. } = &call.0 else {
        panic!()
    };
    for (arg, expected) in args.iter().zip(["(Int32)1", "Cast<Float>(2)"]) {
        assert_eq!(&source[arg.1.start as usize..arg.1.end as usize], expected);
        assert!(
            matches!(&arg.0, ast::Expr::Call { expr, type_args, args } if expr.0 == ast::Expr::Ident("Cast") && type_args.len() == 1 && args.len() == 1)
        );
    }
    assert!(
        matches!(&body(&module).stmts[1].0, ast::Stmt::Expr(expr) if matches!(expr.0, ast::Expr::DynCast { .. }))
    );
    assert!(
        matches!(&body(&module).stmts[2].0, ast::Stmt::Expr(expr) if matches!(expr.0, ast::Expr::New { .. }))
    );
}

#[test]
fn custom_generic_like_native_types_reach_validation() {
    for source in [
        "function Run(value: ref<Host>) {}",
        "function Run(value: ref<Int32>) {}",
    ] {
        let (module, errors) = parse_file_with_metadata(source, file());
        assert!(module.is_some(), "{source}: {errors:?}");
        assert!(errors.iter().any(|error| matches!(
            error,
            Error::Semantic {
                kind: redscript_redmod_adapter::SemanticError::UnsupportedType,
                ..
            }
        )));
        assert!(errors.iter().all(|error| !matches!(error, Error::Parse(_))));
    }
    // Exercise the native API independently of the parser's type grammar.
    let mut native = redscript_redmod_parser::parse_module("function Run(value: Int32) {}", file())
        .0
        .unwrap();
    let redscript_redmod_ast::Item::Function { params, .. } = &mut native.items[0].0.item else {
        panic!()
    };
    let span = params[0].0.typ.1;
    params[0].0.typ.0 = redscript_redmod_ast::Type::Named {
        name: "Custom",
        args: vec![(redscript_redmod_ast::Type::plain("Int32"), span)],
    };
    let module = redscript_redmod_adapter::bridge::lower(native).unwrap();
    let errors = redscript_redmod_adapter::validate_module(&module.module);
    assert!(matches!(
        errors.as_slice(),
        [Error::Semantic {
            kind: redscript_redmod_adapter::SemanticError::UnsupportedType,
            ..
        }]
    ));
}

#[test]
fn grouped_locals_and_plain_blocks_keep_function_scope_and_execution_order() {
    let source = "function Run() -> Int32 { x = 2; { var x, y: Int32; y = x; } return y; }";
    let module = parsed(source);
    let stmts = &body(&module).stmts;
    for (stmt, span) in &stmts[1..3] {
        let ast::Stmt::Let { name, .. } = stmt else {
            panic!()
        };
        assert_eq!(name.1, *span);
        assert_eq!(&source[span.start as usize..span.end as usize], name.0);
    }
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _, errors) = redscript_redmod_adapter::process_sources(
        [module],
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.into_reported().is_empty());
    let function = unit.functions.values().next().unwrap();
    assert!(matches!(
        function.block.stmts[0],
        ir::Stmt::InitDefault { .. }
    ));
    assert!(matches!(
        function.block.stmts[1],
        ir::Stmt::InitDefault { .. }
    ));
    let (_, errors) =
        parse_file_with_metadata("function Run(x: Int32) { { var y, x: Int32; } }", file());
    assert!(errors.iter().any(|error| matches!(
        error,
        Error::Semantic {
            kind: redscript_redmod_adapter::SemanticError::DuplicateBinding,
            ..
        }
    )));
}

#[test]
fn grouped_fields_keep_name_spans_and_declaration_order() {
    let source = "class Host { var before: Bool; var first, second: Int32; var after: Bool; }";
    let parsed = parsed(source);
    let ast::Item::Class(host) = &parsed.module.items[0].0.item else {
        panic!()
    };
    let names = host
        .items
        .iter()
        .map(|(decl, _)| {
            let ast::Item::Let(field) = &decl.item else {
                panic!()
            };
            field.name.0
        })
        .collect::<Vec<_>>();
    assert_eq!(names, ["before", "first", "second", "after"]);
    for (decl, span) in &host.items[1..3] {
        let ast::Item::Let(field) = &decl.item else {
            panic!()
        };
        assert_eq!(*span, field.name.1);
        assert_eq!(
            &source[span.start as usize..span.end as usize],
            field.name.0
        );
        assert_eq!(
            &source[field.typ.1.start as usize..field.typ.1.end as usize],
            "Int32"
        );
    }
}

#[test]
fn discarded_sequences_keep_written_spans_but_not_value_semantics() {
    let source = "function Run() { First(), x = 1, Last(); }";
    let parsed = parsed(source);
    let statements = &body(&parsed).stmts;
    assert_eq!(statements.len(), 3);
    for ((_, span), expected) in statements.iter().zip(["First()", "x = 1", "Last()"]) {
        assert_eq!(&source[span.start as usize..span.end as usize], expected);
    }
    for (source, expected) in [
        (
            "function Run() { First(), x = y = 1; }",
            CapabilityError::AssignmentValue,
        ),
        (
            "function Run() { First(), Use(, 1); }",
            CapabilityError::OmittedArgument,
        ),
        (
            "function Run() { First(), Use(Before(), [1]); }",
            CapabilityError::UnsafeArrayEvaluation,
        ),
    ] {
        let (parsed, errors) = parse_file_with_metadata(source, file());
        assert!(parsed.is_none());
        assert!(
            matches!(errors.as_slice(), [Error::Capability { kind, .. }] if *kind == expected),
            "{errors:?}"
        );
    }
}

#[test]
fn wrappers_rewrite_only_unshadowed_unqualified_callees() {
    for (source, expected) in [
        (
            "@wrapMethod(Host) function Run() { Run(); }",
            "wrappedMethod",
        ),
        (
            "@wrapMethod(Host) function Run(Run: Callback) { Run(); }",
            "Run",
        ),
        (
            "@wrapMethod(Host) function Run() { Run(); if (true) { var Run: Callback; } }",
            "Run",
        ),
        (
            "@wrapMethod(Host) function Run() { var Run: Callback = Run(); }",
            "Run",
        ),
        ("function Run() { Run(); }", "Run"),
    ] {
        let module = parsed(source);
        let expr = match &body(&module).stmts[0].0 {
            ast::Stmt::Expr(expr)
            | ast::Stmt::Let {
                value: Some(expr), ..
            } => expr,
            _ => panic!(),
        };
        let ast::Expr::Call { expr: callee, .. } = &expr.0 else {
            panic!()
        };
        assert_eq!(callee.0, ast::Expr::Ident(expected));
        assert_eq!(
            &source[callee.1.start as usize..callee.1.end as usize],
            "Run"
        );
    }
    let module = parsed("@wrapMethod(Host) function Run() { this.Run(Run()); Use(Run); }");
    let ast::Stmt::Expr(expr) = &body(&module).stmts[0].0 else {
        panic!()
    };
    let ast::Expr::Call { expr, args, .. } = &expr.0 else {
        panic!()
    };
    assert!(matches!(expr.0, ast::Expr::Member { member: "Run", .. }));
    assert!(
        matches!(&args[0].0, ast::Expr::Call { expr, .. } if expr.0 == ast::Expr::Ident("wrappedMethod"))
    );
    let ast::Stmt::Expr(expr) = &body(&module).stmts[1].0 else {
        panic!()
    };
    assert!(
        matches!(&expr.0, ast::Expr::Call { args, .. } if args[0].0 == ast::Expr::Ident("Run"))
    );
}

#[test]
fn override_metadata_and_same_field_hints_survive_lowering() {
    let source =
        "class Host { override function Run() {} hint value = \"Help\"; var value: Int32; }";
    let module = parsed(source);
    assert_eq!(module.overrides.len(), 1);
    let span = module.overrides[0];
    assert_eq!(&source[span.start as usize..span.end as usize], "Run");
    let ast::Item::Class(class) = &module.module.items[0].0.item else {
        panic!()
    };
    assert_eq!(class.items.len(), 2);
    let hint = &class.items[1].0.annotations[0].0;
    assert_eq!(hint.name, "hint");
    assert!(
        matches!(&*hint.args, [(ast::Expr::Constant(ast::Constant::String(value)), _)] if value == "Help")
    );
}

#[test]
fn hint_metadata_reaches_symbols_and_capability_failures_isolate_files() {
    let sources = ast::SourceMap::new();
    let good_source = "class Host { var value: Int32; hint value = \"Help\"; }";
    let bad_source = "function Unsupported() { for (;;) { Use(, 1); } }";
    let good_file = sources.push_back("good.script", good_source);
    let bad_file = sources.push_back("bad.script", bad_source);
    let (good, errors) = parse_file_with_metadata(good_source, good_file);
    assert!(errors.is_empty(), "{errors:?}");
    let (bad, errors) = parse_file_with_metadata(bad_source, bad_file);
    assert!(bad.is_none());
    assert!(matches!(
        errors.as_slice(),
        [Error::Capability {
            kind: CapabilityError::OmittedArgument,
            ..
        }]
    ));
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = redscript_redmod_adapter::process_sources(
        good.into_iter().chain(bad),
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.into_reported().is_empty());
    assert_eq!(unit.classes.len(), 1);
    let class = symbols[*unit.classes.keys().next().unwrap()]
        .schema()
        .as_aggregate()
        .unwrap();
    let field = class.fields().iter().next().unwrap();
    assert_eq!(field.field().hint().map(AsRef::as_ref), Some("Help"));
    assert!(field.field().properties().is_empty());
}

#[test]
fn hint_validation_accepts_only_a_field_and_one_string_literal() {
    let valid = "class Host { @hint(\"Help\") var value: Int32; }";
    parsed(valid);
    for source in [
        "class Host { @hint() var value: Int32; }",
        "class Host { @hint(1) var value: Int32; }",
        "class Host { @hint(\"one\", \"two\") var value: Int32; }",
        "@hint(\"Help\") function Run() {}",
        "@hint(\"Help\") class Host {}",
    ] {
        let (module, errors) = parse_file_with_metadata(source, file());
        assert!(module.is_some(), "{source}: {errors:?}");
        assert!(
            matches!(
                errors.as_slice(),
                [Error::Semantic {
                    kind: redscript_redmod_adapter::SemanticError::InvalidHint,
                    ..
                }]
            ),
            "{source}: {errors:?}"
        );
    }
}

#[test]
fn synthetic_cast_callee_does_not_hide_target_type_or_operand_from_find_at() {
    for cast in ["(Target)obj", "Cast<Target>(obj)"] {
        let source = format!("function Run() {{ {cast}; }}");
        let module = parsed(&source);
        let ast::Stmt::Expr(expr) = &body(&module).stmts[0].0 else {
            panic!()
        };
        let ast::Expr::Call {
            expr: callee,
            type_args,
            ..
        } = &expr.0
        else {
            panic!()
        };
        assert_eq!(callee.1.start, expr.1.start);
        assert_eq!(callee.1.start, callee.1.end);
        assert_eq!(callee.1.file, file());
        let target_pos = source.find("Target").unwrap() as u32;
        assert_eq!(type_args[0].1.start, target_pos);
        assert_eq!(
            &source[type_args[0].1.start as usize..type_args[0].1.end as usize],
            "Target"
        );
        for pos in target_pos..target_pos + 6 {
            assert!(matches!(
                module.module.find_at(pos),
                Some(ast::QueryResult::Type(ast::Type::Named {
                    name: "Target",
                    ..
                }))
            ));
        }
        let operand_pos = source.find("obj").unwrap() as u32;
        assert!(matches!(
            module.module.find_at(operand_pos),
            Some(ast::QueryResult::Expr(ast::Expr::Ident("obj")))
        ));
    }
}

#[test]
fn unsafe_array_prefix_shapes_are_typed_capability_failures() {
    for statement in [
        "var x: Bool = false && Check([Effect()]);",
        "var x: Bool = true || Check([Effect()]);",
        "var x: array<Int32> = flag ? [Effect()] : existing;",
        "var x: array<Int32> = flag ? existing : [Effect()];",
        "Use(Earlier(), [Effect()]);",
        "Use(value, Nested([Effect()]));",
        "GetReceiver().Use([Effect()]);",
        "receiver.Use([Effect()]);",
        "var x: Int32 = Earlier() + Consume([Effect()]);",
        "var x: Int32 = values[Consume([Effect()])];",
        "receiver.value = [Effect()];",
        "var x: array<Int32> = [Earlier(), Consume([Effect()])];",
        "switch (value) { case Consume([Effect()]): break; }",
        "Use(1, true ? Consume([Effect()]) : 0);",
    ] {
        let source = format!("function Run() {{ {statement} }}");
        let (native, syntax_errors) = redscript_redmod_parser::parse_module(&source, file());
        assert!(syntax_errors.is_empty(), "{source}: {syntax_errors:?}");
        // Direct native bridge callers get the same guard as the parse API.
        assert!(
            matches!(
                redscript_redmod_adapter::bridge::lower(native.unwrap()),
                Err((CapabilityError::UnsafeArrayEvaluation, _))
            ),
            "{source}"
        );
        let (module, errors) = parse_file_with_metadata(&source, file());
        assert!(module.is_none(), "{source}");
        assert!(
            matches!(
                errors.as_slice(),
                [Error::Capability {
                    kind: CapabilityError::UnsafeArrayEvaluation,
                    ..
                }]
            ),
            "{source}: {errors:?}"
        );
        assert_eq!(errors[0].code(), "REDMOD_CAPABILITY");
        assert_eq!(errors[0].span().file, file());
    }
}

#[test]
fn safe_array_initializers_and_eager_shapes_remain_supported() {
    for statement in [
        "var values: array<Int32> = [1, 2];",
        "var values: array<Int32> = [First(), Second()];",
        "values = [Effect()];",
        "return [Effect()];",
        "Use([Effect()]);",
        "Use(1, [Effect()], Later());",
        "var x: Int32 = Consume([Effect()]) + Later();",
        "var x: Int32 = [Effect()][0];",
        "var x: Bool = Check([Effect()]) && Later();",
        "var x: Int32 = Check([Effect()]) ? First() : Second();",
        "if (flag) { var values: array<Int32> = [Effect()]; }",
        "switch (value) { case 1: var values: array<Int32> = [Effect()]; break; }",
    ] {
        let source = format!("function Run() {{ {statement} }}");
        parsed(&source);
    }
}
