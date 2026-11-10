use redscript_redmod_ast::{Expr, FileId, FunctionKind, Item, Modifier, Span, Statement};
use redscript_redmod_parser::{Error, Token, parse_module};

fn file() -> FileId {
    FileId::from_i32(7)
}

fn function_name<'src>(item: &Item<'src>) -> Option<&'src str> {
    match item {
        Item::Function {
            kind: FunctionKind::Function((name, _)),
            ..
        } => Some(name),
        _ => None,
    }
}

#[test]
fn true_source_eof_in_source_and_batch_entry_points() {
    for trailing in [" \n\t", " // comment", " /* comment */\r\n"] {
        let source = format!("function Test({trailing}");
        let eof = Span::new(source.len() as u32, source.len() as u32, file());
        for errors in [
            parse_module(&source, file()).1,
            redscript_redmod_parser::parse_modules([(file(), source.as_str())]).1,
        ] {
            assert!(errors.iter().any(|error| error.span() == eof), "{errors:?}");
        }
        for errors in [
            redscript_redmod_parser::parse_expr(trailing, file()).1,
            redscript_redmod_parser::parse_stmt(trailing, file()).1,
            redscript_redmod_parser::parse_item(trailing, file()).1,
            redscript_redmod_parser::parse_item_decl(trailing, file()).1,
        ] {
            assert!(
                errors
                    .iter()
                    .any(|error| error.span().start == trailing.len() as u32)
            );
        }
        assert!(parse_module(trailing, file()).1.is_empty());
    }
}

#[test]
fn doc_comments_are_trivia_and_token_apis_are_native() {
    for source in [
        "function Run() { /// Explain next call\n Use(); }",
        "function Run() { Use(); /// trailing body\n } /// trailing file",
        "class Host { /// trailing class\n } /// trailing file",
        "/// only documentation",
        "function Run(/// parameter\n value: /// type\n Int32) { return /// operand\n value; }",
        "function Run() { switch (1) { case 1: /// case body\n Use(); /// before default\n default: return; /// trailing case\n } }",
    ] {
        let (module, errors) = parse_module(source, file());
        assert!(errors.is_empty(), "{source}: {errors:?}");
        assert!(module.is_some());
    }
    for source in [
        "/// leading\n Use() /// trailing",
        "Use(/// arg\n 1, 2)",
        "1 /// operator\n + 2",
        "(Int32) /// operand\n value",
    ] {
        let (expr, errors) = redscript_redmod_parser::parse_expr(source, file());
        assert!(errors.is_empty(), "{errors:?}");
        let tokens = redscript_redmod_parser::lex(source, file()).0.unwrap();
        let (actual, errors) = redscript_redmod_parser::parse(
            redscript_redmod_parser::parser::expr(),
            &tokens,
            file(),
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(actual, expr);
    }
    let source = "class Host { /// attached\npublic /// trivia\noverride function Run() {} }";
    let (module, errors) = parse_module(source, file());
    assert!(errors.is_empty(), "{errors:?}");
    let module = module.unwrap();
    let Item::Aggregate { items, .. } = &module.items[0].0.item else {
        panic!()
    };
    assert_eq!(items[0].0.docs, ["/// attached"]);
    assert_eq!(items[0].0.modifiers[0].0, Modifier::Override);
    assert_eq!(
        items[0].1.start as usize,
        source.find("/// attached").unwrap()
    );
}

#[test]
fn recovery_preserves_sibling_declarations_and_statements() {
    for malformed in [
        "function Broken( ;",
        "var bad = 1;",
        "var bad: Int32",
        "function Broken() { var bad = 1; }",
        "function Broken() { Before();",
        "junk (nested; values) { garbage; }",
    ] {
        let source = format!("function Before() {{}} {malformed} function After() {{}}");
        let (module, errors) = parse_module(&source, file());
        assert!(!errors.is_empty(), "{source}");
        let module = module.unwrap();
        let names = module
            .items
            .iter()
            .filter_map(|(d, _)| function_name(&d.item))
            .collect::<Vec<_>>();
        assert!(
            names.contains(&"Before") && names.contains(&"After"),
            "{source}: {names:?}"
        );
    }
    for malformed in [
        "var bad = 1;",
        "var bad: = f((a), [1, 2]);",
        "if missing { f(); }",
    ] {
        let source = format!("function Test() {{ Before(); {malformed} After(); return; }}");
        let (module, errors) = parse_module(&source, file());
        assert!(!errors.is_empty());
        let module = module.unwrap();
        let Item::Function {
            body: Some(body), ..
        } = &module.items[0].0.item
        else {
            panic!()
        };
        assert!(matches!(body.last().unwrap().0, Statement::Return(None)));
        assert!(body.len() >= 4, "{source}: {body:?}");
    }
    let source = "class Test { var first: Int32; var broken: Int32 var after: Int32; }";
    let (module, errors) = parse_module(source, file());
    assert!(!errors.is_empty());
    let module = module.unwrap();
    let Item::Aggregate { items, .. } = &module.items[0].0.item else {
        panic!()
    };
    assert_eq!(items.len(), 2);
    assert!(matches!(&items[1].0.item, Item::Field(field) if field.names[0].0 == "after"));
}

#[test]
fn incomplete_blocks_preserve_declaration_prefixes_and_sibling_docs() {
    for prefix in [
        "/// documentation\n",
        "@if(true) ",
        "static ",
        "final ",
        "abstract ",
        "const ",
        "exec ",
        "import ",
        "importonly ",
    ] {
        let source = format!("function Before() {{ BeforeCall(); {prefix}function After() {{}}");
        let (module, errors) = parse_module(&source, file());
        assert!(!errors.is_empty(), "{source}");
        let module = module.unwrap();
        assert_eq!(module.items.len(), 2, "{source}: {module:?}");
        assert_eq!(function_name(&module.items[1].0.item), Some("After"));
        if prefix.starts_with("///") {
            assert_eq!(module.items[1].0.docs, ["/// documentation"]);
        }
    }
}

#[test]
fn lexer_retains_recoverable_tokens_and_precise_invalid_symbol_ranges() {
    for source in [
        "999999999999999999999999999999; after",
        "\"unterminated",
        "'unterminated",
        "/* outer /* inner",
        "\"incomplete\\",
    ] {
        let (tokens, errors) = redscript_redmod_parser::lex(source, file());
        assert!(!errors.is_empty(), "{source}");
        let tokens = tokens.unwrap();
        assert!(!tokens.is_empty() || source.starts_with("/*"), "{source}");
        if source.ends_with("after") {
            assert_eq!(tokens.last().unwrap().0, Token::Ident("after"));
        }
    }
    let (tokens, errors) = redscript_redmod_parser::lex("1 § 猫 2", file());
    assert_eq!(
        tokens.unwrap().iter().map(|t| &t.0).collect::<Vec<_>>(),
        [&Token::Int(1), &Token::Int(2)]
    );
    assert_eq!(
        errors.iter().map(Error::span).collect::<Vec<_>>(),
        [Span::new(2, 4, file()), Span::new(5, 8, file())]
    );
}

#[test]
fn no_parser_side_semantic_rewriting() {
    let source = "@wrapMethod(Target) override function Run() { Run(); }";
    let (decl, errors) = redscript_redmod_parser::parse_item_decl(source, file());
    assert!(errors.is_empty(), "{errors:?}");
    let decl = decl.unwrap();
    let Item::Function {
        body: Some(body), ..
    } = decl.item
    else {
        panic!()
    };
    let Statement::Expr(expr) = &body[0].0 else {
        panic!()
    };
    assert!(matches!(&expr.0, Expr::Call { expr, .. } if expr.0 == Expr::Ident("Run")));
    // Override legality and annotation lowering are the compiler bridge's responsibility.
    assert_eq!(decl.modifiers[0].0, Modifier::Override);
}
