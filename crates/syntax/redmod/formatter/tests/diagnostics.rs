use std::fmt::Write;

use redscript_redmod_ast::{Constant, Expr, FileId, Item, Statement};
use redscript_redmod_formatter::{PrintErrorKind, print_member_decl, print_module};

#[test]
fn typed_errors_keep_native_expression_spans() {
    let source = "var value: String = \"\";";
    let (module, errors) = redscript_redmod_parser::parse_module(source, FileId::from_i32(0));
    assert!(errors.is_empty());
    let mut module = module.unwrap();
    let mut diagnostics = String::new();
    for (expr, kind) in [
        (Expr::Error, PrintErrorKind::RecoveredExpression),
        (Expr::Omitted, PrintErrorKind::InvalidOmittedArgument),
        (
            Expr::Sequence(Box::new([])),
            PrintErrorKind::InvalidSequence,
        ),
        (
            Expr::ArrayLit(Box::new([])),
            PrintErrorKind::EmptyArrayLiteral,
        ),
        (Expr::Ident("return"), PrintErrorKind::InvalidIdentifier),
        (
            Expr::Constant(Constant::F32(f32::NAN)),
            PrintErrorKind::NonFiniteFloat,
        ),
        (
            Expr::Constant(Constant::F64(f64::INFINITY)),
            PrintErrorKind::NonFiniteDouble,
        ),
        (
            Expr::Constant(Constant::U32(1)),
            PrintErrorKind::UnsignedLiteral,
        ),
        (
            Expr::Constant(Constant::U64(1)),
            PrintErrorKind::UnsignedLiteral,
        ),
    ] {
        let Item::Field(field) = &mut module.items[0].0.item else {
            panic!()
        };
        let value = field.default.as_mut().unwrap();
        value.0 = expr;
        let span = value.1;
        let error = print_module(&module).unwrap_err();
        assert_eq!(error.kind, kind);
        assert_eq!(error.span, Some(span));
        writeln!(
            diagnostics,
            "{kind:?}: {error} [{}..{}]",
            span.start, span.end
        )
        .unwrap();
    }
    insta::assert_snapshot!(diagnostics);
}

#[test]
fn malformed_native_trees_fail_instead_of_losing_nodes() {
    let file = FileId::from_i32(0);
    let (module, errors) = redscript_redmod_parser::parse_module(
        "@using \"Other\"; function F() { for (;;) {} }",
        file,
    );
    assert!(errors.is_empty());
    let mut module = module.unwrap();
    assert_eq!(
        print_member_decl(&module.items[0].0).unwrap_err().kind,
        PrintErrorKind::MemberImport
    );
    module.items.swap(0, 1);
    assert_eq!(
        print_module(&module).unwrap_err().kind,
        PrintErrorKind::ImportsAfterDeclarations
    );
    module.items.pop();
    let Item::Function {
        body: Some(body), ..
    } = &mut module.items[0].0.item
    else {
        panic!()
    };
    let span = body[0].1;
    let Statement::For { init, .. } = &mut body[0].0 else {
        panic!()
    };
    *init = Some(Box::new((Statement::Break, span)));
    let error = print_module(&module).unwrap_err();
    assert_eq!(error.kind, PrintErrorKind::InvalidForInitializer);
    assert_eq!(error.span, Some(span));
}

#[test]
fn identifiers_use_native_context_not_exporter_policy() {
    let file = FileId::from_i32(0);
    let (module, errors) = redscript_redmod_parser::parse_module("var value: name = value;", file);
    assert!(errors.is_empty());
    let mut module = module.unwrap();
    for name in ["name", "cast", "operator", "override", "_value2"] {
        let Item::Field(field) = &mut module.items[0].0.item else {
            panic!()
        };
        field.names[0].0 = name;
        let output = print_module(&module).unwrap();
        assert!(
            redscript_redmod_parser::parse_module(&output, file)
                .1
                .is_empty(),
            "{output}"
        );
    }
    for name in ["", "1name", "a-b", "a.b", "a b", "café", "a\n", "true"] {
        let Item::Field(field) = &mut module.items[0].0.item else {
            panic!()
        };
        let span = field.names[0].1;
        field.names[0].0 = name;
        let error = print_module(&module).unwrap_err();
        assert_eq!(error.kind, PrintErrorKind::InvalidIdentifier);
        assert_eq!(error.span, Some(span));
    }
}
