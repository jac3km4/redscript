use redscript_community_ast::{self as ast, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, Symbols, TypeInterner, infer_from_sources, process_sources,
};

fn check(source: &str, expected: &[(&str, &str)]) {
    let sources = SourceMap::new();
    let file = sources.push_back("omitted.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let diagnostics = reporter.reported();
    assert_eq!(diagnostics.len(), expected.len(), "{diagnostics:?}");
    for (diagnostic, (code, following)) in diagnostics.iter().zip(expected) {
        assert_eq!(diagnostic.code(), *code, "{diagnostics:?}");
        match *code {
            "REQUIRED_ARGUMENT_OMITTED" => assert_eq!(
                diagnostic.to_string(),
                "a required argument cannot be omitted"
            ),
            "UNSUPPORTED_OMITTED_ARGUMENT" => assert_eq!(
                diagnostic.to_string(),
                "an argument can only be omitted for an optional parameter in a function or method call"
            ),
            _ => {}
        }
        let span = diagnostic.span();
        assert_eq!(span.file, file);
        if *code != "MULTIPLE_MATCHING_OVERLOADS" {
            assert_eq!(span.start, span.end);
        }
        assert!(source[span.start as usize..].starts_with(following));
    }
}

#[test]
fn holes_filter_optionality_but_do_not_supply_types() {
    check(
        "func Target(a: String, b: Int32) {} func Target(opt a: Int32, b: Int32) {} func Run() { Target(, 42); }",
        &[],
    );
    check(
        "func Target(opt a: String, b: Int32) {} func Target(opt a: Int32, b: Int32) {} func Run() { Target(, 42); }",
        &[("MULTIPLE_MATCHING_OVERLOADS", "Target(, 42); }")],
    );
}

#[test]
fn trailing_comma_does_not_add_an_argument() {
    check("func Target(a: Int32) {} func Run() { Target(42,); }", &[]);
}

#[test]
fn required_slots_are_rejected_at_the_hole() {
    check(
        "func Target(a: Int32, b: Int32) {} func Run() { Target(, 42); }",
        &[("REQUIRED_ARGUMENT_OMITTED", ", 42); }")],
    );
    check(
        "func Target(opt a: Int32, b: Int32) {} func Run() { Target(,,); }",
        &[("REQUIRED_ARGUMENT_OMITTED", ",); }")],
    );
}

#[test]
fn unsupported_call_kinds_are_rejected_at_the_hole() {
    for source in [
        "struct Record { let value: Int32; } func Run() { Record(, ); }",
        "func Run(f: (Int32, Int32) -> Int32) { f(, 42); }",
    ] {
        check(source, &[("UNSUPPORTED_OMITTED_ARGUMENT", ", ")]);
    }
}

#[test]
fn omitted_ast_is_not_an_ordinary_value() {
    let sources = SourceMap::new();
    let source = "func Run() { value; }";
    let file = sources.push_back("omitted.reds", source);
    let (module, errors) = redscript_community_parser::parse_module(source, file);
    assert!(errors.is_empty());
    let mut module = module.unwrap();
    let ast::Item::Function(function) = &mut module.items[0].0.item else {
        panic!("expected function");
    };
    let Some(ast::FunctionBody::Block(block)) = &mut function.body else {
        panic!("expected block");
    };
    let ast::Stmt::Expr(expr) = &mut block.stmts[0].0 else {
        panic!("expected expression");
    };
    expr.0 = ast::Expr::Omitted;
    let span = expr.1;
    let interner = TypeInterner::default();
    let evaluator = Evaluator::from_modules([&module]);
    let mut reporter = CompileErrorReporter::default();
    process_sources(
        [module],
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
    );
    assert_eq!(reporter.reported().len(), 1, "{:?}", reporter.reported());
    assert_eq!(
        reporter.reported()[0].code(),
        "UNSUPPORTED_OMITTED_ARGUMENT"
    );
    assert_eq!(reporter.reported()[0].span(), span);
}
