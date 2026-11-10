use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, TypeInterner, infer_from_sources, ir,
};

#[test]
fn static_hiding_compares_instantiated_base_parameters() {
    for (base_param, child_param, argument, expected) in [
        ("T", "Int32", "1", "Child"),
        ("[T]", "[Int32]", "[1]", "Child"),
        ("T", "String", "1", "Base"),
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "hiding.reds",
            format!(
                "class Base<T> {{ public static func Pick(value: {base_param}) -> Int32 = 1; }}
                 class Middle<U> extends Base<U> {{}}
                 class Child extends Middle<Int32> {{ public static func Pick(value: {child_param}) -> Int32 = 2; }}
                 func Run() -> Int32 {{ return Child.Pick({argument}); }}"
            ),
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        let codes = reporter
            .reported()
            .iter()
            .map(redscript_compiler_api::Diagnostic::code)
            .collect::<Vec<_>>();
        let expected_codes = if expected == "Base" {
            vec!["CALLING_BASE_STATIC_METHOD"]
        } else {
            vec![]
        };
        assert_eq!(codes, expected_codes, "{base_param}, {child_param}");
        let entry = symbols
            .free_functions()
            .find(|entry| entry.name().to_string() == "Run")
            .unwrap();
        let ir::Stmt::Return(Some(expr), _) =
            unit.functions[entry.key()].block.stmts.back().unwrap()
        else {
            panic!("expected return")
        };
        let ir::Expr::Call { call, .. } = &**expr else {
            panic!("expected call")
        };
        let ir::Call::Static { method, .. } = &**call else {
            panic!("expected static call")
        };
        assert_eq!(method.parent().as_str(), expected);
    }
}
