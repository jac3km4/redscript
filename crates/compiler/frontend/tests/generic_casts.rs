use redscript_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, Type, TypeInterner, expr_type, infer_from_sources, ir, predef,
};

#[test]
fn class_bounded_casts_preserve_the_target_parameter() {
    for (params, source, target, weak) in [
        ("T extends State", "State", "T", false),
        ("T extends State", "WindowState", "T", false),
        ("T extends State", "wref<State>", "T", true),
        ("T extends State, U extends T", "State", "U", false),
        ("T extends State, U extends State", "U", "T", false),
        ("T extends State", "wref<WindowState>", "T", true),
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "cast.reds",
            format!("class State {{}} class WindowState extends State {{}} func Test<{params}>(value: {source}) -> {} {{ return value as {target}; }}", if weak { format!("wref<{target}>") } else { target.to_owned() }),
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        let diagnostics = reporter.into_reported();
        assert!(
            diagnostics.is_empty(),
            "{params}, {source}: {diagnostics:?}"
        );
        let entry = symbols
            .free_functions()
            .find(|entry| entry.name().to_string() == "Test")
            .unwrap();
        let function = &unit.functions[entry.key()];
        let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
            panic!("expected return");
        };
        let typ = expr_type(expr, function, &symbols).unwrap();
        let pointee = if weak {
            let Type::Data(app) = &typ else {
                panic!("expected weak reference");
            };
            assert_eq!(app.id(), predef::WREF);
            &app.args()[0]
        } else {
            &typ
        };
        assert!(
            matches!(pointee, Type::Ctx(var) if var.name() == target),
            "{typ}"
        );
    }
}
