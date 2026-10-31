use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, LoweredFunction, Symbols, TypeInterner,
    infer_from_sources, ir,
};

fn analyze(
    source: &str,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.push_back("shared.reds", source);
    sources.populate_boot_lib();
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
        .map(|d| d.code().to_owned())
        .collect::<Vec<_>>();
    test(&unit, &symbols, &codes);
}

fn function<'a, 'ctx>(
    unit: &'a LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    name: &str,
) -> &'a LoweredFunction<'ctx> {
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == name)
        .unwrap();
    &unit.functions[entry.key()]
}

fn returned<'a, 'ctx>(function: &'a LoweredFunction<'ctx>) -> &'a ir::Expr<'ctx> {
    let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
        panic!("expected return")
    };
    expr
}

#[test]
fn imported_callback_prototypes_are_shared_but_not_native_or_implemented() {
    analyze(
        "native class Listener { protected cb func OnReady(); }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let (id, _) = symbols
                .types()
                .find(|(id, _)| id.to_string() == "Listener")
                .unwrap();
            let method = symbols[id]
                .schema()
                .as_aggregate()
                .unwrap()
                .methods()
                .iter()
                .next()
                .unwrap();
            assert!(method.func().flags().is_callback());
            assert!(!method.func().flags().is_native());
            assert!(method.func().flags().is_unimplemented());
            assert!(unit.classes[&id].methods.is_empty());
        },
    );
    for source in [
        "class Listener { protected cb func OnReady(); }",
        "native importonly class Listener { protected cb func OnReady(); }",
        "native class Listener { protected func OnReady(); }",
    ] {
        analyze(source, |_, _, codes| {
            assert!(codes.iter().any(|code| code == "MISSING_BODY"), "{codes:?}");
        });
    }
}

