use redscript_ast::SourceMap;
use redscript_compiler_api::{CompileErrorReporter, SourceMapExt, Symbols, TypeInterner};
use redscript_compiler_frontend::infer_from_sources;

#[test]
fn native_struct_advice_uses_valid_zero_argument_constructor() {
    let sources = SourceMap::new();
    sources.push_back(
        "native.reds",
        "native struct Native {}\nfunc Test() { Native(1); Native(); }",
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let diagnostics = reporter.into_reported();
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "NON_FULLY_DEFINED_CTR");
    assert!(
        diagnostics[0]
            .to_string()
            .ends_with("constructed without arguments: `Native()`")
    );
}
