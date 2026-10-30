use redscript_community_ast::SourceMap;
use redscript_compiler_api::{CompileErrorReporter, SourceMapExt, Symbols, TypeInterner};
use redscript_compiler_frontend::infer_from_sources;

#[test]
fn assignment_diagnostics_highlight_the_offending_operand() {
    let source = "func Value() -> Int32 = 1;\nfunc Test() { Value() = 2; let x = 1; x = true; }";
    let sources = SourceMap::new();
    let file = sources.push_back("assignment.reds", source);
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
    let invalid_place = diagnostics
        .iter()
        .find(|d| d.code() == "INVALID_PLACE")
        .unwrap();
    let mismatch = diagnostics.iter().find(|d| d.code() == "TYPE_ERR").unwrap();
    let file = sources.get(file).unwrap();
    assert_eq!(file.span_contents(invalid_place.span()), "Value()");
    assert_eq!(file.span_contents(mismatch.span()), "true");
}
