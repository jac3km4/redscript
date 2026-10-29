use redscript_ast::SourceMap;
use redscript_compiler_api::{CompileErrorReporter, SourceMapExt, Symbols, TypeInterner};
use redscript_compiler_frontend::infer_from_sources;

#[test]
fn reports_matching_candidates_and_callee_span() {
    let sources =
        SourceMap::from_files(["tests/data/ambiguous-overloads/unrelated-classes.reds"]).unwrap();
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
    assert_eq!(
        diagnostics.len(),
        1,
        "unexpected diagnostics: {diagnostics:?}"
    );
    let diagnostic = &diagnostics[0];
    assert_eq!(diagnostic.code(), "MULTIPLE_MATCHING_OVERLOADS");
    let span = diagnostic.span();
    let file = sources.get(span.file).unwrap();
    assert_eq!(file.span_contents(span), "Choose");
    insta::assert_snapshot!(diagnostic.display(&sources).unwrap());
}
