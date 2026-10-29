use redscript_ast::SourceMap;
use redscript_compiler_api::{CompileErrorReporter, SourceMapExt, Symbols, TypeInterner};
use redscript_compiler_frontend::infer_from_sources;

#[test]
fn reports_distinct_accepted_counts_and_actual_count() {
    let sources =
        SourceMap::from_files(["tests/data/overload-argument-counts/counts.reds"]).unwrap();
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let mut diagnostics = reporter.into_reported();
    diagnostics.sort_by_key(|diagnostic| diagnostic.span().start);
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code() == "INVALID_ARG_COUNT"),
        "unexpected diagnostics: {diagnostics:?}"
    );
    let messages: Vec<_> = diagnostics.iter().map(ToString::to_string).collect();
    assert_eq!(
        messages,
        [
            "invalid number of arguments, expected 0 or 2 arguments, found 1",
            "invalid number of arguments, expected 0 or 2 arguments, found 3",
            "invalid number of arguments, expected between 0 and 1 or between 3 and 4 arguments, found 2",
            "invalid number of arguments, expected between 0 and 1 or between 3 and 4 arguments, found 5",
            "invalid number of arguments, expected between 1 and 3 arguments, found 0",
            "invalid number of arguments, expected between 1 and 3 arguments, found 4",
            "invalid number of arguments, expected between 0 and 2 arguments, found 3",
            "invalid number of arguments, expected 1 arguments, found 0",
            "invalid number of arguments, expected 2 arguments, found 1",
            "invalid number of arguments, expected 2 arguments, found 1",
        ]
    );
}
