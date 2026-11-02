use redscript_community_ast::{FileId, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner, infer_from_sources,
};

pub fn analyze(
    path: &str,
    source: &str,
    check: impl for<'ctx> FnOnce(
        FileId,
        &LoweredCompilationUnit<'ctx>,
        &Symbols<'ctx>,
        &CompileErrorReporter<'ctx>,
    ),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back(path, source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    check(file, &unit, &symbols, &reporter);
}
