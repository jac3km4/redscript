#![allow(dead_code)]

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::{TranspileDiagnostic, transpile_sources};

pub fn try_emit(source: &str, dependencies: &str) -> Result<String, Vec<TranspileDiagnostic>> {
    let sources = SourceMap::new();
    let file = sources.push_back("test.reds", source);
    sources.push_back("dependencies.reds", dependencies);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(&sources, &[file], Symbols::with_default_types(), &interner)
        .map(|output| output[0].module.text.clone())
}

pub fn try_emit_source(source: &str) -> Result<String, Vec<TranspileDiagnostic>> {
    try_emit(source, "")
}

pub fn emit(source: &str, dependencies: &str) -> String {
    try_emit(source, dependencies).unwrap()
}

pub fn emit_parsed(source: &str, dependencies: &str) -> String {
    let text = emit(source, dependencies);
    let sources = SourceMap::new();
    let file = sources.push_back("emitted.script", text.clone());
    // Local syntax acceptance is not evidence of official grammar compatibility.
    let (module, errors) = redscript_redmod_parser::parse_module(&text, file);
    assert!(errors.is_empty(), "{errors:?}\n{text}");
    assert!(module.is_some());
    text
}

/// The frontend needs synthetic signatures; REDmod supplies its own operators.
pub fn without_synthetic_operator_imports(text: &str) -> String {
    text.lines()
        .filter(|line| !line.starts_with("import function Operator"))
        .collect::<Vec<_>>()
        .join("\n")
}
