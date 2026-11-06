use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::{transpile_sources, validate::OfficialValidator};

pub fn emit(source: &str, dependencies: &str) -> String {
    let sources = SourceMap::new();
    let file = sources.push_back("grouping.reds", source);
    sources.push_back("dependencies.reds", dependencies);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let text = transpile_sources(&sources, &[file], Symbols::with_default_types(), &interner)
        .unwrap()[0]
        .module
        .text
        .clone();
    // Local syntax acceptance is not evidence of official grammar compatibility.
    let (module, errors) = redscript_redmod_parser::parse_module(&text, file);
    assert!(errors.is_empty(), "{errors:?}\n{text}");
    assert!(module.is_some());
    text
}

pub fn validate_official(name: &str, text: &str) {
    let validator = OfficialValidator {
        executable: PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE")),
        timeout: Duration::from_secs(60),
    };
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let directory = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "{name}-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
    // The community frontend needs these signatures; REDmod supplies its own
    // operators and cannot bind the synthetic imports.
    let text = text
        .lines()
        .filter(|line| !line.starts_with("import function Operator"))
        .collect::<Vec<_>>()
        .join("\n");
    std::fs::write(directory.join("r6/scripts/grouping.script"), text).unwrap();
    validator.validate_with_root(&directory, &root).unwrap();
}
