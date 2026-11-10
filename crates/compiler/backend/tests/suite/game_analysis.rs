//! Analyze a local game without writing to its installation.
//! Set REDSCRIPT_GAME_ROOT and run:
//! cargo test -p redscript-compiler-backend --test game_analysis -- --ignored --nocapture
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::{env, fs, thread};

use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{SourceMapExt, TypeFlagRegistry};
use redscript_compiler_backend::CompilationInputs;
use redscript_compiler_frontend::{CompileErrorReporter, TypeInterner, infer_from_sources, predef};
use redscript_io::ScriptBundle;

#[test]
#[ignore = "requires REDSCRIPT_GAME_ROOT and a local game installation"]
fn game_analysis() {
    let root = PathBuf::from(env::var_os("REDSCRIPT_GAME_ROOT").expect("set REDSCRIPT_GAME_ROOT"));
    thread::Builder::new()
        .name("game-analysis".into())
        .stack_size(256 * 1024 * 1024)
        .spawn(move || analyze(&root))
        .unwrap()
        .join()
        .unwrap();
}

fn analyze(root: &Path) {
    let mut paths = Vec::new();
    for source_root in [root.join("r6/scripts"), root.join("red4ext/plugins")] {
        collect_sources(&source_root, &mut paths);
    }
    paths.sort();
    let sources = SourceMap::from_files(paths).unwrap();
    sources.populate_boot_lib();
    eprintln!("analyzing {} sources including boot library", sources.len());
    let bytes = fs::read(root.join("r6/cache/final.redscripts")).unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let interner = TypeInterner::default();
    let symbols =
        CompilationInputs::load_without_mapping(&bundle, &interner, &TypeFlagRegistry::default())
            .unwrap();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
    let mut codes = BTreeMap::new();
    let mut ambiguities = BTreeMap::from([("IsA", 0), ("GetClassName", 0), ("IsExactlyA", 0)]);
    for diagnostic in reporter.reported() {
        *codes.entry(diagnostic.code()).or_insert(0) += 1;
        if diagnostic.code() == "MULTIPLE_MATCHING_OVERLOADS" {
            let message = diagnostic.to_string();
            for (name, count) in &mut ambiguities {
                if message.contains(&format!("func {name}(")) {
                    *count += 1;
                }
            }
        }
    }
    eprintln!("diagnostic counts: {codes:?}");
    eprintln!("root-method ambiguities: {ambiguities:?}");
    assert!(
        ambiguities.values().all(|count| *count == 0),
        "{ambiguities:?}"
    );
    for name in ambiguities.keys() {
        let methods = symbols
            .query_methods_by_name(predef::ISCRIPTABLE, name)
            .collect::<Vec<_>>();
        assert_eq!(methods.len(), 1, "{name}: {methods:?}");
        assert!(methods[0].func().flags().is_final());
        assert_eq!(
            methods[0].func().overriden().unwrap().parent(),
            predef::ISERIALIZABLE
        );
    }
}

fn collect_sources(root: &Path, paths: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(root).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if entry.file_type().unwrap().is_dir() {
            collect_sources(&path, paths);
        } else if path.extension().is_some_and(|ext| ext == "reds") {
            paths.push(path);
        }
    }
}
