use std::env;
use std::fmt::Write;

use redscript_ast::{Dialect, File, SourceMap};
use redscript_convert::{ConvertDiagnostic, ConvertOptions, NameArena, convert};
use redscript_formatter::{
    FormatSettings, ParseOptions, parse_document, parse_document_with_options, print_document,
};

fn print_diagnostics(file: &File, diagnostics: &[ConvertDiagnostic]) -> String {
    let mut out = String::new();
    for d in diagnostics {
        writeln!(out, "{}: {d}", file.lookup(d.span.start)).unwrap();
    }
    out
}

/// Converts community `source` to REDmod and returns the printed output.
fn convert_source(source: &str, map: &SourceMap) -> (String, Vec<ConvertDiagnostic>) {
    let arena = NameArena::new();
    let id = map.push_back("tmp.reds", source);
    let file = map.get(id).unwrap();
    // like the driver, community sources are parsed in the 0.5.x compatibility mode
    let opts = ParseOptions::compat(Dialect::Community);
    let (doc, errors) = parse_document_with_options(file.source(), id, opts);
    assert!(errors.is_empty(), "{errors:?}\n{source}");
    let (doc, diagnostics) = convert(doc.unwrap(), &ConvertOptions::default(), &arena);
    let settings = FormatSettings {
        dialect: Dialect::Redmod,
        ..FormatSettings::default()
    };
    (print_document(&doc, &settings).to_string(), diagnostics)
}

#[test]
fn converted_files() {
    insta::glob!("data/*.reds", |path| {
        let current = env::current_dir().unwrap().canonicalize().unwrap();
        let relative = path.strip_prefix(&current).unwrap();
        let sources = SourceMap::from_files([relative]).unwrap();
        let (_, file) = sources.files().next().unwrap();

        let (output, diagnostics) = convert_source(file.source(), &sources);
        let snapshot = format!(
            "{output}\n// diagnostics:\n{}",
            print_diagnostics(file, &diagnostics)
        );
        insta::assert_snapshot!(snapshot);
    });
}

fn comments_of(source: &str) -> Vec<String> {
    let mut comments = vec![];
    let mut rest = source;
    while let Some(start) = rest.find("//").into_iter().chain(rest.find("/*")).min() {
        let tail = &rest[start..];
        let len = if tail.starts_with("//") {
            tail.find('\n').unwrap_or(tail.len())
        } else {
            tail.find("*/").map_or(tail.len(), |i| i + 2)
        };
        comments.push(tail[..len].trim_end().to_owned());
        rest = &tail[len..];
    }
    comments
}

fn assert_parses(source: &str, dialect: Dialect) {
    let map = SourceMap::new();
    let id = map.push_back(format!("check.{}", dialect.extension()), source);
    let (doc, errors) = parse_document(map.get(id).unwrap().source(), id, dialect);
    assert!(
        doc.is_some() && errors.is_empty(),
        "output does not parse as {dialect:?}: {errors:?}\n{source}"
    );
}

#[test]
fn round_trips() {
    insta::glob!("data/*.reds", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        let map = SourceMap::new();

        let (converted, _) = convert_source(&source, &map);
        assert_parses(&converted, Dialect::Redmod);
        for comment in comments_of(&source) {
            assert_eq!(
                converted.matches(comment.as_str()).count(),
                1,
                "comment {comment:?} should appear exactly once in:\n{converted}"
            );
        }

        // the header comments stay above the module header
        if let Some(header) = comments_of(&source).first() {
            assert!(
                converted.find(header.as_str()) < converted.find("module "),
                "{converted}"
            );
        }
    });
}

#[test]
fn redmod_input_is_a_no_op() {
    let source = "function F() -> Int32 {\n  return 1;\n}\n";
    let map = SourceMap::new();
    let id = map.push_back("a.script", source);
    let arena = NameArena::new();
    let (doc, _) = parse_document(map.get(id).unwrap().source(), id, Dialect::Redmod);
    let doc = doc.unwrap();
    let before = doc.module.clone();
    let (doc, diagnostics) = convert(doc, &ConvertOptions::default(), &arena);
    assert_eq!(doc.module, before);
    assert!(diagnostics.is_empty());
}

#[test]
fn diagnostics_have_no_runs_of_spaces() {
    let map = SourceMap::new();
    let mut count = 0;
    for source in [
        include_str!("data/community-passes.reds"),
        include_str!("data/community-typed.reds"),
    ] {
        let (_, diagnostics) = convert_source(source, &map);
        for d in &diagnostics {
            assert!(!d.to_string().contains("  "), "{d}");
            count += 1;
        }
    }
    assert!(count > 0);
}

#[test]
fn converts_0_5_syntax() {
    let source = include_str!("compat/community-compat.reds");
    let map = SourceMap::new();
    let (converted, diagnostics) = convert_source(source, &map);
    assert_parses(&converted, Dialect::Redmod);
    for comment in comments_of(source) {
        // misplaced doc comments are printed as line comments
        let comment = comment.strip_prefix('/').unwrap_or(&comment);
        assert_eq!(
            converted.matches(comment).count(),
            1,
            "comment {comment:?} should appear exactly once in:\n{converted}"
        );
    }
    let snapshot = format!(
        "{converted}\n// diagnostics:\n{}",
        diagnostics
            .iter()
            .map(|d| format!("{d}\n"))
            .collect::<String>()
    );
    insta::assert_snapshot!(snapshot);
}
