mod common;

use std::env;

use common::{assert_preserved, ordered_comments};
use redscript_ast::SourceMap;
use redscript_formatter::FormatSettings;

#[track_caller]
fn assert_comments_differ(original: &str, changed: &str) {
    let sources = SourceMap::new();
    let id = sources.push_back("test.reds", original.to_owned());
    assert_ne!(
        ordered_comments(original, id),
        ordered_comments(changed, id),
        "comment comparison did not detect a changed sequence"
    );
}

const COMMENTED_CHAIN: &str = "\
/// documentation
func F() {
    return /* chain prefix */ value.Member.Method();
    // before closing brace
}";

#[test]
fn formatting_preserves_chain_and_closing_brace_comments() {
    assert_preserved(COMMENTED_CHAIN, &FormatSettings::DEFAULT, "test.reds");
}

#[test]
fn comment_comparison_detects_loss_before_closing_brace() {
    assert_comments_differ(
        COMMENTED_CHAIN,
        &COMMENTED_CHAIN.replace("    // before closing brace\n", ""),
    );
}

#[test]
fn comment_comparison_detects_duplicated_chain_prefix() {
    assert_comments_differ(
        COMMENTED_CHAIN,
        &COMMENTED_CHAIN.replace(
            "/* chain prefix */",
            "/* chain prefix */ /* chain prefix */",
        ),
    );
}

#[test]
fn comment_comparison_detects_reordering() {
    assert_comments_differ(
        COMMENTED_CHAIN,
        "\
/// documentation
func F() {
    // before closing brace
    return /* chain prefix */ value.Member.Method();
}",
    );
}

#[test]
fn ordered_comments_include_nested_interpolations_but_not_string_contents() {
    let source = r#"
/// documentation
func F() {
    let plain = "// not a comment /* either */";
    return s"/* not a comment */ \(/* outer */ s"\(/* inner */ value)")";
    // before closing brace
}
"#;
    let sources = SourceMap::new();
    let id = sources.push_back("test.reds", source.to_owned());
    assert_eq!(
        ordered_comments(source, id),
        [
            "/// documentation",
            "/* outer */",
            "/* inner */",
            "// before closing brace",
        ]
    );
    assert_eq!(
        ordered_comments(source, id),
        ordered_comments(&source.replace("    ", "  "), id),
        "comment comparison should ignore surrounding whitespace"
    );
}

#[test]
fn formatted_files() {
    insta::glob!("data/*.reds", |path| {
        let current = env::current_dir().unwrap().canonicalize().unwrap();
        let relative = path.strip_prefix(&current).unwrap();
        let sources = SourceMap::from_files([relative]).unwrap();

        let settings = FormatSettings::default();
        for (_, file) in sources.files() {
            let formatted = assert_preserved(file.source(), &settings, file.path());
            insta::assert_snapshot!(formatted);
        }
    });
}
