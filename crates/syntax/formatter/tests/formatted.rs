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

const NESTED_INTERPOLATION_COMMENTS: &str = r#"
/// documentation
func F() {
    let plain = "// not a comment /* either */";
    return s"/* not a comment */ \(/* outer */ s"\(/* inner */ value)")";
    // before closing brace
}
"#;

#[test]
fn formatting_preserves_nested_interpolation_comments() {
    assert_preserved(
        NESTED_INTERPOLATION_COMMENTS,
        &FormatSettings::DEFAULT,
        "nested-interpolation.reds",
    );
}

#[test]
fn ordered_comments_include_nested_interpolations_but_not_string_contents() {
    let source = NESTED_INTERPOLATION_COMMENTS;
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
fn literal_widths_count_signs_suffixes_and_escapes() {
    let mut cases = String::new();
    for literal in [
        "0",
        "-1",
        "-2147483648",
        "0l",
        "-1l",
        "-9223372036854775808l",
        "0u",
        "123u",
        "0ul",
        "123ul",
        "true",
        "false",
        r#""\n\t""#,
        r#"n"\n\t""#,
        r#"r"\n\t""#,
        r#"t"\n\t""#,
        r#"s"\n\t""#,
    ] {
        for (extra_width, breaks) in [(-1, true), (0, false), (1, false)] {
            let settings = FormatSettings {
                max_width: (literal.len() as i32 + 2 + extra_width) as u16,
                ..FormatSettings::default()
            };
            let source = format!("func F() {{ Call({literal}); }}");
            let formatted = assert_preserved(&source, &settings, "test.reds");
            assert_eq!(formatted.contains("Call(\n"), breaks, "{formatted}");
            cases.push_str(&format!(
                "=== literal: {literal}, max_width: {} ===\n{formatted}\n",
                settings.max_width
            ));
        }
    }
    insta::assert_snapshot!(cases);
}

#[test]
fn parameter_widths_count_qualifiers() {
    let mut cases = String::new();
    for parameter in [
        "x: Int32",
        "out x: Int32",
        "opt x: Int32",
        "const x: Int32",
        "out opt const x: Int32",
    ] {
        for (extra_width, breaks) in [(-1, true), (0, false), (1, false)] {
            let settings = FormatSettings {
                max_width: (parameter.len() as i32 + extra_width) as u16,
                ..FormatSettings::default()
            };
            let source = format!("func F({parameter}) {{}}");
            let formatted = assert_preserved(&source, &settings, "test.reds");
            assert_eq!(formatted.contains("func F(\n"), breaks, "{formatted}");
            cases.push_str(&format!(
                "=== parameter: {parameter}, max_width: {} ===\n{formatted}\n",
                settings.max_width
            ));
        }
    }
    insta::assert_snapshot!(cases);
}

#[test]
fn operator_chain_widths_count_both_spaces() {
    let mut cases = String::new();
    for (max_width, breaks) in [(14, true), (15, false), (16, false)] {
        let settings = FormatSettings {
            max_width,
            ..FormatSettings::default()
        };
        let formatted = assert_preserved("func F() { a + b + c + d; }", &settings, "test.reds");
        assert_eq!(formatted.contains("a\n    + b"), breaks, "{formatted}");
        cases.push_str(&format!(
            "=== operator chain: a + b + c + d, max_width: {max_width} ===\n{formatted}\n"
        ));
    }
    insta::assert_snapshot!(cases);
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
