use redscript_ast::SourceMap;
use redscript_formatter::{FormatSettings, format_document};
use redscript_parser::parse_module;

fn format(source: &str, settings: &FormatSettings) -> String {
    let sources = SourceMap::new();
    let id = sources.push_back("test.reds", source.to_owned());
    let (formatted, errors) = format_document(source, id, settings);
    assert!(errors.is_empty(), "formatting failed: {errors:?}");
    formatted.expect("missing formatted document").to_string()
}

fn assert_preserved(source: &str, settings: &FormatSettings) -> String {
    let formatted = format(source, settings);
    assert_eq!(formatted, format(&formatted, settings));
    let sources = SourceMap::new();
    let id = sources.push_back("test.reds", source.to_owned());
    let (original, errors) = parse_module(source, id);
    assert!(errors.is_empty(), "original failed to parse: {errors:?}");
    let (result, errors) = parse_module(&formatted, id);
    assert!(errors.is_empty(), "formatted failed to parse: {errors:?}");
    assert_eq!(original.unwrap().unwrapped(), result.unwrap().unwrapped());
    formatted
}

#[test]
fn nested_assignments_keep_parentheses() {
    for expression in [
        "(a = b) + c",
        "a + (b = c)",
        "(a = b).Member",
        "(a = b).Method()",
        "(a = b)[c]",
        "(a = b)(c)",
        "!(a = b)",
        "(a = b) as Bool",
        "(a = b) ? c : d",
        "(a = b) = c",
        "a = (b = c)",
        "a = b ? c : d",
    ] {
        let source = format!("func F() {{ return {expression}; }}");
        assert_preserved(&source, &FormatSettings::default());
    }
}

#[test]
fn conditional_conditions_keep_parentheses() {
    for expression in [
        "(a ? b : c) ? d : e",
        "((a ? b : c) ? d : e) ? f : g",
        "a ? (b ? c : d) : e",
        "a ? b : (c ? d : e)",
        "a + b ? c : d",
        "a as Bool ? b : c",
        "!a ? b : c",
    ] {
        let source = format!("func F() {{ return {expression}; }}");
        assert_preserved(&source, &FormatSettings::default());
    }
}

#[test]
fn chain_wrapping_counts_the_base_expression() {
    for (max_width, breaks) in [(24, false), (23, true)] {
        let settings = FormatSettings {
            max_width,
            ..FormatSettings::default()
        };
        let formatted = assert_preserved("func F() { abcdefghijklmnop.Value; }", &settings);
        assert_eq!(
            formatted.contains("abcdefghijklmnop\n    .Value"),
            breaks,
            "{formatted}"
        );
    }
}

#[test]
fn parenthesized_chain_prefixes_are_emitted_once() {
    for expression in [
        "a && /* keep */ (b || c)",
        "a * /* keep */ (b + c)",
        "/* keep */ (a + b).Method()",
        "/* keep */ a.Member.Method()",
    ] {
        let source = format!("func F() {{ return {expression}; }}");
        let formatted = assert_preserved(&source, &FormatSettings::default());
        assert_eq!(formatted.matches("/* keep */").count(), 1, "{formatted}");
    }
}

#[test]
fn pattern_prefixes_are_preserved() {
    let formatted = assert_preserved(
        "func F() { if let /* binding explanation */ bound = value {} }",
        &FormatSettings::default(),
    );
    assert_eq!(formatted.matches("/* binding explanation */").count(), 1);
}

#[test]
fn inline_line_comments_keep_their_delimiters() {
    for comment in [
        "// literal */ marker",
        "// unmatched /* marker",
        "// ordinary comment",
    ] {
        let source = format!("func F() {{ return {comment}\n true; }}");
        let formatted = assert_preserved(&source, &FormatSettings::default());
        assert!(formatted.contains(&format!("{comment}\n")), "{formatted}");
    }
}

#[test]
fn aggregate_patterns_keep_explicit_fields() {
    for pattern in [
        "Outer { inner: Inner { value } }",
        "Outer { inner: value? }",
        "Outer { inner: [first, second] }",
        "Outer { inner: value as Int32 }",
        "Outer { inner: renamed }",
        "Outer { inner }",
    ] {
        let source = format!("func F() {{ if let {pattern} = value {{}} }}");
        let formatted = assert_preserved(&source, &FormatSettings::default());
        assert!(formatted.contains(pattern), "{formatted}");
    }
}

#[test]
fn doubles_keep_their_suffix() {
    for trunc_sig_digits in [None, Some(3)] {
        let settings = FormatSettings {
            trunc_sig_digits,
            ..FormatSettings::default()
        };
        let formatted = assert_preserved("func F() { let value = 1.25d; }", &settings);
        assert!(formatted.contains("1.25d"), "{formatted}");
    }
}
