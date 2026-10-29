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
