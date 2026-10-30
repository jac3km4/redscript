use redscript_community_ast::SourceMap;
use redscript_community_formatter::{FormatSettings, format_document};

#[test]
fn shared_qualifiers_round_trip() {
    let source = r#"testonly class Example {
  @hint("Editor hint")
  const editable instanceeditable inlined persistent replicated testonly let value: Int32;
}
"#;
    let settings = FormatSettings::default();
    let sources = SourceMap::new();
    let file = sources.push_back("qualifiers.reds", source);
    let (formatted, errors) = format_document(source, file, &settings);
    assert!(errors.is_empty(), "{errors:?}");
    let formatted = formatted.unwrap().to_string();
    assert_eq!(formatted.trim(), source.trim());
    let (reformatted, errors) = format_document(&formatted, file, &settings);
    assert!(errors.is_empty(), "{errors:?}");
    assert_eq!(reformatted.unwrap().to_string().trim(), source.trim());
}
