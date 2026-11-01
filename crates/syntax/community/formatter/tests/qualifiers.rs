use redscript_community_ast::SourceMap;
use redscript_community_formatter::{FormatSettings, format_document};

#[test]
fn shared_qualifiers_round_trip() {
    let source = r#"testonly class Example {
  @hint("Editor hint")
  const editable instanceeditable inlined persistent replicated testonly let value: Int32;
}

native func Lazy(lhs: Bool, skip rhs: Bool) -> Bool;

func Locals() {
  const value: Int32;
  const inferred = 1;
  const initialized: Int32 = 2;
}

native implicit func Cast(value: Uint8) -> Float;

native explicit func Cast(value: Int8) -> Float;
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

#[test]
fn const_let_declarations_are_rejected() {
    for declaration in [
        "const let thing = 1;",
        "const let thing: Int32 = 1;",
        "const let thing: Int32;",
    ] {
        let source = format!("func Locals() {{ {declaration} }}");
        let sources = SourceMap::new();
        let file = sources.push_back("const.reds", source.as_str());
        let (_, errors) = format_document(&source, file, &FormatSettings::default());
        assert!(!errors.is_empty(), "{source}");
    }
}
