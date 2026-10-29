use redscript_ast::SourceMap;
use redscript_compiler_api::{CompileErrorReporter, SourceMapExt, Symbols, TypeInterner};
use redscript_compiler_frontend::infer_from_sources;

fn diagnostics_for(source: &str) -> Vec<(String, String)> {
    let sources = SourceMap::new();
    sources.push_back("constructors.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    reporter
        .into_reported()
        .iter()
        .map(|diagnostic| (diagnostic.code().to_owned(), diagnostic.to_string()))
        .collect()
}

#[test]
fn new_on_value_types_preserves_argument_advice_without_class_error() {
    for declaration in [
        "struct Value { let value: Int32; }",
        "@neverRef() class Value { let value: Int32; }",
    ] {
        for args in ["", "42"] {
            let diagnostics = diagnostics_for(&format!(
                "{declaration} func Test() {{ new Value({args}); }}"
            ));
            assert_eq!(diagnostics, [(
                "INVALID_NEW_USE".into(),
                "`Value` cannot be constructed with the `new` operator; remove `new` and keep the constructor arguments".into(),
            )]);
        }
    }
    let diagnostics = diagnostics_for("class Value {} func Test() { new Value(42); }");
    assert_eq!(
        diagnostics,
        [(
            "INVALID_NEW_USE".into(),
            "class constructors do not accept positional arguments".into(),
        )]
    );
}

#[test]
fn private_constructor_always_explains_the_error() {
    for (hint, suffix) in [
        ("", ""),
        (
            "Use Value.Create() instead",
            "; help: Use Value.Create() instead",
        ),
    ] {
        let diagnostics = diagnostics_for(&format!(
            r#"@privateConstructor("{hint}") class Value {{
                public static func Create() -> Value {{ return new Value(); }}
            }}
            func Test() {{ new Value(); Value.Create(); }}"#,
        ));
        assert_eq!(
            diagnostics,
            [(
                "PRIVATE_CONSTRUCTOR".into(),
                format!("constructor of `Value` is private and cannot be called here{suffix}"),
            )]
        );
    }
}

#[test]
fn native_struct_advice_uses_valid_zero_argument_constructor() {
    let sources = SourceMap::new();
    sources.push_back(
        "native.reds",
        "native struct Native {}\nfunc Test() { Native(1); Native(); }",
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let diagnostics = reporter.into_reported();
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "NON_FULLY_DEFINED_CTR");
    insta::assert_snapshot!(diagnostics[0].display(&sources).unwrap());
}
