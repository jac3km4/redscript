use redscript_community_ast::{self as ast, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{CapabilityError, Error, parse_file_with_metadata, process_sources};

#[derive(Debug, PartialEq, Eq)]
struct ResolvedField {
    name: String,
    properties: Vec<(String, String)>,
    hint: Option<String>,
}

fn resolve_fields(source: &str, aggregate: &str) -> Vec<ResolvedField> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("metadata.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, file)| {
            let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    symbols[interner.intern(aggregate)]
        .schema()
        .as_aggregate()
        .unwrap()
        .fields()
        .iter()
        .map(|entry| ResolvedField {
            name: entry.name().to_string(),
            properties: entry
                .field()
                .properties()
                .iter()
                .map(|(key, value)| (key.to_string(), value.to_string()))
                .collect(),
            hint: entry.field().hint().map(ToString::to_string),
        })
        .collect()
}

#[test]
fn field_metadata_preserves_resolved_strings_duplicates_and_order() {
    let fields = resolve_fields(
        r#"
        class Host {
            [category = "First", tooltip = "", category = "Second"]
            [customEditor = "Authored", toolitp = "Keep spelling", Tooltip = "Keep case"]
            [rangeMin = "-1.5", rangeMax = "2.5", unsavable = "true"]
            [customInnerTypeEditor = "Inner", profile = "Profile", unknownKey = "λ\\n[raw]"]
            @runtimeProperty("category", "Explicit")
            var value: Int32;
            var plain: Int32;
            hint value = "Separate hint";
        }
        "#,
        "Host",
    );
    assert_eq!(
        fields,
        [
            ResolvedField {
                name: "value".into(),
                properties: [
                    ("category", "First"),
                    ("tooltip", ""),
                    ("category", "Second"),
                    ("customEditor", "Authored"),
                    ("toolitp", "Keep spelling"),
                    ("Tooltip", "Keep case"),
                    ("rangeMin", "-1.5"),
                    ("rangeMax", "2.5"),
                    ("unsavable", "true"),
                    ("customInnerTypeEditor", "Inner"),
                    ("profile", "Profile"),
                    ("unknownKey", r"λ\\n[raw]"),
                    ("category", "Explicit"),
                ]
                .map(|(key, value)| (key.into(), value.into()))
                .into(),
                hint: Some("Separate hint".into()),
            },
            ResolvedField {
                name: "plain".into(),
                properties: vec![],
                hint: None,
            },
        ]
    );
}

#[test]
fn grouped_fields_share_properties_but_keep_individual_hints() {
    let fields = ["class", "struct"].map(|kind| {
        let source = format!(
            r#"{kind} Host {{ [category = "Shared", category = "Second"] var first, second: Int32; hint second = "Only second"; }}"#
        );
        resolve_fields(&source, "Host")
    });
    assert_eq!(fields[0], fields[1]);
    insta::assert_debug_snapshot!(fields[0]);
}

#[test]
fn struct_property_named_hint_is_not_a_hint_directive() {
    let fields = resolve_fields(
        r#"struct Host { [hint = "Property"] var value: Int32; }"#,
        "Host",
    );
    assert_eq!(
        fields,
        [ResolvedField {
            name: "value".into(),
            properties: vec![("hint".into(), "Property".into())],
            hint: None,
        }]
    );
}

#[test]
fn annotation_preparation_preserves_key_value_and_entry_spans() {
    let source = "class Host { [category = \"Value\"] var value: Int32; }";
    let sources = SourceMap::new();
    let file = sources.push_back("metadata.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let parsed = parsed.unwrap();
    let ast::Item::Class(host) = &parsed.module.items[0].0.item else {
        panic!()
    };
    let (annotation, span) = &host.items[0].0.annotations[0];
    assert_eq!(annotation.name, "runtimeProperty");
    assert_eq!(
        &source[span.start as usize..span.end as usize],
        "category = \"Value\""
    );
    for ((expr, span), (expected, value)) in annotation
        .args
        .iter()
        .zip([("category", "category"), ("\"Value\"", "Value")])
    {
        assert_eq!(span.file, file);
        assert_eq!(&source[span.start as usize..span.end as usize], expected);
        assert!(matches!(expr, ast::Expr::Constant(ast::Constant::String(s)) if s == value));
    }
}

#[test]
fn nonfield_targets_and_nonstring_values_fail_with_typed_capability_errors() {
    insta::assert_snapshot!(CapabilityError::Metadata.to_string(), @"bracket metadata requires a field and string literal values");
    let cases = [
        (
            r#"[category = "Test"] class Host {}"#,
            "category = \"Test\"",
        ),
        (
            r#"[category = "Test"] struct Host {}"#,
            "category = \"Test\"",
        ),
        (
            r#"[category = "Test"] enum Choice { A }"#,
            "category = \"Test\"",
        ),
        (
            r#"[category = "Test"] function Run() {}"#,
            "category = \"Test\"",
        ),
        (
            r#"class Host { [category = "Test"] function Run() {} }"#,
            "category = \"Test\"",
        ),
        (
            r#"class Host { [category = "Test"] event OnAttach() {} }"#,
            "category = \"Test\"",
        ),
        (
            r#"class Host { [category = "Test"] default value = 1; }"#,
            "category = \"Test\"",
        ),
        ("class Host { [key = true] var value: Int32; }", "true"),
        ("class Host { [key = 42] var value: Int32; }", "42"),
        ("class Host { [key = 1.5] var value: Int32; }", "1.5"),
        (
            "class Host { [key = n\"Name\"] var value: Int32; }",
            "n\"Name\"",
        ),
        ("class Host { [key = null] var value: Int32; }", "null"),
        ("class Host { [key = Other] var value: Int32; }", "Other"),
        ("class Host { [key = Make()] var value: Int32; }", "Make()"),
        (
            "class Host { [key = \"a\" + \"b\"] var value: Int32; }",
            "\"a\" + \"b\"",
        ),
        (
            r#"class Host { [key = "Good", key = false] var value: Int32; }"#,
            "false",
        ),
    ];
    for (source, rejected) in cases {
        let sources = SourceMap::new();
        let file = sources.push_back("metadata.script", source);
        let (native, errors) = redscript_redmod_parser::parse_module(source, file);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        let (kind, span) = redscript_redmod_adapter::bridge::lower(native.unwrap()).unwrap_err();
        assert_eq!(kind, CapabilityError::Metadata, "{source}");
        assert_eq!(&source[span.start as usize..span.end as usize], rejected);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(parsed.is_none(), "{source}");
        assert_eq!(errors.len(), 1, "{source}: {errors:?}");
        assert!(matches!(
            errors[0],
            Error::Capability {
                kind: CapabilityError::Metadata,
                ..
            }
        ));
        assert_eq!(errors[0].span(), span);
        assert_eq!(errors[0].code(), "REDMOD_CAPABILITY");
        assert_eq!(errors[0].to_string(), CapabilityError::Metadata.to_string());
    }
}

#[test]
fn metadata_does_not_enable_defaults_or_unmapped_modifiers() {
    for (source, expected) in [
        (
            r#"class Host { [category = "Test"] var value: Int32; default value = 1; }"#,
            CapabilityError::DefaultDirective,
        ),
        (
            r#"class Host { [category = "Test"] export var value: Int32; }"#,
            CapabilityError::Modifier(redscript_redmod_ast::Modifier::Export),
        ),
        (
            r#"class Host { [category = "Test"] virtual var value: Int32; }"#,
            CapabilityError::Modifier(redscript_redmod_ast::Modifier::Virtual),
        ),
        (
            r#"class Host { [category = "Test"] constexpr var value: Int32; }"#,
            CapabilityError::Modifier(redscript_redmod_ast::Modifier::Constexpr),
        ),
        (
            r#"class Host { [category = "Test"] mutable var value: Int32; }"#,
            CapabilityError::Modifier(redscript_redmod_ast::Modifier::Mutable),
        ),
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("metadata.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(parsed.is_none(), "{source}");
        assert_eq!(errors.len(), 1, "{source}: {errors:?}");
        assert!(
            matches!(errors[0], Error::Capability { kind, .. } if kind == expected),
            "{source}: {errors:?}"
        );
    }
}
