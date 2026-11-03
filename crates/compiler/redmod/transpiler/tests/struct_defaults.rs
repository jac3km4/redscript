//! Complete source layouts use positional constructors, never a blanket Type().
use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::{TranspileDiagnostic, transpile_sources};

fn emit(source: &str, dependency: &str) -> Result<String, Vec<TranspileDiagnostic>> {
    let sources = SourceMap::new();
    let selected = sources.push_back("struct-defaults.reds", source);
    sources.push_back("dependency.reds", dependency);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .map(|output| output[0].module.text.clone())
}

const SOURCE: &str = r#"
class DefaultObject {}
struct DefaultLeaf {
    public let count: Int32;
    public let enabled: Bool;
    public let weight: Float;
}
struct DefaultPair {
    public let first: DefaultLeaf;
    public let second: DefaultLeaf;
    public let strong: ref<DefaultObject>;
    public let observer: wref<DefaultObject>;
    public let label: String;
    public let tag: CName;
}
func ResetStructs(again: Bool) {
    let pair: DefaultPair;
    while again {
        let pair: DefaultPair;
        pair = DefaultPair(DefaultLeaf(42, true, 1.0), DefaultLeaf(1, true, 2.0), null, null, "dirty", n"dirty");
        if again { continue; }
        break;
    }
    if again { let pair: DefaultPair; }
}
"#;

#[test]
fn nested_defaults_follow_field_order_and_keep_each_reset_site() {
    let text = emit(SOURCE, "").unwrap();
    let value =
        "DefaultPair(DefaultLeaf(0, false, 0.0), DefaultLeaf(0, false, 0.0), NULL, NULL, \"\", '')";
    assert!(
        text.contains(&format!("var pair: DefaultPair = {value};")),
        "{text}"
    );
    assert!(
        text.contains(&format!("while (again) {{\n        pair_2 = {value};")),
        "{text}"
    );
    assert!(
        text.contains(&format!("if (again) {{\n        pair_3 = {value};")),
        "{text}"
    );
    assert_eq!(text.matches(value).count(), 3, "{text}");
    assert!(text.contains("var pair_2: DefaultPair;"), "{text}");
    assert!(text.contains("var pair_3: DefaultPair;"), "{text}");
    assert!(!text.contains("DefaultPair()"), "{text}");
}

#[test]
fn tweakdbid_defaults_reset_locals_and_complete_struct_fields() {
    let text = emit(
        r#"struct Record { public let id: TweakDBID; }
        func Test(again: Bool) {
            let id: TweakDBID;
            while again { let id: TweakDBID; id = t"dirty"; }
            let record: Record;
        }"#,
        "",
    )
    .unwrap();
    assert!(text.contains("var id: TweakDBID = T\"\";"), "{text}");
    assert!(
        text.contains("while (again) {\n        id_2 = T\"\";"),
        "{text}"
    );
    assert!(text.contains("Record(T\"\")"), "{text}");
}

#[test]
fn enum_defaults_choose_zero_not_first_or_invalid_and_reset_in_place() {
    let text = emit(
        r#"enum Choice { First = 3, Zero = 0, Invalid = 4 }
        struct Record { public let choice: Choice; }
        func Test(again: Bool) {
            let choice: Choice;
            while again { let choice: Choice; choice = Choice.Invalid; }
            let record: Record;
        }"#,
        "",
    )
    .unwrap();
    assert!(text.contains("var choice: Choice = Choice.Zero;"), "{text}");
    assert!(
        text.contains("while (again) {\n        choice_2 = Choice.Zero;"),
        "{text}"
    );
    assert!(text.contains("Record(Choice.Zero)"), "{text}");
    let errors = emit(
        "enum Choice { First = 3 } func Test() { let value: Choice; }",
        "",
    )
    .unwrap_err();
    assert!(
        errors[0].message.contains("no declared zero-valued member"),
        "{errors:?}"
    );
}

#[test]
fn source_dependency_layouts_and_empty_structs_are_complete() {
    let text = emit(
        "func Test() { let value: Record; let empty: Empty; }",
        "struct Record { public let z: Int32; public let a: Bool; } struct Empty {}",
    )
    .unwrap();
    assert!(
        text.contains("var value: Record = Record(0, false);"),
        "{text}"
    );
    assert!(text.contains("var empty: Empty = Empty();"), "{text}");
}

#[test]
fn unsupported_fields_and_native_layouts_fail_closed_with_spans() {
    for dependency in [
        "native struct Record {}",
        "native struct Record { public let value: Int32; }",
        "struct Record { public let values: [Int32]; }",
        "struct Record { public let values: [Int32; 2]; }",
        "struct Record { public let value: Variant; }",
        "enum Choice { First = 1 } struct Record { public let value: Choice; }",
        "native struct ItemID {} struct Record { public let value: ItemID; }",
        "native struct inkAnimOptions { public let loop: Bool; } struct Record { public let value: inkAnimOptions; }",
        "struct Inner { public let values: [Int32]; } struct Record { public let value: Inner; }",
        "struct Inner { public let count: Int32; } struct Record { public let value: script_ref<Inner>; }",
    ] {
        let errors = emit("func Test() { let record: Record; }", dependency).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("cannot default-initialize")),
            "{dependency}: {errors:?}"
        );
        assert!(errors.iter().all(|e| e.span.is_some()), "{errors:?}");
    }
}

#[test]
fn generated_defaults_typecheck_through_the_full_target_api() {
    let text = emit(SOURCE, "").unwrap();
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("defaults.script", text.clone());
    let (parsed, errors) = redscript_redmod_adapter::parse_file_with_metadata(&text, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, source)| {
            let (module, errors) = redscript_community_parser::parse_module(source.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut reporter = redscript_compiler_frontend::CompileErrorReporter::default();
    let (unit, _, errors) = redscript_redmod_adapter::process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    assert_eq!(unit.functions.len(), 1);
}

#[test]
fn complete_native_flags_do_not_substitute_for_a_verified_schema() {
    use redscript_compiler_frontend::{Aggregate, AggregateFlags, TypeDef, TypeSchema};
    let sources = SourceMap::new();
    let file = sources.push_back("default.reds", "func Test() { let record: Record; }");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut symbols = Symbols::with_default_types();
    let mut aggregate = Aggregate::default();
    aggregate.set_flags(
        AggregateFlags::default()
            .with_is_struct(true)
            .with_is_native(true)
            .with_is_fully_defined(true),
    );
    symbols.add_type(
        interner.intern("Record"),
        TypeDef::new([], TypeSchema::Aggregate(Box::new(aggregate)), []),
    );
    let errors = transpile_sources(&sources, &[file], symbols, &interner).unwrap_err();
    assert!(
        errors.iter().any(|e| e
            .message
            .contains("cannot default-initialize Record with native or incomplete fields")),
        "{errors:?}"
    );
}

#[test]
fn low_level_recursive_and_unproven_layouts_fail_closed() {
    use std::collections::BTreeMap;

    use redscript_compiler_frontend::{Field, FieldFlags, Type, TypeApp, TypeSchema};
    use redscript_redmod_transpiler::{
        DeclarationPath, ResolvedDeclaration, TranspileModule, analyze_sources, transpile,
    };

    let sources = SourceMap::new();
    let file = sources.push_back("use.reds", "func Test() { let record: Record; }");
    let dependency = sources.push_back("record.reds", "struct Record {}");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let analysis = analyze_sources(&sources, Symbols::with_default_types(), &interner).unwrap();
    let id = interner.get("Record").unwrap();
    let function = *analysis
        .symbols()
        .free_functions()
        .find(|e| e.name().to_string() == "Test")
        .unwrap()
        .key();
    let module = &analysis
        .modules()
        .iter()
        .find(|(id, _)| *id == file)
        .unwrap()
        .1;
    for mode in ["recursive", "missing-field-span", "base", "import-only"] {
        let mut symbols = analysis.symbols().clone();
        let TypeSchema::Aggregate(aggregate) = symbols[id].schema_mut() else {
            panic!("struct schema")
        };
        match mode {
            "recursive" => {
                aggregate
                    .fields_mut()
                    .add(
                        "selfValue",
                        Field::new(
                            FieldFlags::default(),
                            Type::nullary(id),
                            [],
                            [],
                            Some(redscript_community_ast::Span::new(0, 1, dependency)),
                        ),
                    )
                    .unwrap();
            }
            "missing-field-span" => {
                aggregate
                    .fields_mut()
                    .add(
                        "hidden",
                        Field::new(
                            FieldFlags::default(),
                            Type::nullary(redscript_compiler_frontend::predef::INT32),
                            [],
                            [],
                            None,
                        ),
                    )
                    .unwrap();
            }
            "base" => aggregate.set_base(Some(TypeApp::new(id, []))),
            "import-only" => aggregate.set_flags(aggregate.flags().with_is_import_only(true)),
            _ => unreachable!(),
        }
        let errors = transpile(
            &[TranspileModule {
                source: module,
                bindings: BTreeMap::from([(
                    DeclarationPath(vec![0]),
                    ResolvedDeclaration::Function(function),
                )]),
            }],
            analysis.unit(),
            &symbols,
        )
        .unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("cannot default-initialize")),
            "{mode}: {errors:?}"
        );
        assert!(errors.iter().all(|e| e.span.is_some()), "{errors:?}");
    }
}

#[test]
fn shared_constructor_api_requires_all_complete_source_fields() {
    use redscript_redmod_transpiler::analyze_sources;

    for (args, valid) in [("", false), ("0", false), ("0, false", true)] {
        let sources = SourceMap::new();
        sources.push_back("constructor.reds", format!("struct Record {{ public let count: Int32; public let flag: Bool; }} func Test() -> Record {{ return Record({args}); }}"));
        sources.populate_boot_lib();
        let interner = TypeInterner::default();
        assert_eq!(
            analyze_sources(&sources, Symbols::with_default_types(), &interner).is_ok(),
            valid,
            "Record({args})"
        );
    }
}

#[test]
fn constructor_bytecode_field_limit_fails_closed() {
    let fields = (0..256)
        .map(|i| format!("public let f{i}: Int32;"))
        .collect::<String>();
    let errors = emit(
        "func Test() { let record: Record; }",
        &format!("struct Record {{ {fields} }}"),
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("255 constructor fields")),
        "{errors:?}"
    );
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn struct_constructors_with_official_compiler() {
    use std::path::{Path, PathBuf};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    use redscript_redmod_transpiler::validate::OfficialValidator;

    let validator = OfficialValidator {
        executable: PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE")),
        timeout: Duration::from_secs(60),
    };
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let parent = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "struct-defaults-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    let generated = emit(SOURCE, "").unwrap();
    for (name, source, accepted) in [
        ("TranspiledDefaults", generated.as_str(), true),
        (
            "ZeroArgumentSource",
            "struct ProbeRecord { var value: Int32; } function Probe() { var value: ProbeRecord; value = ProbeRecord(); }",
            true,
        ),
        (
            "WrongFieldType",
            "struct ProbeTyped { var value: Int32; } function Probe() { var value: ProbeTyped; value = ProbeTyped(false); }",
            false,
        ),
        (
            "EmptyStruct",
            "struct ProbeEmpty {} function Probe() { var value: ProbeEmpty; value = ProbeEmpty(); }",
            true,
        ),
        (
            "NativeAnim",
            "function Probe() { var value: inkAnimOptions; value = inkAnimOptions(); }",
            true,
        ),
        (
            "NativeItem",
            "function Probe() { var value: ItemID; value = ItemID(); }",
            true,
        ),
    ] {
        let directory = parent.join(name);
        std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
        std::fs::write(directory.join("r6/scripts/authored.script"), source).unwrap();
        let result = validator.validate_with_root(&directory, &root);
        println!("{name}: {}\n{result:?}", directory.display());
        assert_eq!(result.is_ok(), accepted, "{name}: {result:?}");
    }
}
