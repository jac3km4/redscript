//! Struct defaults use zero-argument construction at each declaration site.
mod support;
use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};

use support::try_emit as emit;

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
fn nested_defaults_keep_each_reset_site() {
    insta::assert_snapshot!(emit(SOURCE, "").unwrap());
}

#[test]
fn scalar_defaults_remain_explicit() {
    let text = emit(
        r#"enum Choice { First = 3, Zero = 0, Invalid = 4 }
        func Test(again: Bool) {
            let id: TweakDBID;
            let choice: Choice;
            while again { let choice: Choice; choice = Choice.Invalid; }
        }"#,
        "",
    )
    .unwrap();
    assert!(text.contains("var id: TweakDBID = T\"\";"), "{text}");
    assert!(text.contains("var choice: Choice = Choice.Zero;"), "{text}");
    assert!(
        text.contains("while (again) {\n        choice_2 = Choice.Zero;"),
        "{text}"
    );
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
fn defaults_do_not_enumerate_fields_or_require_visible_layouts() {
    for dependency in [
        "native struct Record {}",
        "native struct Record { public let value: Int32; }",
        "importonly struct Record {}",
        "struct Record { public let values: [Int32]; public let value: Variant; }",
        "enum Choice { First = 1 } struct Record { public let value: Choice; }",
        "native struct Native {} struct Record { public let value: Native; }",
        "struct Record { public let id: TweakDBID; }",
        "struct Inner { public let value: Int32; } struct Record { public let value: Inner; }",
        "struct Record {}",
    ] {
        let text = emit("func Test(again: Bool) { let value: Record; while again { let nested: Record; nested; } }", dependency).unwrap();
        assert!(text.contains("var value: Record = Record();"), "{text}");
        assert!(
            text.contains("while (again) {\n        nested = Record();"),
            "{text}"
        );
    }
    let fields = (0..256)
        .map(|i| format!("public let f{i}: Int32;"))
        .collect::<String>();
    let text = emit(
        "func Test() { let value: Record; }",
        &format!("struct Record {{ {fields} }}"),
    )
    .unwrap();
    assert!(text.contains("var value: Record = Record();"), "{text}");
}

#[test]
fn community_explicit_constructors_still_require_all_fields() {
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
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn struct_constructors_with_official_compiler() {
    let compiler = redscript_test_support::OfficialCompiler::from_env();
    let generated = emit(SOURCE, "").unwrap();
    for (name, source, accepted) in [
        ("TranspiledDefaults", generated.as_str(), true),
        (
            "ZeroArgumentSource",
            "struct ProbeRecord { var value: Int32; } function Probe() { var value: ProbeRecord; value = ProbeRecord(); }",
            true,
        ),
        (
            "DeclaredDefaults",
            "struct ProbeDefaults { var value: Int32; default value = 42; } function Probe() -> ProbeDefaults { var value: ProbeDefaults = ProbeDefaults(); return value; }",
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
    ] {
        if accepted {
            compiler.compile(name, source);
        } else {
            compiler.reject(name, source, "Unable to convert from 'Bool' to 'Int32'");
        }
    }
}
