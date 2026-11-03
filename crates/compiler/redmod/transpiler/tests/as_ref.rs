//! Borrowed String operands retain their invocation slot and temporary lifetime.
use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner, ir};
use redscript_redmod_transpiler::{analyze_sources, transpile_sources};

fn emit(source: &str) -> Result<String, Vec<redscript_redmod_transpiler::TranspileDiagnostic>> {
    let sources = SourceMap::new();
    let file = sources.push_back("as-ref.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(&sources, &[file], Symbols::with_default_types(), &interner)
        .map(|output| output[0].module.text.clone())
}

const SOURCE: &str = r#"
class TextOwner { public let text: String; }
native func MakeText() -> String
native func First() -> Int32
native func Last() -> Int32
native func Borrow(first: Int32, value: script_ref<String>, last: Int32) -> Bool
native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
native func GetLocalizedText(key: script_ref<String>) -> String
func Test(enabled: Bool, owner: ref<TextOwner>, texts: [String], index: Int32) -> String {
    Borrow(First(), AsRef(MakeText()), Last());
    Borrow(First(), owner.text, Last());
    Borrow(First(), texts[index], Last());
    while enabled && Borrow(First(), MakeText(), Last()) { continue; }
    let unused: Bool = enabled && Borrow(First(), MakeText(), Last());
    return s"Label: \(GetLocalizedText(s"LocKey#\(index)"))";
}
"#;

#[derive(Default)]
struct Calls {
    events: Vec<String>,
}
impl Calls {
    fn record<'ctx>(&mut self, call: &ir::Call<'ctx>, symbols: &Symbols<'ctx>) {
        let signature = match call {
            ir::Call::FreeFunction { function, .. } => {
                let (name, def) = symbols.get_free_function(*function).unwrap();
                Some((name.to_string(), def.type_()))
            }
            ir::Call::Static { method, .. } | ir::Call::Instance { method, .. } => {
                let (name, def) = symbols.get_method(*method).unwrap();
                Some((name.to_string(), def.type_()))
            }
            _ => None,
        };
        if let Some((name, signature)) = signature {
            self.events.push(format!(
                "{name}: {:?} -> {:?}; temporary slots: {:?}",
                signature
                    .params()
                    .iter()
                    .map(|p| (p.type_(), p.flags()))
                    .collect::<Vec<_>>(),
                signature.return_type(),
                call.args()
                    .iter()
                    .map(|arg| arg.is_prvalue_ref(symbols))
                    .collect::<Vec<_>>()
            ));
        }
    }
}
struct Visitor<'a, 'ctx> {
    calls: &'a mut Calls,
    symbols: &'a Symbols<'ctx>,
}
impl<'ctx> ir::Visitor<'ctx> for Visitor<'_, 'ctx> {
    fn visit_call(&mut self, call: &ir::Call<'ctx>, _: Span) {
        self.calls.record(call, self.symbols);
        if let Some(receiver) = call.receiver() {
            self.visit_expr(receiver);
        }
        for arg in call.args() {
            self.visit_expr(arg);
        }
    }
}

#[test]
fn roundtrip_restores_exact_signatures_and_temporary_argument_slots() {
    use ir::Visitor as _;
    let sources = SourceMap::new();
    sources.push_back("original.reds", SOURCE);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let original = analyze_sources(&sources, Symbols::with_default_types(), &interner).unwrap();
    let mut before = Calls::default();
    for f in original.unit().functions.values() {
        Visitor {
            calls: &mut before,
            symbols: original.symbols(),
        }
        .visit_block(&f.block);
    }
    let text = emit(SOURCE).unwrap();
    assert!(
        text.contains("Borrow(First(), MakeText(), Last());"),
        "{text}"
    );
    assert!(
        text.contains("Borrow(First(), owner.text, Last());"),
        "{text}"
    );
    assert!(
        text.contains("Borrow(First(), texts[index], Last());"),
        "{text}"
    );
    assert_eq!(
        text.matches("enabled && Borrow(First(), MakeText(), Last())")
            .count(),
        2,
        "{text}"
    );
    assert!(
        text.contains("GetLocalizedText(\"LocKey#\" + IntToString(index))"),
        "{text}"
    );
    assert!(!text.contains("AsRef("), "{text}");
    let target = SourceMap::new();
    target.populate_boot_lib();
    target.push_back(
        "native.reds",
        "native func IntToString(value: Int32) -> String",
    );
    let file = target.push_back("target.script", text.clone());
    let (parsed, errors) = redscript_redmod_adapter::parse_file_with_metadata(&text, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = target
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, source)| {
            let (module, errors) = redscript_community_parser::parse_module(source.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let mut reporter = redscript_compiler_frontend::CompileErrorReporter::default();
    let (unit, symbols, errors) = redscript_redmod_adapter::process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}\n{text}");
    assert!(
        reporter.reported().is_empty(),
        "{:?}\n{text}",
        reporter.reported()
    );
    let mut after = Calls::default();
    for f in unit.functions.values() {
        Visitor {
            calls: &mut after,
            symbols: &symbols,
        }
        .visit_block(&f.block);
    }
    // Preserve call-tree order as well as exact signatures and invocation bits.
    // ToString<Int32> is emitted as the verified native IntToString mapping.
    before
        .events
        .retain(|event| !event.starts_with("ToString:"));
    after
        .events
        .retain(|event| !event.starts_with("IntToString:"));
    assert_eq!(before.events, after.events, "{text}");
}

#[test]
fn escaping_nested_untyped_and_non_native_temporaries_fail_closed() {
    for source in [
        "func Test() { AsRef(\"value\"); }",
        "func Test() -> script_ref<String> { return AsRef(\"value\"); }",
        "func Test() { let value: script_ref<String> = AsRef(\"value\"); }",
        "func Take(value: script_ref<String>) {} func Test() { Take(AsRef(\"value\")); }",
        "native func Take(value: script_ref<Int32>) func Test() { Take(AsRef(42)); }",
        "native func Take(out value: script_ref<String>) func Test() { Take(AsRef(\"value\")); }",
        "native func Take(value: script_ref<String>) native func Take(value: String) func Test() { Take(AsRef(\"value\")); }",
        "native func Take(value: script_ref<String>) func Test() { Take(Deref(AsRef(\"value\"))); }",
    ] {
        let errors = emit(source).unwrap_err();
        assert!(
            errors.iter().all(|e| e.span.is_some()),
            "{source}: {errors:?}"
        );
    }
}

#[test]
fn conditional_and_right_nested_string_operands_keep_grouping() {
    let text = emit(
        r#"
        native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String
        native func GetLocalizedText(value: script_ref<String>) -> String
        func Test(enabled: Bool, a: String, b: String) -> String {
            return GetLocalizedText(AsRef(enabled ? a : b)) + (a + b);
        }
    "#,
    )
    .unwrap();
    assert!(
        text.contains("GetLocalizedText(enabled ? a : b) + (a + b)"),
        "{text}"
    );
}

#[test]
fn ambiguous_methods_and_unsupported_operators_do_not_supply_borrow_context() {
    for source in [
        "native class Owner { public native func Read(value: script_ref<String>) -> Bool public native func Read(value: String) -> Bool } func Test(owner: ref<Owner>) { owner.Read(AsRef(\"temporary\")); }",
        "native func OperatorEqual(a: script_ref<String>, b: script_ref<String>) -> Bool func Test() -> Bool { return AsRef(\"a\") == AsRef(\"b\"); }",
        "native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String native func OperatorAdd(a: String, b: String) -> String func Test() -> String { return AsRef(\"a\") + AsRef(\"b\"); }",
    ] {
        assert!(emit(source).is_err(), "{source}");
    }
}

#[test]
fn irrelevant_overloads_do_not_block_native_borrows() {
    let text = emit(
        r#"
        native class Other {
            public native func Read(value: String) -> Bool
            public native static func ReadStatic(value: String) -> Bool
        }
        native class Base {
            public native func Read(value: script_ref<String>) -> Bool
            public native func Read(value: Int32) -> Bool
            public native func Read(value: String, required: Int32) -> Bool
            public native static func ReadStatic(value: script_ref<String>) -> Bool
            public native static func ReadStatic(value: Int32) -> Bool
        }
        native class Child extends Base {}
        native func ReadFree(value: script_ref<String>) -> Bool
        native func ReadFree(value: Int32) -> Bool
        native func ReadFree(value: String, required: Int32) -> Bool
        native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String
        native func OperatorAdd(a: Int32, b: Int32) -> Int32
        func Test(receiver: ref<Child>) -> String {
            ReadFree(AsRef("temporary"));
            receiver.Read(AsRef("temporary"));
            Child.ReadStatic(AsRef("temporary"));
            return AsRef("a") + AsRef("b");
        }
    "#,
    )
    .unwrap();
    assert!(text.contains("ReadFree(\"temporary\");"), "{text}");
    assert!(text.contains("receiver.Read(\"temporary\");"), "{text}");
    assert!(text.contains("Child.ReadStatic(\"temporary\");"), "{text}");
    assert!(text.contains("return \"a\" + \"b\";"), "{text}");
}

#[test]
fn mixed_numeric_siblings_do_not_prove_borrow_overloads_disjoint() {
    for (numeric, expression, params) in [
        ("Float", "1", ""),
        ("Double", "1", ""),
        ("Int64", "1", ""),
        ("Uint32", "1", ""),
        ("Float", "number", "number: Int32"),
    ] {
        let source = format!(
            "native func Take(value: script_ref<String>, number: Int32) native func Take(value: String, number: {numeric}) func Test({params}) {{ Take(AsRef(\"temporary\"), {expression}); }}"
        );
        let errors = emit(&source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("AsRef requires a local variable here")),
            "{source}: {errors:?}"
        );
    }
}

#[test]
fn optional_out_and_null_siblings_do_not_hide_borrow_competitors() {
    for (selected, other, argument, params) in [
        ("number: Int32", "opt number: Float", "1", ""),
        (
            "number: Int32",
            "out number: Float",
            "number",
            "number: Int32",
        ),
        ("other: ref<Value>", "other: ref<Other>", "null", ""),
    ] {
        let source = format!(
            "class Value {{}} class Other {{}} native func Take(value: script_ref<String>, {selected}) native func Take(value: String, {other}) func Test({params}) {{ Take(AsRef(\"temporary\"), {argument}); }}"
        );
        assert!(emit(&source).is_err(), "{source}");
    }
}

#[test]
fn unique_borrow_with_null_sibling_keeps_native_context() {
    let text = emit(
        "class Value {} native func Take(value: script_ref<String>, other: ref<Value>) func Test() { Take(AsRef(\"temporary\"), null); }"
    ).unwrap();
    assert!(text.contains("Take(\"temporary\", NULL);"), "{text}");
}

#[test]
fn optional_string_operator_is_not_a_verified_fixed_signature() {
    assert!(emit(
        "native func OperatorAdd(a: script_ref<String>, opt b: script_ref<String>) -> String func Test() -> String { return AsRef(\"a\") + AsRef(\"b\"); }"
    ).is_err());
}

#[test]
fn inherited_viable_string_overload_blocks_borrow_erasure() {
    assert!(emit(
        r#"
        native class Base { public native func Read(value: String, opt extra: Int32) -> Bool }
        native class Child extends Base { public native func Read(value: script_ref<String>) -> Bool }
        func Test(receiver: ref<Child>) { receiver.Read(AsRef("temporary")); }
    "#,
    ).is_err());
}

#[test]
fn native_methods_use_only_their_immediate_non_out_parameter() {
    let text = emit(
        r#"
        native class Receiver {
            public native func Read(value: script_ref<String>) -> Bool
            public native static func ReadStatic(value: script_ref<String>) -> Bool
        }
        native func MakeText() -> String
        func Test(receiver: ref<Receiver>) {
            receiver.Read(MakeText());
            Receiver.ReadStatic(MakeText());
        }
    "#,
    )
    .unwrap();
    assert!(text.contains("receiver.Read(MakeText());"), "{text}");
    assert!(text.contains("Receiver.ReadStatic(MakeText());"), "{text}");
}

// Decode only bounded v14 name/header tables and match a known invocation
// prefix inside a named probe. Full official definition decoding is not assumed.
fn official_invocations(bytes: &[u8], probe: &str, callee_prefix: &str) -> Vec<(u16, u8)> {
    let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap()) as usize;
    assert_eq!(&bytes[..4], b"REDS");
    assert_eq!(word(4), 14);
    let names = (0..word(48))
        .map(|i| {
            let start = word(32) + word(word(44) + i * 4);
            let end = bytes[start..].iter().position(|b| *b == 0).unwrap() + start;
            std::str::from_utf8(&bytes[start..end]).unwrap()
        })
        .collect::<Vec<_>>();
    let definitions = (0..word(84))
        .map(|i| {
            let at = word(80) + i * 20;
            (
                names[word(at)],
                word(at + 8),
                word(at + 12),
                u16::from_le_bytes(bytes[at + 16..at + 18].try_into().unwrap()),
            )
        })
        .collect::<Vec<_>>();
    let (_, start, size, _) = definitions
        .iter()
        .find(|(name, _, _, tag)| *tag == 5 && name.starts_with(probe))
        .unwrap();
    let body = &bytes[*start..*start + *size];
    let mut calls = Vec::new();
    for window in body.windows(12) {
        if window[0] != 0x24 {
            continue;
        }
        let index = u32::from_le_bytes(window[5..9].try_into().unwrap()) as usize;
        if definitions
            .get(index)
            .is_some_and(|(name, _, _, tag)| *tag == 5 && name.starts_with(callee_prefix))
        {
            calls.push((
                u16::from_le_bytes(window[9..11].try_into().unwrap()),
                window[11],
            ));
        }
    }
    calls
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn borrowed_strings_with_official_compiler() {
    use std::path::{Path, PathBuf};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    use redscript_redmod_transpiler::validate::OfficialValidator;
    let validator = OfficialValidator {
        executable: PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE")),
        timeout: Duration::from_secs(60),
    };
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "as-ref-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    // Real engine signatures come from the trusted installation, not invented
    // imports. Compare temporary and stable-place invocation lifetime metadata.
    let source = r#"
        native func GetLocalizedText(key: script_ref<String>) -> String
        native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String
        func AsRefTemporary() -> String { return GetLocalizedText(AsRef("LocKey#" + "42")); }
        func AsRefNested(value: String) -> String { return GetLocalizedText(AsRef("LocKey#" + GetLocalizedText(value))); }
        func AsRefLoop(enabled: Bool, value: String) -> Bool {
            while enabled && StrContains(GetLocalizedText(value), "label") { continue; }
            return enabled && StrContains(GetLocalizedText(value), "label");
        }
        native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
        native func StrContains(value: script_ref<String>, part: script_ref<String>) -> Bool
        func AsRefLocal(value: String) -> String { return GetLocalizedText(AsRef(value)); }
        func AsRefInterpolation(value: String) -> String { return s"Label: \(GetLocalizedText(value))"; }
    "#;
    let generated = emit(source).unwrap();
    let body = generated
        .lines()
        .filter(|line| !line.starts_with("import function"))
        .collect::<Vec<_>>()
        .join("\n");
    std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
    std::fs::write(directory.join("r6/scripts/authored.script"), &body).unwrap();
    let report = validator.validate_with_root(&directory, &root).unwrap();
    println!("{}\n{}", directory.display(), report.stdout);
    let bytes = std::fs::read(directory.join("r6/cache/final.redscripts")).unwrap();
    // 0x66 is AsRef. The temporary is call-owned (bit 0); the local is
    // borrowed by identity without allocating call-owned temporary storage.
    assert_eq!(
        official_invocations(&bytes, "AsRefTemporary", "GetLocalizedText"),
        [(1, 0x66)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefNested", "GetLocalizedText"),
        [(1, 0x66), (0, 0x66)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefNested", "OperatorAdd;"),
        [(2, 0x66)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefLoop", "StrContains"),
        [(1, 0x66), (1, 0x66)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefLocal", "GetLocalizedText"),
        [(0, 0x66)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefInterpolation", "OperatorAdd;"),
        [(2, 0x66)]
    );
}

#[test]
fn temporary_argument_mask_is_bounded_but_local_identity_is_unchanged() {
    let prefix = (0..16)
        .map(|i| format!("a{i}: Int32"))
        .collect::<Vec<_>>()
        .join(", ");
    let args = vec!["0"; 16].join(", ");
    let source = format!(
        "native func Take({prefix}, value: script_ref<String>) func Test() {{ Take({args}, AsRef(\"temporary\")); }}"
    );
    assert!(emit(&source).is_err());
    assert!(emit("native func Take(value: script_ref<String>) func Test(value: String) { Take(AsRef(value)); }").is_ok());
}
