//! Borrowed operands retain their invocation slot and temporary lifetime.
mod support;

use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner, ir};
use redscript_redmod_transpiler::analyze_sources;
use support::try_emit_source as emit;

#[test]
fn scripted_field_borrows_keep_overload_and_place_guards() {
    let definitions =
        "class Owner { public let text: String; } func Borrow(value: script_ref<String>) {}";
    assert!(
        emit(&format!(
            "{definitions} func Test(owner: ref<Owner>) {{ Borrow(owner.text); }}"
        ))
        .is_ok()
    );
    let competing = format!(
        "{definitions} func Borrow(value: String) {{}} func Test(owner: ref<Owner>) {{ Borrow(AsRef(owner.text)); }}"
    );
    assert!(emit(&competing).is_err());
}

#[test]
fn scripted_index_borrows_keep_their_argument_site() {
    let source = "struct Value { public let count: Int32; } class Owner { public let values: [Value]; } func Borrow(value: script_ref<Value>) {} func Index(out i: Int32) -> Int32 { i = 1; return 0; } func Test(owner: ref<Owner>, i: Int32) { Borrow(owner.values[Index(i)]); }";
    let text = emit(source).unwrap();
    assert!(text.contains("Borrow(owner.m_values[Index(i)]);"), "{text}");
    let competing = source
        .replace("func Test(", "func Borrow(value: Value) {} func Test(")
        .replace(
            "Borrow(owner.values[Index(i)])",
            "Borrow(AsRef(owner.values[Index(i)]))",
        );
    assert!(emit(&competing).is_err());
}

#[test]
fn scripted_temporary_borrows_preserve_call_sites_and_guards() {
    let source = "struct Value { public let count: Int32; } func Make() -> Value { return Value(42); } func Take(value: script_ref<Value>) -> Bool { return true; } native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool func Test(enabled: Bool) -> Bool { while enabled && Take(Make()) { continue; } return Take(Make()); }";
    let text = emit(source).unwrap();
    assert!(text.contains("while (enabled && Take(Make()))"), "{text}");
    assert!(text.contains("return Take(Make());"), "{text}");
    let competing = source
        .replace(
            "func Test(",
            "func Take(value: Value) -> Bool { return false; } func Test(",
        )
        .replace("Take(Make())", "Take(AsRef(Make()))");
    assert!(emit(&competing).is_err());
}

#[test]
fn borrow_diagnostics_explain_context_and_overload_failures() {
    for (source, expected) in [
        (
            "func Test() { AsRef(42); }",
            "immediate exact non-out script_ref<T> call parameter",
        ),
        (
            "native func Take(value: script_ref<String>, number: Int32) native func Take(value: String, number: Float) func Test(number: Int32) { Take(AsRef(\"temporary\"), number); }",
            "competing overloads",
        ),
        (
            "func Take(out value: script_ref<Int32>) {} func Test() { Take(AsRef(42)); }",
            "out",
        ),
    ] {
        let errors = emit(source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains(expected) && e.span.is_some()),
            "{errors:?}"
        );
    }
}

const SOURCE: &str = r#"
struct BorrowedValue { public let number: Int32; }
class TextOwner { public let text: String; public let number: Int32; public let value: BorrowedValue; }
native func BorrowInt(value: script_ref<Int32>, number: Int32)
native func BorrowInt(value: script_ref<Int32>, number: Float)
native func BorrowValue(value: script_ref<BorrowedValue>)
native func MakeText() -> String
native func First() -> Int32
native func Last() -> Int32
native func Borrow(first: Int32, value: script_ref<String>, last: Int32) -> Bool
native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String
native func OperatorAdd(a: Int32, b: script_ref<String>) -> String
native func OperatorAdd(a: Float, b: script_ref<String>) -> String
native func OperatorAdd(a: Bool, b: script_ref<String>) -> String
native func OperatorAdd(a: script_ref<String>, b: Int32) -> String
native func OperatorAdd(a: script_ref<String>, b: Float) -> String
native func OperatorAdd(a: script_ref<String>, b: Bool) -> String
native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
native func OperatorEqual(a: script_ref<String>, b: script_ref<String>) -> Bool
native func OperatorNotEqual(a: script_ref<String>, b: script_ref<String>) -> Bool
native func OperatorBitNot(value: script_ref<String>) -> String
native func ReadMixed(value: script_ref<String>, number: Int32) -> String
native func ReadMixed(value: script_ref<String>, number: Float) -> String
native func GetLocalizedText(key: script_ref<String>) -> String
func Test(enabled: Bool, owner: ref<TextOwner>, texts: [String], index: Int32) -> String {
    BorrowInt(AsRef(42), index);
    BorrowInt(owner.number, index);
    BorrowValue(BorrowedValue(index));
    BorrowValue(owner.value);
    Borrow(First(), AsRef(MakeText()), Last());
    Borrow(First(), owner.number + " / " + index, Last());
    Borrow(First(), "value: " + index, Last());
    Borrow(First(), "value: " + 1.5, Last());
    Borrow(First(), 1.5 + " value", Last());
    Borrow(First(), "value: " + enabled, Last());
    Borrow(First(), enabled + " value", Last());
    Borrow(First(), ReadMixed(MakeText(), index), Last());
    Borrow(First(), ~AsRef(MakeText()), Last());
    let equal: Bool = MakeText() == "value";
    let different: Bool = MakeText() != "value";
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
        text.contains("Borrow(First(), owner.m_text, Last());"),
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
fn escaping_nested_untyped_and_out_borrows_fail_closed() {
    for source in [
        "func Test() { AsRef(\"value\"); }",
        "func Test() -> script_ref<String> { return AsRef(\"value\"); }",
        "func Test() { let value: script_ref<String> = AsRef(\"value\"); }",
        "native func Take(value: script_ref<Int32>) func Test() { Take(AsRef(true)); }",
        "native func Take(value: script_ref<Int32>) native func Take(value: Int32) func Test() { Take(AsRef(42)); }",
        "native func Take(out value: script_ref<Int32>) func Test() { Take(AsRef(42)); }",
        "func Test() -> script_ref<Int32> { return AsRef(42); }",
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
fn competing_value_and_non_native_overloads_do_not_supply_borrow_context() {
    for source in [
        "native class Owner { public native func Read(value: script_ref<String>) -> Bool public native func Read(value: String) -> Bool } func Test(owner: ref<Owner>) { owner.Read(AsRef(\"temporary\")); }",
        "native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String native func OperatorAdd(a: String, b: String) -> String func Test() -> String { return AsRef(\"a\") + AsRef(\"b\"); }",
        "native func OperatorAdd(a: Int32, b: script_ref<String>) -> String native func OperatorAdd(a: Int32, b: String) -> String func Test(value: Int32) -> String { return value + AsRef(\"a\"); }",
        "native func OperatorAdd(a: script_ref<String>, b: Int32) -> String func OperatorAdd(a: script_ref<String>, b: Float) -> String { return \"\"; } func Test(value: Int32) -> String { return AsRef(\"a\") + value; }",
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
                .any(|e| e.message.contains("competing overloads")),
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
fn optional_operator_parameters_use_the_same_argument_context() {
    let text = emit(
        "native func OperatorAdd(a: script_ref<String>, opt b: script_ref<String>) -> String func Test() -> String { return AsRef(\"a\") + AsRef(\"b\"); }"
    ).unwrap();
    assert!(text.contains("return \"a\" + \"b\";"), "{text}");
}

#[test]
fn generated_equality_guards_reuse_native_argument_context() {
    let text = emit(
        r#"
        native func OperatorEqual(a: script_ref<String>, b: script_ref<String>) -> Bool
        native func Read(values: [Int32]) -> String
        func Test() { while Read([1]) == "value" { continue; } }
    "#,
    )
    .unwrap();
    assert!(
        text.contains("!(Read(arrayLiteral) == \"value\")"),
        "{text}"
    );
    assert!(!text.contains(" != "), "{text}");
    assert!(!text.contains("AsRef("), "{text}");
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

// Keep the invocation mask and first-argument borrowing assertions local;
// shared decoding retains breakpoints and stays within the named physical body.
fn official_invocations(bytes: &[u8], probe: &str, callee_prefix: &str) -> Vec<(u16, bool)> {
    use redscript_io::Instr;
    let bundle = redscript_test_support::OfficialBundle::new(bytes);
    let code = bundle.function_code_prefix(probe);
    code.windows(2)
        .filter_map(|ops| {
            let Instr::InvokeStatic {
                function, flags, ..
            } = &ops[0]
            else {
                return None;
            };
            if !bundle
                .definition_name((*function).into())
                .starts_with(callee_prefix)
            {
                return None;
            }
            let mask = (0..16)
                .filter(|bit| flags.is_rvalue_ref(*bit))
                .fold(0, |mask, bit| mask | (1 << bit));
            Some((mask, matches!(ops[1], Instr::AsRef(_))))
        })
        .collect()
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn borrowed_values_with_official_compiler() {
    use redscript_test_support::OfficialCompiler;

    // Real engine signatures come from the trusted installation, not invented
    // imports. Compare temporary and stable-place invocation lifetime metadata.
    let source = r#"
        struct AsRefValue { public let number: Int32; }
        native func AsRefTakeInt(value: script_ref<Int32>)
        native func AsRefTakeValue(value: script_ref<AsRefValue>)
        func AsRefMakeInt() -> Int32 { return 42; }
        func AsRefScalar() { AsRefTakeInt(AsRef(AsRefMakeInt())); }
        func AsRefMakeValue(number: Int32) -> AsRefValue { return AsRefValue(number); }
        func AsRefStruct(number: Int32) { AsRefTakeValue(AsRef(AsRefMakeValue(number))); }
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
        native func OperatorAdd(a: Int32, b: script_ref<String>) -> String
        native func OperatorAdd(a: Float, b: script_ref<String>) -> String
        native func OperatorAdd(a: Bool, b: script_ref<String>) -> String
        native func OperatorAdd(a: script_ref<String>, b: Int32) -> String
        native func OperatorAdd(a: script_ref<String>, b: Float) -> String
        native func OperatorAdd(a: script_ref<String>, b: Bool) -> String
        class AsRefNumbers { public let lhs: Int32; public let rhs: Int32; }
        func AsRefMixed(values: ref<AsRefNumbers>) -> String { return values.lhs + " / " + values.rhs; }
        func AsRefMixedReverse(value: Int32) -> String { return "value: " + value; }
        func AsRefMixedFloat(value: Float) -> String { return value + " / " + value; }
        func AsRefMixedBool(value: Bool) -> String { return value + " / " + value; }
        native func OperatorAssignAdd(a: script_ref<String>, b: script_ref<String>) -> String
        func AsRefAssign(value: String) -> String { value += GetLocalizedText("LocKey#42"); return value; }
        func AsRefLocal(value: String) -> String { return GetLocalizedText(AsRef(value)); }
        func AsRefInterpolation(value: String) -> String { return s"Label: \(GetLocalizedText(value))"; }
    "#;
    let generated = emit(source).unwrap();
    let body = generated
        .lines()
        .filter(|line| !line.starts_with("import function"))
        .collect::<Vec<_>>()
        .join("\n");
    // Typed script sinks exercise the target's generic borrowing ABI without
    // inventing native imports absent from the trusted engine installation.
    let body = format!(
        "{body}\nfunction AsRefTakeInt(value: script_ref<Int32>);\nfunction AsRefTakeValue(value: script_ref<AsRefValue>);\n"
    );
    let output = OfficialCompiler::from_env().compile("as-ref", &body);
    output.assert_version("v1.2.2");
    let bytes = output.bundle_bytes();
    assert_eq!(
        official_invocations(&bytes, "AsRefScalar;", "AsRefTakeInt;"),
        [(1, true)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefStruct;", "AsRefTakeValue;"),
        [(1, true)]
    );
    // The boolean records an AsRef first argument. The temporary is call-owned
    // (bit 0); the local is
    // borrowed by identity without allocating call-owned temporary storage.
    assert_eq!(
        official_invocations(&bytes, "AsRefTemporary", "GetLocalizedText"),
        [(1, true)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefNested", "GetLocalizedText"),
        [(1, true), (0, true)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefNested", "OperatorAdd;"),
        [(2, true)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefLoop", "StrContains"),
        [(1, true), (1, true)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefLocal", "GetLocalizedText"),
        [(0, true)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefInterpolation", "OperatorAdd;"),
        [(2, true)]
    );
    // Literals borrow stable pooled storage (mask 0). The outer concatenation
    // owns the inner String result in slot 0, before evaluating the last value.
    for probe in ["AsRefMixed;", "AsRefMixedFloat;", "AsRefMixedBool;"] {
        let calls = official_invocations(&bytes, probe, "OperatorAdd;");
        assert_eq!(calls.len(), 2, "{probe}: {calls:?}");
        assert_eq!(calls[0], (1, true), "{probe}");
        assert_eq!(calls[1].0, 0, "{probe}");
    }
    assert_eq!(
        official_invocations(&bytes, "AsRefMixedReverse;", "OperatorAdd;"),
        [(0, true)]
    );
    assert_eq!(
        official_invocations(&bytes, "AsRefAssign;", "OperatorAssignAdd;"),
        [(2, true)]
    );
}

#[test]
fn temporary_argument_mask_is_bounded_but_local_identity_is_unchanged() {
    let prefix = (0..16)
        .map(|i| format!("a{i}: Int32"))
        .collect::<Vec<_>>()
        .join(", ");
    let args = vec!["0"; 16].join(", ");
    for (modifier, body) in [("native ", ""), ("", "{} ")] {
        for (ty, temporary) in [("String", "\"temporary\""), ("Int32", "42")] {
            let declaration =
                format!("{modifier}func Take({prefix}, value: script_ref<{ty}>) {body}");
            let source =
                format!("{declaration} func Test() {{ Take({args}, AsRef({temporary})); }}");
            let diagnostics = emit(&source).unwrap_err();
            assert!(
                diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.message.contains("16 argument lifetime slots")),
                "{source}: {diagnostics:?}"
            );
            let local =
                format!("{declaration} func Test(value: {ty}) {{ Take({args}, AsRef(value)); }}");
            assert!(emit(&local).is_ok(), "{local}");
        }
    }
}
