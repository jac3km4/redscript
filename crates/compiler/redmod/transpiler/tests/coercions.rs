//! Contextual coercions must survive a target-source round trip, not just parse.
mod support;

use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner, ir};
use redscript_redmod_transpiler::analyze_sources;
use support::try_emit_source as emit;

struct Calls<'a, 'ctx> {
    symbols: &'a Symbols<'ctx>,
    events: Vec<String>,
}

impl<'ctx> ir::Visitor<'ctx> for Calls<'_, 'ctx> {
    fn visit_call(&mut self, call: &ir::Call<'ctx>, _: Span) {
        if let ir::Call::FreeFunction { function, .. } = call {
            let (name, def) = self.symbols.get_free_function(*function).unwrap();
            self.events.push(format!(
                "{name}: {:?}",
                def.type_().param_types().collect::<Vec<_>>()
            ));
        } else if let ir::Call::Static { method, .. } | ir::Call::Instance { method, .. } = call {
            let (name, def) = self.symbols.get_method(*method).unwrap();
            self.events.push(format!(
                "{name}: {:?}",
                def.type_().param_types().collect::<Vec<_>>()
            ));
        }
        if let Some(receiver) = call.receiver() {
            self.visit_expr(receiver);
        }
        for arg in call.args() {
            self.visit_expr(arg);
        }
    }
}

fn roundtrip(source: &str) -> String {
    use ir::Visitor;

    let sources = SourceMap::new();
    sources.push_back("coercions.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let original = analyze_sources(&sources, Symbols::with_default_types(), &interner).unwrap();
    let mut before = Calls {
        symbols: original.symbols(),
        events: Vec::new(),
    };
    for function in original.unit().functions.values() {
        before.visit_block(&function.block);
    }
    let text = emit(source).unwrap();
    let target_sources = SourceMap::new();
    target_sources.populate_boot_lib();
    let target_file = target_sources.push_back("coercions.script", text.clone());
    let (parsed, errors) = redscript_redmod_adapter::parse_file_with_metadata(&text, target_file);
    assert!(errors.is_empty(), "{errors:?}\n{text}");
    let dependencies = target_sources
        .files()
        .filter(|(id, _)| *id != target_file)
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
    let mut after = Calls {
        symbols: &symbols,
        events: Vec::new(),
    };
    for function in unit.functions.values() {
        after.visit_block(&function.block);
    }
    for class in original.unit().classes.values() {
        for function in class.methods.values() {
            before.visit_block(&function.block);
        }
    }
    for class in unit.classes.values() {
        for function in class.methods.values() {
            after.visit_block(&function.block);
        }
    }
    // Function-map iteration is identity ordered; compare per-call facts as a
    // multiset as target symbol identities need not have the same allocation order.
    before.events.sort();
    after.events.sort();
    assert_eq!(before.events, after.events, "{text}");
    text.clone()
}

#[test]
fn exact_initializers_assignments_and_returns_restore_both_conversions() {
    let text = roundtrip(
        r#"
        class Value { public let weak: wref<Value>; public let strong: ref<Value>; }
        func Strong(value: wref<Value>) -> ref<Value> { return WeakRefToRef(value); }
        func Weak(value: ref<Value>) -> wref<Value> { return RefToWeakRef(value); }
        func Test(strong: ref<Value>, weak: wref<Value>, owner: ref<Value>) {
            let s: ref<Value> = WeakRefToRef(weak);
            let w: wref<Value> = RefToWeakRef(strong);
            s = WeakRefToRef(weak);
            w = RefToWeakRef(strong);
            owner.strong = WeakRefToRef(weak);
            owner.weak = RefToWeakRef(strong);
        }
    "#,
    );
    assert!(text.contains("var s: Value = weak;"), "{text}");
    assert!(text.contains("var w: wref<Value> = strong;"), "{text}");
    assert!(text.contains("return value;"), "{text}");
    assert!(!text.contains("WeakRefToRef("), "{text}");
}

#[test]
fn unique_known_call_parameters_restore_conversions_without_materialization() {
    let text = roundtrip(
        r#"
        class Value {
            public static func TakeStrong(value: ref<Value>) {}
            public func TakeWeak(value: wref<Value>) {}
        }
        func TakeStrongFree(value: ref<Value>) {}
        func TakeWeakFree(value: wref<Value>) {}
        func Test(strong: ref<Value>, weak: wref<Value>) {
            TakeStrongFree(WeakRefToRef(weak));
            TakeWeakFree(RefToWeakRef(strong));
            Value.TakeStrong(WeakRefToRef(weak));
            strong.TakeWeak(RefToWeakRef(strong));
        }
    "#,
    );
    assert!(text.contains("TakeStrongFree(weak);"), "{text}");
    assert!(text.contains("Value.TakeStrong(weak);"), "{text}");
    assert!(text.contains("strong.TakeWeak(strong);"), "{text}");
}

#[test]
fn lazy_call_arguments_and_loop_back_edges_keep_their_execution_sites() {
    let text = roundtrip(
        r#"
        class Value {}
        native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool
        func GetWeak() -> wref<Value> { return null; }
        func Probe(value: ref<Value>) -> Bool { return true; }
        func Test(enabled: Bool) -> Bool {
            while enabled && Probe(WeakRefToRef(GetWeak())) {}
            return enabled && Probe(WeakRefToRef(GetWeak()));
        }
    "#,
    );
    assert_eq!(text.matches("GetWeak()").count(), 3, "{text}");
    assert_eq!(
        text.matches("enabled && Probe(GetWeak())").count(),
        2,
        "{text}"
    );
    assert!(!text.contains("var "), "{text}");
}

#[test]
fn implicit_conversions_use_the_same_exact_sinks() {
    roundtrip(
        r#"
        class Value {
            public func Strong(value: wref<Value>) -> ref<Value> { return value; }
        }
        func Take(value: ref<Value>) {}
        func Weak(value: ref<Value>) -> wref<Value> { return value; }
        func Test(strong: ref<Value>, weak: wref<Value>) {
            let s: ref<Value> = weak;
            let w: wref<Value> = strong;
            Take(weak);
        }
    "#,
    );
}

#[test]
fn side_effecting_operands_and_sibling_arguments_stay_inline_and_ordered() {
    let text = roundtrip(
        r#"
        class Value {}
        func GetWeak() -> wref<Value> { return null; }
        func GetStrong() -> ref<Value> { return null; }
        func First() -> Int32 { return 1; }
        func Last() -> Int32 { return 2; }
        func Take(a: Int32, b: ref<Value>, c: Int32) {}
        func Test() -> ref<Value> {
            let weak: wref<Value> = RefToWeakRef(GetStrong());
            Take(First(), WeakRefToRef(GetWeak()), Last());
            return WeakRefToRef(GetWeak());
        }
    "#,
    );
    assert!(text.contains("Take(First(), GetWeak(), Last());"), "{text}");
    assert!(
        text.contains("var weak: wref<Value> = GetStrong();"),
        "{text}"
    );
    assert!(text.contains("return GetWeak();"), "{text}");
}

#[test]
fn context_free_nested_and_branch_conversions_fail_closed() {
    for body in [
        "WeakRefToRef(weak);",
        "RefToWeakRef(strong);",
        "let w: wref<Value> = RefToWeakRef(WeakRefToRef(weak));",
        "let s: ref<Value> = WeakRefToRef(RefToWeakRef(strong));",
        "let s: ref<Value> = enabled ? WeakRefToRef(weak) : strong;",
        "IsDefined(WeakRefToRef(RefToWeakRef(strong)));",
        "IsDefined(RefToWeakRef(strong));",
    ] {
        let source = format!(
            "class Value {{}} func Test(strong: ref<Value>, weak: wref<Value>, enabled: Bool) {{ {body} }}"
        );
        let errors = emit(&source).unwrap_err();
        assert!(
            errors.iter().any(|e| e.message.contains(
                "cannot convert this reference here; use a typed local, assignment, or return"
            )),
            "{body}: {errors:?}"
        );
        assert!(errors.iter().all(|e| e.span.is_some()), "{errors:?}");
    }
}

#[test]
fn is_defined_checks_weak_handles_without_promoting_them() {
    let text = emit(
        r#"
        class Value { public let weak: wref<Value>; }
        native func OperatorLogicNot(value: Bool) -> Bool
        func GetWeak(values: [Int32]) -> wref<Value> { return null; }
        func Test(weak: wref<Value>, owner: ref<Value>) -> Bool {
            IsDefined(weak);
            IsDefined(WeakRefToRef(weak));
            IsDefined(owner.weak);
            while IsDefined(GetWeak([1])) { continue; }
            return !IsDefined(WeakRefToRef(owner.weak));
        }
    "#,
    )
    .unwrap();
    assert_eq!(text.matches("weak != NULL;").count(), 3, "{text}");
    assert!(text.contains("GetWeak("), "{text}");
    assert!(text.contains(") == NULL)"), "{text}");
    assert!(text.contains("owner.weak != NULL"), "{text}");
    assert!(!text.contains("WeakRefToRef"), "{text}");
}

#[test]
fn viable_reference_overloads_do_not_change_selection_after_erasure() {
    for (conversion, argument) in [
        ("WeakRefToRef(weak)", "weak: wref<Value>"),
        ("RefToWeakRef(strong)", "strong: ref<Value>"),
    ] {
        let source = format!(
            "class Value {{}} func Take(value: ref<Value>) {{}} func Take(value: wref<Value>) {{}} func Test({argument}) {{ Take({conversion}); }}"
        );
        let errors = emit(&source).unwrap_err();
        assert!(
            errors.iter().any(|e| e.message.contains(
                "cannot convert this reference here; use a typed local, assignment, or return"
            ) || e.message.contains("MULTIPLE_MATCHING_OVERLOADS")),
            "{errors:?}"
        );
    }
    let errors = emit(
        r#"
        class Value {
            public static func Take(value: ref<Value>) {}
            public static func Take(value: wref<Value>) {}
        }
        func Test(weak: wref<Value>) { Value.Take(WeakRefToRef(weak)); }
    "#,
    )
    .unwrap_err();
    assert!(
        errors.iter().any(|e| e.message.contains(
            "cannot convert this reference here; use a typed local, assignment, or return"
        ) || e.message.contains("MULTIPLE_MATCHING_OVERLOADS")),
        "{errors:?}"
    );
}

#[test]
fn inapplicable_overloads_and_unrelated_methods_do_not_block_erasure() {
    let text = roundtrip(
        r#"
        class Value {}
        class Other {
            public func Take(value: wref<Value>) {}
            public static func TakeStatic(value: wref<Value>) {}
        }
        class Base {
            public func Take(value: ref<Value>) {}
            public func Take(value: Int32) {}
            public func Take(value: wref<Value>, required: Int32) {}
            public static func TakeStatic(value: ref<Value>) {}
            public static func TakeStatic(value: Int32) {}
        }
        class Child extends Base {}
        func TakeFree(value: ref<Value>) {}
        func TakeFree(value: Int32) {}
        func TakeFree(value: wref<Value>, required: Int32) {}
        func Test(owner: ref<Child>, weak: wref<Value>) {
            TakeFree(WeakRefToRef(weak));
            owner.Take(WeakRefToRef(weak));
            Child.TakeStatic(WeakRefToRef(weak));
        }
    "#,
    );
    assert!(text.contains("TakeFree(weak);"), "{text}");
    assert!(text.contains("owner.Take(weak);"), "{text}");
    assert!(text.contains("Child.TakeStatic(weak);"), "{text}");
}

#[test]
fn inherited_viable_method_overloads_still_block_erasure() {
    let errors = emit(
        r#"
        class Value {}
        class Base { public func Take(value: wref<Value>, opt extra: Int32) {} }
        class Child extends Base { public func Take(value: ref<Value>) {} }
        func Test(owner: ref<Child>, weak: wref<Value>) { owner.Take(WeakRefToRef(weak)); }
    "#,
    )
    .unwrap_err();
    assert!(
        errors.iter().any(|e| e.message.contains(
            "cannot convert this reference here; use a typed local, assignment, or return"
        ) || e.message.contains("MULTIPLE_MATCHING_OVERLOADS")),
        "{errors:?}"
    );
}

#[test]
fn mixed_numeric_siblings_do_not_prove_reference_overloads_disjoint() {
    for (numeric, expression, params) in [
        ("Float", "1", ""),
        ("Double", "1", ""),
        ("Int64", "1", ""),
        ("Uint32", "1", ""),
        ("Float", "number", ", number: Int32"),
    ] {
        let source = format!(
            "class Value {{}} func Take(value: ref<Value>, number: Int32) {{}} func Take(value: wref<Value>, number: {numeric}) {{}} func Test(weak: wref<Value>{params}) {{ Take(WeakRefToRef(weak), {expression}); }}"
        );
        let errors = emit(&source).unwrap_err();
        assert!(
            errors.iter().any(|e| e.message.contains(
                "cannot convert this reference here; use a typed local, assignment, or return"
            )),
            "{source}: {errors:?}"
        );
    }
}

#[test]
fn optional_out_and_null_siblings_do_not_hide_competitors() {
    for (other, argument, params) in [
        ("opt number: Float", "1", ""),
        ("out number: Float", "number", ", number: Int32"),
        ("other: ref<Other>", "null", ""),
    ] {
        let selected = if argument == "null" {
            "other: ref<Value>"
        } else {
            "number: Int32"
        };
        let source = format!(
            "class Value {{}} class Other {{}} func Take(value: ref<Value>, {selected}) {{}} func Take(value: wref<Value>, {other}) {{}} func Test(weak: wref<Value>{params}) {{ Take(WeakRefToRef(weak), {argument}); }}"
        );
        assert!(emit(&source).is_err(), "{source}");
    }
}

#[test]
fn unique_call_with_null_sibling_keeps_exact_reference_context() {
    let text = roundtrip(
        "class Value {} func Take(value: ref<Value>, other: ref<Value>) {} func Test(weak: wref<Value>) { Take(WeakRefToRef(weak), null); }",
    );
    assert!(text.contains("Take(weak, NULL);"), "{text}");
}

#[test]
fn repeated_calls_ignore_unrelated_free_function_groups() {
    let unrelated = (0..256)
        .map(|i| format!("func Unrelated{i}(value: Int32) {{}}\n"))
        .collect::<String>();
    let calls = "Take(WeakRefToRef(weak)); Ordinary(1);\n".repeat(64);
    let source = format!(
        "class Value {{}} func Take(value: ref<Value>) {{}} func Ordinary(value: Int32) {{}} {unrelated} func Test(weak: wref<Value>) {{ {calls} }}"
    );
    let text = emit(&source).unwrap();
    assert_eq!(text.matches("Take(weak);").count(), 64);
    assert_eq!(text.matches("Ordinary(1);").count(), 64);
}

#[test]
fn immediate_reference_conversions_allow_class_upcasts() {
    let text = roundtrip(
        r#"
        class Base {
            public static func Take(value: ref<Base>, other: ref<Base>) -> Bool { return true; }
        }
        class Value extends Base {}
        func Strong(value: wref<Value>) -> ref<Base> { return WeakRefToRef(value); }
        func Weak(value: ref<Value>) -> wref<Base> { return RefToWeakRef(value); }
        func Test(value: wref<Value>, other: wref<Base>, strong: ref<Value>) -> Bool {
            let base: ref<Base> = WeakRefToRef(value);
            let observer: wref<Base> = RefToWeakRef(strong);
            base = WeakRefToRef(value);
            observer = RefToWeakRef(strong);
            return Base.Take(value, other);
        }
    "#,
    );
    assert!(text.contains("return Base.Take(value, other);"), "{text}");
    assert!(text.contains("base = value;"), "{text}");
    assert!(text.contains("observer = strong;"), "{text}");
    assert!(!text.contains("WeakRefToRef("), "{text}");
    assert!(!text.contains("RefToWeakRef("), "{text}");
}

#[test]
fn upcasts_do_not_bypass_competing_reference_overloads() {
    let errors = emit(
        r#"
        class Base {}
        class Value extends Base {}
        func Take(value: ref<Base>) {}
        func Take(value: wref<Value>) {}
        func Test(value: wref<Value>) { Take(WeakRefToRef(value)); }
    "#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("cannot convert this reference here")
                || e.message.contains("MULTIPLE_MATCHING_OVERLOADS")),
        "{errors:?}"
    );
}

#[test]
fn mismatched_sink_and_out_parameters_are_not_contextual_conversions() {
    for source in [
        "class Value {} func Test(weak: wref<Value>) { let w: wref<Value> = WeakRefToRef(weak); }",
        "class Value {} func Take(out value: ref<Value>) {} func Test(weak: wref<Value>) { Take(WeakRefToRef(weak)); }",
        "class Base {} class Value extends Base {} func Test(weak: wref<Base>) -> ref<Value> { return WeakRefToRef(weak); }",
        "class Base {} class Value extends Base {} func Take(value: ref<Value>) {} func Test(weak: wref<Base>) { Take(WeakRefToRef(weak)); }",
    ] {
        assert!(emit(source).is_err(), "{source}");
    }
}
