use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, TypeInterner, infer_from_sources, ir,
};

fn analyze(source: &str, check: impl FnOnce(&[String], &[String])) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("selection.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let codes = reporter
        .reported()
        .iter()
        .map(|d| d.code().to_owned())
        .collect::<Vec<_>>();
    let targets = symbols
        .free_functions()
        .filter(|entry| {
            entry
                .name()
                .as_single_component()
                .is_some_and(|name| name.starts_with("Run"))
                && entry.func().span().is_some_and(|span| span.file == file)
        })
        .filter_map(|entry| {
            let function = unit.functions.get(entry.key())?;
            let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back()? else {
                return None;
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                return None;
            };
            Some(match &**call {
                ir::Call::FreeFunction { function, .. } => {
                    format!(
                        "free({})",
                        symbols[*function]
                            .type_()
                            .param_types()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(",")
                    )
                }
                ir::Call::Instance { method, .. } | ir::Call::Static { method, .. } => {
                    let kind = if matches!(&**call, ir::Call::Instance { .. }) {
                        "instance"
                    } else {
                        "static"
                    };
                    format!(
                        "{kind} {}({})",
                        method.parent(),
                        symbols[*method]
                            .type_()
                            .param_types()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(",")
                    )
                }
                ir::Call::Closure { .. } => panic!("unexpected closure"),
            })
        })
        .collect::<Vec<_>>();
    check(&codes, &targets);
}

#[test]
fn unique_pairwise_reference_signature_wins_in_either_order() {
    for declarations in [
        "func Pick(value: Base) -> Int32 = 1; func Pick(value: Child) -> Int32 = 2;",
        "func Pick(value: Child) -> Int32 = 2; func Pick(value: Base) -> Int32 = 1;",
    ] {
        analyze(
            &format!(
                "class Base {{}} class Child extends Base {{}} {declarations} func Run(value: Child) -> Int32 {{ return Pick(value); }}"
            ),
            |codes, targets| {
                assert!(codes.is_empty(), "{codes:?}");
                assert_eq!(targets, ["free(Child)"]);
            },
        );
    }
}

#[test]
fn crossing_preferences_null_holes_and_optional_overlap_stay_ambiguous() {
    for (declarations, call) in [
        (
            "func Pick(a: Child, b: Base) {} func Pick(a: Base, b: Child) {}",
            "Pick(value, value)",
        ),
        ("func Pick(a: Base) {} func Pick(a: Child) {}", "Pick(null)"),
        (
            "func Pick(opt a: Base, b: Int32) {} func Pick(opt a: Child, b: Int32) {}",
            "Pick(, 1)",
        ),
        (
            "func Pick(a: Base) {} func Pick(a: Child, opt b: Int32) {}",
            "Pick(value)",
        ),
        (
            "func Pick(a: Int32) {} func Pick(a: Int32, opt b: Float) {}",
            "Pick(1)",
        ),
        (
            "func Pick(a: Variant) {} func Pick(a: Child) {}",
            "Pick(value)",
        ),
        (
            "func Pick(a: wref<Base>) {} func Pick(a: wref<Child>) {}",
            "Pick(weakValue)",
        ),
        (
            "func Pick(a: script_ref<Base>) {} func Pick(a: script_ref<Child>) {}",
            "Pick(value)",
        ),
    ] {
        analyze(
            &format!(
                "class Base {{}} class Child extends Base {{}} {declarations} func Run(value: Child, weakValue: wref<Child>) {{ {call}; }}"
            ),
            |codes, _| {
                assert_eq!(codes, ["MULTIPLE_MATCHING_OVERLOADS"], "{call}: {codes:?}");
            },
        );
    }
}

#[test]
fn out_parameters_remain_invariant() {
    analyze(
        "class Base {} class Child extends Base {} func Pick(out value: Base) -> Int32 = 1; func Pick(out value: Child) -> Int32 = 2; func Run(value: Child) -> Int32 { return Pick(value); }",
        |codes, targets| {
            assert!(codes.is_empty(), "{codes:?}");
            assert_eq!(targets, ["free(Child)"]);
        },
    );
}

#[test]
fn forwarder_is_fallback_in_either_declaration_order() {
    for declarations in [
        "public static func Pick(self: Host, value: Int32) -> Int32 = 1; public func Pick(value: Int32) -> Int32 = 2;",
        "public func Pick(value: Int32) -> Int32 = 2; public static func Pick(self: Host, value: Int32) -> Int32 = 1;",
    ] {
        analyze(
            &format!(
                "class Root {{}} class Host extends Root {{ {declarations} public static func Other(self: Host, value: String) -> Int32 = 3; }} func Run(host: Host) -> Int32 {{ return host.Pick(1); }} func RunAlias(host: Host) -> Int32 {{ return host.Other(\"x\"); }} func RunStatic(host: Host) -> Int32 {{ return Host.Pick(host, 1); }}"
            ),
            |codes, targets| {
                assert!(codes.is_empty(), "{codes:?}");
                assert!(
                    targets.contains(&"instance Host(Int32)".into()),
                    "{targets:?}"
                );
                assert!(
                    targets.contains(&"static Host(Host,String)".into()),
                    "{targets:?}"
                );
                assert!(
                    targets.contains(&"static Host(Host,Int32)".into()),
                    "{targets:?}"
                );
            },
        );
    }
}

#[test]
fn inherited_real_method_beats_child_forwarder() {
    analyze(
        "class Base { public func Pick(value: Int32) -> Int32 = 1; } class Child extends Base { public static func Pick(self: Child, value: Int32) -> Int32 = 2; } func Run(value: Child) -> Int32 { return value.Pick(1); }",
        |codes, targets| {
            assert!(codes.is_empty(), "{codes:?}");
            assert_eq!(targets, ["instance Base(Int32)"]);
        },
    );
}

#[test]
fn static_lookup_collects_ancestry_and_hides_only_matching_signatures() {
    analyze(
        "class Base { public static func Pick(value: Int32) -> Int32 = 1; public static func Same(value: Int32) -> Int32 = 1; } class Middle extends Base { public static func Pick(value: String) -> Int32 = 2; public static func Same(value: Int32) -> Int32 = 2; } class Child extends Middle { public func Pick(value: Bool) -> Int32 = 3; } func Run() -> Int32 { return Child.Pick(1); } func RunSame() -> Int32 { return Child.Same(1); }",
        |codes, targets| {
            assert_eq!(
                codes,
                ["CALLING_BASE_STATIC_METHOD", "CALLING_BASE_STATIC_METHOD"]
            );
            assert!(
                targets.contains(&"static Base(Int32)".into()),
                "{targets:?}"
            );
            assert!(
                targets.contains(&"static Middle(Int32)".into()),
                "{targets:?}"
            );
        },
    );
}

#[test]
fn ambiguity_does_not_freeze_argument_inference() {
    analyze(
        "class Base {} class Child extends Base {} func Pick(a: Child, b: Base) {} func Pick(a: Base, b: Child) {} func Run() { let value; Pick(value, value); value = new Base(); }",
        |codes, _| assert_eq!(codes, ["MULTIPLE_MATCHING_OVERLOADS"]),
    );
}

#[test]
fn fresh_local_does_not_supply_reference_specificity_evidence() {
    for declarations in [
        "func Pick(value: Base) {} func Pick(value: Child) {}",
        "func Pick(value: Child) {} func Pick(value: Base) {}",
    ] {
        analyze(
            &format!(
                "class Base {{}} class Child extends Base {{}} {declarations} func Run() {{ let value; Pick(value); value = new Base(); }}"
            ),
            |codes, _| assert_eq!(codes, ["MULTIPLE_MATCHING_OVERLOADS"]),
        );
    }
}

#[test]
fn lazy_source_checks_ignore_safe_eager_arguments() {
    for overload in [
        "",
        "native func Choose(values: [Int32], skip value: Int32);",
    ] {
        analyze(
            &format!(
                "native func Choose(values: [Int32], skip value: Bool); {overload} func Run() {{ Choose([1], true); }}"
            ),
            |codes, _| assert!(codes.is_empty(), "{codes:?}"),
        );
        analyze(
            &format!(
                "native func Read(values: [Int32]) -> Bool; native func Choose(values: [Int32], skip value: Bool); {overload} func Run() {{ Choose([1], Read([2])); }}"
            ),
            |codes, _| assert_eq!(codes, ["UNSAFE_ARRAY_EVALUATION"]),
        );
    }
}

#[test]
fn lazy_forwarder_receiver_rejects_nested_array_initialization() {
    for values in ["[]", "[1]"] {
        analyze(
            &format!(
                "class Host {{ public static func Get(values: [Int32]) -> Host = null; public static func Pick(skip self: Host) {{}} }} func Run() {{ Host.Get({values}).Pick(); }}"
            ),
            |codes, _| assert_eq!(codes, ["UNSAFE_ARRAY_EVALUATION"], "{values}: {codes:?}"),
        );
    }
    analyze(
        "class Host { public static func Get() -> Host = null; public static func Pick(skip self: Host) {} } func Run() { Host.Get().Pick(); } func RunLocal(host: Host) { host.Pick(); }",
        |codes, _| assert!(codes.is_empty(), "{codes:?}"),
    );
}

#[test]
fn lazy_source_checks_use_only_the_selected_overload() {
    for declarations in [
        "native func Choose(value: [Int32]); native func Choose(skip value: Bool);",
        "native func Choose(skip value: Bool); native func Choose(value: [Int32]);",
    ] {
        analyze(
            &format!("{declarations} func Run() {{ Choose([1]); }}"),
            |codes, _| assert!(codes.is_empty(), "{codes:?}"),
        );
    }
}

#[test]
fn lazy_source_checks_reject_empty_and_nested_arrays() {
    for value in ["[]", "[1]", "Read([1])"] {
        let parameter = if value.starts_with("Read") {
            "Bool"
        } else {
            "[Int32]"
        };
        analyze(
            &format!(
                "native func Read(values: [Int32]) -> Bool; native func Choose(skip value: {parameter}); native func Choose(skip value: String); func Run() {{ Choose({value}); }}"
            ),
            |codes, _| assert_eq!(codes, ["UNSAFE_ARRAY_EVALUATION"], "{value}: {codes:?}"),
        );
    }
}

#[test]
fn numeric_overloads_do_not_gain_an_unprobed_preference() {
    analyze(
        "func Pick(value: Int32) -> Int32 = 1; func Pick(value: Int64) -> Int32 = 2; func Run() -> Int32 { return Pick(1); }",
        |codes, targets| {
            assert!(codes.is_empty(), "{codes:?}");
            assert_eq!(targets, ["free(Int32)"]);
        },
    );
}

#[test]
fn static_hiding_retains_visibility_and_same_class_duplicates() {
    analyze(
        "class Base { public static func Pick(value: Int32) {} } class Child extends Base { private static func Pick(value: Int32) {} } func Run() { Child.Pick(1); }",
        |codes, _| assert_eq!(codes, ["INACCESSIBLE_METHOD"]),
    );
    analyze(
        "class Host { public static func Pick(value: Int32) -> Int32 = 1; public static func Pick(value: Int32) -> Bool = true; } func Run() -> Int32 { return Host.Pick(1); }",
        |codes, _| assert_eq!(codes, ["MULTIPLE_MATCHING_OVERLOADS"]),
    );
}

#[test]
fn return_only_duplicates_are_not_ranked_by_expected_result() {
    analyze(
        "class Root {} class Host extends Root { public func Pick(value: Int32) -> Int32 = 1; public func Pick(value: Int32) -> Bool = true; } func Run(host: Host) -> Int32 { return host.Pick(1); }",
        |codes, _| {
            assert!(codes.iter().any(|code| code == "DUP_METHOD"), "{codes:?}");
            assert!(
                codes
                    .iter()
                    .any(|code| code == "MULTIPLE_MATCHING_OVERLOADS"),
                "{codes:?}"
            );
        },
    );
}
