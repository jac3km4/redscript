use redscript_community_ast::SourceMap;
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, FunctionType, LoweredCompilationUnit, Symbols, Type,
    TypeInterner, parse_files, predef, process_sources_with_policy,
};

fn check(
    files: &[(&str, &str)],
    inspect: impl FnOnce(&LoweredCompilationUnit<'_>, &Symbols<'_>, &[String]),
) {
    let sources = SourceMap::new();
    for (path, source) in files {
        sources.push_back(*path, *source);
    }
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, symbols) = process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[],
    );
    let messages = reporter
        .reported()
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>();
    inspect(&unit, &symbols, &messages);
}

#[test]
fn unresolved_slots_preserve_names_flags_spans_and_arity_without_lowering_bodies() {
    for missing in 0..3 {
        let types = [0, 1, 2].map(|i| if i == missing { "Missing" } else { "Int32" });
        let source = format!(
            "func Broken(out first: {}, opt middle: {}, opt out last: {}) -> Int32 {{ return first; }}",
            types[0], types[1], types[2]
        );
        check(&[("slots.reds", &source)], |unit, symbols, messages| {
            assert_eq!(messages.len(), 1, "{messages:?}");
            assert!(messages[0].contains("`Missing` is not a known type"));
            let entry = symbols.free_functions().next().unwrap();
            let typ = entry.func().type_();
            assert!(!typ.is_valid());
            assert_eq!(typ.min_args(), 1);
            assert_eq!(typ.max_args(), 3);
            assert!(!unit.functions.contains_key(entry.key()));
            for (i, param) in typ.params().iter().enumerate() {
                assert_eq!(param.name(), ["first", "middle", "last"][i]);
                assert_eq!(param.flags().is_optional(), i != 0);
                assert_eq!(param.flags().is_out(), i != 1);
                assert!(param.span().is_some());
                assert_eq!(
                    param.type_(),
                    &if i == missing {
                        Type::Nothing
                    } else {
                        Type::nullary(predef::INT32)
                    }
                );
            }
        });
    }
}

#[test]
fn explicit_failed_return_is_not_omitted_void_and_cannot_be_called() {
    check(
        &[(
            "return.reds",
            "func Broken() -> Missing { return 1; } func Omitted() {} func Use() { Broken(); }",
        )],
        |unit, symbols, messages| {
            assert_eq!(messages.len(), 2, "{messages:?}");
            assert!(messages.iter().any(|m| m.contains("invalid signature")));
            for entry in symbols.free_functions() {
                let typ = entry.func().type_();
                if entry.name().to_string() == "Broken" {
                    assert!(!typ.is_valid());
                    assert_eq!(typ.return_type(), &Type::Nothing);
                    assert!(!unit.functions.contains_key(entry.key()));
                } else if entry.name().to_string() == "Omitted" {
                    assert!(typ.is_valid());
                    assert_eq!(typ.return_type(), &Type::nullary(predef::VOID));
                    assert!(unit.functions.contains_key(entry.key()));
                }
            }
        },
    );
}

#[test]
fn invalid_overloads_do_not_collapse_or_match_at_written_or_shortened_arity() {
    check(
        &[(
            "overloads.reds",
            "func Pick(id: Missing, value: Int32) {} func Pick(id: OtherMissing, value: Int32) {} func Pick(value: Int32) {} func Use() { Pick(1); Pick(1, 2); }",
        )],
        |unit, symbols, messages| {
            assert_eq!(messages.len(), 3, "{messages:?}");
            assert_eq!(
                messages
                    .iter()
                    .filter(|m| m.contains("invalid signature"))
                    .count(),
                1
            );
            assert_eq!(
                symbols
                    .free_functions()
                    .filter(|e| e.name().to_string() == "Pick")
                    .count(),
                3
            );
            assert_eq!(unit.functions.len(), 2);
        },
    );
}

#[test]
fn invalid_methods_do_not_become_overrides_duplicates_or_forwarders() {
    check(
        &[(
            "methods.reds",
            "class Base { public func Test(value: Int32) {} } class Child extends Base { public func Test(id: Missing, value: Int32) {} public func Test(id: OtherMissing, value: Int32) {} public static func Forward(self: Child, value: Missing) {} }",
        )],
        |unit, symbols, messages| {
            assert_eq!(messages.len(), 3, "{messages:?}");
            let (_, child) = symbols
                .types()
                .find(|(id, _)| id.to_string() == "Child")
                .unwrap();
            let methods = child.schema().as_aggregate().unwrap().methods();
            assert_eq!(methods.iter().count(), 3);
            for entry in methods.iter() {
                assert!(!entry.func().type_().is_valid());
                assert!(entry.func().overriden().is_none());
            }
            assert_eq!(
                unit.classes
                    .values()
                    .map(|class| class.methods.len())
                    .sum::<usize>(),
                1
            );
        },
    );
}

#[test]
fn provider_restoration_and_unaffected_files_work_in_both_orders() {
    for reverse in [false, true] {
        for restored in [false, true] {
            let mut files = vec![
                (
                    "consumer.reds",
                    "func Broken(id: Provider, value: Int32) -> Int32 { return value; } func Good(value: Int32) -> Int32 { return value; }",
                ),
                (
                    "provider.reds",
                    if restored { "class Provider {}" } else { "" },
                ),
            ];
            if reverse {
                files.reverse();
            }
            check(&files, |unit, symbols, messages| {
                assert_eq!(messages.len(), usize::from(!restored), "{messages:?}");
                for entry in symbols.free_functions() {
                    let valid = restored || entry.name().to_string() == "Good";
                    assert_eq!(entry.func().type_().is_valid(), valid);
                    assert_eq!(unit.functions.contains_key(entry.key()), valid);
                    assert_eq!(
                        entry.func().type_().params().last().unwrap().name(),
                        "value"
                    );
                }
            });
        }
    }
}

#[test]
fn invalid_instance_static_and_operator_calls_fail_closed() {
    for (declaration, call) in [
        (
            "class Host { public func Test(id: Missing, value: Int32) {} }",
            "func Use(host: Host) { host.Test(1); host.Test(1, 2); }",
        ),
        (
            "class Host { public static func Test(id: Missing, value: Int32) {} }",
            "func Use() { Host.Test(1); Host.Test(1, 2); }",
        ),
        (
            "func OperatorAdd(lhs: Missing, rhs: Int32) -> Int32 { return rhs; }",
            "func Use(value: Int32) { let result: Int32 = value + value; }",
        ),
    ] {
        let source = format!("{declaration} {call}");
        check(&[("calls.reds", &source)], |_, _, messages| {
            assert!(
                messages.iter().any(|m| m.contains("invalid signature")),
                "{messages:?}"
            );
            assert!(
                messages
                    .iter()
                    .all(|m| m.contains("invalid signature") || m.contains("is not a known type")),
                "{messages:?}"
            );
        });
    }
}

#[test]
fn nested_unknown_types_and_failed_generic_bounds_invalidate_the_signature() {
    for source in [
        "func Broken(id: [Missing], value: Int32) {}",
        "func Broken() -> [Missing] {}",
        "func Broken<T extends Missing>(value: T) {}",
    ] {
        check(&[("nested.reds", source)], |unit, symbols, messages| {
            assert!(!messages.is_empty());
            let entry = symbols.free_functions().next().unwrap();
            assert!(!entry.func().type_().is_valid());
            assert!(!unit.functions.contains_key(entry.key()));
        });
    }
}

#[test]
fn invalid_generic_applications_do_not_reach_inference_or_inheritance() {
    for invalid_type in ["array<Int32, Int32>", "Box<Int32>"] {
        let source = format!(
            "class Bound {{}} class Box<T extends Bound> {{}} \
             func Broken(value: {invalid_type}) {{ missing; }} \
             native func Native() -> {invalid_type}; \
             func Generic<T extends {invalid_type}>(value: T) {{ missing; }} \
             class Base {{ public func Test(value: Int32) {{}} }} \
             class Child extends Base {{ \
                 public func Test(value: {invalid_type}) {{ missing; }} \
                 public func Return() -> {invalid_type} {{ missing; }} \
                 public static func Forward(self: Child, value: {invalid_type}) {{ missing; }} \
             }}"
        );
        check(&[("generic.reds", &source)], |unit, symbols, messages| {
            assert!(!messages.is_empty());
            assert_eq!(messages.len(), 6, "{messages:?}");
            for entry in symbols.free_functions() {
                assert!(!entry.func().type_().is_valid());
                assert!(!unit.functions.contains_key(entry.key()));
            }
            let (_, child) = symbols
                .types()
                .find(|(id, _)| id.to_string() == "Child")
                .unwrap();
            let methods = child.schema().as_aggregate().unwrap().methods();
            // A synthesized static forwarder must also remain invalid.
            for entry in methods.iter() {
                assert!(!entry.func().type_().is_valid());
                assert!(entry.func().overriden().is_none());
            }
            assert_eq!(
                unit.classes
                    .values()
                    .map(|c| c.methods.len())
                    .sum::<usize>(),
                1
            );
        });
    }
}

#[test]
fn invalid_generic_base_signatures_are_not_override_candidates() {
    for (base_type, child_type) in [
        ("array<Int32, Int32>", "Int32"),
        ("Int32", "array<Int32, Int32>"),
    ] {
        let source = format!(
            "class Base {{ public func Test() -> {base_type} {{ return 1; }} }} \
             class Child extends Base {{ public func Test() -> {child_type} {{ return 1; }} }}"
        );
        check(&[("base.reds", &source)], |_, symbols, messages| {
            assert_eq!(messages.len(), 1, "{messages:?}");
            assert!(messages[0].contains("expects 1 type argument, but found 2"));
            let (_, child) = symbols
                .types()
                .find(|(id, _)| id.to_string() == "Child")
                .unwrap();
            for entry in child.schema().as_aggregate().unwrap().methods().iter() {
                assert!(entry.func().overriden().is_none());
            }
        });
    }
}

#[test]
fn signature_diagnostics_follow_declaration_order_within_each_phase() {
    let files = [
        (
            "first.reds",
            "class Zebra { func Test(value: array<Int32, Int32>) {} } \
             func ZebraFunction(value: array<Int32, Int32, Int32>) {} \
             class Alpha { func Test(value: array<Int32, Int32, Int32, Int32>) {} }",
        ),
        (
            "second.reds",
            "class Middle { func Test(value: array<Int32, Int32, Int32, Int32, Int32>) {} } \
             func AlphaFunction(value: array<Int32, Int32, Int32, Int32, Int32, Int32>) {}",
        ),
    ];
    for _ in 0..16 {
        check(&files, |_, _, messages| {
            assert_eq!(messages.len(), 5, "{messages:?}");
            for (message, count) in messages.iter().zip([3, 6, 2, 4, 5]) {
                assert!(
                    message.ends_with(&format!("but found {count}")),
                    "{messages:?}"
                );
            }
        });
    }
}

#[test]
fn signature_transformations_do_not_clear_invalidity() {
    let invalid = FunctionType::default().with_invalid_signature(true);
    assert!(!invalid.clone().with_params([]).is_valid());
    assert!(
        !invalid
            .with_return_type(Type::nullary(predef::BOOL))
            .is_valid()
    );
}
