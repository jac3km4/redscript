use redscript_compiler_frontend::Shared;

use redscript_community_ast::{self as ast, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, DiagnosticLevel, Evaluator, SemanticPolicy, Symbols, TypeInterner,
    parse_files, process_sources_with_policy,
};

fn check(source: &str, expected: &[&str]) {
    check_level(
        source,
        expected,
        DiagnosticLevel::Warning,
        "a `const` parameter",
    );
}

fn check_forwarding(source: &str, expected: &[&str]) {
    check_level(
        source,
        expected,
        DiagnosticLevel::Warning,
        "forwarding a const parameter to a non-const parameter may allow mutation",
    );
}

fn check_level(source: &str, expected: &[&str], level: DiagnosticLevel, message: &str) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("const.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let evaluator = Evaluator::from_modules(&modules);
    process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[],
    );
    let spans = reporter
        .reported()
        .iter()
        .map(|diagnostic| {
            assert_eq!(diagnostic.level(), level, "{source}: {diagnostic:?}");
            let rendered = diagnostic.to_string();
            assert!(
                rendered.contains(message),
                "{source}: expected `{rendered}` to contain `{message}`"
            );
            assert_eq!(
                diagnostic.code(),
                if message == "`this` in a `const` method" {
                    "CONST_RECEIVER_WRITE"
                } else if message == "a `const` field" {
                    "CONST_FIELD_WRITE"
                } else if message == "a `const` parameter" {
                    "CONST_PARAMETER_WRITE"
                } else {
                    "CONST_FORWARDING"
                }
            );
            let span = diagnostic.span();
            assert_eq!(span.file, file);
            &source[span.start as usize..span.end as usize]
        })
        .collect::<Vec<_>>();
    assert_eq!(spans, expected, "{source}");
}

#[test]
fn strict_assignments_and_inherited_fields() {
    check("func Write(const value: Int32) { value = 1; }", &["value"]);
    check(
        "class Base { public let field: Int32; } class Child extends Base {}\nfunc Write(const value: Child) { value.field = 1; }",
        &["value.field"],
    );
    check(
        "func Write(const value: [Int32]) { value[0] = 1; }",
        &["value[0]"],
    );
    check(
        "func Write(const value: [[Int32]]) { value[0][0] = 1; }",
        &["value[0][0]"],
    );
    check(
        "struct Value { let field: Int32; } func Write(const value: [Value]) { value[0].field = 1; }",
        &["value[0].field"],
    );
    check("func Write(value: [Int32]) { value[0] = 1; }", &[]);
    check(
        "func Read(const value: [Int32]) -> Int32 { return value[0]; }",
        &[],
    );
}

#[test]
fn strict_forwarding_checks_resolved_parameter_flags_and_types() {
    check(
        "func Take(out value: Int32) {} func Forward(const value: Int32) { Take(value); }",
        &["value"],
    );
    check(
        "func Take(value: Int32) {} func Forward(const value: Int32) { Take(value); }",
        &[],
    );
    check_forwarding(
        "func Take(value: [Int32]) {} func Forward(const value: [Int32]) { Take(value); }",
        &["value"],
    );
    check(
        "func Take(const value: [Int32]) {} func Forward(const value: [Int32]) { Take(value); }",
        &[],
    );
    check_forwarding(
        "class Value {} func Take(value: Value) {} func Forward(const value: Value) { Take(value); }",
        &["value"],
    );
    check_forwarding(
        "class Value {} class Calls { public static func Take(value: Value) {} public func Pass(value: Value) {} } func Forward(const value: Value, calls: Calls) { Calls.Take(value); calls.Pass(value); }",
        &["value", "value"],
    );
    check(
        "func Take(out value: Int32) {} func Forward(const value: [Int32]) { Take(value[0]); }",
        &["value[0]"],
    );
}

#[test]
fn array_mutation_and_reference_wrappers_are_checked() {
    for call in [
        "ArrayClear(value)",
        "ArrayResize(value, 1)",
        "ArrayPush(value, 1)",
        "ArrayPop(value)",
        "ArrayInsert(value, 0, 1)",
        "ArrayRemove(value, 1)",
        "ArrayGrow(value, 1)",
        "ArrayErase(value, 0)",
        "ArraySort(value)",
    ] {
        check(
            &format!("func Write(const value: [Int32]) {{ {call}; }}"),
            &["value"],
        );
    }
    check(
        "func OperatorAssignAdd(out lhs: Int32, rhs: Int32) {} func Write(const value: Int32) { value += 1; }",
        &["value"],
    );
    check(
        "@intrinsic(ArrayPush) func Push<T>(out array: [T], value: T)\nfunc Write(const value: [Int32]) { Push(value, 1); }",
        &["value"],
    );
    check_forwarding(
        "func Take(value: script_ref<Int32>) {} func Forward(const value: Int32) { Take(AsRef(value)); }",
        &["AsRef(value)"],
    );
    check(
        "func Write(const value: script_ref<Int32>) { Deref(value) = 1; }",
        &["Deref(value)"],
    );
}

#[test]
fn strong_weak_conversions_preserve_readonly_provenance() {
    check(
        "class Value { public let field: Int32; } func Write(const value: wref<Value>) { WeakRefToRef(value).field = 1; }",
        &["WeakRefToRef(value).field"],
    );
    check(
        "class Value { public let field: Int32; } func Write(const value: ref<Value>) { RefToWeakRef(value).field = 1; }",
        &["RefToWeakRef(value).field"],
    );
    for (source_type, target_type, conversion) in [
        ("wref<Value>", "ref<Value>", "WeakRefToRef(value)"),
        ("ref<Value>", "wref<Value>", "RefToWeakRef(value)"),
        (
            "ref<Value>",
            "ref<Value>",
            "WeakRefToRef(RefToWeakRef(value))",
        ),
    ] {
        check_forwarding(
            &format!(
                "class Value {{}} func Take(value: {target_type}) {{}} func Forward(const value: {source_type}) {{ Take({conversion}); }}"
            ),
            &[conversion],
        );
        check(
            &format!(
                "class Value {{}} func Take(const value: {target_type}) {{}} func Forward(const value: {source_type}) {{ Take({conversion}); }}"
            ),
            &[],
        );
    }
    check_forwarding(
        "class Value {} func Take(value: ref<Value>) {} func Forward(const value: wref<Value>) { Take(value); }",
        &["value"],
    );
    check_forwarding(
        "class Value {} func Take(value: wref<Value>) {} func Forward(const value: ref<Value>) { Take(value); }",
        &["value"],
    );
}

#[test]
fn conditional_roots_check_both_branches() {
    for expr in [
        "flag ? value : other",
        "flag ? other : value",
        "flag ? value : value",
    ] {
        let source = format!(
            "func Take(value: [Int32]) {{}} func Forward(const value: [Int32], other: [Int32], flag: Bool) {{ Take({expr}); }}"
        );
        check_forwarding(&source, &[expr]);
    }
}

#[test]
fn closures_check_captures_without_confusing_parameter_indices() {
    check_level(
        "func Forward(const value: [Int32], f: ([Int32]) -> Void) { f(value); }",
        &["value"],
        DiagnosticLevel::Warning,
        "forwarding a const parameter to a function value may allow mutation",
    );
    check(
        "func Write(const value: Int32) { let f: () -> Void = () -> { value = 1; }; }",
        &["value"],
    );
    check(
        "func Write(const value: [Int32]) { let f: () -> Void = () -> { value[0] = 1; }; }",
        &["value[0]"],
    );
    check(
        "func Write(const value: Int32) { let f: (Int32) -> Void = (other: Int32) -> { other = 1; }; }",
        &[],
    );
    check(
        "func Write(const value: Int32) { let f: () -> Void = () -> { let g: () -> Void = () -> { value = 1; }; }; }",
        &["value"],
    );
}

#[test]
fn const_relaxations_are_independent_and_file_local_in_both_orders() {
    for reverse in [false, true] {
        for (elements, forwarding) in [(false, false), (true, false), (false, true), (true, true)] {
            let sources = SourceMap::new();
            sources.push_back("calls.reds", "func Take(value: [Int32]) {} func Out(out value: [Int32]) {} @intrinsic(ArrayClear) func Clear<T>(out value: [T])");
            let body =
                "(const value: [Int32]) { value[0] = 1; Take(value); Out(value); Clear(value); }";
            let relaxed = sources.push_back("relaxed.reds", format!("func Relaxed{body}"));
            let strict = sources.push_back("strict.reds", format!("func Strict{body}"));
            let interner = TypeInterner::default();
            let mut reporter = CompileErrorReporter::default();
            let mut modules = parse_files(&sources, &mut reporter);
            assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
            if reverse {
                modules.reverse();
            }
            let evaluator = Evaluator::from_modules(&modules);
            process_sources_with_policy(
                modules,
                Symbols::with_default_types(),
                evaluator,
                &mut reporter,
                &interner,
                &[(
                    relaxed,
                    Shared::new(SemanticPolicy {
                        const_param_element_writes: elements,
                        relaxed_const_checks: forwarding,
                        ..SemanticPolicy::default()
                    }),
                )],
            );
            let count = |file: ast::FileId| {
                reporter
                    .reported()
                    .iter()
                    .filter(|d| d.span().file == file)
                    .count()
            };
            assert_eq!(count(strict), 4, "{:?}", reporter.reported());
            assert_eq!(
                count(relaxed),
                4 - usize::from(elements) - usize::from(forwarding),
                "{:?}",
                reporter.reported()
            );
            for (file, warning_count) in [
                (strict, 4),
                (relaxed, 4 - usize::from(elements) - usize::from(forwarding)),
            ] {
                for (level, expected) in [
                    (DiagnosticLevel::Warning, warning_count),
                    (DiagnosticLevel::Error, 0),
                ] {
                    assert_eq!(
                        reporter
                            .reported()
                            .iter()
                            .filter(|d| d.span().file == file && d.level() == level)
                            .count(),
                        expected,
                        "{:?}",
                        reporter.reported()
                    );
                }
            }
        }
    }
}

#[test]
fn generic_forwarding_checks_instantiated_parameter_types() {
    check_forwarding(
        "func Take<T>(value: T) {} func Forward(const value: Int32) { Take(value); }",
        &[],
    );
    check_forwarding(
        "func Take<T>(value: T) {} func Forward(const value: [Int32]) { Take(value); }",
        &["value"],
    );
    check_forwarding(
        "class Item {} func Take<T>(value: T) {} func Forward(const value: Item) { Take(value); }",
        &["value"],
    );
    check_forwarding(
        "class Box<T> { public func Set(value: T) {} } func Forward(box: Box<[Int32]>, const value: [Int32]) { box.Set(value); }",
        &["value"],
    );
    check_forwarding(
        "func Take<T>(const value: T) {} func Forward(const value: [Int32]) { Take(value); }",
        &[],
    );
}

#[test]
fn non_const_methods_on_const_parameters_are_rejected() {
    check(
        "class Item { public func Mutate() {} public const func Read() -> Int32 { return 1; } }\nfunc Use(const item: Item) { item.Read(); item.Mutate(); }",
        &["item"],
    );
}

#[test]
fn to_variant_checks_const_mutable_forwarding() {
    check_forwarding(
        "func Box(const value: Int32) -> Variant { return ToVariant(value); }",
        &[],
    );
    check_forwarding(
        "func Box(const value: [Int32]) -> Variant { return ToVariant(value); }",
        &["value"],
    );
    check_forwarding(
        "class Item {} func Box(const value: Item) -> Variant { return ToVariant(value); }",
        &["value"],
    );
}

#[test]
fn const_methods_enforce_readonly_this_in_community_policy() {
    check_level(
        "class Item { private let value: Int32; private let items: [Int32]; public func Mutate() {} public const func Get() -> Int32 { return this.value; } public const func BadWrite() { this.value = 1; } public const func BadArray() { ArrayPush(this.items, 1); } public const func BadCall() { this.Mutate(); } }",
        &["this.value", "this.items", "this"],
        DiagnosticLevel::Warning,
        "`this` in a `const` method",
    );

    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back(
        "const.reds",
        "class Item { private let value: Int32; public const func Write() { this.value = 1; } }",
    );
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let evaluator = Evaluator::from_modules(&modules);
    let policy = SemanticPolicy {
        relaxed_const_checks: true,
        ..SemanticPolicy::default()
    };
    process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[(file, Shared::new(policy))],
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
}

#[test]
fn const_fields_are_enforced_in_community_policy() {
    check_level(
        "class Item { public const let value: Int32; public const let items: [Int32]; public func Mutate() {} }\nclass Holder { public const let item: Item; }\nfunc Use(holder: Holder) { let x = holder.item.value; holder.item.value = 1; ArrayPush(holder.item.items, 1); holder.item.Mutate(); }",
        &["holder.item.value", "holder.item.items", "holder.item"],
        DiagnosticLevel::Warning,
        "a `const` field",
    );

    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back(
        "const.reds",
        "class Item { public const let value: Int32; public func Write() { this.value = 1; } }",
    );
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let evaluator = Evaluator::from_modules(&modules);
    let policy = SemanticPolicy {
        relaxed_const_checks: true,
        ..SemanticPolicy::default()
    };
    process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[(file, Shared::new(policy))],
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
}

#[test]
fn override_const_compatibility_is_enforced_in_community_policy() {
    let check_override = |source: &str, expected_errors: usize, policy: SemanticPolicy| {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let file = sources.push_back("const.reds", source);
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(&sources, &mut reporter);
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        let evaluator = Evaluator::from_modules(&modules);
        process_sources_with_policy(
            modules,
            Symbols::with_default_types(),
            evaluator,
            &mut reporter,
            &interner,
            &[(file, Shared::new(policy))],
        );
        let errors: Vec<_> = reporter
            .reported()
            .iter()
            .filter(|d| d.code() == "OVERRIDE_CONST_MISMATCH")
            .inspect(|d| assert_eq!(d.level(), DiagnosticLevel::ErrorAllowedAtRuntime, "{d:?}"))
            .collect();
        assert_eq!(errors.len(), expected_errors, "{source}: {errors:?}");
    };

    // Valid Liskov-safe overrides: adding const on method/param, or dropping const on return (non-script_ref).
    check_override(
        "class Item {}
        class Base {
            public func Mut() {}
            public func Take(x: Item) {}
            public func Get() -> const Item { return new Item(); }
        }
        class Child extends Base {
            public const func Mut() {}
            public func Take(const x: Item) {}
            public func Get() -> Item { return new Item(); }
        }",
        0,
        SemanticPolicy::default(),
    );

    // Invalid overrides in community policy: dropping const on method/param, or adding const on return.
    let invalid = "class Item {}
        class Base {
            public const func Read() {}
            public func Take(const x: Item) {}
            public func Get() -> Item { return new Item(); }
        }
        class Child extends Base {
            public func Read() {}
            public func Take(x: Item) {}
            public func Get() -> const Item { return new Item(); }
        }";
    check_override(invalid, 3, SemanticPolicy::default());
    check_override(
        invalid,
        0,
        SemanticPolicy {
            relaxed_const_checks: true,
            ..SemanticPolicy::default()
        },
    );

    // script_ref return const mismatches are already reported by OVERRIDE_RETURN_TYPE_MISMATCH.
    check_override(
        "class Item {}
        class Base {
            public func Ref(item: Item) -> script_ref<Item> { return item; }
        }
        class Child extends Base {
            public func Ref(item: Item) -> const script_ref<Item> { return item; }
        }",
        0,
        SemanticPolicy::default(),
    );
}

#[test]
fn const_write_diagnostic_messages() {
    let source = "
        class Item {
            public const let value: Int32;
            public let count: Int32;
            public let items: [Int32];
            public func Mutate() {}
            public const func BadReceiver() {
                this.count = 1;
                ArrayPush(this.items, 1);
                this.Mutate();
            }
        }
        func OutInt(out x: Int32) {}
        func Test(const param: Item, const arr: [Int32]) {
            param = new Item();
            param.count = 1;
            ArrayPush(arr, 1);
            OutInt(param.count);
            param.Mutate();
            const local: Item;
            local = new Item();
            local.count = 1;
            local.value = 2;
        }
    ";
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("const.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let evaluator = Evaluator::from_modules(&modules);
    process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[],
    );
    let messages: Vec<String> = reporter
        .reported()
        .iter()
        .map(ToString::to_string)
        .collect();
    assert_eq!(
        messages,
        [
            "cannot assign to a `const` parameter",
            "cannot modify a `const` parameter",
            "cannot modify a `const` parameter with `ArrayPush`",
            "cannot pass a `const` parameter to an `out` parameter",
            "cannot call non-`const` method `Mutate` on a `const` parameter",
            "cannot assign to a `const` local",
            "cannot modify a `const` local",
            "cannot modify a `const` local",
            "cannot modify `this` in a `const` method",
            "cannot modify `this` in a `const` method with `ArrayPush`",
            "cannot call non-`const` method `Mutate` on `this` in a `const` method",
        ]
    );
}
