use std::rc::Rc;

use redscript_community_ast::{self as ast, SourceMap};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, DiagnosticLevel, Evaluator, SemanticPolicy, Symbols,
    TypeInterner, parse_files, process_sources_with_policy,
};

fn check(source: &str, expected: &[&str]) {
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
            assert!(
                matches!(diagnostic, Diagnostic::Other(error, DiagnosticLevel::Error, _)
            if error.to_string() == "cannot write through a const parameter"),
                "{source}: {diagnostic:?}"
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
    check(
        "func Take(value: [Int32]) {} func Forward(const value: [Int32]) { Take(value); }",
        &["value"],
    );
    check(
        "func Take(const value: [Int32]) {} func Forward(const value: [Int32]) { Take(value); }",
        &[],
    );
    check(
        "class Value {} func Take(value: Value) {} func Forward(const value: Value) { Take(value); }",
        &["value"],
    );
    check(
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
    check(
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
        check(
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
    check(
        "class Value {} func Take(value: ref<Value>) {} func Forward(const value: wref<Value>) { Take(value); }",
        &["value"],
    );
    check(
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
        check(&source, &[expr]);
    }
}

#[test]
fn closures_check_captures_without_confusing_parameter_indices() {
    check(
        "func Forward(const value: [Int32], f: ([Int32]) -> Void) { f(value); }",
        &["value"],
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
                    Rc::new(SemanticPolicy {
                        const_param_element_writes: elements,
                        const_param_non_out_forwarding: forwarding,
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
            assert!(reporter.reported().iter().all(|d| matches!(d, Diagnostic::Other(error, DiagnosticLevel::Error, _) if error.to_string() == "cannot write through a const parameter")), "{:?}", reporter.reported());
        }
    }
}
