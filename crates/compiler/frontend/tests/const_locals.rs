use std::rc::Rc;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, DiagnosticLevel, Evaluator, SemanticPolicy, Symbols, TypeInterner,
    parse_files, process_sources_with_policy,
};

fn check(source: &str, expected: &[&str]) {
    check_policy(source, expected, false);
}

fn check_policy(source: &str, expected: &[&str], native: bool) {
    check_with_policy(
        source,
        expected,
        SemanticPolicy {
            hoist_locals: native,
            const_param_element_writes: native,
            const_param_non_out_forwarding: native,
            const_local_handle_rebinding: native,
            ..SemanticPolicy::default()
        },
    );
}

fn check_with_policy(source: &str, expected: &[&str], policy: SemanticPolicy) {
    check_diagnostics(source, expected, policy, &[]);
}

fn check_diagnostics(
    source: &str,
    expected: &[&str],
    policy: SemanticPolicy,
    levels: &[DiagnosticLevel],
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("const.reds", source);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, _) = process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[(file, Rc::new(policy))],
    );
    assert!(
        unit.functions
            .values()
            .flat_map(|f| f.locals.iter())
            .any(|l| l.is_const)
    );
    if !levels.is_empty() {
        assert_eq!(levels.len(), expected.len());
    }
    let spans = reporter
        .reported()
        .iter()
        .enumerate()
        .map(|(i, d)| {
            let level = levels.get(i).copied().unwrap_or(DiagnosticLevel::Error);
            assert_eq!(d.level(), level, "{d:?}");
            let message = d.to_string();
            assert!(
                if level == DiagnosticLevel::ErrorAllowedAtRuntime {
                    message.starts_with("forwarding a const ")
                        && (message.ends_with(" to a non-const parameter may allow mutation")
                            || message.ends_with(" to a function value may allow mutation"))
                } else {
                    message == "cannot write through a const local"
                        || message == "cannot write through a const parameter"
                },
                "{d:?}"
            );
            let span = d.span();
            assert_eq!(span.file, file);
            &source[span.start as usize..span.end as usize]
        })
        .collect::<Vec<_>>();
    assert_eq!(spans, expected, "{source}");
}

#[test]
fn strict_bindings_values_and_referents() {
    check("func F() { const x: Int32; x = 1; x = 2; }", &["x", "x"]);
    check("func F() { const x = 1; }", &[]);
    check("struct V {} func F(v: V) { const x: V; x = v; }", &["x"]);
    check(
        "class V { public let field: Int32; } func F(v: V) { const x: V; x = v; x.field = 1; }",
        &["x", "x.field"],
    );
    check(
        "func F() { const x: [Int32]; x[0] = 1; ArrayResize(x, 2); }",
        &["x[0]", "x"],
    );
}

#[test]
fn strict_declaration_initializers_are_the_only_binding_write_exception() {
    check(
        "func OperatorAssignAdd(out lhs: Int32, rhs: Int32) {} func F() { const x = 1; x = 2; x += 1; const y: Int32 = 3; y = 4; }",
        &["x", "x", "y"],
    );
    check(
        "struct V {} func F(v: V) { const x: V = v; x = v; }",
        &["x"],
    );
    check(
        "class V { public let field: Int32; public func Change() {} } func F(v: V) { const x: V = v; x.field = 1; x.Change(); }",
        &["x.field", "x"],
    );
    check("func F() { const x = 1; const y = (x = 2); }", &["x"]);
    check(
        "func F() { const x = 1; let f: () -> Void = () -> { const y = 2; y = 3; x = 4; }; x = 5; }",
        &["y", "x", "x"],
    );
    check(
        "func F(b: Bool) { while b { const x = 1; x = 2; } }",
        &["x"],
    );
    check_with_policy(
        "func F() { x = 0; const x: Int32 = (x = 1); x = 2; const y: Int32; y = 3; }",
        &["x", "x", "x", "y"],
        SemanticPolicy {
            hoist_locals: true,
            ..SemanticPolicy::default()
        },
    );
}

#[test]
fn strict_forwarding_and_capture_provenance() {
    check_diagnostics(
        "class V {} func Take(v: V) {} func Out(out v: V) {} func F() { const x: V; Take(x); Out(x); }",
        &["x", "x"],
        SemanticPolicy::default(),
        &[
            DiagnosticLevel::ErrorAllowedAtRuntime,
            DiagnosticLevel::Error,
        ],
    );
    check(
        "class V { public let field: Int32; } func F() { const x: V; let f: () -> Void = () -> { x.field = 1; }; }",
        &["x.field"],
    );
    check(
        "class V { public let field: Int32; } func F() { const x: wref<V>; WeakRefToRef(x).field = 1; }",
        &["WeakRefToRef(x).field"],
    );
    check(
        "class V { public let field: Int32; } func F() { const x: V; let f: () -> Void = () -> { let g: () -> Void = () -> { x.field = 1; }; }; }",
        &["x.field"],
    );
    check(
        "class V { public let field: Int32; } func F() { const x: V; let f: () -> Void = () -> { const y: V; y.field = 1; }; }",
        &["y.field"],
    );
}

#[test]
fn possible_mutation_forwarding_is_compatible_but_writes_remain_hard() {
    use DiagnosticLevel::{Error, ErrorAllowedAtRuntime};

    check_diagnostics(
        "class V { public let field: Int32; public func Change() {} } func Take(v: V) {} func Out(out v: V) {} func F(v: V) { const x: V = v; Take(x); Out(x); x = v; x.field = 1; x.Change(); }",
        &["x", "x", "x", "x.field", "x"],
        SemanticPolicy::default(),
        &[ErrorAllowedAtRuntime, Error, Error, Error, Error],
    );
    check_diagnostics(
        "func Take(v: [Int32]) {} func Out(out v: [Int32]) {} func F() { const x: [Int32]; Take(x); Out(x); x[0] = 1; ArrayClear(x); }",
        &["x", "x", "x[0]", "x"],
        SemanticPolicy::default(),
        &[ErrorAllowedAtRuntime, Error, Error, Error],
    );
    for expr in ["b ? p : x", "b ? x : p"] {
        check_diagnostics(
            &format!(
                "func Take(v: [Int32]) {{}} func F(const p: [Int32], b: Bool) {{ const x: [Int32]; Take({expr}); }}"
            ),
            &[expr],
            SemanticPolicy::default(),
            &[ErrorAllowedAtRuntime],
        );
    }
    for expr in ["x", "x.items", "x.nested[0]"] {
        check_diagnostics(
            &format!(
                "class V {{ public let items: [Int32]; public let nested: [[Int32]]; }} func Take(v: [Int32]) {{}} func F() {{ const x: {}; Take({expr}); }}",
                if expr == "x" { "[Int32]" } else { "V" }
            ),
            &[expr],
            SemanticPolicy::default(),
            &[ErrorAllowedAtRuntime],
        );
    }
    check_diagnostics(
        "func Take(v: script_ref<Int32>) {} func F() { const x = 1; Take(AsRef(x)); }",
        &["AsRef(x)"],
        SemanticPolicy::default(),
        &[ErrorAllowedAtRuntime],
    );
}

#[test]
fn closure_forwarding_without_const_flags_is_compatible() {
    use DiagnosticLevel::{Error, ErrorAllowedAtRuntime};

    check_diagnostics(
        "func F(const p: [Int32], f: ([Int32]) -> Void, scalar: (Int32) -> Void) { const x: [Int32]; const n = 1; f(p); f(x); scalar(n); let g: () -> Void = () -> { f(x); x[0] = 1; }; }",
        &["p", "x", "x", "x[0]"],
        SemanticPolicy::default(),
        &[
            ErrorAllowedAtRuntime,
            ErrorAllowedAtRuntime,
            ErrorAllowedAtRuntime,
            Error,
        ],
    );
    check_diagnostics(
        "class V {} func F(const p: V, f: (V) -> Void) { const x: V; f(p); f(x); }",
        &[],
        SemanticPolicy {
            const_param_non_out_forwarding: true,
            const_local_handle_rebinding: true,
            ..SemanticPolicy::default()
        },
        &[],
    );
    // Native parameter forwarding is still permitted; local arrays retain their check.
    check_diagnostics(
        "func F(const p: [Int32], f: ([Int32]) -> Void) { const x: [Int32]; f(p); f(x); }",
        &["x"],
        SemanticPolicy {
            const_param_non_out_forwarding: true,
            const_local_handle_rebinding: true,
            ..SemanticPolicy::default()
        },
        &[ErrorAllowedAtRuntime],
    );
}

#[test]
fn native_policy_separates_binding_and_referent_constness() {
    let declarations = "class V { public let field: Int32; public const func Read() {} public func Change() {} } func Take(v: V) {} func Out(out v: V) {}";
    for body in [
        "const x: V; x = v; x = v; x.Read(); Take(x); Out(x);",
        "const x: V = v; x.Read();",
        "x = v; const x: V; x = v;",
    ] {
        check_policy(
            &format!("{declarations} func F(v: V) {{ {body} }}"),
            &[],
            true,
        );
    }
    check_policy(
        "class V { public const func Read() {} } func Take(v: ref<V>) {} func Out(out v: wref<V>) {} func F(v: ref<V>) { const x: wref<V>; x = v; x = v; x.Read(); Take(x); Out(x); }",
        &[],
        true,
    );
    check_policy(
        &format!(
            "{declarations} func F() {{ const x: wref<V>; x.field = 1; x.Change(); WeakRefToRef(x).Change(); }}"
        ),
        &["x.field", "x", "WeakRefToRef(x)"],
        true,
    );
    check_policy(
        &format!(
            "{declarations} func F() {{ const x: V; let f: () -> Void = () -> {{ x.field = 1; x.Change(); Out(x); }}; }}"
        ),
        &["x.field", "x"],
        true,
    );
    check_policy("func F() { const x: Int32 = 1; x = 2; }", &["x", "x"], true);
    check_policy(
        "struct V {} func F(v: V) { const x: V; x = v; }",
        &["x"],
        true,
    );
    check_policy(
        "func F() { const x: [Int32]; x[0] = 1; ArrayResize(x, 2); }",
        &["x[0]", "x"],
        true,
    );
}

#[test]
fn mixed_conditional_roots_keep_both_parameter_and_local_restrictions() {
    check_policy(
        "class V { public func Change() {} } func F(const p: V, b: Bool) { const x: V; (b ? p : x).Change(); (b ? x : p).Change(); }",
        &["b ? p : x", "b ? x : p"],
        true,
    );
}

#[test]
fn local_rebinding_policy_is_file_local_in_both_orders() {
    for reverse in [false, true] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let native = sources.push_back(
            "native.reds",
            "class V {} func Native(v: V) { const x: V = v; x = v; x = v; const n: Int32 = 1; }",
        );
        let strict = sources.push_back(
            "strict.reds",
            "func Strict(v: V) { const x: V = v; x = v; x = v; const n: Int32 = 1; }",
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let mut modules = parse_files(&sources, &mut reporter);
        assert!(reporter.reported().is_empty());
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
                native,
                Rc::new(SemanticPolicy {
                    const_local_handle_rebinding: true,
                    ..SemanticPolicy::default()
                }),
            )],
        );
        assert_eq!(reporter.reported().len(), 3, "{:?}", reporter.reported());
        assert_eq!(
            reporter
                .reported()
                .iter()
                .filter(|d| d.span().file == strict)
                .count(),
            2,
        );
        let native_errors = reporter
            .reported()
            .iter()
            .filter(|d| d.span().file == native)
            .collect::<Vec<_>>();
        assert_eq!(native_errors.len(), 1);
        assert!(
            native_errors[0]
                .to_string()
                .contains("cannot write through a const local")
        );
    }
}
