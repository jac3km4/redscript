use std::rc::Rc;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, SemanticPolicy, Symbols, TypeInterner, parse_files,
    process_sources_with_policy,
};

#[test]
fn community_covariance_preserves_static_returns_and_canonical_slots() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "slots.reds",
        "
        class Value {} class Sub extends Value {} class Deep extends Sub {}
        class Base { public func Get() -> ref<Value> { return null; } }
        class Middle extends Base { public func Get() -> ref<Sub> { return null; } }
        class Child extends Middle { public func Get() -> ref<Deep> { return null; } }
        func BaseCall(x: ref<Base>) -> ref<Value> { return x.Get(); }
        func MiddleCall(x: ref<Middle>) -> ref<Sub> { return x.Get(); }
        func ChildCall(x: ref<Child>) -> ref<Deep> { return x.Get(); }
    ",
    );
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    let evaluator = Evaluator::from_modules(&modules);
    let (_, symbols) = process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[],
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let root = *symbols
        .query_methods_by_name(interner.intern("Base"), "Get")
        .next()
        .unwrap()
        .key();
    for (owner, result) in [("Base", "Value"), ("Middle", "Sub"), ("Child", "Deep")] {
        let methods = symbols
            .query_methods_by_name(interner.intern(owner), "Get")
            .collect::<Vec<_>>();
        assert_eq!(methods.len(), 1);
        assert_eq!(symbols.override_slot(*methods[0].key()), root);
        assert_eq!(
            methods[0]
                .func()
                .type_()
                .return_type()
                .unwrap_ref_or_self()
                .upper_bound()
                .unwrap()
                .id(),
            interner.intern(result)
        );
    }
}

#[test]
fn community_rejects_widening_and_unsupported_return_representations() {
    for (base, child) in [
        ("ref<Sub>", "ref<Value>"),
        ("ref<Value>", "wref<Sub>"),
        ("array<ref<Value>>", "array<ref<Sub>>"),
        ("Int32", "Int64"),
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "invalid.reds",
            format!(
                "
            class Value {{}} class Sub extends Value {{}}
            abstract class Base {{ public func Get() -> {base} }}
            abstract class Child extends Base {{ public func Get() -> {child} }}
        "
            ),
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(&sources, &mut reporter);
        let evaluator = Evaluator::from_modules(&modules);
        process_sources_with_policy(
            modules,
            Symbols::with_default_types(),
            evaluator,
            &mut reporter,
            &interner,
            &[],
        );
        assert!(
            reporter
                .reported()
                .iter()
                .any(|d| d.code() == "OVERRIDE_RETURN_TYPE_MISMATCH"),
            "{base} -> {child}: {:?}",
            reporter.reported()
        );
    }
}

#[test]
fn override_visibility_and_static_advice_follow_child_and_call_site_policy() {
    for native_child in [false, true] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let base = sources.push_back(
            "base.reds",
            "class Base { public func Get() {} public static func Static() {} }",
        );
        let child = sources.push_back(
            "child.reds",
            "class Child extends Base { protected func Get() {} }",
        );
        let calls = sources.push_back("calls.reds", "func Run() { Child.Static(); }");
        sources.push_back(
            "community-calls.reds",
            "func CommunityRun() { Child.Static(); }",
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(&sources, &mut reporter);
        let evaluator = Evaluator::from_modules(&modules);
        let policy = Rc::new(SemanticPolicy {
            allow_narrower_overrides: true,
            allow_inherited_static_calls: true,
            ..SemanticPolicy::default()
        });
        let policies = [
            (if native_child { child } else { base }, policy.clone()),
            (calls, policy),
        ];
        let (_, symbols) = process_sources_with_policy(
            modules,
            Symbols::with_default_types(),
            evaluator,
            &mut reporter,
            &interner,
            &policies,
        );
        assert_eq!(
            reporter
                .reported()
                .iter()
                .filter(|d| d.code() == "LESS_VISIBLE_OVERRIDE")
                .count(),
            usize::from(!native_child),
            "{:?}",
            reporter.reported()
        );
        assert_eq!(
            reporter
                .reported()
                .iter()
                .filter(|d| d.code() == "CALLING_BASE_STATIC_METHOD")
                .count(),
            1,
            "{:?}",
            reporter.reported()
        );
        let method = symbols
            .query_methods_by_name(interner.intern("Child"), "Get")
            .next()
            .unwrap();
        assert_eq!(
            method.func().flags().visibility(),
            redscript_compiler_frontend::Visibility::Protected
        );
    }
}

#[test]
fn const_script_ref_struct_covariant_returns_are_allowed_and_enforced_readonly() {
    let run = |source: &str| {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back("covariant_script_ref.reds", source);
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(&sources, &mut reporter);
        let evaluator = Evaluator::from_modules(&modules);
        process_sources_with_policy(
            modules,
            Symbols::with_default_types(),
            evaluator,
            &mut reporter,
            &interner,
            &[],
        );
        reporter
            .reported()
            .iter()
            .map(|d| d.code().to_owned())
            .collect::<Vec<_>>()
    };

    assert_eq!(
        run(
            "
            native struct BaseInner { public native let a: Int32; }
            native struct SubInner extends BaseInner { public native let b: Int32; }
            native class BaseNode {
                public native func GetInnerNode(index: Int32) -> const script_ref<BaseInner>
            }
            native class SubNode extends BaseNode {
                public native func GetInnerNode(index: Int32) -> const script_ref<SubInner>
            }
            native func OperatorAdd(a: Int32, b: Int32) -> Int32
            func Read(const r: script_ref<BaseInner>) -> Int32 { return Deref(r).a; }
            func Test(base: ref<BaseNode>, sub: ref<SubNode>) -> Int32 {
                const pinned: script_ref<BaseInner> = base.GetInnerNode(0);
                return Read(base.GetInnerNode(0)) + Deref(pinned).a + sub.GetInnerNode(0).b;
            }
            "
        ),
        Vec::<String>::new()
    );

    for (base_ret, child_ret) in [
        ("script_ref<BaseInner>", "script_ref<SubInner>"),
        ("const script_ref<BaseInner>", "script_ref<BaseInner>"),
        ("script_ref<BaseInner>", "const script_ref<BaseInner>"),
        ("const script_ref<BaseInner>", "script_ref<SubInner>"),
        ("script_ref<BaseInner>", "const script_ref<SubInner>"),
        ("const script_ref<ref<BaseClass>>", "const script_ref<ref<SubClass>>"),
    ] {
        let codes = run(&format!(
            "
            class BaseClass {{}}
            class SubClass extends BaseClass {{}}
            native struct BaseInner {{ public native let a: Int32; }}
            native struct SubInner extends BaseInner {{ public native let b: Int32; }}
            native class BaseNode {{
                public native func GetInnerNode(index: Int32) -> {base_ret}
            }}
            native class SubNode extends BaseNode {{
                public native func GetInnerNode(index: Int32) -> {child_ret}
            }}
            "
        ));
        assert!(
            codes.iter().any(|c| c == "OVERRIDE_RETURN_TYPE_MISMATCH"),
            "{base_ret} -> {child_ret}: {codes:?}"
        );
    }

    let write_codes = run(
        "
        struct BaseInner { public let a: Int32; }
        native class BaseNode {
            public native func GetInnerNode(index: Int32) -> const script_ref<BaseInner>
        }
        func Out(out x: BaseInner) {}
        func Mutate(r: script_ref<BaseInner>) {}
        func Test(base: ref<BaseNode>) {
            Deref(base.GetInnerNode(0)) = BaseInner(1);
            Deref(base.GetInnerNode(0)).a = 2;
            Out(Deref(base.GetInnerNode(0)));
            let leaked: script_ref<BaseInner> = base.GetInnerNode(0);
            Mutate(base.GetInnerNode(0));
        }
        ",
    );
    assert_eq!(
        write_codes,
        [
            "CONST_RETURN_WRITE",
            "CONST_RETURN_WRITE",
            "CONST_RETURN_WRITE",
            "CONST_FORWARDING",
            "CONST_FORWARDING",
        ]
    );
}
