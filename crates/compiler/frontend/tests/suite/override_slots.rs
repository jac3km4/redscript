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
fn const_script_ref_override_covariance_and_rejections() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "script_ref_slots.reds",
        "
        public native struct worldCookedMeshInnerNode {
            public native let mesh: Int32;
        }
        public native struct worldCookedGenericProxyMeshInnerNode extends worldCookedMeshInnerNode {
            public native let nbNodesUnderProxy: Uint32;
        }
        public native class worldCookedMeshNode {
            public native func GetInnerNode(index: Int32) -> const_script_ref<worldCookedMeshInnerNode>
        }
        public native class worldCookedGenericProxyMeshNode extends worldCookedMeshNode {
            public native func GetInnerNode(index: Int32) -> const_script_ref<worldCookedGenericProxyMeshInnerNode>
        }

        class Value {}
        class Sub extends Value {}
        @neverRef() class NeverBase {}
        @neverRef() class NeverSub extends NeverBase {}

        class BaseHolder {
            public func ReadClass() -> const_script_ref<Value> { return null; }
            public func ReadWeak() -> const_script_ref<wref<Value>> { let w: wref<Value>; return w; }
            public func ReadNever() -> const_script_ref<NeverBase> { let n: NeverBase; return n; }
        }
        class SubHolder extends BaseHolder {
            public func ReadClass() -> const_script_ref<Sub> { return null; }
            public func ReadWeak() -> const_script_ref<wref<Sub>> { let w: wref<Sub>; return w; }
            public func ReadNever() -> const_script_ref<NeverSub> { let n: NeverSub; return n; }
        }
        class MutSubHolder extends BaseHolder {
            public func ReadClass() -> script_ref<Sub> { let s: Sub = new Sub(); return s; }
        }

        func ReadBaseField(node: ref<worldCookedMeshNode>) -> Int32 {
            return node.GetInnerNode(0).mesh;
        }
        func ReadProxyFields(proxy: ref<worldCookedGenericProxyMeshNode>) -> Uint32 {
            let base: worldCookedMeshInnerNode = proxy.GetInnerNode(0);
            let _mesh: Int32 = base.mesh;
            return proxy.GetInnerNode(0).nbNodesUnderProxy;
        }
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
        .query_methods_by_name(interner.intern("worldCookedMeshNode"), "GetInnerNode")
        .next()
        .unwrap()
        .key();
    let child = symbols
        .query_methods_by_name(
            interner.intern("worldCookedGenericProxyMeshNode"),
            "GetInnerNode",
        )
        .next()
        .unwrap();
    assert_eq!(symbols.override_slot(*child.key()), root);
}

#[test]
fn const_script_ref_rejects_unsound_overrides() {
    for (base, child) in [
        ("script_ref<BaseStruct>", "script_ref<SubStruct>"),
        ("script_ref<BaseStruct>", "const_script_ref<BaseStruct>"),
        ("const_script_ref<SubStruct>", "const_script_ref<BaseStruct>"),
        ("BaseStruct", "SubStruct"),
        ("const_script_ref<[Value]>", "const_script_ref<[Sub]>"),
        ("const_script_ref<Value>", "const_script_ref<wref<Sub>>"),
        ("const_script_ref<NeverBase>", "const_script_ref<OrdSub>"),
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "invalid_script_ref.reds",
            format!(
                "
            struct BaseStruct {{ let x: Int32; }}
            struct SubStruct extends BaseStruct {{ let y: Int32; }}
            class Value {{}} class Sub extends Value {{}}
            @neverRef() class NeverBase {{}}
            class OrdSub extends NeverBase {{}}
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
