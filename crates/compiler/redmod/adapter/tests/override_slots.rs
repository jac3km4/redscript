use redscript_community_ast::SourceMap;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{Error, SemanticError, parse_file_with_metadata, process_sources};

fn check(source: &str) -> Vec<Error> {
    let sources = SourceMap::new();
    let file = sources.push_back("slots.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        [parsed.unwrap()],
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    errors
}

#[test]
fn native_returns_use_original_slot_and_preserve_static_call_types() {
    for (root, middle, child) in [
        ("Value", "Sub", "Value"),
        ("Value", "Sub", "Sibling"),
        ("weak<Value>", "weak<Sub>", "weak<Value>"),
    ] {
        let errors = check(&format!("
            class Value {{}} class Sub extends Value {{}} class Sibling extends Value {{}}
            class Base {{ public function Get() -> {root} {{ return NULL; }} }}
            class Middle extends Base {{ public override function Get() -> {middle} {{ return NULL; }} }}
            class Child extends Middle {{ public override function Get() -> {child} {{ return NULL; }} }}
            class Leaf extends Child {{ public override function Get() -> {root} {{ return NULL; }} }}
            function BaseCall(x: Base) -> {root} {{ return x.Get(); }}
            function MiddleCall(x: Middle) -> {middle} {{ return x.Get(); }}
            function ChildCall(x: Child) -> {child} {{ return x.Get(); }}
        "));
        assert!(errors.is_empty(), "{errors:?}");
    }
}

#[test]
fn native_rejects_widening_beyond_slot_and_unsupported_abi_shapes() {
    for (base, child) in [
        ("Sub", "Value"),
        ("Value", "Other"),
        ("Value", "weak<Sub>"),
        ("array<Value>", "array<Sub>"),
        ("Int32", "Int64"),
    ] {
        let errors = check(&format!("
            class Value {{}} class Sub extends Value {{}} class Other {{}}
            class Base {{ public function Get() -> {base} {{ var result: {base}; return result; }} }}
            class Child extends Base {{ public override function Get() -> {child} {{ var result: {child}; return result; }} }}
        "));
        assert!(
            errors.iter().any(|e| matches!(
                e,
                Error::Semantic {
                    kind: SemanticError::OverrideReturnTypeMismatch,
                    ..
                }
            )),
            "{base} -> {child}: {errors:?}"
        );
    }
}

#[test]
fn mixed_source_override_contracts_follow_the_child_dialect() {
    for native_child in [false, true] {
        let sources = SourceMap::new();
        let (native, community) = if native_child {
            (
                "class Child extends Middle { protected override function Get() -> Value { return NULL; } }",
                "class Value {} class Sub extends Value {} class Base<T> { public func Get() -> T { let result: T; return result; } } class Middle extends Base<ref<Value>> { public func Get() -> ref<Sub> { return null; } }",
            )
        } else {
            (
                "class Value {} class Sub extends Value {} class Base { public function Get() -> Value { return NULL; } } class Middle extends Base { public override function Get() -> Sub { return NULL; } }",
                "class Child extends Middle { protected func Get() -> ref<Value> { return null; } }",
            )
        };
        let file = sources.push_back("native.script", native);
        let community_file = sources.push_back("community.reds", community);
        let (parsed, errors) = parse_file_with_metadata(native, file);
        assert!(errors.is_empty(), "{errors:?}");
        let (dependency, errors) =
            redscript_community_parser::parse_module(community, community_file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            [parsed.unwrap()],
            [dependency.unwrap()],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        let codes = reporter
            .reported()
            .iter()
            .map(redscript_compiler_frontend::Diagnostic::code)
            .collect::<Vec<_>>();
        if native_child {
            assert!(codes.is_empty(), "{:?}", reporter.reported());
        } else {
            assert!(
                codes.contains(&"OVERRIDE_RETURN_TYPE_MISMATCH"),
                "{codes:?}"
            );
            assert!(codes.contains(&"LESS_VISIBLE_OVERRIDE"), "{codes:?}");
        }
    }
}

#[test]
fn native_narrowing_preserves_implicit_visibility_and_call_site_access() {
    let sources = SourceMap::new();
    let source = "class Base { public function Get() {} private static function Hidden() {} }
        class Middle extends Base { protected override function Get() {} }
        class Child extends Middle { override function Get() {} public function Run() { Base.Hidden(); } }
        function BaseCall(x: Base) { x.Get(); }
        function ChildCall(x: Child) { x.Get(); }";
    let file = sources.push_back("access.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols, errors) = process_sources(
        [parsed.unwrap()],
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert_eq!(
        reporter.reported().iter().filter(|d| d.is_error()).count(),
        2,
        "{:?}",
        reporter.reported()
    );
    let child = symbols
        .query_methods_by_name(interner.intern("Child"), "Get")
        .next()
        .unwrap();
    assert_eq!(
        child.func().flags().visibility(),
        redscript_compiler_frontend::Visibility::Protected
    );
}

#[test]
fn native_narrower_visibility_and_inherited_static_calls_are_accepted() {
    let errors = check(
        "
        class Base { public function Get() {} public static function Static() {} }
        class Middle extends Base { protected override function Get() {} }
        class Child extends Middle {
            private override function Get() {}
            public function Run() { Static(); Child.Static(); }
        }
    ",
    );
    assert!(errors.is_empty(), "{errors:?}");
}
