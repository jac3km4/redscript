use redscript_community_ast::{Item, ItemQualifiers, SourceMap};
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{CapabilityError, Error, parse_file_with_metadata, process_sources};

#[test]
fn export_and_constexpr_are_analysis_only_function_modifiers() {
    let source = r#"
        export function Exported() -> Int32 { return 1; }
        constexpr function Impure(out value: Int32) { value = 7; }
        class Host {
            private export constexpr function Value() -> Int32 { return 1; }
            public export static function Static() {}
            export event OnEvent() {}
        }
    "#;
    let sources = SourceMap::new();
    let file = sources.push_back("modifiers.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let parsed = parsed.unwrap();
    assert_eq!(parsed.module.items[0].0.qualifiers, ItemQualifiers::empty());
    assert_eq!(parsed.module.items[1].0.qualifiers, ItemQualifiers::empty());
    assert!(matches!(parsed.module.items[0].0.item, Item::Function(_)));
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        [parsed],
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
}

#[test]
fn virtual_methods_preserve_private_override_and_bodyless_void_semantics() {
    let source = r#"
        class Base { private virtual function Run() -> Int32 { return 1; } public virtual function Empty(); }
        class Child extends Base { private override function Run() -> Int32 { return 2; } }
        class Plain { private function Final() {} }
    "#;
    let sources = SourceMap::new();
    let file = sources.push_back("modifiers.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = process_sources(
        parsed,
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let base = symbols[interner.intern("Base")]
        .schema()
        .as_aggregate()
        .unwrap();
    assert!(
        !base
            .methods()
            .by_name("Run")
            .next()
            .unwrap()
            .func()
            .flags()
            .is_final()
    );
    let empty = base.methods().by_name("Empty").next().unwrap();
    assert!(!empty.func().flags().is_unimplemented());
    assert!(unit.classes.contains_key(&interner.intern("Base")));
    let plain = symbols[interner.intern("Plain")]
        .schema()
        .as_aggregate()
        .unwrap();
    assert!(
        plain
            .methods()
            .by_name("Final")
            .next()
            .unwrap()
            .func()
            .flags()
            .is_final()
    );
}

#[test]
fn virtual_free_static_and_bodyless_value_functions_fail_closed() {
    for source in [
        "virtual function Run() {}",
        "class Host { public static virtual function Run() {} }",
        "abstract class Host { public virtual function Run() -> Int32; }",
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("modifiers.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(parsed.is_none(), "{source}");
        assert!(
            errors
                .iter()
                .all(|error| matches!(error, Error::Capability { .. })),
            "{source}: {errors:?}"
        );
    }
}

#[test]
fn function_modifiers_do_not_enable_other_declaration_targets() {
    for (source, modifier) in [
        (
            "export class Host {}",
            redscript_redmod_ast::Modifier::Export,
        ),
        (
            "class Host { export var value: Int32; }",
            redscript_redmod_ast::Modifier::Export,
        ),
        (
            "constexpr class Host {}",
            redscript_redmod_ast::Modifier::Constexpr,
        ),
        (
            "class Host { constexpr var value: Int32; }",
            redscript_redmod_ast::Modifier::Constexpr,
        ),
        (
            "class Host { virtual var value: Int32; }",
            redscript_redmod_ast::Modifier::Virtual,
        ),
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("modifiers.script", source);
        let (_, errors) = parse_file_with_metadata(source, file);
        assert!(errors.iter().any(|error| matches!(error, Error::Capability { kind: CapabilityError::Modifier(actual), .. } if *actual == modifier)), "{source}: {errors:?}");
    }
}
