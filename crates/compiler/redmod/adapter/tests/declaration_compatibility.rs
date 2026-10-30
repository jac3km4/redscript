use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{Error, parse_file_with_metadata, process_sources};

fn check(source: &str) -> Vec<String> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("test.script", source);
    let interner = TypeInterner::default();
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(
        errors
            .iter()
            .all(|error| matches!(error, Error::Semantic { .. })),
        "{errors:?}"
    );
    if !errors.is_empty() {
        return errors.into_iter().map(|error| error.to_string()).collect();
    }
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, file)| {
            let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        [parsed.unwrap()],
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let mut messages: Vec<_> = errors.into_iter().map(|error| error.to_string()).collect();
    messages.extend(
        reporter
            .into_reported()
            .into_iter()
            .map(|error| error.to_string()),
    );
    messages
}

#[test]
fn workspot_condition_override_can_add_const() {
    let errors = check(
        "class Entity {}
         class WorkspotCondition {
           public function CheckCondition(ent: Entity) -> Bool { return true; }
         }
         class Condition extends WorkspotCondition {
           public override function CheckCondition(const ent: Entity) -> Bool { return true; }
         }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn override_const_and_optional_flags_may_change_in_either_direction() {
    for (base, child) in [
        ("const ", ""),
        ("", "optional "),
        ("optional ", ""),
        ("const optional ", ""),
    ] {
        let errors = check(&format!(
            "class Base {{ public function Test({base}value: Int32) {{}} }}
             class Child extends Base {{ public override function Test({child}value: Int32) {{}} }}"
        ));
        assert!(errors.is_empty(), "{errors:?}");
    }
}

#[test]
fn override_out_flags_must_match_in_both_directions() {
    for (base, child) in [("out ", ""), ("", "out ")] {
        let errors = check(&format!(
            "class Base {{ public function Test({base}value: Int32) {{}} }}
             class Child extends Base {{ public override function Test({child}value: Int32) {{}} }}"
        ));
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(
            errors[0].contains("override parameter out flags differ from the base signature"),
            "{errors:?}"
        );
    }
    let errors = check(
        "class Base { public function Test(out value: Int32) {} }
         class Child extends Base { public override function Test(out value: Int32) {} }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn persistent_primitive_struct_fields_are_valid() {
    let errors = check(
        "struct State {
           public persistent let count: Int32;
           public persistent let enabled: Bool;
         }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn persistent_requires_a_class_or_struct_field() {
    for source in [
        "persistent function Test() {}",
        "class State { persistent function Test() {} }",
        "struct State { persistent function Test() {} }",
        "persistent struct State { let count: Int32; }",
    ] {
        let errors = check(source);
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(
            errors[0].contains("persistent requires a class or struct field"),
            "{errors:?}"
        );
    }
}

#[test]
fn persistent_struct_fields_still_require_persistable_types() {
    let errors = check("struct State { persistent let text: String; }");
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(
        errors[0].contains("fields of type `String` cannot be marked as persistent"),
        "{errors:?}"
    );
}

#[test]
fn protected_and_implicit_events_are_valid() {
    let errors = check(
        "class Event {} class Listener {
           protected event OnAttach(evt: Event) {}
           event OnDetach(evt: Event) {}
         }
         class Child extends Listener {
           protected event OnAttach(evt: Event) {}
           event OnDetach(evt: Event) {}
         }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn public_and_private_events_are_invalid() {
    for visibility in ["public", "private"] {
        let errors = check(&format!(
            "class Listener {{ {visibility} event OnAttach() {{}} }}"
        ));
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(
            errors[0].contains("events must omit visibility or be protected"),
            "{errors:?}"
        );
    }
}
