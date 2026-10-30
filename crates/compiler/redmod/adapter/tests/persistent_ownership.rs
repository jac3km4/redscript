use redscript_community_ast::SourceMap;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{Error, SemanticError, parse_file_with_metadata, process_sources};

fn check(source: &str) -> Vec<Error> {
    let sources = SourceMap::new();
    let file = sources.push_back("ownership.script", source);
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
fn imported_class_persistent_fields_are_accessible_from_derived_classes() {
    let errors = check(
        "import class NativeState {
           protected persistent var count: Int32;
         }
         class ModState extends NativeState {
           public function Read() -> Int32 { return this.count; }
         }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn mod_class_persistent_fields_remain_restricted_to_the_declaring_class() {
    let errors = check(
        "class State {
           public persistent var count: Int32;
           public function Read(other: State) -> Int32 { return other.count; }
         }
         class Child extends State {
           public function ReadInherited() -> Int32 { return this.count; }
         }
         function ReadExternal(state: State) -> Int32 { return state.count; }",
    );
    assert_eq!(errors.len(), 2, "{errors:?}");
    assert!(
        errors.iter().all(|error| matches!(
            error,
            Error::Semantic {
                kind: SemanticError::PersistentFieldAccess,
                ..
            }
        )),
        "{errors:?}"
    );
}

#[test]
fn persistent_struct_fields_are_accessible_externally() {
    let errors = check(
        "struct State {
           public persistent var count: Int32;
         }
         function ReadExternal(state: State) -> Int32 { return state.count; }
         function WriteExternal(out state: State) { state.count = 1; }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}
