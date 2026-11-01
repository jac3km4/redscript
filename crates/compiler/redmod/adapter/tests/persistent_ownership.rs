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
fn mod_class_persistent_fields_admit_descendants_but_not_external_access() {
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
    assert_eq!(errors.len(), 1, "{errors:?}");
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
fn deep_descendants_can_read_write_and_use_base_typed_receivers() {
    let errors = check(
        "class State {
           protected persistent var count: Int32;
           protected persistent var values: array<Int32>;
         }
         class Middle extends State {}
         class Child extends Middle {
           public function Read(other: State) -> Int32 { return other.count; }
           public function Write(other: State) { other.count = 1; other.values[0] = other.count; }
           public static function ReadStatic(other: State) -> Int32 { return other.count; }
         }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn cross_file_descendants_obey_ownership() {
    let sources = SourceMap::new();
    let base_source =
        "class State { public persistent var count: Int32; private persistent var secret: Int32; }";
    let child_source = "class Child extends State { public function Read(other: State) -> Int32 { return other.count; } }

        function External(other: State) -> Int32 { return other.count; }";
    let base_file = sources.push_back("base.script", base_source);
    let child_file = sources.push_back("child.script", child_source);
    let mut modules = Vec::new();
    for (source, file) in [(child_source, child_file), (base_source, base_file)] {
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        modules.push(parsed.unwrap());
    }
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        modules,
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(matches!(
        errors[0],
        Error::Semantic {
            kind: SemanticError::PersistentFieldAccess,
            ..
        }
    ));
}

#[test]
fn source_defined_patch_targets_remain_rejected() {
    let sources = SourceMap::new();
    let source = "class State { public persistent var count: Int32; }
        class Child extends State {}
        @addMethod(Child) function PatchRead(other: State) -> Int32 { return other.count; }";
    let file = sources.push_back("patch.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    process_sources(
        [parsed.unwrap()],
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(
        reporter.reported().iter().any(|d| matches!(
            d,
            redscript_compiler_frontend::Diagnostic::UserSymbolAnnotation(_)
        )),
        "{:?}",
        reporter.reported()
    );
}

#[test]
fn descendants_still_cannot_read_private_persistent_fields() {
    let sources = SourceMap::new();
    let source = "class State { private persistent var secret: Int32; } class Child extends State { public function Read() -> Int32 { return this.secret; } }";
    let file = sources.push_back("private.script", source);
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
    assert!(errors.is_empty(), "{errors:?}");
    assert!(
        reporter
            .reported()
            .iter()
            .any(redscript_compiler_frontend::Diagnostic::is_error),
        "private visibility must remain enforced"
    );
}

#[test]
fn persistent_ceiling_is_native_source_and_owner_local() {
    let sources = SourceMap::new();
    let native = "class NativeState { public persistent var count: Int32; }
        function ReadCommunity(other: CommunityState) -> Int32 { return other.count; }
        function ReadNative(other: NativeState) -> Int32 { return other.count; }";
    let community = "class CommunityState { public persistent let count: Int32; }
        func DependencyRead(other: ref<NativeState>) -> Int32 { return other.count; }";
    let native_file = sources.push_back("native.script", native);
    let community_file = sources.push_back("dependency.reds", community);
    let (parsed, errors) = parse_file_with_metadata(native, native_file);
    assert!(errors.is_empty(), "{errors:?}");
    let (dependency, errors) = redscript_community_parser::parse_module(community, community_file);
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
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(matches!(
        errors[0],
        Error::Semantic {
            kind: SemanticError::PersistentFieldAccess,
            ..
        }
    ));
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
