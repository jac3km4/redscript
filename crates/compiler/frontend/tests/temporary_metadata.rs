use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::ir::{Local, TemporaryPurpose as Purpose};
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, TypeInterner, infer_from_sources,
};

fn assert_locals(body: &str, expected: &[(Option<&str>, Option<Purpose>)]) {
    let sources = SourceMap::new();
    sources.push_back(
        "temporaries.reds",
        format!(
            "native func Values() -> [Int32];
             native func Value() -> Int32;
             native func OperatorAssignAdd(out lhs: Int32, rhs: Int32) -> Int32;
             native func OperatorLess(lhs: Int32, rhs: Int32) -> Bool;
             native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool;
             func Test(input: [Int32]) {{ {body} }}"
        ),
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty(), "{reporter:?}");
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Test")
        .unwrap();
    let locals = &unit.functions[entry.key()].locals;
    assert!(
        locals
            .iter()
            .filter(|info| matches!(info.id, Local::Param(_)))
            .all(|info| info.temporary_purpose.is_none())
    );
    let variables: Vec<_> = locals
        .iter()
        .filter(|info| matches!(info.id, Local::Var(_)))
        .map(|info| {
            assert!(info.span.is_some());
            (info.name, info.temporary_purpose)
        })
        .collect();
    assert_eq!(variables, expected, "{body}");
}

#[test]
fn array_and_iteration_temporaries_keep_purpose_separate_from_names() {
    assert_locals(
        "let authored = [1]; for item in Values() {}",
        &[
            (None, Some(Purpose::ArrayLiteral)),
            (Some("authored"), None),
            (None, Some(Purpose::IterationIndex)),
            (None, Some(Purpose::IterationIterable)),
            (Some("item"), None),
        ],
    );
}

#[test]
fn existing_iterable_locals_are_not_retagged() {
    for iterable in ["input", "[1]"] {
        let mut expected = vec![];
        if iterable == "[1]" {
            expected.push((None, Some(Purpose::ArrayLiteral)));
        }
        expected.extend([(None, Some(Purpose::IterationIndex)), (Some("item"), None)]);
        assert_locals(&format!("for item in {iterable} {{}}"), &expected);
    }
}

#[test]
fn conditional_pattern_scrutinees_are_tagged_but_bindings_are_authored() {
    for keyword in ["if", "while"] {
        assert_locals(
            &format!("{keyword} let bound = Value() {{}}"),
            &[
                (None, Some(Purpose::PatternScrutinee)),
                (Some("bound"), None),
            ],
        );
    }
}

#[test]
fn pattern_switches_tag_only_extracted_scrutinees() {
    assert_locals(
        "switch Value() { case let bound: break; }",
        &[
            (None, Some(Purpose::SwitchScrutinee)),
            (Some("bound"), None),
        ],
    );
    assert_locals(
        "let authored = Value(); switch authored { case let bound: break; }",
        &[(Some("authored"), None), (Some("bound"), None)],
    );
    assert_locals("switch Value() { case 0: break; }", &[]);
}
