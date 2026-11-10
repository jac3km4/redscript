use std::rc::Rc;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::ir::{
    Local, LocalInfo, TemporaryOwner as Owner, TemporaryPurpose as Purpose,
};
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, SemanticPolicy, Symbols, TypeInterner, parse_files,
    process_sources_with_policy,
};

fn assert_locals(body: &str, expected: &[(Option<&str>, Option<Purpose>)]) {
    with_locals(body, |locals| {
        assert!(
            locals
                .iter()
                .filter(|info| matches!(info.id, Local::Param(_)))
                .all(|info| info.temporary_purpose.is_none() && info.temporary_owner.is_none())
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
    });
}

fn with_locals(body: &str, check: impl FnOnce(&[LocalInfo<'_>])) {
    with_local_policy(body, false, check);
}

fn with_local_policy(body: &str, hoist_locals: bool, check: impl FnOnce(&[LocalInfo<'_>])) {
    let sources = SourceMap::new();
    let file = sources.push_back(
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
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{reporter:?}");
    let evaluator = Evaluator::from_modules(&modules);
    let policy = SemanticPolicy {
        hoist_locals,
        ..SemanticPolicy::default()
    };
    let (unit, symbols) = process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[(file, Rc::new(policy))],
    );
    assert!(reporter.reported().is_empty(), "{reporter:?}");
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Test")
        .unwrap();
    let function = &unit.functions[entry.key()];
    check(&function.locals);
}

#[test]
fn array_and_iteration_temporaries_keep_purpose_separate_from_names() {
    assert_locals(
        "let authored = [1]; for item in Values() {}",
        &[
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
fn array_destinations_use_local_identity_under_shadowing() {
    with_locals(
        "let values = [1]; if true { let values = [2]; values = [3]; } values = [4];",
        |locals| {
            let destinations: Vec<_> = locals
                .iter()
                .filter(|info| info.name == Some("values"))
                .map(|info| info.id)
                .collect();
            assert_eq!(destinations.len(), 2);
            assert_ne!(destinations[0], destinations[1]);
            let owners: Vec<_> = locals
                .iter()
                .filter(|info| info.temporary_purpose == Some(Purpose::ArrayLiteral))
                .map(|info| {
                    assert_eq!(info.name, None);
                    info.temporary_owner
                })
                .collect();
            assert_eq!(
                owners,
                [1, 0].map(|i| Some(Owner::Destination(destinations[i])))
            );
        },
    );
}

#[test]
fn hoisted_array_declarations_and_writes_share_the_destination_identity() {
    with_local_policy("values = [1]; let values: [Int32] = [2];", true, |locals| {
        let destination = locals
            .iter()
            .find(|info| info.name == Some("values"))
            .unwrap();
        assert_eq!(destination.temporary_owner, None);
        let arrays: Vec<_> = locals
            .iter()
            .filter(|info| info.temporary_purpose == Some(Purpose::ArrayLiteral))
            .collect();
        assert_eq!(arrays.len(), 2);
        for array in arrays {
            assert_eq!(
                array.temporary_owner,
                Some(Owner::Destination(destination.id))
            );
        }
    });
}

#[test]
fn nested_iteration_owners_use_distinct_shadowed_bindings() {
    with_locals(
        "for item in Values() { for item in Values() {} } for item in Values() {}",
        |locals| {
            let bindings: Vec<_> = locals
                .iter()
                .filter(|info| info.name == Some("item"))
                .map(|info| info.id)
                .collect();
            assert_eq!(bindings.len(), 3);
            let temps: Vec<_> = locals
                .iter()
                .filter(|info| info.temporary_purpose.is_some())
                .collect();
            assert_eq!(temps.len(), 6);
            for (pair, binding) in temps.as_chunks::<2>().0.iter().zip(bindings) {
                assert_eq!(pair[0].temporary_purpose, Some(Purpose::IterationIndex));
                assert_eq!(pair[1].temporary_purpose, Some(Purpose::IterationIterable));
                for temp in pair {
                    assert_eq!(temp.name, None);
                    assert_eq!(temp.temporary_owner, Some(Owner::IterationBinding(binding)));
                }
            }
        },
    );
}

#[test]
fn reused_iterables_keep_their_original_metadata() {
    with_locals(
        "let values = [1]; for item in values {} for item in input {} for item in [2] {}",
        |locals| {
            for local in locals
                .iter()
                .filter(|info| info.temporary_purpose.is_none())
            {
                assert_eq!(local.temporary_owner, None);
            }
            let arrays: Vec<_> = locals
                .iter()
                .filter(|info| info.temporary_purpose == Some(Purpose::ArrayLiteral))
                .collect();
            assert_eq!(arrays.len(), 1);
            assert_eq!(arrays[0].temporary_owner, None);
            assert!(
                locals
                    .iter()
                    .all(|info| { info.temporary_purpose != Some(Purpose::IterationIterable) })
            );
            for (index, binding) in locals
                .iter()
                .filter(|info| info.temporary_purpose == Some(Purpose::IterationIndex))
                .zip(locals.iter().filter(|info| info.name == Some("item")))
            {
                assert_eq!(
                    index.temporary_owner,
                    Some(Owner::IterationBinding(binding.id))
                );
            }
        },
    );
}

#[test]
fn arrays_without_local_destinations_have_no_owner() {
    with_locals("[1]; let nested = [[2]];", |locals| {
        let owners: Vec<_> = locals
            .iter()
            .filter(|info| info.temporary_purpose == Some(Purpose::ArrayLiteral))
            .map(|info| info.temporary_owner)
            .collect();
        assert_eq!(owners, [None, None]);
    });
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
