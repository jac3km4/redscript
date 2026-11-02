use std::rc::Rc;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::ir::{
    Block, Expr, LocalInfo, Stmt, TemporaryOwner, TemporaryPurpose,
};
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, SemanticPolicy, Symbols, TypeInterner, parse_files,
    process_sources_with_policy,
};

fn check(body: &str, policy: SemanticPolicy, inspect: impl FnOnce(&[LocalInfo<'_>], &Block<'_>)) {
    let sources = SourceMap::new();
    let file = sources.push_back(
        "arrays.reds",
        format!(
            "native func First() -> Int32;
         native func Second() -> Int32;
         func Test(again: Bool) {{ {body} }}"
        ),
    );
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    let evaluator = Evaluator::from_modules(&modules);
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
    inspect(&function.locals, &function.block);
}

#[test]
fn fresh_literals_initialize_only_the_authored_local() {
    check(
        "let a = [First(), Second()]; const b: [Int32] = [3];",
        SemanticPolicy::default(),
        |locals, block| {
            assert_eq!(locals.len(), 3); // parameter and two authored locals, no discarded temps
            assert!(locals.iter().all(|local| local.temporary_purpose.is_none()));
            assert_eq!(block.stmts.len(), 2);
            for (stmt, name) in block.stmts.iter().zip(["a", "b"]) {
                let Stmt::InitArray {
                    local, elements, ..
                } = stmt
                else {
                    panic!("{stmt:?}")
                };
                let info = locals.iter().find(|info| info.id == *local).unwrap();
                assert_eq!(info.name, Some(name));
                assert!(info.initializer.is_some());
                assert_eq!(info.is_const, name == "b");
                assert_eq!(elements.len(), if name == "a" { 2 } else { 1 });
                if name == "a" {
                    assert!(elements[0].span().start < elements[1].span().start);
                }
            }
        },
    );
}

#[test]
fn shadowed_initializer_reads_the_outer_binding() {
    check(
        "let values = [1]; if true { let values = [values[0]]; }",
        SemanticPolicy::default(),
        |locals, block| {
            let bindings: Vec<_> = locals
                .iter()
                .filter(|info| info.name == Some("values"))
                .collect();
            assert_eq!(bindings.len(), 2);
            assert!(locals.iter().all(|info| info.temporary_purpose.is_none()));
            let Stmt::Branches { branches, .. } = &block.stmts[1] else {
                panic!("{block:?}")
            };
            let Stmt::InitArray {
                local, elements, ..
            } = &branches[0].block.stmts[0]
            else {
                panic!("{block:?}")
            };
            assert_eq!(*local, bindings[1].id);
            let Expr::Index { array, .. } = &elements[0] else {
                panic!("{elements:?}")
            };
            assert!(matches!(&**array, Expr::Local(id, _) if *id == bindings[0].id));
        },
    );
}

#[test]
fn loop_declarations_initialize_at_each_execution_including_empty_clears() {
    check(
        "while again { let empty: [Int32] = []; ArrayPush(empty, 1); let filled = [First(), Second()]; }",
        SemanticPolicy::default(),
        |locals, block| {
            assert!(locals.iter().all(|info| info.temporary_purpose.is_none()));
            assert_eq!(block.stmts.len(), 1);
            let Stmt::While(loop_, _) = &block.stmts[0] else {
                panic!("{block:?}")
            };
            assert_eq!(loop_.block.stmts.len(), 3);
            assert!(matches!(loop_.block.stmts[0], Stmt::InitDefault { .. }));
            assert!(
                matches!(loop_.block.stmts[2], Stmt::InitArray { ref elements, .. } if elements.len() == 2)
            );
        },
    );
}

#[test]
fn existing_assignments_keep_temporary_copy_and_owner() {
    check(
        "let values = [1]; values = [values[0], 2];",
        SemanticPolicy::default(),
        |locals, block| {
            let destination = locals
                .iter()
                .find(|info| info.name == Some("values"))
                .unwrap();
            let temps: Vec<_> = locals
                .iter()
                .filter(|info| info.temporary_purpose == Some(TemporaryPurpose::ArrayLiteral))
                .collect();
            assert_eq!(temps.len(), 1);
            assert_eq!(
                temps[0].temporary_owner,
                Some(TemporaryOwner::Destination(destination.id))
            );
            assert_eq!(block.stmts.len(), 3);
            assert!(
                matches!(&block.stmts[2], Stmt::Expr(expr) if matches!(&**expr, Expr::Assign { .. }))
            );
        },
    );
}

#[test]
fn hoisted_and_native_policy_literals_keep_temporaries() {
    for policy in [
        SemanticPolicy {
            hoist_locals: true,
            ..SemanticPolicy::default()
        },
        SemanticPolicy {
            native_object_cast: true,
            ..SemanticPolicy::default()
        },
        SemanticPolicy {
            script_ref_write_through: true,
            ..SemanticPolicy::default()
        },
    ] {
        check("let values: [Int32] = [1];", policy, |locals, _| {
            let destination = locals
                .iter()
                .find(|info| info.name == Some("values"))
                .unwrap();
            let temp = locals
                .iter()
                .find(|info| info.temporary_purpose == Some(TemporaryPurpose::ArrayLiteral))
                .unwrap();
            assert_eq!(
                temp.temporary_owner,
                Some(TemporaryOwner::Destination(destination.id))
            );
        });
    }
}

#[test]
fn nested_literals_keep_only_the_inner_temporaries() {
    check(
        "let values = [[1], [2]];",
        SemanticPolicy::default(),
        |locals, block| {
            assert_eq!(locals.len(), 4);
            assert_eq!(
                locals
                    .iter()
                    .filter(|info| info.temporary_purpose == Some(TemporaryPurpose::ArrayLiteral))
                    .count(),
                2
            );
            assert_eq!(block.stmts.len(), 3);
            let destination = locals
                .iter()
                .find(|info| info.name == Some("values"))
                .unwrap();
            assert!(
                matches!(&block.stmts[2], Stmt::InitArray { local, .. } if *local == destination.id)
            );
        },
    );
}

#[test]
fn direct_literals_keep_element_coercions() {
    check(
        "let values: [Variant] = [1, 2];",
        SemanticPolicy::default(),
        |locals, block| {
            assert!(locals.iter().all(|info| info.temporary_purpose.is_none()));
            let Stmt::InitArray { elements, .. } = &block.stmts[0] else {
                panic!("{block:?}")
            };
            assert_eq!(elements.len(), 2);
            assert!(
                elements
                    .iter()
                    .all(|element| matches!(element, Expr::Call { .. }))
            );
        },
    );
}

#[test]
fn non_array_destination_coercions_keep_temporary() {
    check(
        "let value: Variant = [1];",
        SemanticPolicy::default(),
        |locals, _| {
            assert_eq!(
                locals
                    .iter()
                    .filter(|info| info.temporary_purpose == Some(TemporaryPurpose::ArrayLiteral))
                    .count(),
                1
            );
        },
    );
}
