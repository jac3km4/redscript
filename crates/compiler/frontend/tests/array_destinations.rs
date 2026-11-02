use std::rc::Rc;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::ir::{
    Block, Const, Expr, LocalInfo, Stmt, TemporaryOwner, TemporaryPurpose,
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
         native func OperatorAssignAdd(out lhs: Int32, rhs: Int32) -> Int32;
         native func OperatorLess(lhs: Int32, rhs: Int32) -> Bool;
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

fn collect_statements<'a, 'ctx>(block: &'a Block<'ctx>, statements: &mut Vec<&'a Stmt<'ctx>>) {
    for stmt in &block.stmts {
        statements.push(stmt);
        match stmt {
            Stmt::Block(block, _) => collect_statements(block, statements),
            Stmt::While(loop_, _) => collect_statements(&loop_.block, statements),
            Stmt::Branches {
                branches, default, ..
            } => {
                for branch in branches {
                    collect_statements(&branch.block, statements);
                }
                if let Some(block) = default {
                    collect_statements(block, statements);
                }
            }
            _ => {}
        }
    }
}

fn assert_array_copy<'a, 'ctx>(
    locals: &[LocalInfo<'_>],
    block: &'a Block<'ctx>,
    name: &str,
    element_count: usize,
) -> &'a [Expr<'ctx>] {
    let destination = locals.iter().find(|info| info.name == Some(name)).unwrap();
    let temps: Vec<_> = locals
        .iter()
        .filter(|info| {
            info.temporary_purpose == Some(TemporaryPurpose::ArrayLiteral)
                && info.temporary_owner == Some(TemporaryOwner::Destination(destination.id))
        })
        .collect();
    assert_eq!(temps.len(), 1, "{name}");
    let temp = temps[0];
    assert_ne!(temp.id, destination.id);
    assert_eq!(temp.name, None);
    let mut statements = vec![];
    collect_statements(block, &mut statements);
    assert!(!statements.iter().any(|stmt| {
        matches!(stmt, Stmt::InitArray { local, .. } | Stmt::InitDefault { local, .. } if *local == destination.id)
    }), "must not resize or clear the possibly aliased destination {name}");
    let index = statements
        .iter()
        .position(|stmt| matches!(stmt, Stmt::InitArray { local, .. } if *local == temp.id))
        .unwrap();
    let Stmt::InitArray { elements, .. } = statements[index] else {
        unreachable!()
    };
    assert_eq!(elements.len(), element_count);
    let Stmt::Expr(copy) = statements[index + 1] else {
        panic!("missing copy for {name}")
    };
    let Expr::Assign {
        place, expr, span, ..
    } = &**copy
    else {
        panic!("{copy:?}")
    };
    assert!(matches!(&**place, Expr::Local(local, _) if *local == destination.id));
    assert!(matches!(&**expr, Expr::Local(local, _) if *local == temp.id));
    assert_eq!(destination.initializer, Some(*span));
    elements
}

#[test]
fn loop_declarations_keep_temporary_copies_including_empty_literals() {
    check(
        "while again { let empty: [Int32] = []; ArrayPush(empty, 1); let filled = [First(), Second()]; }",
        SemanticPolicy::default(),
        |locals, block| {
            assert_eq!(block.stmts.len(), 1);
            let Stmt::While(loop_, _) = &block.stmts[0] else {
                panic!("{block:?}")
            };
            assert_eq!(loop_.block.stmts.len(), 5);
            // The backend leaves InitArray([]) inert; the fresh temporary is
            // copied each iteration rather than clearing the escaped destination.
            assert_array_copy(locals, &loop_.block, "empty", 0);
            let elements = assert_array_copy(locals, &loop_.block, "filled", 2);
            assert!(elements[0].span().start < elements[1].span().start);
        },
    );
}

#[test]
fn retained_script_ref_aliases_require_temporary_copies_in_repeated_declarations() {
    let initializer = "let values = [previous[1], previous[0]]; previous = AsRef(values);";
    for repeated in [
        format!("while again {{ {initializer} }}"),
        format!("while again {{ if again {{ if again {{ {initializer} }} }} }}"),
        format!("while again {{ if again {{}} else {{ {initializer} }} }}"),
        format!("for item in seed {{ {initializer} }}"),
        format!("for item in seed {{ if again {{ {initializer} }} }}"),
    ] {
        check(
            &format!(
                "let seed = [1, 2]; let previous: script_ref<[Int32]> = AsRef(seed); {repeated}"
            ),
            SemanticPolicy::default(),
            |locals, block| {
                let elements = assert_array_copy(locals, block, "values", 2);
                for (element, expected) in elements.iter().zip([1, 0]) {
                    let Expr::Index { index, .. } = element else {
                        panic!("{element:?}")
                    };
                    assert!(
                        matches!(&**index, Expr::Const(Const::I32(value), _) if *value == expected)
                    );
                }
            },
        );
    }
}

#[test]
fn repeated_empty_literals_keep_copy_fallback_inside_branches_and_for_in() {
    for repeated in [
        "while again { if again { let empty: [Int32] = []; ArrayPush(empty, 1); } }",
        "for item in [1, 2] { let empty: [Int32] = []; ArrayPush(empty, item); }",
    ] {
        check(repeated, SemanticPolicy::default(), |locals, block| {
            assert_array_copy(locals, block, "empty", 0);
        });
    }
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
