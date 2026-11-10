use std::rc::Rc;

use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::ir::{
    Block, Expr, InitializationKind as Kind, LocalInfo, LocalStorage, Stmt,
};
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, SemanticPolicy, Symbols, TypeInterner, parse_files,
    process_sources_with_policy,
};

fn check(body: &str, hoist: bool, inspect: impl FnOnce(&[LocalInfo<'_>], &Block<'_>, &str)) {
    let source = format!(
        "native func Value() -> Int32;
         native func OperatorAssignAdd(out lhs: Int32, rhs: Int32) -> Int32;
         native func OperatorLess(lhs: Int32, rhs: Int32) -> Bool;
         native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool;
         func Test(input: [Int32]) {{ {body} }}"
    );
    let sources = SourceMap::new();
    let file = sources.push_back("declarations.reds", source.clone());
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{reporter:?}");
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, symbols) = process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[(
            file,
            Rc::new(SemanticPolicy {
                hoist_locals: hoist,
                ..SemanticPolicy::default()
            }),
        )],
    );
    assert!(reporter.reported().is_empty(), "{reporter:?}");
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Test")
        .unwrap();
    let function = &unit.functions[entry.key()];
    inspect(&function.locals, &function.block, &source);
}

fn text(source: &str, span: Span) -> &str {
    &source[span.start as usize..span.end as usize]
}

fn named<'a, 'ctx>(locals: &'a [LocalInfo<'ctx>], name: &str) -> &'a LocalInfo<'ctx> {
    locals
        .iter()
        .find(|local| local.name == Some(name))
        .unwrap()
}

#[test]
fn lexical_values_defaults_and_direct_arrays_keep_distinct_initialization_kinds() {
    check(
        "let value = Value(); let defaulted: Int32; const array = [1]; let empty: [Int32] = []; value = 2;",
        false,
        |locals, block, source| {
            assert!(locals[0].declaration.is_none());
            for (index, (name, kind, declaration)) in [
                ("value", Kind::AuthoredValue, "let value = Value()"),
                ("defaulted", Kind::Default, "let defaulted: Int32"),
                ("array", Kind::AuthoredValue, "const array = [1]"),
                ("empty", Kind::AuthoredValue, "let empty: [Int32] = []"),
            ]
            .into_iter()
            .enumerate()
            {
                let local = named(locals, name);
                let provenance = local.declaration.unwrap();
                assert_eq!(provenance.initialization.kind, kind);
                assert_eq!(provenance.storage, LocalStorage::Lexical);
                assert_eq!(provenance.initialization.site, block.stmts[index].span());
                assert_eq!(
                    text(source, provenance.span).trim_end_matches(';'),
                    declaration
                );
                assert_eq!(
                    local.initializer,
                    (kind == Kind::AuthoredValue).then_some(provenance.initialization.site)
                );
                assert_eq!(
                    provenance.scope,
                    named(locals, "value").declaration.unwrap().scope
                );
            }
            assert!(matches!(block.stmts[2], Stmt::InitArray { .. }));
            assert!(matches!(block.stmts[3], Stmt::InitDefault { .. }));
            assert_ne!(
                named(locals, "value").initializer,
                Some(block.stmts[4].span())
            );
        },
    );
}

#[test]
fn sibling_and_nested_shadowing_have_distinct_scope_and_local_identities() {
    check(
        "let x = 0; if true { let x = 1; if true { let x = 2; } } if true { let x = 3; let y = 4; }",
        false,
        |locals, _, _| {
            let bindings: Vec<_> = locals
                .iter()
                .filter(|local| local.name == Some("x"))
                .collect();
            assert_eq!(bindings.len(), 4);
            for (i, local) in bindings.iter().enumerate() {
                for other in &bindings[i + 1..] {
                    assert_ne!(local.id, other.id);
                    assert_ne!(
                        local.declaration.unwrap().scope,
                        other.declaration.unwrap().scope
                    );
                }
            }
            assert_eq!(
                bindings[3].declaration.unwrap().scope,
                named(locals, "y").declaration.unwrap().scope
            );
        },
    );
}

#[test]
fn hoisted_defaults_at_entry_are_separate_from_authored_initializers() {
    check(
        "if true { const value: Int32 = 1; let defaulted: Int32; } let inferred = 2;",
        true,
        |locals, block, source| {
            let value = named(locals, "value");
            let defaulted = named(locals, "defaulted");
            let Stmt::Branches { branches, .. } = &block.stmts[2] else {
                panic!("{block:?}")
            };
            for (index, local) in [value, defaulted].into_iter().enumerate() {
                let declaration = local.declaration.unwrap();
                let LocalStorage::Hoisted {
                    scope,
                    default_site,
                } = declaration.storage
                else {
                    panic!("{declaration:?}")
                };
                assert_ne!(scope, declaration.scope);
                assert_eq!(default_site, block.stmts[index].span());
                assert_eq!(text(source, default_site), local.name.unwrap());
                assert!(text(source, declaration.span).contains(local.name.unwrap()));
                if index == 0 {
                    assert_eq!(declaration.initialization.kind, Kind::AuthoredValue);
                    assert_eq!(
                        declaration.initialization.site,
                        branches[0].block.stmts[0].span()
                    );
                    assert_ne!(default_site, declaration.initialization.site);
                    assert_eq!(local.initializer, Some(declaration.initialization.site));
                } else {
                    assert_eq!(declaration.initialization.kind, Kind::Default);
                    assert_eq!(declaration.initialization.site, default_site);
                    assert_eq!(local.initializer, None);
                }
            }
            let inferred = named(locals, "inferred").declaration.unwrap();
            assert_eq!(inferred.storage, LocalStorage::Lexical);
            let LocalStorage::Hoisted { scope, .. } = value.declaration.unwrap().storage else {
                unreachable!()
            };
            assert_eq!(inferred.scope, scope);
        },
    );
}

#[test]
fn pattern_and_iteration_sites_identify_their_binding_assignments() {
    check(
        "if let bound = Value() { let inside = 1; } while let repeated = Value() {} switch Value() { case let matched: break; } for item in input { let body = 1; }",
        false,
        |locals, block, source| {
            for name in ["bound", "repeated", "matched"] {
                let local = named(locals, name);
                let declaration = local.declaration.unwrap();
                assert_eq!(declaration.initialization.kind, Kind::Pattern);
                assert_eq!(text(source, declaration.initialization.site), name);
                assert!(text(source, declaration.span).contains(name));
                assert_eq!(local.initializer, None);
            }
            assert_eq!(
                named(locals, "bound").declaration.unwrap().scope,
                named(locals, "inside").declaration.unwrap().scope
            );
            assert_ne!(
                named(locals, "bound").declaration.unwrap().scope,
                named(locals, "repeated").declaration.unwrap().scope
            );
            let item = named(locals, "item");
            let declaration = item.declaration.unwrap();
            assert_eq!(declaration.initialization.kind, Kind::Iteration);
            assert!(text(source, declaration.span).starts_with("for item in input"));
            assert_eq!(
                text(source, declaration.initialization.site),
                "item in input"
            );
            assert_eq!(item.initializer, None);
            assert_eq!(
                declaration.scope,
                named(locals, "body").declaration.unwrap().scope
            );
            let Stmt::Block(for_, _) = block.stmts.back().unwrap() else {
                panic!("{block:?}")
            };
            let Stmt::While(loop_, _) = &for_.stmts[1] else {
                panic!("{for_:?}")
            };
            assert_eq!(declaration.initialization.site, loop_.block.stmts[0].span());
            for local in locals
                .iter()
                .filter(|local| local.temporary_purpose.is_some())
            {
                assert_eq!(local.declaration, None);
            }
        },
    );
}

#[test]
fn destructured_patterns_share_the_declaration_but_not_identity_or_initialization_site() {
    check(
        "if let [first, second] = input {} switch input { case let [other]: break; }",
        false,
        |locals, block, source| {
            let first = named(locals, "first");
            let second = named(locals, "second");
            let a = first.declaration.unwrap();
            let b = second.declaration.unwrap();
            assert_ne!(first.id, second.id);
            assert_eq!(a.span, b.span);
            assert_eq!(text(source, a.span), "[first, second]");
            assert_eq!(a.scope, b.scope);
            assert_ne!(a.initialization.site, b.initialization.site);
            let Stmt::Branches { branches, .. } = &block.stmts[1] else {
                panic!("{block:?}")
            };
            for (local, stmt) in [first, second].into_iter().zip(&branches[0].block.stmts) {
                assert_eq!(local.declaration.unwrap().initialization.site, stmt.span());
                let Stmt::Expr(expr) = stmt else {
                    panic!("{stmt:?}")
                };
                assert!(
                    matches!(&**expr, Expr::Assign { place, .. } if matches!(&**place, Expr::Local(id, _) if *id == local.id))
                );
            }
            assert_ne!(a.scope, named(locals, "other").declaration.unwrap().scope);
        },
    );
}

#[test]
fn hoisted_closure_storage_belongs_to_the_closure_not_the_enclosing_function() {
    check(
        "let outer: Int32 = 1; let f: () -> Void = () -> { if true { let inner: Int32 = 2; } };",
        true,
        |locals, block, _| {
            let LocalStorage::Hoisted {
                scope: outer_scope, ..
            } = named(locals, "outer").declaration.unwrap().storage
            else {
                panic!("{locals:?}")
            };
            let Stmt::Expr(expr) = block.stmts.back().unwrap() else {
                panic!("{block:?}")
            };
            let Expr::Assign { expr, .. } = &**expr else {
                panic!("{expr:?}")
            };
            let Expr::NewClosure { closure, .. } = &**expr else {
                panic!("{expr:?}")
            };
            let inner = named(&closure.locals, "inner");
            let declaration = inner.declaration.unwrap();
            let LocalStorage::Hoisted {
                scope,
                default_site,
            } = declaration.storage
            else {
                panic!("{declaration:?}")
            };
            assert_ne!(scope, outer_scope);
            assert_ne!(scope, declaration.scope);
            assert_eq!(closure.block.stmts[0].span(), default_site);
            let Stmt::Branches { branches, .. } = &closure.block.stmts[1] else {
                panic!("{closure:?}")
            };
            assert_eq!(
                declaration.initialization.site,
                branches[0].block.stmts[0].span()
            );
            assert_eq!(inner.initializer, Some(declaration.initialization.site));
        },
    );
}

#[test]
fn closures_have_independent_declarations_and_distinct_scopes() {
    check(
        "let x = 1; let f: () -> Void = () -> { let x = 2; }; let g: () -> Void = () -> { let x = 3; };",
        false,
        |locals, block, _| {
            let outer = named(locals, "x");
            let mut closures = vec![];
            for stmt in block.stmts.iter().skip(1) {
                let Stmt::Expr(expr) = stmt else {
                    panic!("{stmt:?}")
                };
                let Expr::Assign { expr, .. } = &**expr else {
                    panic!("{expr:?}")
                };
                let Expr::NewClosure { closure, .. } = &**expr else {
                    panic!("{expr:?}")
                };
                closures.push(named(&closure.locals, "x"));
            }
            assert_eq!(closures.len(), 2);
            for local in &closures {
                assert_ne!(local.id, outer.id);
                assert_ne!(
                    local.declaration.unwrap().scope,
                    outer.declaration.unwrap().scope
                );
                assert_eq!(local.declaration.unwrap().storage, LocalStorage::Lexical);
            }
            assert_ne!(
                closures[0].declaration.unwrap().scope,
                closures[1].declaration.unwrap().scope
            );
        },
    );
}
