use std::rc::Rc;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, SemanticPolicy, Symbols, TypeInterner, ir, parse_files,
    process_sources_with_policy,
};

#[test]
fn named_module_implicit_static_calls_keep_resolved_parent_identity() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back(
        "implicit.reds",
        "module Example
         class Host {
             public static func Take(values: [Int32]) -> Int32 = 1;
             public static func RunStatic() -> Int32 { return Take([1]); }
             public func RunInstance() -> Int32 { return Take([1]); }
         }",
    );
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
        &[(
            file,
            Rc::new(SemanticPolicy {
                implicit_members: true,
                ..SemanticPolicy::default()
            }),
        )],
    );
    assert!(reporter.reported().is_empty(), "{reporter:?}");
    let (host, _) = symbols
        .types()
        .find(|(id, _)| id.as_str() == "Example.Host")
        .unwrap();
    for name in ["RunStatic", "RunInstance"] {
        let entry = symbols.query_methods_by_name(host, name).next().unwrap();
        let function = &unit.classes[&host].methods[&entry.key().index()];
        let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
            panic!("expected return")
        };
        let ir::Expr::Call { call, .. } = &**expr else {
            panic!("expected call")
        };
        let ir::Call::Static { method, .. } = &**call else {
            panic!("expected static call")
        };
        assert_eq!(method.parent(), host);
        assert_eq!(symbols[*method].type_().params().len(), 1);
    }
}

#[test]
fn local_receivers_shadow_types_only_with_implicit_members() {
    for implicit_members in [false, true] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let file = sources.push_back(
            "shadow.reds",
            "module Example
             class Host {
                 public static func Take() -> Int32 = 1;
             }
             class Receiver {
                 public func Take() -> Int32 = 2;
             }
             class Runner {
                 public static func Parameter(Host: Receiver) -> Int32 {
                     return Host.Take();
                 }
                 public static func Local(value: Receiver) -> Int32 {
                     let Host = value;
                     return Host.Take();
                 }
             }",
        );
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
            &[(
                file,
                Rc::new(SemanticPolicy {
                    implicit_members,
                    ..SemanticPolicy::default()
                }),
            )],
        );
        assert!(reporter.reported().is_empty(), "{reporter:?}");
        let runner = interner.intern("Example.Runner");
        for (name, local) in [
            ("Parameter", ir::Local::Param(0)),
            ("Local", ir::Local::Var(1)),
        ] {
            let entry = symbols.query_methods_by_name(runner, name).next().unwrap();
            let function = &unit.classes[&runner].methods[&entry.key().index()];
            let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
                panic!("expected return")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("expected call")
            };
            if implicit_members {
                let ir::Call::Instance {
                    receiver,
                    receiver_type,
                    method,
                    ..
                } = &**call
                else {
                    panic!("expected instance call")
                };
                assert_eq!(method.parent(), interner.intern("Example.Receiver"));
                assert_eq!(
                    receiver_type.coalesced(&symbols).unwrap().id(),
                    method.parent()
                );
                assert!(
                    matches!(&**receiver, ir::Expr::Local(id, _) if *id == local),
                    "{name}: expected {local:?}, got {receiver:?}",
                );
            } else {
                let ir::Call::Static {
                    parent_id, method, ..
                } = &**call
                else {
                    panic!("expected static call")
                };
                assert_eq!(method.parent(), interner.intern("Example.Host"));
                assert_eq!(*parent_id, method.parent());
            }
        }
    }
}
