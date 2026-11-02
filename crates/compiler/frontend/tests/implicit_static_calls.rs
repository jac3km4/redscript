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
