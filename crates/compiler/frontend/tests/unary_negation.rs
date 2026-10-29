use redscript_community_ast::SourceMap;
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, LoweredCompilationUnit, SemanticPolicy, Symbols, TypeInterner,
    ir, parse_files, process_sources, process_sources_with_policy,
};

fn returned_expr<'a, 'ctx>(
    unit: &'a LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    name: &str,
) -> &'a ir::Expr<'ctx> {
    let function = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == name)
        .unwrap();
    let block = &unit.functions[function.key()].block;
    assert_eq!(block.stmts.len(), 1);
    let ir::Stmt::Return(Some(expr), _) = &block.stmts[0] else {
        panic!("expected a single return in {name}")
    };
    expr
}

#[test]
fn integer_negation_folds_when_representable_and_otherwise_calls_operator() {
    for with_policy in [false, true] {
        let sources = SourceMap::new();
        let file = sources.push_back(
            "negation.reds",
            "native func OperatorNeg(value: Int32) -> Int32;
             func Ordinary() -> Int32 { return -42; }
             func Hinted() -> Int8 { return -42; }
             func Minimum() -> Int32 { return -2147483648; }
             func Nested() -> Int32 { return -(-2147483648); }",
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let modules = parse_files(&sources, &mut reporter);
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        let evaluator = Evaluator::from_modules(&modules);
        let (unit, symbols) = if with_policy {
            process_sources_with_policy(
                modules,
                Symbols::with_default_types(),
                evaluator,
                &mut reporter,
                &interner,
                &[(
                    file,
                    SemanticPolicy {
                        hoist_locals: true,
                        ..Default::default()
                    },
                )],
            )
        } else {
            process_sources(
                modules,
                Symbols::with_default_types(),
                evaluator,
                &mut reporter,
                &interner,
            )
        };
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
        assert!(matches!(
            returned_expr(&unit, &symbols, "Ordinary"),
            ir::Expr::Const(ir::Const::I32(-42), _)
        ));
        assert!(matches!(
            returned_expr(&unit, &symbols, "Hinted"),
            ir::Expr::Const(ir::Const::I8(-42), _)
        ));
        assert!(matches!(
            returned_expr(&unit, &symbols, "Minimum"),
            ir::Expr::Const(ir::Const::I32(i32::MIN), _)
        ));
        let ir::Expr::Call { call, .. } = returned_expr(&unit, &symbols, "Nested") else {
            panic!("expected OperatorNeg call")
        };
        let ir::Call::FreeFunction { function, args, .. } = &**call else {
            panic!("expected free function call")
        };
        let operator = symbols
            .free_functions()
            .find(|entry| entry.name().to_string() == "OperatorNeg")
            .unwrap();
        assert_eq!(*function, *operator.key());
        assert!(matches!(
            &**args,
            [ir::Expr::Const(ir::Const::I32(i32::MIN), _)]
        ));
    }
}
