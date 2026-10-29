use std::fmt::Write;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Symbols, TypeInterner, expr_type, infer_from_sources, ir,
};

#[test]
fn inferred_types() {
    insta::glob!("data/type-queries/*.reds", |path| {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let file = sources.push_back(
            path.file_name().unwrap().to_str().unwrap(),
            std::fs::read_to_string(path).unwrap(),
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );

        let mut output = String::new();
        // Some recovery fixtures intentionally diagnose shadowing. Include every
        // diagnostic so unexpected errors cannot silently pass as successful queries.
        for diagnostic in reporter.reported() {
            writeln!(output, "{}", diagnostic.display(&sources).unwrap()).unwrap();
        }

        let mut functions = symbols
            .free_functions()
            .filter(|entry| entry.func().span().is_some_and(|span| span.file == file))
            .collect::<Vec<_>>();
        functions.sort_by_key(|entry| entry.func().span().unwrap().start);
        assert!(
            !functions.is_empty(),
            "expected query functions in {path:?}"
        );
        for entry in functions {
            let function = &unit.functions[entry.key()];
            let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
                panic!("expected a final return in {}", entry.name());
            };
            write!(
                output,
                "{}: {}",
                entry.name(),
                expr_type(expr, function, &symbols).unwrap()
            )
            .unwrap();
            if let ir::Expr::Call { call, .. } = &**expr {
                let (kind, type_args) = match &**call {
                    ir::Call::FreeFunction { type_args, .. } => ("free", type_args),
                    ir::Call::Instance { type_args, .. } => ("instance", type_args),
                    ir::Call::Static { type_args, .. } => ("static", type_args),
                    ir::Call::Closure { .. } => panic!("unexpected closure call"),
                };
                let args = type_args
                    .iter()
                    .map(|typ| typ.coalesced(&symbols).unwrap().to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(output, " ({kind} call <{args}>)").unwrap();
            }
            output.push('\n');
        }
        insta::assert_snapshot!(output);
    });
}
