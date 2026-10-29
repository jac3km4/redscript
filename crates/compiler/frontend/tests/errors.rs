use std::env;

use redscript_ast::{Item, SourceMap};
use redscript_compiler_api::{
    CompileErrorReporter, Diagnostic, SourceMapExt, Symbols, TypeInterner,
};
use redscript_compiler_frontend::pass::{DiagnosticPass, UnusedLocals};
use redscript_compiler_frontend::{Evaluator, infer_from_sources, parse_files, process_sources};

#[test]
fn compilation_errors() {
    insta::glob!("data/*.reds", |path| {
        let current = env::current_dir().unwrap().canonicalize().unwrap();
        let relative = path
            .strip_prefix(&current)
            .unwrap()
            .to_string_lossy()
            .replace("\\", "/");
        let sources = SourceMap::from_files(&[relative]).unwrap();
        sources.populate_boot_lib();

        let interner = TypeInterner::default();
        let symbols = Symbols::with_default_types();
        let mut reporter = CompileErrorReporter::default();

        let (_, _) = infer_from_sources(&sources, symbols, &mut reporter, &interner);
        insta::assert_snapshot!(DisplayDiagnostics(reporter.into_reported(), &sources));
    });
}

#[test]
fn implicit_enum_overflow() {
    let sources = SourceMap::from_files(&["tests/data/enum-resolution/overflow.reds"]).unwrap();
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let mut modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty());

    // Enum syntax only accepts Int32 literals; inject i64 boundary values to
    // exercise name resolution without changing the parser's accepted syntax.
    let mut replaced = 0;
    for module in &mut modules {
        for (decl, _) in &mut module.items {
            if let Item::Enum(enum_) = &mut decl.item {
                let seed = match enum_.name.0 {
                    "OverflowingEnum" => i64::MAX,
                    "NearMaximumEnum" => i64::MAX - 1,
                    _ => continue,
                };
                enum_.variants[0].0.value = Some(seed);
                replaced += 1;
            }
        }
    }
    assert_eq!(replaced, 2);

    let evaluator = Evaluator::from_modules(&modules);
    let (_, _) = process_sources(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
    );

    assert!(matches!(
        reporter.reported(),
        [Diagnostic::ValueOverflow(_)]
    ));
    insta::assert_snapshot!(DisplayDiagnostics(reporter.into_reported(), &sources));
}

#[test]
fn unused_locals() {
    let sources = SourceMap::from_files(&["tests/data/passes/unused-locals.reds"]).unwrap();
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(reporter.reported().is_empty());

    for function in unit.all_functions() {
        UnusedLocals.run(function, &mut reporter);
    }
    let mut diagnostics = reporter.into_reported();
    assert_eq!(diagnostics.len(), 2);
    assert!(
        diagnostics
            .iter()
            .all(|d| matches!(d, Diagnostic::UnusedLocal(_)))
    );
    // The pass collects locals in a hash map; this fixture uses a single file.
    diagnostics.sort_by_key(|diagnostic| {
        let span = diagnostic.span();
        (span.start, span.end)
    });
    insta::assert_snapshot!(DisplayDiagnostics(diagnostics, &sources));
}

struct DisplayDiagnostics<'ctx>(Vec<Diagnostic<'ctx>>, &'ctx SourceMap);

impl std::fmt::Display for DisplayDiagnostics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0
            .iter()
            .try_for_each(|diagnostic| writeln!(f, "{}", diagnostic.display(self.1).unwrap()))
    }
}
