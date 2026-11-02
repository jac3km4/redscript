use std::rc::Rc;

use redscript_community_ast::{ConversionKind, SourceMap};
use redscript_compiler_api::{Diagnostic, SourceMapExt};
use redscript_compiler_frontend::{
    CompileErrorReporter, Evaluator, SemanticPolicy, Symbols, TypeInterner, infer_from_sources,
    parse_files, process_sources_with_policy,
};

#[test]
fn community_intent_is_queryable_but_not_dispatchable() {
    for qualifier in ["implicit", "explicit"] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let provider = sources.push_back("provider.reds", format!("struct Data {{ let value: Int32; }} {qualifier} func Cast(value: Data) -> Int32 {{ return value.value; }}"));
        sources.push_back(
            "consumer.reds",
            "func Use(value: Data) -> Int32 { return Cast<Int32>(value); }",
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        let codes = reporter
            .reported()
            .iter()
            .map(Diagnostic::code)
            .collect::<Vec<_>>();
        assert_eq!(codes, ["UNSUPPORTED_NATIVE_CONVERSION"]);
        assert_eq!(
            reporter.reported()[0].to_string(),
            "this conversion declaration is not supported for this use"
        );
        let function = symbols
            .free_functions()
            .find(|entry| {
                entry
                    .func()
                    .span()
                    .is_some_and(|span| span.file == provider)
                    && entry.func().type_().conversion().is_some()
            })
            .unwrap();
        assert_eq!(
            function.func().type_().conversion(),
            Some(if qualifier == "implicit" {
                ConversionKind::Implicit
            } else {
                ConversionKind::Explicit
            })
        );
    }
}

#[test]
fn conversion_intent_is_ineligible_for_members_and_operator_builders() {
    for source in [
        "class Host { public implicit func Convert(value: Int32) -> Int32 = value; } func Use(host: Host) -> Int32 { return host.Convert(1); }",
        "class Host { public static explicit func Convert(value: Int32) -> Int32 = value; } func Use() -> Int32 { return Host.Convert(1); }",
        "struct Data {} implicit func OperatorAdd(lhs: Data, rhs: Data) -> Data = lhs; func Use(value: Data) -> Data { return value + value; }",
        "struct Data {} explicit func OperatorEqual(lhs: Data, rhs: Data) -> Bool = true; func Use(value: Data) -> Bool { return value == value; }",
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back("invalid.reds", source);
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        // Equality may safely fall back to the built-in Equals intrinsic.
        if !source.contains("OperatorEqual") {
            assert!(!reporter.reported().is_empty(), "{source}");
        }
        struct Calls<'a, 'ctx>(&'a Symbols<'ctx>);
        impl<'ctx> redscript_compiler_frontend::ir::Visitor<'ctx> for Calls<'_, 'ctx> {
            fn visit_call(
                &mut self,
                call: &redscript_compiler_frontend::ir::Call<'ctx>,
                _: redscript_community_ast::Span,
            ) {
                use redscript_compiler_frontend::ir::Call;
                let typ = match call {
                    Call::FreeFunction { function, .. } => Some(self.0[*function].type_()),
                    Call::Instance { method, .. } | Call::Static { method, .. } => {
                        Some(self.0[*method].type_())
                    }
                    Call::Closure { .. } => None,
                };
                assert!(typ.is_none_or(|typ| typ.conversion().is_none()));
                for arg in call.args() {
                    self.visit_expr(arg);
                }
            }
        }
        use redscript_compiler_frontend::ir::Visitor;
        let mut calls = Calls(&symbols);
        for function in unit.functions.values() {
            calls.visit_block(&function.block);
        }
    }
}

#[test]
fn unsupported_implicit_return_is_not_silently_coerced() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("provider.reds", "struct Data { let value: Int32; } implicit func Cast(value: Data) -> Int32 { return value.value; }");
    sources.push_back(
        "consumer.reds",
        "func Use(value: Data) -> Int32 { return value; }",
    );
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let codes = reporter
        .reported()
        .iter()
        .map(Diagnostic::code)
        .collect::<Vec<_>>();
    assert_eq!(codes, ["UNSUPPORTED_NATIVE_CONVERSION"]);
    assert_eq!(
        reporter.reported()[0].to_string(),
        "this conversion declaration is not supported for this use"
    );
}

#[test]
fn ambiguous_native_conversions_explain_selection_failure() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let first = sources.push_back(
        "first.reds",
        "module First\npublic native explicit func Cast(value: Int32) -> String;",
    );
    let second = sources.push_back(
        "second.reds",
        "module Second\npublic native explicit func Cast(value: Int32) -> String;",
    );
    let consumer = sources.push_back(
        "consumer.reds",
        "import First.Cast\nimport Second.Cast\nfunc Use(value: Int32) -> String { return Cast<String>(value); }",
    );
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{reporter:?}");
    let evaluator = Evaluator::from_modules(&modules);
    let policy = Rc::new(SemanticPolicy {
        native_object_cast: true,
        ..SemanticPolicy::default()
    });
    process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
        &[
            (first, policy.clone()),
            (second, policy.clone()),
            (consumer, policy),
        ],
    );
    assert_eq!(reporter.reported().len(), 1, "{reporter:?}");
    let diagnostic = &reporter.reported()[0];
    assert_eq!(diagnostic.code(), "UNSUPPORTED_NATIVE_CONVERSION");
    assert_eq!(
        diagnostic.to_string(),
        "multiple native conversion declarations match; cannot choose one"
    );
    assert_eq!(diagnostic.span().file, consumer);
    insta::assert_snapshot!(diagnostic.display(&sources).unwrap());
}
