use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner, ir};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn analyze(source: &str, expected_codes: &[&str], expected_targets: &[&str]) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "operators.reds",
        "native func OperatorLogicNot(value: Bool) -> Bool;",
    );
    let file = sources.push_back("indexed.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, source)| {
            let (module, errors) = redscript_community_parser::parse_module(source.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let codes = reporter
        .reported()
        .iter()
        .map(redscript_compiler_api::Diagnostic::code)
        .collect::<Vec<_>>();
    assert_eq!(codes, expected_codes, "{source}: {:?}", reporter.reported());
    let mut targets = symbols
        .free_functions()
        .filter(|entry| {
            entry
                .name()
                .as_single_component()
                .is_some_and(|name| name.starts_with("Run"))
        })
        .filter_map(|entry| {
            let ir::Stmt::Return(Some(expr), _) =
                unit.functions.get(entry.key())?.block.stmts.back()?
            else {
                return None;
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                return None;
            };
            let ir::Call::FreeFunction { function, .. } = &**call else {
                return None;
            };
            Some(
                symbols[*function]
                    .type_()
                    .param_types()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(","),
            )
        })
        .collect::<Vec<_>>();
    targets.sort();
    let mut expected = expected_targets.to_vec();
    expected.sort();
    assert_eq!(targets, expected, "{source}");
}

#[test]
fn indexed_derived_and_direct_values_select_parent_overload() {
    for declarations in [
        "function Pick(value: Base): Int32 { return 1; } function Pick(value: String): Int32 { return 2; }",
        "function Pick(value: String): Int32 { return 2; } function Pick(value: Base): Int32 { return 1; }",
    ] {
        analyze(
            &format!(
                "class Base {{}} class Derived extends Base {{}} {declarations}
             function Run(const values: ref<array<Derived>>): Int32 {{ return Pick(values[0]); }}
             function RunDirect(value: Derived): Int32 {{ return Pick(value); }}"
            ),
            &[],
            &["Base", "Base"],
        );
    }
}

#[test]
fn indexed_and_direct_weak_values_accept_parent_overload() {
    analyze(
        "class Base {} class Derived extends Base {}
         function Pick(value: weak<Base>): Int32 { return 1; } function Pick(value: String): Int32 { return 2; }
         function Run(values: ref<array<weak<Derived>>>): Int32 { return Pick(values[0]); }
         function RunDirect(value: weak<Derived>): Int32 { return Pick(value); }",
        &[], &["wref<Base>", "wref<Base>"],
    );
}

#[test]
fn indexed_bare_and_weak_conditions_accept_is_defined() {
    analyze(
        "class Derived extends IScriptable {}
         function Check(const values: ref<array<Derived>>, weakValues: ref<array<weak<Derived>>>, value: Derived, weakValue: weak<Derived>) {
             if (!(values[0])) {} if (weakValues[0]) {} if (value) {} if (weakValue) {}
         }",
        &[], &[],
    );
}

#[test]
fn indexed_out_parameters_remain_invariant() {
    analyze(
        "class Base {} class Derived extends Base {}
         function Pick(out value: Base): Int32 { return 1; } function Pick(out value: Derived): Int32 { return 2; }
         function Run(values: ref<array<Derived>>): Int32 { return Pick(values[0]); }",
        &[], &["Derived"],
    );
    analyze(
        "class Base {} class Derived extends Base {}
         function Pick(out value: Base) {} function Pick(out value: String) {}
         function Run(values: ref<array<Derived>>) { Pick(values[0]); }",
        &["TYPE_ERR"],
        &[],
    );
}

#[test]
fn unrelated_indexed_values_are_rejected_and_null_stays_ambiguous() {
    analyze(
        "class Base {} class Other {}
         function Pick(value: Base) {} function Pick(value: String) {}
         function Run(values: ref<array<Other>>) { Pick(values[0]); }",
        &["TYPE_ERR"],
        &[],
    );
    analyze(
        "class Base {} class Derived extends Base {}
         function Pick(value: Base) {} function Pick(value: Derived) {}
         function Run() { Pick(NULL); }",
        &["MULTIPLE_MATCHING_OVERLOADS"],
        &[],
    );
}
