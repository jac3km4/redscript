use redscript_compiler_frontend::ir;

use crate::common;

fn analyze(source: &str, expected_codes: &[&str], expected_targets: &[&str]) {
    common::analyze("indexed.reds", source, |_, unit, symbols, reporter| {
        let codes = reporter
            .reported()
            .iter()
            .map(redscript_compiler_frontend::Diagnostic::code)
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
    });
}

#[test]
fn indexed_derived_values_select_parent_overload_in_either_order() {
    for declarations in [
        "func Pick(value: Base) -> Int32 = 1; func Pick(value: String) -> Int32 = 2;",
        "func Pick(value: String) -> Int32 = 2; func Pick(value: Base) -> Int32 = 1;",
    ] {
        analyze(
            &format!(
                "class Base {{}} class Derived extends Base {{}} {declarations}
             func Run(values: [Derived]) -> Int32 {{ return Pick(values[0]); }}
             func RunDirect(value: Derived) -> Int32 {{ return Pick(value); }}"
            ),
            &[],
            &["Base", "Base"],
        );
    }
}

#[test]
fn indexed_and_direct_weak_values_accept_parent_parameters() {
    analyze(
        "class Base {} class Derived extends Base {}
         func Pick(value: wref<Base>) -> Int32 = 1; func Pick(value: String) -> Int32 = 2;
         func Run(values: [wref<Derived>]) -> Int32 { return Pick(values[0]); }
         func RunDirect(value: wref<Derived>) -> Int32 { return Pick(value); }",
        &[],
        &["wref<Base>", "wref<Base>"],
    );
}

#[test]
fn indexed_out_parameters_remain_invariant() {
    analyze(
        "class Base {} class Derived extends Base {}
         func Pick(out value: Base) -> Int32 = 1; func Pick(out value: Derived) -> Int32 = 2;
         func Run(values: [Derived]) -> Int32 { return Pick(values[0]); }",
        &[],
        &["Derived"],
    );
    analyze(
        "class Base {} class Derived extends Base {}
         func Pick(out value: Base) {} func Pick(out value: String) {}
         func Run(values: [Derived]) { Pick(values[0]); }",
        &["TYPE_ERR"],
        &[],
    );
}

#[test]
fn unrelated_indexed_values_are_not_eligible() {
    analyze(
        "class Base {} class Other {}
         func Pick(value: Base) {} func Pick(value: String) {}
         func Run(values: [Other]) { Pick(values[0]); }",
        &["TYPE_ERR"],
        &[],
    );
}

#[test]
fn null_and_fresh_arguments_stay_ambiguous_without_freezing() {
    for argument in ["null", "value"] {
        analyze(
            &format!(
                "class Base {{}} class Derived extends Base {{}}
             func Pick(value: Base) {{}} func Pick(value: Derived) {{}}
             func Run() {{ let value; Pick({argument}); value = new Base(); }}"
            ),
            &["MULTIPLE_MATCHING_OVERLOADS"],
            &[],
        );
    }
}
