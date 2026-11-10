use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn analyze(
    source: &str,
    dependency: &str,
    inspect: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &'ctx TypeInterner),
) -> Vec<String> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("dependency.reds", dependency);
    let file = sources.push_back("events.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    if !errors.is_empty() {
        return errors.into_iter().map(|e| e.to_string()).collect();
    }
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, file)| {
            let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
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
    inspect(&unit, &symbols, &interner);
    errors
        .into_iter()
        .map(|e| e.to_string())
        .chain(reporter.into_reported().into_iter().map(|e| e.to_string()))
        .collect()
}

#[test]
fn implicit_bool_signatures_include_prototypes_and_call_results() {
    let errors = analyze(
        "import class Native { event OnNative(); }
         class Listener extends Native {
           event OnNative() { return true; }
           event OnEmpty();
           event OnPartial(value: Bool) { if(value) { return false; } }
           public function Run(): Bool { return OnEmpty(); }
         }",
        "",
        |unit, symbols, interner| {
            for name in ["Native", "Listener"] {
                let aggregate = symbols[interner.intern(name)]
                    .schema()
                    .as_aggregate()
                    .unwrap();
                for method in aggregate
                    .methods()
                    .iter()
                    .filter(|m| m.func().flags().is_callback())
                {
                    assert_eq!(method.func().type_().return_type().to_string(), "Bool");
                }
            }
            let native = &unit.classes[&interner.intern("Native")];
            assert!(native.methods.is_empty(), "native callback stays bodyless");
            assert_eq!(unit.classes[&interner.intern("Listener")].methods.len(), 4);
        },
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn bare_returns_and_non_bool_values_are_not_fallthrough() {
    for body in [
        "return;",
        "if(value) { return true; } return;",
        "return \"text\";",
    ] {
        let errors = analyze(
            &format!("class Listener {{ event OnProbe(value: Bool) {{ {body} }} }}"),
            "",
            |_, _, _| {},
        );
        assert!(
            errors.iter().any(|e| e.contains("Bool")),
            "{body}: {errors:?}"
        );
    }
    let errors = analyze(
        "class Listener { event OnProbe(): Bool { return true; } }",
        "",
        |_, _, _| {},
    );
    assert!(
        errors
            .iter()
            .any(|e| e.contains("events cannot declare a return type")),
        "{errors:?}"
    );
}

#[test]
fn integer_returns_require_general_conversion_support() {
    for source in [
        "class Listener { event OnProbe(value: Int32) { return value; } }",
        "class Listener { event OnProbe() { return 7; } }",
        "function Probe(value: Int32): Bool { return value; }",
        "function Probe(): Bool { return 7; }",
        "class Listener { public function Probe(value: Int32): Bool { return value; } }",
    ] {
        for dependency in [
            "",
            "native func Cast(value: Int32) -> Bool;",
            "func Cast(value: Int32) -> Bool { return true; }",
        ] {
            let errors = analyze(source, dependency, |_, _, _| {});
            assert!(
                errors
                    .iter()
                    .any(|e| e.contains("Int32") && e.contains("Bool")),
                "implicit integer return conversion must be rejected: {source}: {errors:?}"
            );
        }
    }
    let errors = analyze(
        "class Listener { event OnProbe(value: Float) { return value; } }",
        "native func Cast(value: Float) -> Bool;",
        |_, _, _| {},
    );
    assert!(!errors.is_empty(), "Float conversion is explicit in REDmod");
    let errors = analyze(
        "class Listener { event OnProbe() { return true; } }",
        "native func Cast(value: Int32) -> Bool;
         class Community { protected cb func OnNumeric(value: Int32) -> Bool { return value; } }",
        |_, _, _| {},
    );
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Int32") && e.contains("Bool")),
        "community integer returns must remain rejected: {errors:?}"
    );
}

#[test]
fn community_callback_contracts_are_not_rewritten() {
    let dependency = "class Community { protected cb func OnVoid() {} protected cb func OnBool() -> Bool { return true; } }";
    let errors = analyze(
        "class Listener extends Community { event OnBool() {} }",
        dependency,
        |_, symbols, interner| {
            let aggregate = symbols[interner.intern("Community")]
                .schema()
                .as_aggregate()
                .unwrap();
            for (name, ret) in [("OnVoid", "Void"), ("OnBool", "Bool")] {
                assert_eq!(
                    aggregate
                        .methods()
                        .by_name(name)
                        .next()
                        .unwrap()
                        .func()
                        .type_()
                        .return_type()
                        .to_string(),
                    ret
                );
            }
        },
    );
    assert!(errors.is_empty(), "{errors:?}");
    let errors = analyze(
        "class Listener extends Community { event OnVoid() {} }",
        dependency,
        |_, _, _| {},
    );
    assert!(
        errors
            .iter()
            .any(|e| e.contains("inherited callback must return Bool")),
        "{errors:?}"
    );
}
