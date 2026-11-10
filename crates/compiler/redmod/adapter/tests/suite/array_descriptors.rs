use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn check_with_dependencies(source: &str, boot: bool, dependency: &str) -> Vec<String> {
    let sources = SourceMap::new();
    if boot {
        sources.populate_boot_lib();
    }
    sources.push_back("dependency.reds", dependency);
    let file = sources.push_back("arrays.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{source}: {errors:?}");
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
    let (_, _, errors) = process_sources(
        parsed,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    errors
        .into_iter()
        .map(|error| error.to_string())
        .chain(
            reporter
                .into_reported()
                .into_iter()
                .map(|error| error.to_string()),
        )
        .collect()
}

fn check(source: &str) -> Vec<String> {
    check_with_dependencies(source, true, "")
}

#[test]
fn grow_and_count_use_existing_generic_intrinsic_signatures() {
    for source in [
        "function Run(values: array<Int32>, amount: Int32): Int32 { values.Grow(amount); return values.Count(7); }",
        "function Run(): Int32 { var values: array<String>; values.Grow(2); return values.Count(\"needle\"); }",
        "function Run(values: array<Bool>): Int32 { values.Grow(0); return values.Count(true); }",
        "class Holder { var values: array<Int32>; public function Run(): Int32 { values.Grow(2); return this.values.Count(7); } }",
        "function Run(values: ref<array<Int32>>): Int32 { values.Grow(2); return values.Count(7); }",
    ] {
        let errors = check(source);
        assert!(errors.is_empty(), "{source}: {errors:?}");
    }
}

#[test]
fn grow_and_count_reject_wrong_arity_and_argument_types() {
    for call in [
        "values.Grow()",
        "values.Grow(1, 2)",
        "values.Grow(true)",
        "values.Grow(\"two\")",
        "values.Count()",
        "values.Count(1, 2)",
        "values.Count(true)",
        "values.Count(\"needle\")",
    ] {
        let source = format!("function Run(values: array<Int32>) {{ {call}; }}");
        let errors = check(&source);
        assert!(!errors.is_empty(), "{source}");
    }
    for body in [
        "var result: Int32 = values.Grow(2);",
        "var result: Bool = values.Count(2);",
    ] {
        let source = format!("function Run(values: array<Int32>) {{ {body} }}");
        assert!(!check(&source).is_empty(), "{source}");
    }
}

#[test]
fn both_mutation_and_read_require_receiver_places() {
    for call in ["Make().Grow(2)", "Make().Count(7)"] {
        let source = format!(
            "function Make(): array<Int32> {{ var values: array<Int32>; return values; }} function Run() {{ {call}; }}"
        );
        let errors = check(&source);
        assert!(
            errors.iter().any(|error| error.contains("temporary")),
            "{source}: {errors:?}"
        );
    }
}

#[test]
fn count_reads_const_arrays_but_grow_cannot_mutate_them() {
    for source in [
        "function Run(const values: array<Int32>): Int32 { return values.Count(7); }",
        "function Run(): Int32 { const var values: array<Int32>; return values.Count(7); }",
    ] {
        let errors = check(source);
        assert!(errors.is_empty(), "{source}: {errors:?}");
    }
    for (source, expected) in [
        (
            "function Run(const values: array<Int32>) { values.Grow(2); }",
            "const parameter",
        ),
        (
            "function Run() { const var values: array<Int32>; values.Grow(2); }",
            "const local",
        ),
    ] {
        let errors = check(source);
        assert!(
            errors.iter().any(|error| error.contains(expected)),
            "{source}: {errors:?}"
        );
    }
}

#[test]
fn descriptors_do_not_alias_other_names_or_fixed_array_methods() {
    for call in [
        "values.grow(2)",
        "values.count(7)",
        "values.ArrayGrow(2)",
        "values.ArrayCount(7)",
        "values.EraseFast(0)",
        "ArrayGrow(values, 2)",
        "ArrayCount(values, 7)",
    ] {
        let source = format!("function Run(values: array<Int32>) {{ {call}; }}");
        assert!(!check(&source).is_empty(), "{source}");
    }
    for call in ["values.Grow(2)", "values.Count(7)"] {
        let source = format!("function Run(values: Int32[1]) {{ {call}; }}");
        assert!(!check(&source).is_empty(), "{source}");
    }
    let errors = check(
        "class Other { public function Grow(value: Bool): Bool { return value; } public function Count(): String { return \"custom\"; } } function Run(value: Other): String { value.Grow(true); return value.Count(); }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn descriptors_require_real_intrinsic_symbols() {
    for (call, name, fake) in [
        (
            "values.Grow(2)",
            "ArrayGrow",
            "func ArrayGrow(out values: array<Int32>, count: Int32) {}",
        ),
        (
            "values.Count(7)",
            "ArrayCount",
            "func ArrayCount(out values: array<Int32>, value: Int32) -> Int32 { return 0; }",
        ),
    ] {
        let source = format!("function Run(values: array<Int32>) {{ {call}; }}");
        for dependency in ["", fake] {
            let errors = check_with_dependencies(&source, false, dependency);
            assert!(
                errors.iter().any(|error| error.contains(name)),
                "{source}: {errors:?}"
            );
        }
    }
}
