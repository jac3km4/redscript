use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::{DeclarationPath, analyze_sources, transpile_sources};

#[test]
fn exact_and_select_imports_do_not_widen_to_modules() {
    for import in ["import Models.Value", "import Models.{Value}"] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "models.reds",
            "module Models public struct Value {} public struct Unselected {}",
        );
        let selected = sources.push_back("use.reds", format!("module Consumer {import} public func Echo(value: Value) -> Value {{ return value; }}"));
        let interner = TypeInterner::default();
        let output = transpile_sources(
            &sources,
            &[selected],
            Symbols::with_default_types(),
            &interner,
        )
        .unwrap();
        let text = &output[0].module.text;
        assert!(text.contains("@module \"Consumer\"\n\n"), "{text}");
        assert!(text.contains("@using \"Models.Value\"\n"), "{text}");
        assert!(!text.contains("@using \"Models\""), "{text}");
        assert!(!text.contains("Unselected"), "{text}");
        assert!(
            text.contains("value: Models.Value) -> Models.Value"),
            "{text}"
        );
        assert_eq!(output[0].module.mappings[0].source.file, selected);
        // Qualified type emission is a separate, existing target limitation.
        // Check the import grammar independently rather than claiming a roundtrip.
        let (parsed, errors) =
            redscript_redmod_parser::parse_module("@using \"Models.Value\"\n", selected);
        assert!(errors.is_empty(), "{errors:?}");
        assert!(parsed.is_some());
    }
}

#[test]
fn select_imports_keep_each_symbol_and_the_whole_overload_family() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("models.reds", "module Models public struct Value {} public func Choose(value: Int32) -> Int32 { return value; } public func Choose(value: String) -> String { return value; } public func Unselected() -> Int32 { return 0; }");
    let selected = sources.push_back("use.reds", "module Consumer import Models.{Value, Choose} public func Run(value: Value) -> Int32 { return Choose(1); }");
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    let text = &output[0].module.text;
    assert!(text.contains("@using \"Models.Choose\"\n"), "{text}");
    assert!(text.contains("@using \"Models.Value\"\n"), "{text}");
    assert!(!text.contains("@using \"Models\""), "{text}");
    assert!(text.contains("Models.Choose(1)"), "{text}");
    assert!(text.contains("value: Models.Value"), "{text}");
}

#[test]
fn exact_import_selects_the_whole_overload_family() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("helpers.reds", "module Helpers public func Choose(value: Int32) -> Int32 { return value; } public func Choose(value: String) -> String { return value; }");
    let selected = sources.push_back(
        "use.reds",
        "import Helpers.Choose public func Run() -> String { return Choose(\"chosen\"); }",
    );
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    assert!(output[0].module.text.contains("Helpers.Choose(\"chosen\")"));
    assert!(
        output[0]
            .module
            .text
            .contains("@using \"Helpers.Choose\"\n")
    );
}

#[test]
fn import_planning_does_not_revalidate_global_target_name_collisions() {
    for (dependency, consumer) in [
        (
            "public struct Collision {}",
            "public struct Collision {} public func Run(value: Collision) -> Collision { return value; }",
        ),
        (
            "public func Collision(value: String) -> String { return value; }",
            "public func Collision(value: Int32) -> Int32 { return value; } public func Run() -> Int32 { return Collision(1); }",
        ),
        (
            "public struct Collision {}",
            "public func Collision(value: Int32) -> Int32 { return value; }",
        ),
    ] {
        for import in ["import Models.Collision", "import Models.{Collision}"] {
            let sources = SourceMap::new();
            sources.populate_boot_lib();
            sources.push_back("models.reds", format!("module Models {dependency}"));
            let selected =
                sources.push_back("use.reds", format!("module Consumer {import} {consumer}"));
            let interner = TypeInterner::default();
            let analysis =
                analyze_sources(&sources, Symbols::with_default_types(), &interner).unwrap();
            let output = analysis.transpile_selected(&[selected]).unwrap();
            assert!(
                output[0]
                    .module
                    .text
                    .contains("@using \"Models.Collision\"\n")
            );
        }
    }
}

#[test]
fn unrelated_type_and_function_collisions_do_not_require_widening() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("models.reds", "module Models public struct Wanted {} public struct Other {} public func Choose(value: String) -> String { return value; }");
    let selected = sources.push_back("use.reds", "module Consumer import Models.Wanted public struct Other {} public func Choose(value: Int32) -> Int32 { return value; } public func Run() -> Int32 { return Choose(1); }");
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    let text = &output[0].module.text;
    assert!(text.contains("@using \"Models.Wanted\"\n"), "{text}");
    assert!(!text.contains("@using \"Models\""), "{text}");
    assert!(text.contains("Consumer.Choose(1)"), "{text}");
}

#[test]
fn private_conflicts_in_unselected_dependencies_do_not_block_imports() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("models.reds", "module Models public struct Wanted {}");
    sources.push_back("other.reds", "module Other private struct Wanted {}");
    let selected = sources.push_back(
        "use.reds",
        "import Models.Wanted public func Run() -> Int32 { return 1; }",
    );
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    assert!(output[0].module.text.contains("@using \"Models.Wanted\"\n"));
}

#[test]
fn symbol_module_prefix_overlap_cannot_widen_imports() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "models.reds",
        "module Models public func Pick(value: Int32) -> Int32 { return value; }",
    );
    let nested = sources.push_back(
        "nested.reds",
        "module Models.Pick public func Extra(value: Int32) -> Int32 { return value; }",
    );
    let selected = sources.push_back(
        "use.reds",
        "import Models.Pick public func Run() -> Int32 { return Pick(1); }",
    );
    let interner = TypeInterner::default();
    // The nested module is harmless while it is analysis-only.
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    assert!(output[0].module.text.contains("@using \"Models.Pick\"\n"));

    let errors = transpile_sources(
        &sources,
        &[selected, nested],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap_err();
    assert!(
        errors[0].message.contains("overlaps a module"),
        "{errors:?}"
    );
    assert_eq!(errors[0].span.unwrap().file, selected);
    assert_eq!(errors[0].declaration, Some(DeclarationPath(vec![0])));
}

#[test]
fn invalid_or_private_imports_still_fail_analysis() {
    for import in [
        "import Models.Missing",
        "import Models.{Wanted, Missing}",
        "import Models.Hidden",
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "models.reds",
            "module Models public struct Wanted {} private struct Hidden {}",
        );
        let selected = sources.push_back(
            "use.reds",
            format!("{import} public func Run() -> Int32 {{ return 1; }}"),
        );
        let interner = TypeInterner::default();
        let errors = transpile_sources(
            &sources,
            &[selected],
            Symbols::with_default_types(),
            &interner,
        )
        .unwrap_err();
        assert!(
            errors[0].message.contains("community analysis"),
            "{errors:?}"
        );
        assert_eq!(errors[0].span.unwrap().file, selected);
    }
}

#[test]
fn wildcard_imports_keep_the_existing_contract() {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "models.reds",
        "module Models public struct Wanted {} public struct Other {}",
    );
    let selected = sources.push_back(
        "use.reds",
        "import Models.* public func Run(value: Wanted) -> Wanted { return value; }",
    );
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    assert!(output[0].module.text.contains("@using \"Models\"\n"));
}

fn emitted_directive_sources(import: &str) -> Vec<String> {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let library = sources.push_back(
        "library.reds",
        "module DirectiveLibrary public func First() -> Int32 { return 1; } public func Second() -> Int32 { return 2; }",
    );
    let consumer = sources.push_back(
        "consumer.reds",
        format!("module DirectiveConsumer {import} public func DirectiveProbe() -> Int32 {{ return 1; }}"),
    );
    let interner = TypeInterner::default();
    transpile_sources(
        &sources,
        &[library, consumer],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap()
    .into_iter()
    .map(|output| output.module.text)
    .collect()
}

#[test]
fn emitted_directives_have_no_semicolons() {
    let mut snapshot = String::new();
    for import in [
        "",
        "import DirectiveLibrary.First",
        "import DirectiveLibrary.{First, Second}",
        "import DirectiveLibrary.*",
    ] {
        snapshot.push_str(&format!("=== {import:?} ===\n"));
        for text in emitted_directive_sources(import) {
            for line in text.lines().filter(|line| line.starts_with('@')) {
                assert!(!line.ends_with(';'), "{line}");
            }
            snapshot.push_str(&text);
        }
    }
    insta::assert_snapshot!(snapshot);
}

// Module/using directives apply to an entire file; preserve file boundaries.
fn validate_official_files(
    compiler: &redscript_test_support::OfficialCompiler,
    name: &str,
    sources: &[String],
    diagnostic: Option<&str>,
) {
    let sources = sources.iter().map(String::as_str).collect::<Vec<_>>();
    if let Some(diagnostic) = diagnostic {
        compiler.reject_files(name, &sources, diagnostic);
    } else {
        compiler.compile_files(name, &sources);
    }
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn emitted_directives_with_official_compiler() {
    let compiler = redscript_test_support::OfficialCompiler::from_env();
    for (name, import) in [
        ("Module", ""),
        ("Exact", "import DirectiveLibrary.First"),
        ("Select", "import DirectiveLibrary.{First, Second}"),
        ("Wildcard", "import DirectiveLibrary.*"),
    ] {
        // Keep emitted directives unchanged; qualified-reference support is separate.
        validate_official_files(
            &compiler,
            &format!("imports-emitted-{name}"),
            &emitted_directive_sources(import),
            None,
        );
    }
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn native_symbol_using_does_not_expose_other_functions() {
    let compiler = redscript_test_support::OfficialCompiler::from_env();
    for (name, directive, call, accepted) in [
        ("Exact", "First.Pick", "Pick(1)", true),
        ("Overload", "First.Pick", "Pick(1.0)", true),
        ("Unselected", "First.Pick", "Other(1)", false),
        ("Wildcard", "First", "Other(1)", true),
        ("Qualified", "First.Pick", "First.Pick(1)", false),
        ("ModuleOverlap", "First.Pick", "Extra(1)", true),
    ] {
        let mut sources = vec![String::from(
            "@module \"First\"\nfunction Pick(value: Int32) -> Int32 { return value; } function Pick(value: Float) -> Float { return value; } function Other(value: Int32) -> Int32 { return value; }",
        )];
        if name == "ModuleOverlap" {
            sources.push(String::from(
                "@module \"First.Pick\"\nfunction Extra(value: Int32) -> Int32 { return value; }",
            ));
        }
        let result = if name == "Overload" { "Float" } else { "Int32" };
        sources.push(format!(
            "@using \"{directive}\"\nfunction Run() -> {result} {{ return {call}; }}"
        ));
        validate_official_files(
            &compiler,
            &format!("imports-{name}"),
            &sources,
            if accepted {
                None
            } else if name == "Unselected" {
                Some("was not declared as Used")
            } else {
                Some("Invalid identifier in global scope 'First'")
            },
        );
    }
}
