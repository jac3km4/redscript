use std::env;

use redscript_community_ast::{Item, SourceMap};
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
fn duplicate_types_across_modules() {
    let sources = SourceMap::from_files([
        "tests/data/duplicate-types/first.reds",
        "tests/data/duplicate-types/duplicate.reds",
        "tests/data/duplicate-types/distinct.reds",
        "tests/data/duplicate-types/imports.reds",
    ])
    .unwrap();
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );

    assert!(matches!(
        reporter.reported(),
        [
            Diagnostic::NameRedefinition(_),
            Diagnostic::NameRedefinition(_)
        ]
    ));
    for name in [
        "First.SharedClass",
        "First.SharedStruct",
        "Second.SharedClass",
        "Second.SharedStruct",
    ] {
        let id = interner.intern(name);
        let aggregate = symbols[id].schema().as_aggregate().unwrap();
        assert_eq!(aggregate.methods().iter().count(), 1, "{name}");
        assert_eq!(unit.classes[&id].methods.len(), 1, "{name}");
    }
    insta::assert_snapshot!(DisplayDiagnostics(reporter.into_reported(), &sources));
}

#[test]
fn implicit_enum_overflow() {
    let sources = SourceMap::from_files(["tests/data/enum-resolution/overflow.reds"]).unwrap();
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
    let sources = SourceMap::from_files(["tests/data/passes/unused-locals.reds"]).unwrap();
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

fn declaration_diagnostics(source: &str) -> Vec<(String, String, String)> {
    let sources = SourceMap::new();
    let file = sources.push_back("declarations.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let file = sources.get(file).unwrap();
    reporter
        .reported()
        .iter()
        .map(|diagnostic| {
            (
                diagnostic.code().to_owned(),
                diagnostic.to_string(),
                file.span_contents(diagnostic.span()).to_owned(),
            )
        })
        .collect()
}

#[test]
fn unused_qualifiers_agree_in_number() {
    let diagnostics =
        declaration_diagnostics("final func Single() {}\nfinal static func Multiple() {}");
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert_eq!(
        diagnostics[0].1,
        "`final` qualifier has no effect on this item"
    );
    assert!(
        diagnostics[1]
            .1
            .ends_with("qualifiers have no effect on this item")
    );
    assert!(diagnostics[1].1.contains("`final`"));
    assert!(diagnostics[1].1.contains("`static`"));
}

#[test]
fn duplicate_enum_variants_include_context() {
    let diagnostics = declaration_diagnostics("enum Example { First = 1, First = 2, Other = 2 }");
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "DUP_VARIANT_NAME");
    assert_eq!(
        diagnostics[0].1,
        "enum `Example` has a duplicate variant name `First`"
    );
    assert_eq!(diagnostics[1].0, "DUP_VARIANT_VAL");
    assert_eq!(
        diagnostics[1].1,
        "variant `Example.Other` has duplicate value 2"
    );
}

#[test]
fn declaration_type_argument_errors_include_actual_counts() {
    let diagnostics = declaration_diagnostics(
        "class Generic<T> {}\nclass Fields {\nlet missing: Generic;\nlet extra: Generic<Int32, String>;\n}",
    );
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "INVALID_TYPE_ARG_COUNT");
    assert_eq!(diagnostics[1].0, "INVALID_TYPE_ARG_COUNT");
    insta::assert_snapshot!(
        "declaration_type_argument_counts",
        diagnostics
            .iter()
            .map(|(_, message, _)| message.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    );

    let diagnostics =
        declaration_diagnostics("class Pair<A, B> {}\nclass Fields { let field: Pair<Int32>; }");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "INVALID_TYPE_ARG_COUNT");
    insta::assert_snapshot!("declaration_type_argument_counts_plural", diagnostics[0].1);
}

#[test]
fn persistent_field_errors_include_container_types() {
    let diagnostics = declaration_diagnostics(
        "class Fields {\npersistent let direct: String;\npersistent let nested: array<array<String>>;\npersistent let allowed: array<Int32>;\n}",
    );
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "INVALID_PERSISTENT");
    assert_eq!(diagnostics[1].0, "INVALID_PERSISTENT");
    insta::assert_snapshot!(
        "persistent_field_container_types",
        diagnostics
            .iter()
            .map(|(_, message, _)| message.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    );
}

#[test]
fn unknown_annotation_targets_do_not_report_missing_methods() {
    let diagnostics = declaration_diagnostics(
        "@wrapMethod(Unknown)\ncb func Wrapped() {}\n@replaceMethod(Unknown)\ncb func Replaced() {}",
    );
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert!(
        diagnostics
            .iter()
            .all(|(code, _, span)| code == "UNRESOLVED_TYPE" && span == "Unknown")
    );

    let diagnostics =
        declaration_diagnostics("class Known {}\n@wrapMethod(Known)\nfunc Missing() {}");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "INVALID_ANN_USE");
    assert!(diagnostics[0].1.contains("could not find a method"));
}

#[test]
fn missing_implementations_are_sorted_by_signature() {
    let source = "abstract class Base {\nfunc Zebra();\nfunc Alpha(x: String);\nfunc Alpha(x: Int32);\n}\nclass Derived extends Base {}";
    for _ in 0..8 {
        let diagnostics = declaration_diagnostics(source);
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(diagnostics[0].0, "MISSING_IMPL");
        let signatures = diagnostics[0].1.lines().skip(1).collect::<Vec<_>>();
        assert_eq!(signatures.len(), 3);
        let mut sorted = signatures.clone();
        sorted.sort();
        assert_eq!(signatures, sorted);
    }
}

#[test]
fn final_override_highlights_method_name() {
    let diagnostics = declaration_diagnostics(
        "class Base { final func Final() {} }\nclass Derived extends Base { func Final() {} }",
    );
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "FINAL_FN_OVERRIDE");
    assert_eq!(diagnostics[0].2, "Final");
}

#[test]
fn unsatisfied_bound_uses_correct_code() {
    let diagnostics = declaration_diagnostics(
        "class Animal {}\nclass Chair {}\nclass Generic<T extends Animal> {}\nclass Invalid extends Generic<Chair> {}",
    );
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "UNSATISFIED_BOUND");
    assert_eq!(
        diagnostics[0].1,
        "type `Chair` does not satisfy expected bound `Animal` for type parameter `T`"
    );
}

#[test]
fn function_variance_explains_the_restriction() {
    let diagnostics = declaration_diagnostics("func Invalid<+T>() {}");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "NON_DATA_VARIANCE");
    assert_eq!(
        diagnostics[0].1,
        "function type parameters cannot have variance annotations"
    );
}

#[test]
fn unexpected_items_explain_their_scope() {
    let diagnostics = declaration_diagnostics("let field: Int32;");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "UNEXPECTED_ITEM");
    assert_eq!(
        diagnostics[0].1,
        "fields cannot be declared at module scope"
    );

    for (parent, scope) in [("class", "class"), ("struct", "struct")] {
        for nested in [
            "class Nested {}",
            "struct Nested {}",
            "enum Nested { Value = 0 }",
        ] {
            let diagnostics = declaration_diagnostics(&format!("{parent} Outer {{ {nested} }}"));
            assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
            assert_eq!(diagnostics[0].0, "UNEXPECTED_ITEM");
            assert_eq!(
                diagnostics[0].1,
                format!("nested type declarations are not allowed inside a {scope}")
            );
        }
        let diagnostics = declaration_diagnostics(&format!("{parent} Outer {{ import Missing }}"));
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(diagnostics[0].0, "UNEXPECTED_ITEM");
        assert_eq!(
            diagnostics[0].1,
            format!("imports cannot be declared inside a {scope}")
        );
    }
}

#[test]
fn inheritance_errors_explain_the_problem() {
    let diagnostics = declaration_diagnostics("class Invalid<T> extends T {}");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "INVALID_BASE");
    assert_eq!(diagnostics[0].1, "base type must be a class or struct type");

    let diagnostics =
        declaration_diagnostics("class First extends Second {}\nclass Second extends First {}");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].0, "INVALID_BASE");
    assert_eq!(diagnostics[0].1, "inheritance cycle detected");
}

struct DisplayDiagnostics<'ctx>(Vec<Diagnostic<'ctx>>, &'ctx SourceMap);

impl std::fmt::Display for DisplayDiagnostics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0
            .iter()
            .try_for_each(|diagnostic| writeln!(f, "{}", diagnostic.display(self.1).unwrap()))
    }
}
