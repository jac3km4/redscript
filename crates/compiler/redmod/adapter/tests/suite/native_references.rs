use redscript_community_ast::{SourceMap, Type};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, LoweredCompilationUnit, Symbols, TypeInterner, ir,
};
use redscript_redmod_adapter::{Error, SemanticError, parse_file_with_metadata, process_sources};

fn analyze(
    native: &str,
    community: &str,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("shared.reds", community);
    let file = sources.push_back("references.script", native);
    let (parsed, errors) = parse_file_with_metadata(native, file);
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
        .map(|error| error.code().to_string())
        .collect::<Vec<_>>();
    test(&unit, &symbols, &codes);
}

fn assert_const_write(reporter: &CompileErrorReporter<'_>, source: &str) {
    assert!(!reporter.reported().is_empty(), "{source}");
    assert!(
        reporter.reported().iter().all(|diagnostic| {
            matches!(diagnostic, Diagnostic::ConstParameterWrite(_))
                && diagnostic.to_string() == "cannot write through a const parameter"
        }),
        "{source}: {:?}",
        reporter.reported()
    );
}

#[test]
fn native_ref_is_a_script_reference_with_written_type_spans() {
    let source = "function Take(value: ref<String>) -> String { return value; }";
    let sources = SourceMap::new();
    let file = sources.push_back("reference.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let parsed = parsed.unwrap();
    let redscript_community_ast::Item::Function(function) = &parsed.module.items[0].0.item else {
        panic!("expected function")
    };
    let (Type::Named { name, args }, span) = function.params[0].0.typ.as_ref().unwrap() else {
        panic!("expected named type")
    };
    assert_eq!(*name, "script_ref");
    assert_eq!(
        &source[span.start as usize..span.end as usize],
        "ref<String>"
    );
    assert_eq!(
        &source[args[0].1.start as usize..args[0].1.end as usize],
        "String"
    );
}

#[test]
fn refs_accept_realistic_values_and_preserve_the_shared_reference_type() {
    analyze(
        r#"
        struct Value { var number: Int32; }
        class Object { public var number: Int32; }
        function Scalar(value: ref<Int32>) -> Int32 { value = 7; return value; }
        function Text(value: ref<String>) -> String { value = "changed"; return value; }
        function Structure(value: ref<Value>) -> Int32 { value.number = 7; return value.number; }
        function ObjectRef(value: ref<Object>) -> Int32 { return value.number; }
        function ArrayRef(value: ref<array<Int32>>) -> Int32 { value.PushBack(7); return value.Size(); }
        function WeakRef(value: ref<weak<Object>>) -> weak<Object> { return value; }
        function WeakMember(value: ref<weak<Object>>) -> Int32 { return value.number; }
        function Pass(value: ref<String>) -> String { return Text(value); }
        function Calls(object: Object, weakObject: weak<Object>) {
            var number: Int32;
            var text: String;
            var value: Value;
            var values: array<Int32>;
            Scalar(number); Text(text); Text("temporary"); Structure(value);
            ObjectRef(object); ObjectRef(NULL); ArrayRef(values); WeakRef(weakObject);
        }
    "#,
        "",
        |_, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for name in [
                "Scalar",
                "Text",
                "Structure",
                "ObjectRef",
                "ArrayRef",
                "WeakRef",
                "Pass",
            ] {
                let entry = symbols
                    .free_functions()
                    .find(|entry| entry.name().to_string() == name)
                    .unwrap();
                assert!(
                    entry.func().type_().params()[0]
                        .type_()
                        .to_string()
                        .starts_with("script_ref<"),
                    "{name}"
                );
            }
        },
    );
}

#[test]
fn ref_coercions_are_not_strong_object_wrappers() {
    analyze(
        "function Take(value: ref<String>) -> String { return value; } function Call() -> String { return Take(\"text\"); }",
        "",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let entry = symbols
                .free_functions()
                .find(|entry| entry.name().to_string() == "Call")
                .unwrap();
            let ir::Stmt::Return(Some(expr), _) = &unit.functions[entry.key()].block.stmts[0]
            else {
                panic!("expected return")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("expected call")
            };
            let ir::Expr::Call {
                call: conversion, ..
            } = &call.args()[0]
            else {
                panic!("expected reference conversion")
            };
            let ir::Call::FreeFunction { function, .. } = &**conversion else {
                panic!("expected intrinsic function")
            };
            assert_eq!(symbols[*function].intrinsic(), Some(ir::Intrinsic::AsRef));
        },
    );
}

#[test]
fn illegal_reference_contexts_have_specific_typed_errors() {
    for (source, expected) in [
        (
            "function Run() { var value: ref<String>; }",
            SemanticError::NativeReferenceLocal,
        ),
        (
            "function Run(value: ref<String>) -> ref<String> { return value; }",
            SemanticError::NativeReferenceReturn,
        ),
        (
            "function Run(value: ref<ref<String>>) {}",
            SemanticError::NestedNativeReference,
        ),
        (
            "function Run(value: array<ref<String>>) {}",
            SemanticError::NestedNativeReference,
        ),
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("reference.script", source);
        let (_, errors) = parse_file_with_metadata(source, file);
        assert!(
            errors
                .iter()
                .any(|error| matches!(error, Error::Semantic { kind, .. } if *kind == expected)),
            "{source}: {errors:?}"
        );
    }
}

#[test]
// Community strong class handles normalize to bare Object in the shared type model.
fn community_strong_and_script_references_remain_distinct() {
    analyze(
        "function Native(value: ref<Object>) {}",
        "class Object {} func Strong(value: ref<Object>) {} func Script(value: script_ref<Object>) {}",
        |_, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for (name, expected) in [
                ("Native", "script_ref<Object>"),
                ("Strong", "Object"),
                ("Script", "script_ref<Object>"),
            ] {
                let entry = symbols
                    .free_functions()
                    .find(|entry| entry.name().to_string() == name)
                    .unwrap();
                assert_eq!(
                    entry.func().type_().params()[0].type_().to_string(),
                    expected
                );
            }
        },
    );
}

#[test]
fn boolean_refs_are_conditions_but_object_and_weak_refs_are_not() {
    analyze(
        "function Test(value: ref<Bool>) { if (value) {} }",
        "",
        |_, _, codes| {
            assert!(codes.is_empty(), "{codes:?}");
        },
    );
    analyze(
        "class Object {} function Test(value: ref<Object>) { if (value) {} } function Weak(value: ref<weak<Object>>) { if (value) {} }",
        "",
        |_, _, codes| {
            assert_eq!(codes, ["TYPE_ERR", "TYPE_ERR"]);
        },
    );
}

#[test]
fn ref_fields_are_values_not_community_strong_handles() {
    analyze(
        "class Host { public var text: ref<String>; public function Read() -> String { return text; } public function Write() { text = \"changed\"; } }",
        "",
        |_, _, codes| {
            assert!(codes.is_empty(), "{codes:?}");
        },
    );
}

#[test]
fn out_reference_requires_a_reference_place_not_a_plain_value() {
    analyze(
        "function Take(out value: ref<String>) {} function Forward(value: ref<String>) { Take(value); }",
        "",
        |_, _, codes| {
            assert!(codes.is_empty(), "{codes:?}");
        },
    );
    analyze(
        "function Take(out value: ref<String>) {} function Plain() { var value: String; Take(value); }",
        "",
        |_, _, codes| {
            assert!(!codes.is_empty());
        },
    );
}

#[test]
fn assigning_a_ref_parameter_writes_through_a_dereferenced_place() {
    analyze(
        "function Write(value: ref<Int32>) { value = 7; }",
        "",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let entry = symbols
                .free_functions()
                .find(|entry| entry.name().to_string() == "Write")
                .unwrap();
            let ir::Stmt::Expr(expr) = &unit.functions[entry.key()].block.stmts[0] else {
                panic!("expected assignment")
            };
            let ir::Expr::Assign { place, .. } = &**expr else {
                panic!("expected assignment")
            };
            let ir::Expr::Call { call, .. } = &**place else {
                panic!("assignment must dereference the native ref place: {place:?}")
            };
            let ir::Call::FreeFunction { function, .. } = &**call else {
                panic!("expected dereference intrinsic")
            };
            assert_eq!(symbols[*function].intrinsic(), Some(ir::Intrinsic::Deref));
        },
    );
}

#[test]
fn const_refs_allow_forwarding_reads_and_indexed_element_writes() {
    analyze(
        r#"
        function Take(value: ref<String>) {}
        function Forward(const value: ref<String>) -> String { Take(value); return value; }
        function ConditionalForward(const value: ref<String>, other: ref<String>, flag: Bool) {
            Take(flag ? value : value);
            Take(flag ? value : other);
            Take(flag ? other : value);
        }
        function Elements(const value: ref<array<Int32>>) { value[0] = 7; }
        function TakeElement(out value: Int32) {}
        function ConditionalElements(const value: ref<array<Int32>>, other: ref<array<Int32>>, flag: Bool) {
            TakeElement((flag ? value : other)[0]);
        }
        function Size(const value: ref<array<Int32>>) -> Int32 { return value.Size(); }
        class Custom { public function PushBack(value: Int32) {} }
        function CustomMethod(const value: ref<Custom>) { value.PushBack(7); }
    "#,
        "",
        |_, _, codes| assert!(codes.is_empty(), "{codes:?}"),
    );
}

#[test]
fn native_const_relaxations_do_not_leak_into_community_sources() {
    analyze(
        "function Native(const value: ref<array<Int32>>) { value[0] = 7; }",
        "func Community(const value: [Int32]) { value[0] = 7; }",
        |_, _, codes| assert_eq!(codes, ["CONST_PARAMETER_WRITE"]),
    );
}

#[test]
fn ref_pointees_are_valid_out_value_places() {
    analyze(
        "function Take(out value: Int32) { value = 7; } function Forward(value: ref<Int32>) { Take(value); }",
        "",
        |_, _, codes| assert!(codes.is_empty(), "{codes:?}"),
    );
}

#[test]
fn resolved_const_checks_cover_inherited_dependency_fields_and_out_calls() {
    for source in [
        "function Write(const value: ref<Child>) { value.values.PushBack(7); }",
        "function Take(out value: Int32) {} function Write(const value: ref<Int32>) { Take(value); }",
        "function Take(out value: Int32) {} function Write(const value: ref<Int32>, flag: Bool) { Take(flag ? value : value); }",
        "function Take(out value: Int32) {} function Write(const value: ref<Int32>, other: ref<Int32>, flag: Bool) { Take(flag ? value : other); }",
        "function Take(out value: Int32) {} function Write(const value: ref<Int32>, other: ref<Int32>, flag: Bool) { Take(flag ? other : value); }",
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "shared.reds",
            "class Base { public let values: [Int32]; } class Child extends Base {}",
        );
        let file = sources.push_back("const.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let dependencies = sources
            .files()
            .filter(|(id, _)| *id != file)
            .map(|(id, source)| {
                redscript_community_parser::parse_module(source.source(), id)
                    .0
                    .unwrap()
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
        assert!(errors.is_empty(), "{source}: {errors:?}");
        assert_const_write(&reporter, source);
    }
}

#[test]
fn const_ref_parameters_do_not_allow_writes() {
    for source in [
        "function Write(const value: ref<String>) { value = \"changed\"; }",
        "struct Value { var number: Int32; } function Write(const value: ref<Value>) { value.number = 7; }",
        "function Write(const value: ref<array<Int32>>) { value.PushBack(7); }",
        "struct Value { var values: array<Int32>; } function Write(const value: ref<Value>) { value.values.PushBack(7); }",
        "class Value { public var number: Int32; } function Write(const value: ref<Value>) { value.number = 7; }",
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("const.script", source);
        sources.populate_boot_lib();
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        {
            let dependencies = sources
                .files()
                .filter(|(id, _)| *id != file)
                .map(|(id, source)| {
                    redscript_community_parser::parse_module(source.source(), id)
                        .0
                        .unwrap()
                })
                .collect::<Vec<_>>();
            let interner = TypeInterner::default();
            let mut reporter = CompileErrorReporter::default();
            let (_, _, resolved) = process_sources(
                parsed,
                dependencies,
                Symbols::with_default_types(),
                &mut reporter,
                &interner,
            );
            assert!(resolved.is_empty(), "{source}: {resolved:?}");
            assert_const_write(&reporter, source);
        }
    }
}
