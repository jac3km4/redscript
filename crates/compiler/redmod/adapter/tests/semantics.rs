use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner, ir};
use redscript_redmod_adapter::{
    Error, SemanticError, array_operations, parse_file_with_metadata, process_sources,
};

fn check(source: &str) -> (Vec<String>, usize) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("operators.reds", "native func OperatorAdd(a: Int32, b: Int32) -> Int32; native func OperatorLogicNot(a: Bool) -> Bool; native func OperatorLogicAnd(a: Bool, b: Bool) -> Bool;");
    let file = sources.push_back("test.script", source);
    let interner = TypeInterner::default();
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, file)| {
            let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _, errors) = process_sources(
        [parsed.unwrap()],
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let mut messages: Vec<_> = errors.into_iter().map(|e| e.to_string()).collect();
    messages.extend(reporter.into_reported().into_iter().map(|e| e.to_string()));
    (messages, unit.functions.len())
}

#[test]
fn signed_minima_and_native_enum_values_survive_shared_lowering() {
    let sources = SourceMap::new();
    let source = "enum Choice { Minimum = -2147483648, Maximum = 2147483647 } function MinInt(): Int32 { return -2147483648; } function MinLong(): Int64 { return -9223372036854775808l; }";
    let file = sources.push_back("minima.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols, errors) = process_sources(
        parsed,
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    let variants = symbols[interner.intern("Choice")]
        .schema()
        .as_enum()
        .unwrap()
        .variants()
        .collect::<Vec<_>>();
    assert_eq!(
        variants,
        [
            ("Minimum", i64::from(i32::MIN)),
            ("Maximum", i64::from(i32::MAX))
        ]
    );
    for entry in symbols.free_functions() {
        let ir::Stmt::Return(Some(expr), _) = &unit.functions[entry.key()].block.stmts[0] else {
            panic!("expected return")
        };
        match entry.name().to_string().as_str() {
            "MinInt" => assert!(matches!(
                &**expr,
                ir::Expr::Const(ir::Const::I32(i32::MIN), _)
            )),
            "MinLong" => assert!(matches!(
                &**expr,
                ir::Expr::Const(ir::Const::I64(i64::MIN), _)
            )),
            name => panic!("unexpected function {name}"),
        }
    }
}

#[test]
fn array_descriptors_cover_the_supported_members_and_boot_intrinsics() {
    let operations = array_operations();
    insta::assert_snapshot!(
        operations
            .iter()
            .map(|operation| format!("{} -> {:?}", operation.name, operation.intrinsic))
            .collect::<Vec<_>>()
            .join("\n"),
        @"
        Size -> ArraySize
        PushBack -> ArrayPush
        PopBack -> ArrayPop
        Clear -> ArrayClear
        Resize -> ArrayResize
        Contains -> ArrayContains
        FindFirst -> ArrayFindFirst
        FindLast -> ArrayFindLast
        Insert -> ArrayInsert
        Remove -> ArrayRemove
        Erase -> ArrayErase
        Last -> ArrayLast
        "
    );

    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let dependencies = sources.files().map(|(id, file)| {
        let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
        assert!(errors.is_empty(), "{errors:?}");
        module.unwrap()
    });
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, symbols, errors) = process_sources(
        [],
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.into_reported().is_empty());
    for operation in operations {
        assert!(
            symbols.free_functions().any(|entry| {
                entry.func().intrinsic() == Some(operation.intrinsic)
                    && entry.name().as_single_component() == Some(operation.intrinsic.name())
            }),
            "missing boot intrinsic for {operation:?}",
        );
    }
}

#[test]
fn unsupported_capabilities_are_rejected_even_in_shared_ast_inputs() {
    for source in [
        "func Test() { let f: (Int32) -> Int32 = (x: Int32) -> x; }",
        "@mixedRef() class Object {}",
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("input.reds", source);
        let (module, errors) = redscript_community_parser::parse_module(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let errors = redscript_redmod_adapter::validate_module(&module.unwrap());
        assert!(!errors.is_empty(), "{source}");
    }
}

#[test]
fn semantic_errors_expose_typed_kinds_and_preserve_diagnostics() {
    for (source, expected, message) in [
        (
            "class Generic<T> {}",
            SemanticError::UnsupportedGenericType,
            "custom generic types are not supported",
        ),
        (
            "func Generic<T>(x: T) {}",
            SemanticError::UnsupportedGenericFunction,
            "custom generic functions are not supported",
        ),
        (
            "func Test(x: ref<Object>) {}",
            SemanticError::UnsupportedType,
            "unsupported REDmod reference or generic type",
        ),
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("input.reds", source);
        let (module, errors) = redscript_community_parser::parse_module(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let errors = redscript_redmod_adapter::validate_module(&module.unwrap());
        assert_eq!(errors.len(), 1, "{errors:?}");
        let error = &errors[0];
        assert!(matches!(error, Error::Semantic { kind, .. } if *kind == expected));
        assert_eq!(error.span().file, file);
        assert_eq!(error.code(), "REDMOD_SEMANTICS");
        assert_eq!(error.to_string(), message);
    }
}

#[test]
fn explicit_community_intrinsics_and_temporary_arrays_are_rejected() {
    let (errors, _) =
        check("function Test() { let a: array<Int32>; let n: Int32 = ArraySize(a); }");
    assert!(!errors.is_empty(), "{errors:?}");
    let (errors, _) = check(
        "function Make() -> array<Int32> { let a: array<Int32>; return a; } function Test() { let n: Int32 = Make().Size(); }",
    );
    assert!(errors.iter().any(|e| e.contains("temporary")), "{errors:?}");
}

#[test]
fn events_have_void_signatures_without_override_requirements() {
    let (errors, _) = check(
        "class Base { event OnAttach() {} } class Child extends Base { event OnAttach() { return; } }",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let (errors, _) = check("class EventOwner { event OnAttach() { return true; } }");
    assert!(!errors.is_empty(), "event return must not infer Bool");
}

#[test]
fn bare_class_values_do_not_acquire_a_strong_wrapper() {
    let (errors, _) = check(
        "class Object {} function Identity(value: Object) -> Object { let local: Object = value; return local; } function Weak(value: Object) -> wref<Object> { return value; }",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn non_void_community_callbacks_are_not_silently_adopted() {
    let sources = SourceMap::new();
    let dependency = "class Base { protected cb func OnAttach() -> Bool { return true; } }";
    let dep_id = sources.push_back("base.reds", dependency);
    let (dependency, diagnostics) = redscript_community_parser::parse_module(dependency, dep_id);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let source = "class Child extends Base { event OnAttach() {} }";
    let file = sources.push_back("child.script", source);
    let (parsed, diagnostics) = parse_file_with_metadata(source, file);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        parsed,
        dependency,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(
        errors
            .iter()
            .any(|error| error.to_string().contains("non-Void")),
        "{errors:?}"
    );
}

#[test]
fn conditional_overloads_are_not_counted_as_versions() {
    let (errors, _) = check(
        r#"
      @if(!ModuleExists("A")) function Pick(x: Int32) {}
      @if(!ModuleExists("B")) function Pick(x: Bool) {}
      @if(!ModuleExists("C")) function Pick(x: Float) {}
    "#,
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn conditional_add_methods_allow_only_one_version() {
    let (errors, _) = check(
        r#"
      import class Native {}
      @if(!ModuleExists("A")) @addMethod(Native) function Added() {}
      @if(!ModuleExists("B")) @addMethod(Native) function Added() {}
    "#,
    );
    assert!(
        errors.iter().any(|error| error.contains("at most 1")),
        "{errors:?}"
    );
}

#[test]
fn module_conditions_use_dependencies_and_first_active_source_order() {
    for reverse in [false, true] {
        let sources = SourceMap::new();
        let first = r#"@if(ModuleExists("Available")) function Selected() -> Int32 { return 1; }"#;
        let second = r#"@if(!ModuleExists("Missing")) function Selected() -> Int32 { return 2; }"#;
        let first_id = sources.push_back("first.script", first);
        let second_id = sources.push_back("second.script", second);
        let dependency = "module Available\n";
        let dep_id = sources.push_back("dependency.reds", dependency);
        let (dependency, errors) = redscript_community_parser::parse_module(dependency, dep_id);
        assert!(errors.is_empty(), "{errors:?}");
        let (first, errors) = parse_file_with_metadata(first, first_id);
        assert!(errors.is_empty(), "{errors:?}");
        let (second, errors) = parse_file_with_metadata(second, second_id);
        assert!(errors.is_empty(), "{errors:?}");
        let mut modules = vec![first.unwrap(), second.unwrap()];
        if reverse {
            modules.reverse();
        }
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, _, errors) = process_sources(
            modules,
            dependency,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(reporter.into_reported().is_empty());
        let files: Vec<_> = unit
            .functions
            .values()
            .map(|function| function.span.file)
            .collect();
        assert_eq!(files, [if reverse { second_id } else { first_id }]);
    }
}

#[test]
fn conditional_polarity_error_points_to_second_positive_version() {
    let source = r#"
      @if(ModuleExists("A")) function Choice() {}
      @if(!ModuleExists("B")) function Choice() {}
      @if(ModuleExists("C")) function Choice() {}
    "#;
    let sources = SourceMap::new();
    let file = sources.push_back("test.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        parsed,
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let error = errors
        .iter()
        .find(|error| {
            matches!(
                error,
                redscript_redmod_adapter::Error::Condition {
                    kind: redscript_redmod_adapter::ConditionError::MultiplePositiveVersions,
                    ..
                }
            )
        })
        .unwrap();
    assert_eq!(error.span().file, file);
    assert_eq!(error.span().start as usize, source.rfind("Choice").unwrap());
}

#[test]
fn an_unsupported_file_does_not_block_other_files() {
    let sources = SourceMap::new();
    let good = "function Good() {}";
    let bad = "@prefix(Native) function Bad() {}";
    let good_id = sources.push_back("good.script", good);
    let bad_id = sources.push_back("bad.script", bad);
    let (good, _) = parse_file_with_metadata(good, good_id);
    let (bad, _) = parse_file_with_metadata(bad, bad_id);
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _, errors) = process_sources(
        good.into_iter().chain(bad),
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(
        !errors.is_empty(),
        "unsupported semantics must remain diagnosed"
    );
    assert!(
        unit.functions
            .values()
            .any(|function| function.span.file == good_id)
    );
    assert!(reporter.into_reported().is_empty());
}

#[test]
fn persistent_checks_reach_nested_reads_and_places() {
    let (errors, _) = check(
        "class State { public persistent let count: Int32; public function Get() -> Int32 { return this.count; } }
         function Take(value: Int32) {}
         function Test(state: State) -> Int32 {
           let values: array<Int32> = [state.count];
           if (true) { Take(state.count); }
           while (false) { state.count = 1; }
           let value: Int32 = true ? state.count : state.count;
           switch (state.count) {
             case 1: Take(state.count);
             default: Take(state.count);
           }
           Take(values[state.count]);
           return state.count;
         }",
    );
    assert_eq!(errors.len(), 10, "{errors:?}");
    assert!(
        errors
            .iter()
            .all(|error| error.contains("persistent fields")),
        "{errors:?}"
    );
}

#[test]
fn persistent_access_uses_declaring_class_ownership_not_receiver_spelling() {
    let (errors, _) = check(
        "class State {
           public persistent let count: Int32;
           public function Read(other: State) -> Int32 { return other.count; }
         }
         class Child extends State {
           public function ReadInherited() -> Int32 { return this.count; }
         }",
    );
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("persistent fields"), "{errors:?}");
}

#[test]
fn hoisted_binding_and_initializer_visibility() {
    let (errors, count) =
        check("function Test() -> Int32 { x = 2; if (true) { let x: Int32 = x + 1; } return x; }");
    assert!(errors.is_empty(), "{errors:?}");
    assert!(count > 0);
}

#[test]
fn duplicate_names_are_function_wide() {
    let source = "function Test() { if (true) { let x: Int32; } let x: Int32; }";
    let sources = SourceMap::new();
    let file = sources.push_back("test.script", source);
    let (_, errors) = parse_file_with_metadata(source, file);
    assert!(
        errors.iter().any(|e| e.to_string().contains("duplicate")),
        "{errors:?}"
    );
    assert_eq!(errors[0].span().file, file);
}

#[test]
fn unsupported_annotations_are_not_silently_ignored() {
    for annotation in [
        "mixedRef", "neverRef", "prefix", "postfix", "editable", "unknown",
    ] {
        let source = format!("@{annotation} class Test {{}}");
        let sources = SourceMap::new();
        let file = sources.push_back("test.script", source.clone());
        let (_, errors) = parse_file_with_metadata(&source, file);
        assert!(!errors.is_empty(), "{annotation}");
    }
}

#[test]
fn arrays_are_type_checked_and_user_methods_keep_identity() {
    let (errors, _) = check(
        "class Other { public function Size() -> Int32 { return 7; } } function Test(o: Other) { let a: array<Int32>; a.PushBack(1); let n: Int32 = a.Size(); let m: Int32 = o.Size(); }",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let (errors, _) = check("function Test() { let a: array<Int32>; a.PushBack(true); }");
    assert!(!errors.is_empty());
}

#[test]
fn object_and_weak_truthiness_but_not_integer_truthiness() {
    let (errors, _) = check(
        "class Object {} function Test(o: Object, w: wref<Object>) { if (o && !w) {} while (o) { break; } }",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let (errors, _) = check("function Test(n: Int32) { if (n) {} }");
    assert!(!errors.is_empty());
}

#[test]
fn override_requires_a_resolved_matching_signature() {
    let (errors, _) = check(
        "class Base { public function F(x: Int32) {} } class Child extends Base { public function F(x: Int32) {} }",
    );
    assert!(
        errors.iter().any(|e| e.contains("requires override")),
        "{errors:?}"
    );
    let (errors, _) = check(
        "class Base { public function F(x: Int32) {} } class Child extends Base { public override function F(x: Int32) {} }",
    );
    assert!(errors.is_empty(), "{errors:?}");
    let (errors, _) = check(
        "class Base { public function F(x: Int32) {} } class Child extends Base { public override function F(x: Bool) {} }",
    );
    assert!(
        errors
            .iter()
            .any(|e| e.contains("no matching base signature")),
        "{errors:?}"
    );
}

#[test]
fn array_mapping_requires_a_real_intrinsic_symbol() {
    let sources = SourceMap::new();
    let source = "function Test() { let a: array<Int32>; let n: Int32 = a.Size(); }";
    let file = sources.push_back("test.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let fake = "func ArraySize(a: array<Int32>) -> Int32 { return 1; }";
    let dep = sources.push_back("fake.reds", fake);
    let (dependency, errors) = redscript_community_parser::parse_module(fake, dep);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (_, _, errors) = process_sources(
        [parsed.unwrap()],
        [dependency.unwrap()],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    let errors = reporter.into_reported();
    assert!(
        errors
            .iter()
            .any(|error| error.to_string().contains("ArraySize")),
        "{errors:?}"
    );
}

#[test]
fn target_extensions_and_patch_ownership_are_checked() {
    let (errors, _) = check("importonly class Native {} @addField(Native) let added: Int32;");
    assert!(
        errors.iter().any(|e| e.contains("importonly")),
        "{errors:?}"
    );
    let (errors, _) =
        check("class Local { public function F() {} } @wrapMethod(Local) function F() { F(); }");
    assert!(
        errors
            .iter()
            .any(|e| e.contains("user-defined symbol") || e.contains("mod-defined")),
        "{errors:?}"
    );
}

#[test]
fn policy_is_per_source_and_dependency_order_independent() {
    for reverse in [false, true] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let source = "function Target() -> Int32 { return shared(); let x: Int32; }";
        let file = sources.push_back("target.script", source);
        sources.push_back(
            "shared.reds",
            "func shared() -> Int32 { if true { let x: Int32 = 1; } let x: Int32 = 2; return x; }",
        );
        let mut dependencies: Vec<_> = sources
            .files()
            .filter(|(id, _)| *id != file)
            .map(|(id, file)| {
                let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
                assert!(errors.is_empty(), "{errors:?}");
                module.unwrap()
            })
            .collect();
        if reverse {
            dependencies.reverse();
        }
        let (module, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            [module.unwrap()],
            dependencies,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        let diagnostics = reporter.into_reported();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
    }
}

#[test]
fn hoisted_defaults_do_not_reset_on_loop_entry() {
    let sources = SourceMap::new();
    let source = "function Test() { while (true) { let x: Int32; x = 1; break; } }";
    let file = sources.push_back("test.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, _, errors) = process_sources(
        [parsed.unwrap()],
        [],
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    assert!(errors.is_empty(), "{errors:?}");
    assert!(reporter.into_reported().is_empty());
    let function = unit.functions.values().next().unwrap();
    assert!(matches!(
        function.block.stmts[0],
        ir::Stmt::InitDefault { .. }
    ));
}
