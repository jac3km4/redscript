use redscript_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, LoweredCompilationUnit, Symbols, Type, TypeInterner,
    infer_from_sources, ir, predef,
};

fn infer<'ctx>(
    sources: &'ctx SourceMap,
    interner: &'ctx TypeInterner,
) -> (
    LoweredCompilationUnit<'ctx>,
    Symbols<'ctx>,
    Vec<Diagnostic<'ctx>>,
) {
    sources.populate_boot_lib();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        sources,
        Symbols::with_default_types(),
        &mut reporter,
        interner,
    );
    (unit, symbols, reporter.into_reported())
}

fn diagnostic_messages(source: &str) -> Vec<(String, String)> {
    let sources = SourceMap::new();
    sources.push_back("diagnostics.reds", source);
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = infer(&sources, &interner);
    diagnostics
        .iter()
        .map(|diagnostic| (diagnostic.code().to_owned(), diagnostic.to_string()))
        .collect()
}

#[test]
fn bare_return_reports_void_as_found() {
    let diagnostics = diagnostic_messages("func Test() -> Int32 { return; }");
    assert_eq!(
        diagnostics,
        [(
            "TYPE_ERR".into(),
            "type mismatch: found `Void` when expected `Int32`".into()
        )]
    );
}

#[test]
fn local_type_diagnostics_include_actual_counts() {
    for (source, expected) in [
        (
            "func Take<T>() {} func Test() { Take<Int32, Int32>(); }",
            "invalid number of type arguments: expected 1, found 2",
        ),
        (
            "func Test<T>() { let value: T<Int32>; }",
            "invalid number of type arguments: expected 0, found 1",
        ),
        (
            "class Box<T> {} func Test() { let value = new Box<Int32, Int32>(); }",
            "invalid number of type arguments: expected 1, found 2",
        ),
        (
            "struct Box<T> {} func Test() { Box<Int32, Int32>(); }",
            "invalid number of type arguments: expected 1, found 2",
        ),
        (
            "class Box<T> {} func Test() { let value: Box<Int32, Int32>; }",
            "invalid number of type arguments: expected 1, found 2",
        ),
        (
            "func Test() { Cast<Int32, Int32>(1); }",
            "invalid number of type arguments: expected 1, found 2",
        ),
        (
            "func Test() { let value: [Int32; 17]; }",
            "unsupported static array size: found 17 elements, maximum is 16",
        ),
        (
            "func Test() { let value: (Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32) -> Void; }",
            "function arity 9 exceeds maximum 8",
        ),
        (
            "func Test() { let value = (a, b, c, d, e, f, g, h, i) -> {}; }",
            "function arity 9 exceeds maximum 8",
        ),
        (
            "func Test() { let f = () -> {}; f(1, 2, 3, 4, 5, 6, 7, 8, 9); }",
            "function arity 9 exceeds maximum 8",
        ),
    ] {
        let diagnostics = diagnostic_messages(source);
        assert_eq!(diagnostics.len(), 1, "{source}: {diagnostics:?}");
        assert_eq!(diagnostics[0].1, expected, "{source}");
    }
}

#[test]
fn literal_errors_do_not_cascade_into_type_mismatches() {
    for body in [
        "let value: Int8 = 128;",
        "let value: Uint8 = -1;",
        "let value: CName = \"text\";",
        "let value: ResRef = n\"text\";",
        "let value: TweakDBID = r\"text\";",
        "let values: [Int8] = [128];",
        "let value = Holder(128);",
        "return 128;",
    ] {
        let diagnostics = diagnostic_messages(&format!(
            "struct Holder {{ let value: Int8; }} func Test() -> Int8 {{ {body} }}"
        ));
        assert_eq!(diagnostics.len(), 1, "{body}: {diagnostics:?}");
        assert!(
            matches!(
                diagnostics[0].0.as_str(),
                "LIT_OUT_OF_RANGE" | "WRONG_STRING_LIT"
            ),
            "{body}: {diagnostics:?}"
        );
    }
    let diagnostics =
        diagnostic_messages("func Test() { let value: Int8 = 128; let other: CName = \"text\"; }");
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
}

#[test]
fn never_ref_errors_identify_wrapper_and_target() {
    for wrapper in ["ref", "wref"] {
        let diagnostics = diagnostic_messages(&format!(
            "@neverRef() class Value<T> {{}} func Test() {{ let value: {wrapper}<Value<Int32>>; }}"
        ));
        assert_eq!(
            diagnostics,
            [(
                "REF_ON_NEVER_REF".into(),
                format!(
                    "cannot wrap `Value<Int32>` with `{wrapper}`: this type does not support references"
                ),
            )]
        );
    }
}

#[test]
fn nested_type_errors_show_outer_types_before_inner_cause() {
    for (name, variance, found, expected) in [
        ("covariant", "+", "Animal", "Dog"),
        ("contravariant", "-", "Dog", "Animal"),
        ("invariant", "", "Dog", "Animal"),
    ] {
        let diagnostics = diagnostic_messages(&format!(
            "class Animal {{}} class Dog extends Animal {{}} class Container<{variance}T> {{}} func Test(value: Container<{found}>) {{ let other: Container<{expected}> = value; }}"
        ));
        assert_eq!(diagnostics.len(), 1, "{name}: {diagnostics:?}");
        assert_eq!(diagnostics[0].0, "TYPE_ERR");
        insta::assert_snapshot!(format!("nested_type_errors_{name}"), diagnostics[0].1);
    }
}

#[test]
fn numeric_range_diagnostics_are_inclusive() {
    for (typ, value, range) in [("Int8", "128", "-128..=127"), ("Uint8", "-1", "0..=255")] {
        let diagnostics =
            diagnostic_messages(&format!("func Test() {{ let value: {typ} = {value}; }}"));
        assert_eq!(
            diagnostics,
            [(
                "LIT_OUT_OF_RANGE".into(),
                format!(
                    "this literal is out of range for `{typ}`; provide a value in the inclusive range `{range}`"
                ),
            )]
        );
    }
}

#[test]
fn reference_errors_show_expected_and_found_types() {
    let diagnostics = diagnostic_messages(
        "@mixedRef() class Mixed {} func Test() { let value: ref<Mixed> = Mixed(); let other: Mixed = new Mixed(); }",
    );
    assert_eq!(
        diagnostics,
        [
            (
                "UNEXPECTED_NON_REF".into(),
                "expected `ref<Mixed>`, found `Mixed`: a `ref` is required here".into()
            ),
            (
                "UNEXPECTED_REF".into(),
                "expected `Mixed`, found `ref<Mixed>`: a `ref` is not expected here".into()
            ),
        ]
    );
}

#[test]
fn cast_diagnostics_identify_source_and_target() {
    let diagnostics = diagnostic_messages(
        "class CastSource {} class CastTarget {} func Test(value: CastSource) { value as CastTarget; value as Int32; }",
    );
    assert_eq!(
        diagnostics,
        [
            (
                "IMPOSSIBLE_DYN_CAST".into(),
                "cannot cast from `CastSource` to `CastTarget`: the types are not related".into()
            ),
            (
                "INVALID_DYN_CAST".into(),
                "cannot cast from `CastSource` to `Int32` with `as`: the target type must be a class".into()
            ),
        ]
    );
    let diagnostics = diagnostic_messages("func Test() { Cast(1); }");
    assert_eq!(diagnostics, [(
        "INVALID_STATIC_CAST".into(),
        "cannot infer the target type of this cast; specify it with `Cast<T>(value)` or a type annotation".into(),
    )]);
}

#[test]
fn static_method_advice_uses_declaring_type_qualifier() {
    let diagnostics = diagnostic_messages(
        "class Base { public static func Method() {} } class Child extends Base {} func Test() { Child.Method(); }",
    );
    assert_eq!(diagnostics, [(
        "CALLING_BASE_STATIC_METHOD".into(),
        "static method `Method` is declared on `Base`; qualify the call with `Base.Method(...)` instead of its subclass".into(),
    )]);
}

#[test]
fn inference_errors_do_not_assume_member_lookup() {
    let diagnostics = diagnostic_messages("func Test() { let f = (value) -> value.member; }");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(
        diagnostics[0].1,
        "cannot infer the type of this expression; consider adding type annotations"
    );
}

#[test]
fn explicit_type_arguments_cannot_widen_to_fit_values() {
    let sources = SourceMap::new();
    sources.push_back(
        "explicit-type-arguments.reds",
        r#"
        class Animal {}
        class Dog extends Animal {}
        func Take<T>(value: T) {}
        func Test(animal: Animal) { Take<Dog>(animal); }
        "#,
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = infer(&sources, &interner);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "TYPE_ERR");
    let message = diagnostics[0].to_string();
    assert!(
        message.contains("Animal") && message.contains("Dog"),
        "{message}"
    );
}

#[test]
fn explicit_type_arguments_accept_subtypes_and_check_declared_bounds() {
    for (argument, expected_errors) in [("Animal", 0), ("Dog", 0), ("Chair", 1)] {
        let sources = SourceMap::new();
        sources.push_back(
            "explicit-type-bounds.reds",
            format!(
                r#"
                class Animal {{}}
                class Dog extends Animal {{}}
                class Chair {{}}
                func Take<T extends Animal>(value: T) {{}}
                func Test(value: Dog, chair: Chair) {{ Take<{argument}>({value}); }}
                "#,
                value = if argument == "Chair" {
                    "chair"
                } else {
                    "value"
                },
            ),
        );
        let interner = TypeInterner::default();
        let (_, _, diagnostics) = infer(&sources, &interner);
        assert_eq!(
            diagnostics.len(),
            expected_errors,
            "{argument}: {diagnostics:?}"
        );
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic.code() == "TYPE_ERR")
        );
    }
}

#[test]
fn explicit_type_arguments_can_reference_enclosing_type_parameters() {
    let sources = SourceMap::new();
    sources.push_back(
        "explicit-context-type.reds",
        r#"
        func Identity<T>(value: T) -> T { return value; }
        func Outer<U>(value: U) -> U { return Identity<U>(value); }
        "#,
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = infer(&sources, &interner);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn array_elements_receive_contextual_type_hints() {
    let sources = SourceMap::new();
    sources.push_back(
        "array-hints.reds",
        r#"
        func Test() {
          let numbers: [Int64] = [1, -2];
          let nested: [[Int16]] = [[1], [2]];
          let callbacks: [(Int64) -> Int64] = [(x) -> x];
        }
        "#,
    );
    let interner = TypeInterner::default();
    let (unit, symbols, diagnostics) = infer(&sources, &interner);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Test")
        .unwrap();
    let function = &unit.functions[entry.key()];
    let arrays: Vec<_> = function
        .block
        .stmts
        .iter()
        .filter_map(|stmt| {
            if let ir::Stmt::InitArray { elements, .. } = stmt {
                Some(elements)
            } else {
                None
            }
        })
        .collect();
    assert_eq!(arrays.len(), 5);
    assert!(matches!(
        &arrays[0][0],
        ir::Expr::Const(ir::Const::I64(1), _)
    ));
    assert!(matches!(
        &arrays[0][1],
        ir::Expr::Const(ir::Const::I64(-2), _)
    ));
    assert!(matches!(
        &arrays[1][0],
        ir::Expr::Const(ir::Const::I16(1), _)
    ));
    assert!(matches!(
        &arrays[2][0],
        ir::Expr::Const(ir::Const::I16(2), _)
    ));
    let ir::Expr::NewClosure { closure, .. } = &arrays[4][0] else {
        panic!("expected callback closure");
    };
    let typ = closure.typ.coalesced(&symbols).unwrap();
    assert_eq!(
        typ.args(),
        &[Type::nullary(predef::INT64), Type::nullary(predef::INT64)]
    );
}

#[test]
fn body_local_type_applications_check_declared_bounds() {
    for (type_params, body, expected_errors) in [
        ("", "let value: Store<Dog>;", 0),
        ("", "let value: Store<Chair>;", 1),
        ("", "let value: [Store<Chair>];", 1),
        ("<T extends Animal>", "let value: Store<T>;", 0),
        ("<T>", "let value: Store<T>;", 1),
        ("", "let value: Pair<Animal, Dog>;", 0),
        ("", "let value: Pair<Dog, Animal>;", 1),
        ("<T, U extends T>", "let value: Pair<T, U>;", 0),
        (
            "<T extends Animal, U extends Animal>",
            "let value: Pair<T, U>;",
            1,
        ),
        ("", "let value = new Store<Dog>();", 0),
        ("", "let value = new Store<Chair>();", 1),
        ("<T extends Animal>", "let value = new Store<T>();", 0),
        ("", "let value = Holder<Dog>();", 0),
        ("", "let value = Holder<Chair>();", 1),
        ("", "let value = (x: Store<Chair>) -> {};", 1),
    ] {
        let sources = SourceMap::new();
        sources.push_back(
            "local-type-bounds.reds",
            format!(
                r#"
                class Animal {{}}
                class Dog extends Animal {{}}
                class Chair {{}}
                class Store<T extends Animal> {{}}
                class Pair<T, U extends T> {{}}
                struct Holder<T extends Animal> {{}}
                func Test{type_params}() {{ {body} }}
                "#,
            ),
        );
        let interner = TypeInterner::default();
        let (_, _, diagnostics) = infer(&sources, &interner);
        assert_eq!(
            diagnostics.len(),
            expected_errors,
            "{type_params}: {body}: {diagnostics:?}"
        );
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic.code() == "TYPE_ERR")
        );
    }
}

#[test]
fn body_local_bounds_respect_nested_variance_and_inheritance() {
    for (type_params, body, expected_errors) in [
        ("", "let value: NeedsProducer<Producer<Dog>>;", 0),
        ("", "let value: NeedsProducer<DerivedProducer>;", 0),
        ("", "let value: NeedsProducer<Producer<Chair>>;", 1),
        (
            "<T extends Animal>",
            "let value: NeedsProducer<Producer<T>>;",
            0,
        ),
        ("", "let value: NeedsConsumer<Consumer<Animal>>;", 0),
        ("", "let value: NeedsConsumer<Consumer<Chair>>;", 1),
        (
            "<T extends Animal>",
            "let value: NeedsConsumer<Consumer<T>>;",
            1,
        ),
        ("", "let value: NeedsArray<[Animal]>;", 0),
        ("", "let value: NeedsArray<[Dog]>;", 1),
        ("<T extends Animal>", "let value: NeedsArray<[T]>;", 1),
    ] {
        let sources = SourceMap::new();
        sources.push_back(
            "nested-type-bounds.reds",
            format!(
                r#"
                class Animal {{}}
                class Dog extends Animal {{}}
                class Chair {{}}
                class Producer<+T> {{}}
                class DerivedProducer extends Producer<Dog> {{}}
                class Consumer<-T> {{}}
                class NeedsProducer<T extends Producer<Animal>> {{}}
                class NeedsConsumer<T extends Consumer<Dog>> {{}}
                class NeedsArray<T extends [Animal]> {{}}
                func Test{type_params}() {{ {body} }}
                "#,
            ),
        );
        let interner = TypeInterner::default();
        let (_, _, diagnostics) = infer(&sources, &interner);
        assert_eq!(
            diagnostics.len(),
            expected_errors,
            "{type_params}: {body}: {diagnostics:?}"
        );
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic.code() == "TYPE_ERR")
        );
    }
}

#[test]
fn block_lambdas_infer_return_types_after_lowering_the_body() {
    let sources = SourceMap::new();
    sources.push_back(
        "block-lambdas.reds",
        r#"
        func Test() {
          let returning = (x: Int32) -> { return x; };
          let empty = () -> {};
          let bare = () -> { return; };
          let nested = () -> { let inner = () -> { return 1; }; };
        }
        "#,
    );
    let interner = TypeInterner::default();
    let (unit, symbols, diagnostics) = infer(&sources, &interner);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == "Test")
        .unwrap();
    let function = &unit.functions[entry.key()];
    for (name, expected) in [
        ("returning", predef::INT32),
        ("empty", predef::VOID),
        ("bare", predef::VOID),
        ("nested", predef::VOID),
    ] {
        let local = function
            .locals
            .iter()
            .find(|local| local.name == Some(name))
            .unwrap();
        let typ = local.typ.coalesce(&symbols).unwrap();
        let Type::Data(typ) = typ else {
            panic!("expected function type for {name}");
        };
        assert!(typ.id().is_fn());
        assert_eq!(typ.args().last(), Some(&Type::nullary(expected)), "{name}");
    }
}
