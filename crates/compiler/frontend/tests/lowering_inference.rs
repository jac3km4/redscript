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
