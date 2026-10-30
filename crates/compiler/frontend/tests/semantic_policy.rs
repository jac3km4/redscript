use redscript_community_ast::{self as ast, SourceMap, Span};
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, Evaluator, LowerError, LoweredCompilationUnit,
    LoweredFunction, PolyType, SemanticPolicy, Symbols, TypeId, TypeInterner, ir, parse_files,
    predef, process_sources, process_sources_with_policy,
};

fn analyze<'ctx>(
    sources: &'ctx SourceMap,
    interner: &'ctx TypeInterner,
    policies: &[(ast::FileId, SemanticPolicy)],
    reverse: bool,
) -> (
    LoweredCompilationUnit<'ctx>,
    Symbols<'ctx>,
    Vec<Diagnostic<'ctx>>,
) {
    let mut reporter = CompileErrorReporter::default();
    let mut modules = parse_files(sources, &mut reporter);
    assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    if reverse {
        modules.reverse();
    }
    let evaluator = Evaluator::from_modules(&modules);
    let (unit, symbols) = process_sources_with_policy(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        interner,
        policies,
    );
    (unit, symbols, reporter.into_reported())
}

fn function<'a, 'ctx>(
    unit: &'a LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    name: &str,
) -> &'a LoweredFunction<'ctx> {
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == name)
        .unwrap();
    &unit.functions[entry.key()]
}

fn variant_condition<'ctx>(typ: &PolyType<'ctx>, symbols: &Symbols<'ctx>) -> Option<ir::Intrinsic> {
    (typ.upper_bound(symbols)?.id() == predef::VARIANT).then_some(ir::Intrinsic::VariantIsDefined)
}

fn array_member(id: TypeId<'_>, member: &str) -> Option<(ir::Intrinsic, bool)> {
    (id == predef::ARRAY && member == "count").then_some((ir::Intrinsic::ArraySize, true))
}

fn reject_string<'ctx>(
    id: TypeId<'ctx>,
    _symbols: &Symbols<'ctx>,
    span: Span,
) -> Result<(), LowerError<'ctx>> {
    if id == predef::STRING {
        Err(LowerError::UnsupportedType(PolyType::nullary(id), span))
    } else {
        Ok(())
    }
}

fn reject_int_to_variant<'ctx>(
    source: &PolyType<'ctx>,
    target: &PolyType<'ctx>,
    symbols: &Symbols<'ctx>,
    span: Span,
) -> Result<(), LowerError<'ctx>> {
    if source
        .upper_bound(symbols)
        .is_some_and(|t| t.id() == predef::INT32)
        && target
            .upper_bound(symbols)
            .is_some_and(|t| t.id() == predef::VARIANT)
    {
        Err(LowerError::UnexpectedNonConstant(span))
    } else {
        Ok(())
    }
}

#[test]
fn default_policy_matches_the_original_entry_point() {
    let sources = SourceMap::new();
    let file = sources.push_back(
        "default.reds",
        "func Good() -> Int32 { let x = 1; return x; }\n\
         func Bad() { if 1 {} let x: Missing; }",
    );
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let modules = parse_files(&sources, &mut reporter);
    let evaluator = Evaluator::from_modules(&modules);
    let (expected, expected_symbols) = process_sources(
        modules,
        Symbols::with_default_types(),
        evaluator,
        &mut reporter,
        &interner,
    );
    let expected_diagnostics: Vec<_> = reporter
        .reported()
        .iter()
        .map(|d| (d.code(), d.span(), d.to_string()))
        .collect();
    assert_eq!(expected_diagnostics.len(), 2);
    for policies in [vec![], vec![(file, SemanticPolicy::default())]] {
        let (actual, symbols, diagnostics) = analyze(&sources, &interner, &policies, false);
        assert_eq!(
            format!("{:?}", function(&actual, &symbols, "Good")),
            format!("{:?}", function(&expected, &expected_symbols, "Good")),
        );
        assert_eq!(
            diagnostics
                .iter()
                .map(|d| (d.code(), d.span(), d.to_string()))
                .collect::<Vec<_>>(),
            expected_diagnostics,
        );
    }
}

#[test]
fn every_hook_is_file_local_in_both_module_orders() {
    for reverse in [false, true] {
        let sources = SourceMap::new();
        sources.push_back(
            "declarations.reds",
            "@intrinsic(VariantIsDefined) func VariantIsDefined(value: Variant) -> Bool\n\
             @intrinsic(ArraySize) func ArraySize<T>(out value: [T]) -> Int32\n\
             @intrinsic(ToVariant) func ToVariant<T>(value: T) -> Variant",
        );
        let hooked = sources.push_back(
            "hooked.reds",
            "func Hooked(value: Variant, values: [Int32]) {\n\
             if value {} values.count(); before = 1; let before: Int32;\n\
             let text: String; let boxed: Variant = 1; ArraySize(values); }",
        );
        let plain = sources.push_back(
            "plain.reds",
            "func Plain(value: Variant, values: [Int32]) {\n\
             if value {} values.count(); before = 1; let before: Int32;\n\
             let text: String; let boxed: Variant = 1; ArraySize(values); }",
        );
        let interner = TypeInterner::default();
        let policy = SemanticPolicy {
            hoist_locals: true,
            native_object_cast: false,
            condition: Some(variant_condition),
            member_intrinsic: Some(array_member),
            validate_type: Some(reject_string),
            validate_coercion: Some(reject_int_to_variant),
            allow_explicit_intrinsic: Some(|_| false),
        };
        let (_, symbols, diagnostics) = analyze(&sources, &interner, &[(hooked, policy)], reverse);
        let codes = |file| {
            let mut codes: Vec<_> = diagnostics
                .iter()
                .filter(|d| d.span().file == file)
                .map(Diagnostic::code)
                .collect();
            codes.sort_unstable();
            codes
        };
        assert_eq!(
            codes(hooked),
            ["INVALID_CONSTANT", "UNRESOLVED_FN", "UNSUPPORTED_TYPE"],
            "{diagnostics:?}"
        );
        assert_eq!(
            codes(plain),
            ["TYPE_ERR", "UNRESOLVED_MEMBER", "UNRESOLVED_REF"],
            "{diagnostics:?}"
        );
        assert_eq!(diagnostics.len(), 6, "{diagnostics:?}");
        assert_eq!(
            symbols
                .free_functions()
                .filter(|entry| entry.func().intrinsic().is_some())
                .count(),
            3
        );
    }
}

#[test]
fn policies_survive_nested_type_scopes() {
    let sources = SourceMap::new();
    let hooked = sources.push_back(
        "nested.reds",
        "class Container<T> { public func Check<U>() { let text: String; } }\n\
         func Outer<T>() { let callback = () -> { let text: String; }; }",
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            hooked,
            SemanticPolicy {
                validate_type: Some(reject_string),
                ..Default::default()
            },
        )],
        false,
    );
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert!(
        diagnostics
            .iter()
            .all(|d| d.code() == "UNSUPPORTED_TYPE" && d.span().file == hooked)
    );
}

#[test]
fn condition_callbacks_only_select_the_exact_intrinsic() {
    for annotated in [false, true] {
        let sources = SourceMap::new();
        sources.push_back(
            "declarations.reds",
            if annotated {
                "@intrinsic(VariantIsDefined) func VariantIsDefined(value: Variant) -> Bool\n\
                 func VariantIsDefined(value: Int32) -> Bool = true;"
            } else {
                "func VariantIsDefined(value: Variant) -> Bool = true;"
            },
        );
        let file = sources.push_back(
            "condition.reds",
            "func Test(value: Variant) { if value {} }",
        );
        let interner = TypeInterner::default();
        let (unit, symbols, diagnostics) = analyze(
            &sources,
            &interner,
            &[(
                file,
                SemanticPolicy {
                    condition: Some(variant_condition),
                    ..Default::default()
                },
            )],
            false,
        );
        if annotated {
            assert!(diagnostics.is_empty(), "{diagnostics:?}");
            let ir::Stmt::Branches { branches, .. } =
                &function(&unit, &symbols, "Test").block.stmts[0]
            else {
                panic!("expected branches")
            };
            let ir::Expr::Call { call, .. } = &*branches[0].condition else {
                panic!("expected conversion")
            };
            let ir::Call::FreeFunction { function, .. } = &**call else {
                panic!("expected intrinsic")
            };
            assert_eq!(
                symbols[*function].intrinsic(),
                Some(ir::Intrinsic::VariantIsDefined)
            );
        } else {
            assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
            assert_eq!(diagnostics[0].code(), "UNRESOLVED_FN");
        }
    }
}

#[test]
fn condition_intrinsics_must_return_bool() {
    let sources = SourceMap::new();
    sources.push_back(
        "declarations.reds",
        "@intrinsic(VariantIsDefined) func VariantIsDefined(value: Variant) -> Int32",
    );
    let file = sources.push_back(
        "condition.reds",
        "func Test(value: Variant) { if value {} }",
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                condition: Some(variant_condition),
                ..Default::default()
            },
        )],
        false,
    );
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "TYPE_ERR");
}

#[test]
fn member_callbacks_cannot_bind_an_unannotated_same_name_overload() {
    let sources = SourceMap::new();
    sources.push_back(
        "declarations.reds",
        "func ArraySize(out value: [Int32]) -> Int32 = 0;\n\
         @intrinsic(ArraySize) func ArraySize(value: Bool) -> Int32",
    );
    let file = sources.push_back(
        "member.reds",
        "func Test(values: [Int32]) { values.count(); }",
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                member_intrinsic: Some(array_member),
                ..Default::default()
            },
        )],
        false,
    );
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "UNRESOLVED_FN");
}

#[test]
fn explicit_filter_uses_intrinsic_identity_not_the_function_name() {
    let sources = SourceMap::new();
    sources.push_back(
        "declarations.reds",
        "@intrinsic(ArraySize) func Measure(out value: [Int32]) -> Int32\n\
         func ArraySize(value: Bool) -> Int32 = 0;",
    );
    let file = sources.push_back(
        "explicit.reds",
        "func Test(values: [Int32]) { ArraySize(true); Measure(values); values.count(); }",
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                member_intrinsic: Some(array_member),
                allow_explicit_intrinsic: Some(|intrinsic| intrinsic != ir::Intrinsic::ArraySize),
                ..Default::default()
            },
        )],
        false,
    );
    // Measure is rejected explicitly; the generated call cannot find the canonical
    // intrinsic name, and must not bind the ordinary ArraySize function instead.
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "UNRESOLVED_FN");
    assert_eq!(diagnostics[1].code(), "UNRESOLVED_FN");
}

#[test]
fn hoisting_preserves_initializer_sites_and_local_identity() {
    let sources = SourceMap::new();
    let file = sources.push_back(
        "hoist.reds",
        "func Test() { value = 1; if true { let value: Int32 = 2; } value = 3; let inferred = 4; }",
    );
    let interner = TypeInterner::default();
    let (unit, symbols, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                hoist_locals: true,
                ..Default::default()
            },
        )],
        false,
    );
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let function = function(&unit, &symbols, "Test");
    assert_eq!(function.locals.len(), 2);
    let ir::Stmt::InitDefault { local, .. } = function.block.stmts[0] else {
        panic!("expected hoisted default")
    };
    let assignment_local = |stmt: &ir::Stmt<'_>| {
        let ir::Stmt::Expr(expr) = stmt else {
            panic!("expected expression")
        };
        let ir::Expr::Assign { place, .. } = &**expr else {
            panic!("expected assignment")
        };
        let ir::Expr::Local(id, _) = &**place else {
            panic!("expected local")
        };
        *id
    };
    assert_eq!(assignment_local(&function.block.stmts[1]), local);
    let ir::Stmt::Branches { branches, .. } = &function.block.stmts[2] else {
        panic!("expected branches")
    };
    assert_eq!(assignment_local(&branches[0].block.stmts[0]), local);
    assert_eq!(assignment_local(&function.block.stmts[3]), local);
    assert_ne!(assignment_local(&function.block.stmts[4]), local);
}

#[test]
fn hoist_duplicates_and_invalid_types_report_once_at_the_declaration() {
    for (source, code, highlight) in [
        (
            "func Test() { let x: Int32; let x: Bool; }",
            "SYM_REDEFINITION",
            "x",
        ),
        (
            "func Test(x: Int32) { let x: Bool; }",
            "SYM_REDEFINITION",
            "x",
        ),
        (
            "func Test() { let x: Missing; }",
            "UNRESOLVED_TYPE",
            "Missing",
        ),
        (
            "func Test() { let x: Missing; let x: Bool; }",
            "UNRESOLVED_TYPE",
            "Missing",
        ),
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("errors.reds", source);
        let interner = TypeInterner::default();
        let (_, _, diagnostics) = analyze(
            &sources,
            &interner,
            &[(
                file,
                SemanticPolicy {
                    hoist_locals: true,
                    ..Default::default()
                },
            )],
            false,
        );
        let expected_count = if source.contains("Missing; let") {
            2
        } else {
            1
        };
        assert_eq!(
            diagnostics.len(),
            expected_count,
            "{source}: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].code(), code);
        assert_eq!(
            sources
                .get(file)
                .unwrap()
                .span_contents(diagnostics[0].span()),
            highlight
        );
        if expected_count == 2 {
            assert_eq!(diagnostics[1].code(), "SYM_REDEFINITION");
        }
    }
}

#[test]
fn hoist_does_not_collect_closure_locals_into_the_parent() {
    let sources = SourceMap::new();
    let file = sources.push_back(
        "closure.reds",
        "func Test() { if true { () -> { let inner: Int32; }; } inner = 1; }",
    );
    let interner = TypeInterner::default();
    let (unit, symbols, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                hoist_locals: true,
                ..Default::default()
            },
        )],
        false,
    );
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "UNRESOLVED_REF");
    assert!(function(&unit, &symbols, "Test").locals.is_empty());
}

#[test]
fn condition_conversion_covers_control_flow_and_logical_expressions() {
    let sources = SourceMap::new();
    sources.push_back(
        "declarations.reds",
        "@intrinsic(VariantIsDefined) func VariantIsDefined(value: Variant) -> Bool
         native func OperatorLogicAnd(lhs: Bool, rhs: Bool) -> Bool
         native func OperatorLogicOr(lhs: Bool, rhs: Bool) -> Bool
         native func OperatorLogicNot(value: Bool) -> Bool",
    );
    let file = sources.push_back(
        "conditions.reds",
        "func Test(value: Variant) { if value {} while value { break; }
         let a = !value; let b = value && value; let c = value || value;
         let d = value ? 1 : 2; if true {} }",
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                condition: Some(variant_condition),
                ..Default::default()
            },
        )],
        false,
    );
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn callbacks_cannot_bypass_standard_type_constraints() {
    let sources = SourceMap::new();
    let file = sources.push_back(
        "constraints.reds",
        "func Test() { if 1 {} let value: Bool = 1; }",
    );
    let interner = TypeInterner::default();
    let (_, _, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                condition: Some(|_, _| None),
                validate_coercion: Some(|_, _, _, _| Ok(())),
                ..Default::default()
            },
        )],
        false,
    );
    assert_eq!(diagnostics.len(), 2, "{diagnostics:?}");
    assert!(diagnostics.iter().all(|d| d.code() == "TYPE_ERR"));
}

#[test]
fn explicit_filter_does_not_reject_generated_intrinsics() {
    let sources = SourceMap::new();
    sources.push_back(
        "declarations.reds",
        "@intrinsic(ArraySize) func ArraySize<T>(out value: [T]) -> Int32
         @intrinsic(VariantIsDefined) func VariantIsDefined(value: Variant) -> Bool",
    );
    let file = sources.push_back(
        "generated.reds",
        "func Test(values: [Int32], value: Variant) {
         values.count(); if value {} ArraySize(values); }",
    );
    let interner = TypeInterner::default();
    let (unit, symbols, diagnostics) = analyze(
        &sources,
        &interner,
        &[(
            file,
            SemanticPolicy {
                condition: Some(variant_condition),
                member_intrinsic: Some(array_member),
                allow_explicit_intrinsic: Some(|_| false),
                ..Default::default()
            },
        )],
        false,
    );
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code(), "UNRESOLVED_FN");
    let function = function(&unit, &symbols, "Test");
    assert_eq!(function.block.stmts.len(), 2);
    let ir::Stmt::Expr(expr) = &function.block.stmts[0] else {
        panic!("expected call")
    };
    let ir::Expr::Call { call, .. } = &**expr else {
        panic!("expected call")
    };
    let ir::Call::FreeFunction { function, args, .. } = &**call else {
        panic!("expected intrinsic")
    };
    assert_eq!(
        symbols[*function].intrinsic(),
        Some(ir::Intrinsic::ArraySize)
    );
    assert_eq!(args.len(), 1);
    assert!(matches!(args[0], ir::Expr::Local(ir::Local::Param(0), _)));
}

#[test]
fn member_hook_checks_receiver_place_and_type_arguments() {
    for (expression, expected) in [
        ("values.count()", None),
        ("Make().count()", Some("INVALID_TEMP")),
        ("values.count<Int32>()", Some("INVALID_TYPE_ARG_COUNT")),
    ] {
        let sources = SourceMap::new();
        sources.push_back(
            "declarations.reds",
            "@intrinsic(ArraySize) func ArraySize<T>(out value: [T]) -> Int32
             native func Make() -> [Int32]",
        );
        let file = sources.push_back(
            "receiver.reds",
            format!("func Test(values: [Int32]) {{ {expression}; }}"),
        );
        let interner = TypeInterner::default();
        let (_, _, diagnostics) = analyze(
            &sources,
            &interner,
            &[(
                file,
                SemanticPolicy {
                    member_intrinsic: Some(array_member),
                    ..Default::default()
                },
            )],
            false,
        );
        assert_eq!(
            diagnostics.first().map(Diagnostic::code),
            expected,
            "{diagnostics:?}"
        );
        assert_eq!(diagnostics.len(), usize::from(expected.is_some()));
    }
}
