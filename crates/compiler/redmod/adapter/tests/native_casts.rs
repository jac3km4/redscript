use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner, ir,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn analyze(
    native: &str,
    community: &str,
    reverse: bool,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("shared.reds", community);
    let file = sources.push_back("casts.script", native);
    let (parsed, errors) = parse_file_with_metadata(native, file);
    assert!(errors.is_empty(), "{errors:?}");
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
    for error in reporter.reported() {
        if error.code() == "IMPOSSIBLE_DYN_CAST" {
            let span = error.span();
            assert_eq!(span.file, file);
            assert!(matches!(
                &native[span.start as usize..span.end as usize],
                "(" | "Cast<"
            ));
        }
    }
    let codes = reporter
        .reported()
        .iter()
        .map(|error| error.code().to_string())
        .collect::<Vec<_>>();
    test(&unit, &symbols, &codes);
}

fn returned<'a, 'ctx>(
    unit: &'a LoweredCompilationUnit<'ctx>,
    symbols: &Symbols<'ctx>,
    name: &str,
) -> &'a ir::Expr<'ctx> {
    let entry = symbols
        .free_functions()
        .find(|entry| entry.name().to_string() == name)
        .unwrap();
    let ir::Stmt::Return(Some(expr), _) = &unit.functions[entry.key()].block.stmts[0] else {
        panic!("expected return in {name}")
    };
    expr
}

#[test]
fn object_casts_preserve_downcasts_weak_references_and_null() {
    analyze(
        r#"
        class Base {}
        class Child extends Base {}
        function Down(value: Base) -> Child { return (Child)value; }
        function Explicit(value: Base) -> Child { return Cast<Child>(value); }
        function Weak(value: weak<Base>) -> weak<Child> { return (Child)value; }
        function Null() -> Child { return (Child)NULL; }
        "#,
        "",
        false,
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for name in ["Down", "Explicit", "Weak", "Null"] {
                let ir::Expr::DynCast {
                    expr_type,
                    target_type,
                    ..
                } = returned(unit, symbols, name)
                else {
                    panic!("expected dynamic cast in {name}")
                };
                assert_eq!(target_type.coalesced(symbols).unwrap().to_string(), "Child");
                if name == "Weak" {
                    assert_eq!(
                        expr_type.coalesced(symbols).unwrap().to_string(),
                        "wref<Base>"
                    );
                }
            }
        },
    );
}

#[test]
fn identity_and_upcasts_use_normal_coercion() {
    analyze(
        r#"
        class Base {}
        class Child extends Base {}
        function Up(value: Child) -> Base { return (Base)value; }
        function Identity(value: Child) -> Child { return (Child)value; }
        function WeakUp(value: weak<Child>) -> weak<Base> { return (Base)value; }
        function WeakIdentity(value: weak<Child>) -> weak<Child> { return (Child)value; }
        "#,
        "",
        false,
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for name in ["Up", "Identity", "WeakUp", "WeakIdentity"] {
                assert!(matches!(returned(unit, symbols, name), ir::Expr::Local(..)));
            }
        },
    );
}

#[test]
fn incompatible_object_casts_are_rejected() {
    analyze(
        r#"
        class Base {}
        class Other {}
        function Unrelated(value: Base) -> Other { return (Other)value; }
        function Numeric(value: Int32) -> Base { return Cast<Base>(value); }
        function Weak(value: weak<Base>) -> weak<Other> { return (Other)value; }
        "#,
        "",
        false,
        |_, _, codes| {
            assert_eq!(codes, ["IMPOSSIBLE_DYN_CAST"; 3]);
        },
    );
}

#[test]
fn numeric_and_struct_targets_still_resolve_conversion_overloads() {
    analyze(
        r#"
        function Numeric(value: Int32) -> Float { return (Float)value; }
        function Explicit(value: Int32) -> Float { return Cast<Float>(value); }
        function Structure(value: Int32) -> Value { return (Value)value; }
        "#,
        "struct Value {} native func Cast(value: Int32) -> Float; native func Cast(value: Int32) -> Value;",
        false,
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for name in ["Numeric", "Explicit", "Structure"] {
                let ir::Expr::Call { call, .. } = returned(unit, symbols, name) else {
                    panic!("expected conversion call in {name}")
                };
                let ir::Call::FreeFunction { function, .. } = &**call else {
                    panic!("expected free function")
                };
                assert!(symbols[*function].flags().is_native());
            }
        },
    );
}

#[test]
fn ordinary_conversion_literals_keep_the_selected_parameter_context() {
    analyze(
        r#"
        function Wide() -> Value { return (Value)1.5; }
        function Narrow() -> Other { return Cast<Other>(1); }
        function Negative() -> Other { return (Other)-1; }
        function ExplicitNegative() -> Other { return Cast<Other>(-1); }
        function Parenthesized() -> Other { return Cast<Other>((1)); }
        "#,
        "struct Value {} struct Other {} native func Cast(value: Double) -> Value; native func Cast(value: Int8) -> Other;",
        false,
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for (name, expected) in [
                ("Wide", "Double"),
                ("Narrow", "Int8"),
                ("Negative", "Int8"),
                ("ExplicitNegative", "Int8"),
                ("Parenthesized", "Int8"),
            ] {
                let ir::Expr::Call { call, .. } = returned(unit, symbols, name) else {
                    panic!("expected conversion in {name}")
                };
                let [ir::Expr::Const(value, _)] = call.args() else {
                    panic!("expected literal argument in {name}")
                };
                assert_eq!(value.type_id().to_string(), expected);
            }
        },
    );
}

#[test]
fn typed_conversion_fallback_checks_the_lazy_source_operand() {
    for (operand, expected) in [
        ("Read([1])", vec!["UNSAFE_ARRAY_EVALUATION"]),
        ("1", vec![]),
    ] {
        analyze(
            &format!("function Run() -> Value {{ return Cast<Value>({operand}); }}"),
            "struct Value {} native func Read(values: [Int32]) -> Int32; native func Cast(skip value: Int32) -> Value;",
            false,
            |_, _, codes| assert_eq!(codes, expected, "{operand}: {codes:?}"),
        );
    }
}

#[test]
fn variant_casts_select_extraction_with_exact_target_types() {
    for reverse in [false, true] {
        analyze(
            r#"
            function Object(value: Variant) -> Base { return (Base)value; }
            function Explicit(value: Variant) -> Base { return Cast<Base>(value); }
            function Weak(value: Variant) -> weak<Base> { return (weak<Base>)value; }
            function Structure(value: Variant) -> Value { return (Value)value; }
            function Nested() -> Child { return (Child)(Base)GetVariant(); }
            function Effect() -> Base { return (Base)GetVariant(); }
            "#,
            r#"
            class Base {} class Child extends Base {} struct Value {}
            native func GetVariant() -> Variant;
            func Community(value: Variant) -> Base { return Cast<Base>(value); }
            func CommunityExtraction(value: Variant) -> Base { return FromVariant<Base>(value); }
            "#,
            reverse,
            |unit, symbols, codes| {
                use ir::Visitor;
                assert_eq!(codes, ["UNRESOLVED_FN"]);
                for (name, target) in [
                    ("Object", "Base"),
                    ("Explicit", "Base"),
                    ("Weak", "wref<Base>"),
                    ("Structure", "Value"),
                    ("Effect", "Base"),
                    ("CommunityExtraction", "Base"),
                ] {
                    let ir::Expr::Call { call, .. } = returned(unit, symbols, name) else {
                        panic!("expected extraction in {name}")
                    };
                    let ir::Call::FreeFunction {
                        function,
                        type_args,
                        args,
                    } = &**call
                    else {
                        panic!("expected intrinsic call")
                    };
                    assert_eq!(
                        symbols[*function].intrinsic(),
                        Some(ir::Intrinsic::FromVariant)
                    );
                    assert_eq!(type_args.len(), 1);
                    assert_eq!(type_args[0].coalesced(symbols).unwrap().to_string(), target);
                    assert_eq!(args.len(), 1);
                }
                let ir::Expr::DynCast { expr, .. } = returned(unit, symbols, "Nested") else {
                    panic!("expected outer object downcast")
                };
                assert!(matches!(&**expr, ir::Expr::Call { call, .. }
                    if matches!(&**call, ir::Call::FreeFunction { function, .. }
                        if symbols[*function].intrinsic() == Some(ir::Intrinsic::FromVariant))));
                for name in ["Nested", "Effect"] {
                    let mut counts = Counts::default();
                    counts.visit_expr(returned(unit, symbols, name));
                    assert_eq!(
                        counts.calls, 2,
                        "one extraction and one source call: {name}"
                    );
                    assert_eq!(counts.casts, usize::from(name == "Nested"));
                }
            },
        );
    }
}

#[derive(Default)]
struct Counts {
    calls: usize,
    casts: usize,
}

impl<'ctx> ir::Visitor<'ctx> for Counts {
    fn visit_dyn_cast(
        &mut self,
        expr: &ir::Expr<'ctx>,
        _: &ir::Type<'ctx>,
        _: &ir::Type<'ctx>,
        _: Span,
    ) {
        self.casts += 1;
        self.visit_expr(expr);
    }

    fn visit_call(&mut self, call: &ir::Call<'ctx>, _: Span) {
        self.calls += 1;
        for arg in call.args() {
            self.visit_expr(arg);
        }
    }
}

#[test]
fn object_cast_operands_are_evaluated_once_in_ir() {
    analyze(
        r#"
        function Down() -> Child { return (Child)GetBase(); }
        function Up() -> Base { return (Base)GetChild(); }
        function Identity() -> Child { return (Child)GetChild(); }
        function Weak() -> weak<Child> { return (Child)GetWeak(); }
        "#,
        "class Base {} class Child extends Base {} native func GetBase() -> Base; native func GetChild() -> Child; native func GetWeak() -> wref<Base>;",
        false,
        |unit, symbols, codes| {
            use ir::Visitor;
            assert!(codes.is_empty(), "{codes:?}");
            for name in ["Down", "Up", "Identity", "Weak"] {
                let mut counts = Counts::default();
                counts.visit_expr(returned(unit, symbols, name));
                assert_eq!(counts.calls, 1, "{name}");
                assert_eq!(counts.casts, usize::from(matches!(name, "Down" | "Weak")));
            }
        },
    );
}

#[test]
fn community_cast_behavior_is_unchanged_in_mixed_sources() {
    for reverse in [false, true] {
        analyze(
            "function Native(value: Base) -> Child { return (Child)value; }",
            r#"
            class Base {} class Child extends Base {}
            func CommunityStatic(value: Base) -> Child { return Cast<Child>(value); }
            func CommunityUp(value: Child) -> Base { return value as Base; }
            func CommunityIdentity(value: Child) -> Child { return value as Child; }
            "#,
            reverse,
            |unit, symbols, codes| {
                assert_eq!(codes, ["UNRESOLVED_FN", "REDUNDANT_DYN_CAST"]);
                assert!(matches!(
                    returned(unit, symbols, "Native"),
                    ir::Expr::DynCast { .. }
                ));
                // Community identity casts already lower to a dynamic cast.
                assert!(matches!(
                    returned(unit, symbols, "CommunityIdentity"),
                    ir::Expr::DynCast { .. }
                ));
            },
        );
    }
}

fn converted<'a, 'ctx>(
    expr: &'a ir::Expr<'ctx>,
    symbols: &Symbols<'ctx>,
    source: &str,
    target: &str,
) -> &'a ir::Expr<'ctx> {
    let ir::Expr::Call { call, .. } = expr else {
        panic!("expected conversion")
    };
    let ir::Call::FreeFunction {
        function,
        args,
        type_args,
    } = &**call
    else {
        panic!()
    };
    let provider = &symbols[*function];
    assert!(provider.flags().is_native());
    assert!(provider.type_().conversion().is_some());
    assert!(type_args.is_empty());
    assert_eq!(provider.type_().params()[0].type_().to_string(), source);
    assert_eq!(provider.type_().return_type().to_string(), target);
    assert_eq!(args.len(), 1);
    &args[0]
}

#[test]
fn native_provider_explicit_call_evaluates_operand_once() {
    for qualifier in ["", "implicit "] {
        analyze(
            &format!(
                "import {qualifier}cast(value: Int32): Float; import function Tick(): Int32; function Run(): Float {{ return (Float)Tick(); }}"
            ),
            "",
            false,
            |unit, symbols, codes| {
                assert!(codes.is_empty(), "{codes:?}");
                let operand = converted(returned(unit, symbols, "Run"), symbols, "Int32", "Float");
                let ir::Expr::Call { call, .. } = operand else {
                    panic!()
                };
                let ir::Call::FreeFunction { function, args, .. } = &**call else {
                    panic!()
                };
                assert!(
                    symbols
                        .free_functions()
                        .any(|e| e.key() == function && e.name().to_string() == "Tick")
                );
                assert!(args.is_empty());
            },
        );
    }
}

#[test]
fn native_provider_explicit_gates_and_ordinary_precedence() {
    for declaration in [
        "cast(value: Int32): Float { return 1.0; }",
        "import cast(const value: Int32): Float;",
        "import cast(out value: Int32): Float;",
        "import cast(optional value: Int32): Float;",
        "import cast(skip value: Int32): Float;",
        "import cast(value: Int32): const Float;",
        "import cast(value: script_ref<Int32>): Float;",
    ] {
        analyze(
            &format!("{declaration} function Run(value: Int32): Float {{ return (Float)value; }}"),
            "",
            false,
            |_, _, codes| assert!(!codes.is_empty(), "{declaration}"),
        );
    }
    for reverse in [false, true] {
        analyze(
            "import cast(value: Int32): Float; function Run(value: Int32): Float { return (Float)value; }",
            "native func Cast(value: Int32) -> Float;",
            reverse,
            |unit, symbols, codes| {
                assert!(codes.is_empty(), "{codes:?}");
                let ir::Expr::Call { call, .. } = returned(unit, symbols, "Run") else {
                    panic!()
                };
                let ir::Call::FreeFunction { function, .. } = &**call else {
                    panic!()
                };
                assert!(symbols[*function].type_().conversion().is_none());
            },
        );
    }
    analyze(
        "import cast(value: Int32): Float; function Run(value: Int32): Float { return (Float)value; }",
        "native func Cast(value: Bool) -> Float;",
        false,
        |_, _, codes| assert!(!codes.is_empty()),
    );
    analyze(
        "import cast(value: Int32): Float; function Run(): Float { return (Float)1; }",
        "",
        false,
        |_, _, codes| assert_eq!(codes, ["UNSUPPORTED_NATIVE_CONVERSION"]),
    );
}

#[test]
fn native_provider_explicit_ambiguity_and_community_consumers_fail_closed() {
    analyze(
        "import cast(value: Int32): Float; import implicit cast(value: Int32): Float; function Run(value: Int32): Float { return (Float)value; }",
        "",
        false,
        |_, _, codes| assert_eq!(codes, ["UNSUPPORTED_NATIVE_CONVERSION"]),
    );
    analyze(
        "import cast(value: Int32): Float;",
        "func Community(value: Int32) -> Float { return Cast<Float>(value); }",
        false,
        |_, _, codes| assert_eq!(codes, ["UNSUPPORTED_NATIVE_CONVERSION"]),
    );
    analyze(
        "function Run(value: Int32): Float { return (Float)value; }",
        "native explicit func Cast(value: Int32) -> Float;",
        false,
        |_, _, codes| assert_eq!(codes, ["UNSUPPORTED_NATIVE_CONVERSION"]),
    );
    analyze(
        "function Run(value: Int32): Float { return (Float)value; }",
        "native explicit func Cast<T>(value: T) -> Float;",
        false,
        |_, _, codes| assert_eq!(codes, ["UNSUPPORTED_NATIVE_CONVERSION"]),
    );
}
