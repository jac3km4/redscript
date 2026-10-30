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
        function Numeric(value: Int32) -> Base { return (Base)value; }
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
