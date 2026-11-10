use redscript_community_ast::SourceMap;
use redscript_compiler_frontend::{LoweredCompilationUnit, Symbols, ir};
use redscript_redmod_adapter::parse_file_with_metadata;

use crate::support;

fn analyze(
    source: &str,
    check: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    support::analyze(
        source,
        "selection.script",
        &[],
        |unit, symbols, _, errors, reporter| {
            let errors = errors
                .iter()
                .map(ToString::to_string)
                .chain(reporter.reported().iter().map(ToString::to_string))
                .collect::<Vec<_>>();
            check(unit, symbols, &errors);
        },
    );
}

#[test]
fn generated_forwarders_do_not_require_or_supply_override_declarations() {
    for declarations in [
        "public static function Pick(self: Child, value: Int32) -> Int32 { return 2; } public override function Pick(value: Int32) -> Int32 { return 3; }",
        "public override function Pick(value: Int32) -> Int32 { return 3; } public static function Pick(self: Child, value: Int32) -> Int32 { return 2; }",
    ] {
        analyze(
            &format!(
                "class Base {{ public function Pick(value: Int32) -> Int32 {{ return 1; }} }} class Child extends Base {{ {declarations} public function Run() -> Int32 {{ return Pick(1); }} }}"
            ),
            |_, _, errors| {
                assert!(errors.is_empty(), "{errors:?}");
            },
        );
    }
    analyze(
        "class Base { public static function Pick(self: Base, value: Int32) {} } class Child extends Base { public function Pick(value: Int32) {} }",
        |_, _, errors| {
            assert!(errors.is_empty(), "{errors:?}");
        },
    );
    analyze(
        "class Base { public function Pick(value: Int32) {} } class Child extends Base { public function Pick(value: Int32) {} }",
        |_, _, errors| {
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert!(errors[0].contains("override"), "{errors:?}");
        },
    );
    analyze(
        "class Base { public static function Pick(self: Base, value: Int32) {} } class Child extends Base { public override function Pick(value: Int32) {} }",
        |_, _, errors| {
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert!(errors[0].contains("override"), "{errors:?}");
        },
    );
}

#[test]
fn imported_bodyless_static_declaration_has_a_fallback_forwarder() {
    analyze(
        "import class Host { public import static function Pick(self: Host, value: Int32) -> Int32; } function Run(value: Host) -> Int32 { return value.Pick(1); }",
        |unit, symbols, errors| {
            assert!(errors.is_empty(), "{errors:?}");
            let run = symbols
                .free_functions()
                .find(|entry| entry.name().as_single_component() == Some("Run"))
                .unwrap();
            let ir::Stmt::Return(Some(expr), _) =
                unit.functions[run.key()].block.stmts.back().unwrap()
            else {
                panic!("expected return")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("expected call")
            };
            let ir::Call::Static { method, args, .. } = &**call else {
                panic!("expected static alias")
            };
            assert!(symbols[*method].flags().is_static());
            assert_eq!(args.len(), 2);
        },
    );
}

#[test]
fn receiver_expression_occurs_once_before_forwarded_arguments() {
    analyze(
        "class Host { public static function Get() -> Host { return NULL; } public static function Pick(self: Host, value: Int32) -> Int32 { return value; } } function Run() -> Int32 { return Host.Get().Pick(1); }",
        |unit, symbols, errors| {
            assert!(errors.is_empty(), "{errors:?}");
            let run = symbols
                .free_functions()
                .find(|entry| entry.name().as_single_component() == Some("Run"))
                .unwrap();
            let ir::Stmt::Return(Some(expr), _) =
                unit.functions[run.key()].block.stmts.back().unwrap()
            else {
                panic!("expected return")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("expected call")
            };
            let ir::Call::Static { method, args, .. } = &**call else {
                panic!("expected static alias")
            };
            assert!(symbols[*method].flags().is_static());
            assert_eq!(args.len(), 2);
            assert!(matches!(&args[0], ir::Expr::Call { .. }));
            assert!(matches!(&args[1], ir::Expr::Const(_, _)));
        },
    );
}

#[test]
fn forwarder_checks_out_receiver_storage() {
    analyze(
        "class Host { public static function Get() -> Host { return NULL; } public static function Pick(out self: Host) {} } function Run() { Host.Get().Pick(); }",
        |_, _, errors| {
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert!(errors[0].contains("temporary"), "{errors:?}");
        },
    );
}

#[test]
fn lazy_forwarder_receivers_do_not_hoist_array_initializers() {
    let sources = SourceMap::new();
    let source = "class Host { public static function Get(values: array<Int32>) -> Host { return NULL; } public static function Pick(skip self: Host) {} } function Run() { Host.Get([1]).Pick(); }";
    let file = sources.push_back("lazy.script", source);
    let (_, errors) = parse_file_with_metadata(source, file);
    assert!(
        matches!(
            errors.as_slice(),
            [redscript_redmod_adapter::Error::Capability {
                kind: redscript_redmod_adapter::bridge::CapabilityError::UnsafeArrayEvaluation,
                ..
            }]
        ),
        "{errors:?}"
    );
    analyze(
        "class Host { public static function Get() -> Host { return NULL; } public static function Pick(skip self: Host) {} } function Run() { Host.Get().Pick(); }",
        |_, _, errors| assert!(errors.is_empty(), "{errors:?}"),
    );
}

#[test]
fn eager_array_argument_does_not_poison_a_later_lazy_argument() {
    analyze(
        "import function Choose(values: array<Int32>, skip value: Bool); import function Choose(values: array<Int32>, skip value: Int32); function Run() { Choose([1], true); }",
        |_, _, errors| assert!(errors.is_empty(), "{errors:?}"),
    );
}

#[test]
fn native_reference_specificity_uses_the_selected_child_target() {
    analyze(
        "class Base {} class Child extends Base {} class Host { public function Pick(value: Base) -> Int32 { return 1; } public function Pick(value: Child) -> Int32 { return 2; } public function Run(value: Child) -> Int32 { return Pick(value); } }",
        |unit, symbols, errors| {
            assert!(errors.is_empty(), "{errors:?}");
            let (id, run) = unit
                .classes
                .iter()
                .flat_map(|(id, class)| {
                    class
                        .methods
                        .iter()
                        .map(move |(idx, func)| ((*id, *idx), func))
                })
                .find(|((id, idx), _)| {
                    symbols
                        .get_method(redscript_compiler_frontend::MethodId::new(*id, *idx))
                        .is_some_and(|(name, _)| name == "Run")
                })
                .unwrap();
            let ir::Stmt::Return(Some(expr), _) = run.block.stmts.back().unwrap() else {
                panic!("expected return")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("expected call")
            };
            let ir::Call::Instance { method, .. } = &**call else {
                panic!("expected instance")
            };
            assert_eq!(method.parent(), id.0);
            assert_eq!(
                symbols[*method]
                    .type_()
                    .param_types()
                    .next()
                    .unwrap()
                    .unwrap_ref_or_self()
                    .to_string(),
                "Child"
            );
        },
    );
}
