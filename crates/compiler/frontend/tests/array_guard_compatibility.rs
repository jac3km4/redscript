mod common;

use redscript_compiler_frontend::{DiagnosticLevel, Symbols, ir};

fn check(source: &str, inspect: impl FnOnce(&ir::Block<'_>, &Symbols<'_>)) {
    common::analyze(
        "array_compatibility.reds",
        source,
        |_, unit, symbols, reporter| {
            let diagnostics = reporter.reported();
            assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
            let diagnostic = &diagnostics[0];
            assert_eq!(diagnostic.code(), "UNSAFE_ARRAY_EVALUATION");
            assert_eq!(diagnostic.level(), DiagnosticLevel::Warning);
            assert!(!diagnostic.is_error());
            let entry = symbols
                .free_functions()
                .find(|entry| entry.name().to_string() == "Run")
                .unwrap();
            let block = &unit.functions[entry.key()].block;
            assert!(!format!("{block:?}").contains("Nop"), "{block:?}");
            inspect(block, symbols);
        },
    );
}

fn final_call<'a, 'ctx>(block: &'a ir::Block<'ctx>) -> &'a ir::Call<'ctx> {
    let expr = match block.stmts.back().unwrap() {
        ir::Stmt::Expr(expr) | ir::Stmt::Return(Some(expr), _) => expr,
        stmt => panic!("missing original call: {stmt:?}"),
    };
    let ir::Expr::Call { call, .. } = &**expr else {
        panic!("{expr:?}")
    };
    call
}

#[test]
fn eager_call_keeps_array_prefix_and_all_arguments() {
    check(
        "native func Take(value: Int32, values: [Int32]);
         func Run() { let x = 0; Take(x, [x = 1]); }",
        |block, symbols| {
            assert_eq!(block.stmts.len(), 3, "{block:?}");
            let ir::Stmt::InitArray { elements, .. } = &block.stmts[1] else {
                panic!("{block:?}")
            };
            assert!(matches!(&elements[0], ir::Expr::Assign { .. }));
            let ir::Call::FreeFunction { function, args, .. } = final_call(block) else {
                panic!("{block:?}")
            };
            assert_eq!(
                symbols.get_free_function(*function).unwrap().0.to_string(),
                "Take"
            );
            assert_eq!(args.len(), 2);
            assert!(args.iter().all(|arg| matches!(arg, ir::Expr::Local(..))));
        },
    );
}

#[test]
fn indexed_assignment_keeps_original_destination_and_copy() {
    common::analyze(
        "array_assignment.reds",
        "func Run() { let values: [[Int32]]; values[0] = [1]; }",
        |_, unit, symbols, reporter| {
            assert!(reporter.reported().is_empty(), "{reporter:?}");
            let entry = symbols
                .free_functions()
                .find(|entry| entry.name().to_string() == "Run")
                .unwrap();
            let block = &unit.functions[entry.key()].block;
            assert_eq!(block.stmts.len(), 3, "{block:?}");
            let ir::Stmt::InitArray { local, .. } = &block.stmts[1] else {
                panic!("{block:?}")
            };
            let ir::Stmt::Expr(expr) = &block.stmts[2] else {
                panic!("{block:?}")
            };
            let ir::Expr::Assign { place, expr, .. } = &**expr else {
                panic!("{expr:?}")
            };
            assert!(matches!(&**place, ir::Expr::Index { .. }));
            assert!(matches!(&**expr, ir::Expr::Local(id, _) if id == local));
        },
    );
}

#[test]
fn place_assignments_do_not_warn_about_array_rhs() {
    for source in [
        "func Run() { let values: [Int32]; values = [1]; }",
        "class Host { public let values: [Int32]; } func Run(host: Host) { host.values = [1]; }",
        "class Host { public let values: [Int32]; } func Run(host: Host) { let read: () -> Void = () -> { host.values = [1]; }; }",
        "func Run() { let x = 0; let values: [[Int32]]; values[x = 1] = [x = 2]; }",
        "func Run(values: script_ref<[Int32]>) { Deref(values) = [1]; }",
        "func Run() { let values: [Int32]; let write: () -> Void = () -> { values = [1]; }; }",
    ] {
        common::analyze("array_assignment.reds", source, |_, _, _, reporter| {
            assert!(reporter.reported().is_empty(), "{source}: {reporter:?}");
        });
    }
}

#[test]
fn place_assignments_still_check_unsafe_rhs_subexpressions() {
    check(
        "native func Read(values: [Int32]) -> Int32;
         class Host { public let values: [Int32]; }
         func Run(host: Host) { host.values = [true ? Read([1]) : 0]; }",
        |_, _| {},
    );
}

#[test]
fn closure_callee_keeps_call_even_when_initializer_can_change_it() {
    check(
        "native func Change(out f: ([Int32]) -> Int32) -> Int32;
         func Run(f: ([Int32]) -> Int32) -> Int32 { return f([Change(f)]); }",
        |block, symbols| {
            assert_eq!(block.stmts.len(), 2, "{block:?}");
            let ir::Stmt::InitArray {
                local, elements, ..
            } = &block.stmts[0]
            else {
                panic!("{block:?}")
            };
            let ir::Expr::Call { call, .. } = &elements[0] else {
                panic!("{elements:?}")
            };
            let ir::Call::FreeFunction { function, .. } = &**call else {
                panic!("{call:?}")
            };
            assert_eq!(
                symbols.get_free_function(*function).unwrap().0.to_string(),
                "Change"
            );
            let ir::Call::Closure { closure, args, .. } = final_call(block) else {
                panic!("{block:?}")
            };
            assert!(matches!(&**closure, ir::Expr::Local(..)));
            assert!(matches!(&args[..], [ir::Expr::Local(id, _)] if id == local));
        },
    );
}

#[test]
fn runtime_member_and_expression_callees_report_once_and_keep_calls() {
    for source in [
        "class Host { public func Take(values: [Int32]) {} } func Run(host: Host) { host.Take([1]); }",
        "class Host { public let Take: ([Int32]) -> Int32; } func Run(host: Host) { host.Take([1]); }",
        "native func Get() -> ([Int32]) -> Int32; func Run() { Get()([1]); }",
    ] {
        check(source, |block, _| {
            assert_eq!(block.stmts.len(), 2, "{block:?}");
            assert!(matches!(&block.stmts[0], ir::Stmt::InitArray { .. }));
            assert_eq!(final_call(block).args().len(), 1);
        });
    }
}

#[test]
fn type_qualified_static_callee_allows_array_arguments() {
    common::analyze(
        "array_compatibility.reds",
        "class Host { public static func Take(values: [Int32]) {} }
         func Run() { Host.Take([1]); Host.Take([]); }",
        |_, _, _, reporter| {
            assert!(reporter.reported().is_empty(), "{reporter:?}");
        },
    );
}

#[test]
fn free_function_and_closure_without_array_prefix_remain_valid() {
    common::analyze(
        "array_compatibility.reds",
        "func Take(values: [Int32]) {} func Run(f: ([Int32]) -> Int32, values: [Int32]) {
             Take([1]); f(values);
         }",
        |_, _, _, reporter| {
            assert!(reporter.reported().is_empty(), "{reporter:?}");
        },
    );
}

#[test]
fn guarded_lazy_arguments_keep_legacy_eager_prefix_and_original_call() {
    for overload in ["", "native func Lazy(skip values: String);"] {
        check(
            &format!(
                "native func Read(values: [Int32]) -> Bool;
                native func Lazy(skip values: Bool); {overload}
                func Run() {{ Lazy(Read([1])); }}"
            ),
            |block, symbols| {
                assert_eq!(block.stmts.len(), 2, "{block:?}");
                assert!(matches!(&block.stmts[0], ir::Stmt::InitArray { .. }));
                let ir::Call::FreeFunction { function, args, .. } = final_call(block) else {
                    panic!("{block:?}")
                };
                assert_eq!(
                    symbols.get_free_function(*function).unwrap().0.to_string(),
                    "Lazy"
                );
                let [ir::Expr::Call { call, .. }] = &args[..] else {
                    panic!("{args:?}")
                };
                let ir::Call::FreeFunction { function, args, .. } = &**call else {
                    panic!("{call:?}")
                };
                assert_eq!(
                    symbols.get_free_function(*function).unwrap().0.to_string(),
                    "Read"
                );
                assert!(matches!(&args[..], [ir::Expr::Local(..)]));
            },
        );
    }
}

#[test]
fn logical_and_conditional_expressions_keep_their_return_values() {
    for body in ["false && Read([1])", "true ? Read([1]) : false"] {
        check(
            &format!(
                "native func Read(values: [Int32]) -> Bool; native func OperatorLogicAnd(lhs: Bool, skip rhs: Bool) -> Bool; func Run() -> Bool {{ return {body}; }}"
            ),
            |block, _| {
                assert_eq!(block.stmts.len(), 2, "{block:?}");
                assert!(matches!(&block.stmts[0], ir::Stmt::InitArray { .. }));
                let ir::Stmt::Return(Some(expr), _) = block.stmts.back().unwrap() else {
                    panic!("{block:?}")
                };
                assert!(matches!(
                    &**expr,
                    ir::Expr::Call { .. } | ir::Expr::Conditional { .. }
                ));
            },
        );
    }
}

#[test]
fn distinct_unsafe_statements_each_report_once() {
    common::analyze(
        "arrays.reds",
        "native func Lazy(skip values: [Int32]); func Run() { Lazy([1]); Lazy([2]); }",
        |_, unit, symbols, reporter| {
            assert_eq!(reporter.reported().len(), 2, "{reporter:?}");
            assert!(
                reporter
                    .reported()
                    .iter()
                    .all(|diagnostic| diagnostic.level() == DiagnosticLevel::Warning)
            );
            let entry = symbols
                .free_functions()
                .find(|entry| entry.name().to_string() == "Run")
                .unwrap();
            let block = &unit.functions[entry.key()].block;
            assert_eq!(block.stmts.len(), 4, "{block:?}");
            assert!(!format!("{block:?}").contains("Nop"));
        },
    );
}
