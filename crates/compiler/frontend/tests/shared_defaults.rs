mod common;

use redscript_compiler_frontend::{
    LoweredCompilationUnit, LoweredFunction, Symbols, expr_type, ir,
};

fn analyze(
    source: &str,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    common::analyze("shared.reds", source, |_, unit, symbols, reporter| {
        let codes = reporter
            .reported()
            .iter()
            .map(|d| d.code().to_owned())
            .collect::<Vec<_>>();
        test(unit, symbols, &codes);
    });
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

fn returned<'a, 'ctx>(function: &'a LoweredFunction<'ctx>) -> &'a ir::Expr<'ctx> {
    let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
        panic!("expected return")
    };
    expr
}

#[test]
fn assignments_yield_coerced_types_without_readback_or_place_identity() {
    analyze(
        "native func OperatorLogicAnd(lhs: Bool, rhs: Bool) -> Bool
                 func Chain(out a: Variant, out b: Int32) -> Variant { return a = b = 7; }
         func Indexed(out a: [Int32], i: Int32) -> Int32 { return a[i] = 9; }
         func Conditional(out a: Int32, flag: Bool) -> Int32 { return flag ? (a = 1) : (a = 2); }
         func Lazy(out a: Bool, flag: Bool) -> Bool { return flag && (a = true); }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for (name, typ) in [
                ("Chain", "Variant"),
                ("Indexed", "Int32"),
                ("Conditional", "Int32"),
                ("Lazy", "Bool"),
            ] {
                let function = function(unit, symbols, name);
                assert_eq!(function.block.stmts.len(), 1);
                assert_eq!(
                    expr_type(returned(function), function, symbols)
                        .unwrap()
                        .to_string(),
                    typ
                );
            }
            let ir::Expr::Assign { expr, .. } = returned(function(unit, symbols, "Chain")) else {
                panic!("assignment")
            };
            let ir::Expr::Call { call, .. } = &**expr else {
                panic!("coercion")
            };
            assert!(matches!(&call.args()[0], ir::Expr::Assign { .. }));
            let ir::Expr::Assign { place, .. } = returned(function(unit, symbols, "Indexed"))
            else {
                panic!("assignment")
            };
            assert!(matches!(&**place, ir::Expr::Index { .. }));
        },
    );
    analyze(
        "func Invalid(out a: Int32) { (a = 1) = 2; }",
        |_, _, codes| {
            assert_eq!(codes, ["INVALID_PLACE"]);
        },
    );
}

#[test]
fn imported_callback_prototypes_are_shared_but_not_native_or_implemented() {
    analyze(
        "native class Listener { protected cb func OnReady(); }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let (id, _) = symbols
                .types()
                .find(|(id, _)| id.to_string() == "Listener")
                .unwrap();
            let method = symbols[id]
                .schema()
                .as_aggregate()
                .unwrap()
                .methods()
                .iter()
                .next()
                .unwrap();
            assert!(method.func().flags().is_callback());
            assert!(!method.func().flags().is_native());
            assert!(method.func().flags().is_unimplemented());
            assert!(unit.classes[&id].methods.is_empty());
        },
    );
    for source in [
        "class Listener { protected cb func OnReady(); }",
        "native importonly class Listener { protected cb func OnReady(); }",
        "native class Listener { protected func OnReady(); }",
    ] {
        analyze(source, |_, _, codes| {
            assert!(codes.iter().any(|code| code == "MISSING_BODY"), "{codes:?}");
        });
    }
}

#[test]
fn script_reference_coercions_compose_and_receivers_unwrap_by_default() {
    analyze(
        "class Object { public let number: Int32; public func Get() -> Int32 { return this.number; } }
         func ReadWeak(value: script_ref<ref<Object>>) -> wref<Object> { return value; }
         func ReadStrong(value: script_ref<wref<Object>>) -> ref<Object> { return value; }
         func ReadVariant(value: script_ref<Int32>) -> Variant { return value; }
         func WrapWeak(value: ref<Object>) -> script_ref<wref<Object>> { return value; }
         func WrapVariant(value: Int32) -> script_ref<Variant> { return value; }
         func Field(value: script_ref<wref<Object>>) -> Int32 { return value.number; }
         func Method(value: script_ref<ref<Object>>) -> Int32 { return value.Get(); }
         func Index(value: script_ref<[Int32]>) -> Int32 { return value[0]; }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for (name, outer, inner) in [
                ("ReadWeak", ir::Intrinsic::RefToWeakRef, ir::Intrinsic::Deref),
                ("ReadStrong", ir::Intrinsic::WeakRefToRef, ir::Intrinsic::Deref),
                ("ReadVariant", ir::Intrinsic::ToVariant, ir::Intrinsic::Deref),
                ("WrapWeak", ir::Intrinsic::AsRef, ir::Intrinsic::RefToWeakRef),
                ("WrapVariant", ir::Intrinsic::AsRef, ir::Intrinsic::ToVariant),
            ] {
                let ir::Expr::Call { call, .. } = returned(function(unit, symbols, name)) else { panic!("outer") };
                let ir::Call::FreeFunction { function: id, .. } = &**call else { panic!("intrinsic") };
                assert_eq!(symbols[*id].intrinsic(), Some(outer), "{name}: {call:#?}");
                let ir::Expr::Call { call, .. } = &call.args()[0] else { panic!("inner") };
                let ir::Call::FreeFunction { function: id, .. } = &**call else { panic!("intrinsic") };
                assert_eq!(symbols[*id].intrinsic(), Some(inner), "{name}: {call:#?}");
            }
            for name in ["Field", "Method", "Index"] {
                let receiver = match returned(function(unit, symbols, name)) {
                    ir::Expr::Field { receiver, .. } => &**receiver,
                    ir::Expr::Index { array, .. } => &**array,
                    ir::Expr::Call { call, .. } => {
                        let ir::Call::Instance { receiver, .. } = &**call else { panic!("method receiver") };
                        &**receiver
                    }
                    _ => panic!("receiver expression"),
                };
                let ir::Expr::Call { call, .. } = receiver else { panic!("unwrapped receiver") };
                let ir::Call::FreeFunction { function, .. } = &**call else { panic!("dereference") };
                assert_eq!(symbols[*function].intrinsic(), Some(ir::Intrinsic::Deref), "{name}");
            }
        },
    );
}

#[test]
fn omitted_arguments_are_inert_before_array_prefixes() {
    analyze(
        "func Take(opt value: Int32, values: [Int32]) {}
         func Test() { Take(,[1]); Take(, [1],); }",
        |_, _, codes| assert!(codes.is_empty(), "{codes:?}"),
    );
    analyze(
        "class Value {} func Take(opt unused: Int32, values: [Int32], value: ref<Value>) {}
         func Test(const value: wref<Value>) { Take(, [1], WeakRefToRef(value),); }",
        |_, _, codes| assert_eq!(codes, ["OTHER"]),
    );
}

#[test]
fn unsafe_array_prefixes_fail_closed_while_local_construction_remains_supported() {
    let declarations = "func Read(value: Int32) -> Int32 = value;
        func Take(value: Int32, values: [Int32]) {}
        func Size(values: [Int32]) -> Int32 = ArraySize(values);";
    for body in [
        "let x = 0; Take(Read(x), [x = 1]);",
        "let x = 0; Take(x, [x = 1]);",
        "let x = 0; let values = [Read(x), Size([x = 1])];",
        "let x = 0; let value = false && Size([x = 1]) == 1;",
        "let x = 0; let value = true ? Size([x = 1]) : 0;",
        "let x = 0; let values: [[Int32]]; values[x = 1] = [x = 2];",
    ] {
        analyze(
            &format!("{declarations} func Test() {{ {body} }}"),
            |_, _, codes| {
                assert_eq!(codes, ["UNSAFE_ARRAY_EVALUATION"], "{body}");
            },
        );
    }
    analyze(
        &format!(
            "{declarations} func Test() {{ let x = 0; let values = [x = 1, x = 2]; Take(x, values); let nested = [[1], [2]]; }}"
        ),
        |_, _, codes| assert!(codes.is_empty(), "{codes:?}"),
    );
}
