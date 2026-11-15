use redscript_compiler_frontend::{
    LoweredCompilationUnit, LoweredFunction, Symbols, expr_type, ir,
};

use crate::common;

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
         func TakeWeak(const r: script_ref<wref<Object>>) -> Int32 { return 0; }
         func TakeVariant(const r: script_ref<Variant>) -> Int32 { return 0; }
         func WrapWeak(value: ref<Object>) -> Int32 { return TakeWeak(value); }
         func WrapVariant(value: Int32) -> Int32 { return TakeVariant(value); }
         func Field(value: script_ref<wref<Object>>) -> Int32 { return value.number; }
         func Method(value: script_ref<ref<Object>>) -> Int32 { return value.Get(); }
         func Index(value: script_ref<[Int32]>) -> Int32 { return value[0]; }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            for (name, outer, inner) in [
                ("ReadWeak", ir::Intrinsic::RefToWeakRef, ir::Intrinsic::Deref),
                ("ReadStrong", ir::Intrinsic::WeakRefToRef, ir::Intrinsic::Deref),
                ("ReadVariant", ir::Intrinsic::ToVariant, ir::Intrinsic::Deref),
            ] {
                let ir::Expr::Call { call, .. } = returned(function(unit, symbols, name)) else { panic!("outer") };
                let ir::Call::FreeFunction { function: id, .. } = &**call else { panic!("intrinsic") };
                assert_eq!(symbols[*id].intrinsic(), Some(outer), "{name}: {call:#?}");
                let ir::Expr::Call { call, .. } = &call.args()[0] else { panic!("inner") };
                let ir::Call::FreeFunction { function: id, .. } = &**call else { panic!("intrinsic") };
                assert_eq!(symbols[*id].intrinsic(), Some(inner), "{name}: {call:#?}");
            }
            for (name, outer, inner) in [
                ("WrapWeak", ir::Intrinsic::AsRef, ir::Intrinsic::RefToWeakRef),
                ("WrapVariant", ir::Intrinsic::AsRef, ir::Intrinsic::ToVariant),
            ] {
                let ir::Expr::Call { call, .. } = returned(function(unit, symbols, name)) else { panic!("call") };
                let ir::Expr::Call { call, .. } = &call.args()[0] else { panic!("outer") };
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
        |_, _, codes| assert_eq!(codes, ["CONST_FORWARDING"]),
    );
}

#[test]
fn unsafe_array_prefixes_are_diagnosed_while_local_construction_remains_supported() {
    let declarations = "native func OperatorLogicAnd(lhs: Bool, skip rhs: Bool) -> Bool;
        native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool;
        func Read(value: Int32) -> Int32 = value;
        func Take(value: Int32, values: [Int32]) {}
        func Size(values: [Int32]) -> Int32 = ArraySize(values);";
    for body in [
        "let x = 0; Take(Read(x), [x = 1]);",
        "let x = 0; Take(x, [x = 1]);",
        "let x = 0; let values = [Read(x), Size([x = 1])];",
        "let x = 0; let value = false && Size([x = 1]) == 1;",
        "let x = 0; let value = true ? Size([x = 1]) : 0;",
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

#[test]
fn non_const_script_ref_lvalues_reject_temporary_conversions() {
    analyze(
        "class Object {}
         func MutVariant(v: script_ref<Variant>) {}
         func Test(x: Int32) { MutVariant(x); }",
        |_, _, codes| assert_eq!(codes, ["TYPE_ERR"]),
    );
    analyze(
        "class Object {}
         func MutStrong(v: script_ref<ref<Object>>) {}
         func Test(w: wref<Object>) { MutStrong(w); }",
        |_, _, codes| assert_eq!(codes, ["TYPE_ERR"]),
    );
    analyze(
        "class Base {}
         class Derived extends Base {}
         func MutBaseRef(v: script_ref<ref<Base>>) {}
         func Test(d: ref<Derived>) { MutBaseRef(d); }",
        |_, _, codes| assert_eq!(codes, ["TYPE_ERR"]),
    );
    analyze(
        "class Object {}
         func MutWeak(v: script_ref<wref<Object>>) {}
         func Test(o: ref<Object>) { MutWeak(o); }",
        |_, _, codes| assert_eq!(codes, ["TYPE_ERR"]),
    );
    analyze(
        "class Base {}
         class Derived extends Base {}
         func Test(x: Int32, d: ref<Derived>) {
           let a: script_ref<Variant> = x;
           let b: script_ref<ref<Base>> = d;
         }",
        |_, _, codes| assert_eq!(codes, ["TYPE_ERR", "TYPE_ERR"]),
    );
    analyze(
        "class Base {}
         class Derived extends Base {}
         struct BaseS { let a: Int32; }
         struct SubS extends BaseS { let b: Int32; }
         func ConstVariant(const v: script_ref<Variant>) {}
         func ConstStrong(const v: script_ref<ref<Base>>) {}
         func MutVariant(v: script_ref<Variant>) {}
         func MutBaseRef(v: script_ref<ref<Base>>) {}
         func MutBase(s: script_ref<BaseS>) {}
         func Test(x: Int32, w: wref<Base>, d: ref<Derived>, s: SubS) {
           ConstVariant(x);
           ConstStrong(w);
           ConstStrong(d);
           MutVariant(42);
           MutBaseRef(new Derived());
           MutBase(s);
         }",
        |_, _, codes| assert!(codes.is_empty(), "{codes:?}"),
    );
}
