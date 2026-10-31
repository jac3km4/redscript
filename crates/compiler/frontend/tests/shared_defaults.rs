use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, LoweredFunction, Symbols, TypeInterner,
    infer_from_sources, ir,
};

fn analyze(
    source: &str,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.push_back("shared.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let (unit, symbols) = infer_from_sources(
        &sources,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    let codes = reporter
        .reported()
        .iter()
        .map(|d| d.code().to_owned())
        .collect::<Vec<_>>();
    test(&unit, &symbols, &codes);
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

