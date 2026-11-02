use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner, infer_from_sources, ir,
};

fn analyze(
    source: &str,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back("switch.reds", source);
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
        .map(|diagnostic| diagnostic.code().to_owned())
        .collect::<Vec<_>>();
    test(&unit, &symbols, &codes);
}

#[test]
fn switch_literals_use_the_scrutinee_type_and_keep_their_values() {
    for (typ, values, expected) in [
        ("Int8", ["-128", "127"], ["I8(-128)", "I8(127)"]),
        ("Uint8", ["0", "255"], ["U8(0)", "U8(255)"]),
        ("Int16", ["-32768", "32767"], ["I16(-32768)", "I16(32767)"]),
        ("Uint16", ["0", "65535"], ["U16(0)", "U16(65535)"]),
        ("Int32", ["-1", "1"], ["I32(-1)", "I32(1)"]),
        ("Uint32", ["0", "1"], ["U32(0)", "U32(1)"]),
        ("Int64", ["-1", "1"], ["I64(-1)", "I64(1)"]),
        ("Uint64", ["0", "1"], ["U64(0)", "U64(1)"]),
        ("Float", ["0", "1"], ["F32(0.0)", "F32(1.0)"]),
        ("Double", ["0", "1.5"], ["F64(0.0)", "F64(1.5)"]),
    ] {
        analyze(
            &format!(
                "func Test(value: {typ}) {{ switch value {{ case {}: case {}: break; }} }}",
                values[0], values[1]
            ),
            |unit, _, codes| {
                assert!(codes.is_empty(), "{typ}: {codes:?}");
                let function = unit.functions.values().next().unwrap();
                let ir::Stmt::Switch { branches, .. } = &function.block.stmts[0] else {
                    panic!("expected switch")
                };
                let actual = branches
                    .iter()
                    .map(|case| {
                        let ir::Expr::Const(value, _) = &case.matcher else {
                            panic!("expected typed constant: {:?}", case.matcher)
                        };
                        format!("{value:?}")
                    })
                    .collect::<Vec<_>>();
                assert_eq!(actual, expected, "{typ}");
            },
        );
    }
}

#[test]
fn switch_literals_reject_overflow_and_incompatible_values() {
    for (typ, value, code) in [
        ("Int8", "-129", "LIT_OUT_OF_RANGE"),
        ("Int8", "128", "LIT_OUT_OF_RANGE"),
        ("Uint8", "-1", "LIT_OUT_OF_RANGE"),
        ("Uint8", "256", "LIT_OUT_OF_RANGE"),
        ("Int16", "-32769", "LIT_OUT_OF_RANGE"),
        ("Int16", "32768", "LIT_OUT_OF_RANGE"),
        ("Uint16", "-1", "LIT_OUT_OF_RANGE"),
        ("Uint16", "65536", "LIT_OUT_OF_RANGE"),
        ("Uint32", "-1", "LIT_OUT_OF_RANGE"),
        ("Uint64", "-1", "LIT_OUT_OF_RANGE"),
        ("Int32", "1.5", "TYPE_ERR"),
        ("Int8", "1.0", "TYPE_ERR"),
        ("Int8", "other", "TYPE_ERR"),
        ("Variant", "1", "TYPE_ERR"),
        ("script_ref<String>", "\"text\"", "TYPE_ERR"),
    ] {
        analyze(
            &format!(
                "func Test(value: {typ}, other: Int32) {{ switch value {{ case {value}: break; }} }}"
            ),
            |_, _, codes| assert_eq!(codes, [code], "{typ}: {value}"),
        );
    }
}

#[test]
fn switch_case_expressions_use_normal_reference_coercion() {
    analyze(
        "func Test(value: Int8, other: script_ref<Int8>) { switch value { case other: break; } }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let function = unit.functions.values().next().unwrap();
            let ir::Stmt::Switch { branches, .. } = &function.block.stmts[0] else {
                panic!("expected switch")
            };
            let ir::Expr::Call { call, .. } = &branches[0].matcher else {
                panic!("expected dereference")
            };
            let ir::Call::FreeFunction { function, .. } = &**call else {
                panic!("expected intrinsic call")
            };
            assert_eq!(symbols[*function].intrinsic(), Some(ir::Intrinsic::Deref));
        },
    );
}
