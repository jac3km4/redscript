use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{
    CompileErrorReporter, LoweredCompilationUnit, Symbols, TypeInterner, ir,
};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

fn analyze(
    source: &str,
    test: impl for<'ctx> FnOnce(&LoweredCompilationUnit<'ctx>, &Symbols<'ctx>, &[String]),
) {
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let file = sources.push_back("switch.script", source);
    let (parsed, errors) = parse_file_with_metadata(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let dependencies = sources
        .files()
        .filter(|(id, _)| *id != file)
        .map(|(id, source)| {
            let (module, errors) = redscript_community_parser::parse_module(source.source(), id);
            assert!(errors.is_empty(), "{errors:?}");
            module.unwrap()
        })
        .collect::<Vec<_>>();
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
        .map(|diagnostic| diagnostic.code().to_owned())
        .collect::<Vec<_>>();
    test(&unit, &symbols, &codes);
}

#[test]
fn native_switch_literals_have_the_scrutinee_representation() {
    for (typ, values, expected) in [
        ("Int8", ["-128", "127"], ["I8(-128)", "I8(127)"]),
        ("Uint8", ["0", "255"], ["U8(0)", "U8(255)"]),
        ("Int16", ["-32768", "32767"], ["I16(-32768)", "I16(32767)"]),
        ("Uint16", ["0", "65535"], ["U16(0)", "U16(65535)"]),
        ("Uint32", ["0", "1"], ["U32(0)", "U32(1)"]),
        ("Int64", ["-1", "1"], ["I64(-1)", "I64(1)"]),
        ("Uint64", ["0", "1"], ["U64(0)", "U64(1)"]),
        ("Double", ["0", "1.5"], ["F64(0.0)", "F64(1.5)"]),
    ] {
        analyze(
            &format!(
                "function Test(value: {typ}) {{ switch (value) {{ case {}: case {}: break; }} }}",
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
fn native_switch_literals_do_not_relax_ranges_or_guess_variant_semantics() {
    for (typ, value, code) in [
        ("Int8", "-129", "LIT_OUT_OF_RANGE"),
        ("Int8", "128", "LIT_OUT_OF_RANGE"),
        ("Uint8", "-1", "LIT_OUT_OF_RANGE"),
        ("Uint8", "256", "LIT_OUT_OF_RANGE"),
        ("Int16", "32768", "LIT_OUT_OF_RANGE"),
        ("Uint16", "65536", "LIT_OUT_OF_RANGE"),
        ("Uint32", "-1", "LIT_OUT_OF_RANGE"),
        ("Uint64", "-1", "LIT_OUT_OF_RANGE"),
        ("Int32", "1.5", "TYPE_ERR"),
        ("Int8", "1.0", "TYPE_ERR"),
        ("Int8", "other", "TYPE_ERR"),
        ("Variant", "1", "TYPE_ERR"),
        ("ref<String>", "\"text\"", "TYPE_ERR"),
    ] {
        analyze(
            &format!(
                "function Test(value: {typ}, other: Int32) {{ switch (value) {{ case {value}: break; }} }}"
            ),
            |_, _, codes| assert_eq!(codes, [code], "{typ}: {value}"),
        );
    }
}

#[test]
fn native_case_calls_keep_order_default_position_fallthrough_and_continue() {
    analyze(
        "function Read(): Uint8 { return 255; }
         function Test(value: Uint8, run: Bool) {
           while (run) {
             switch (Read()) {
               case 0: Read();
               default: continue;
               case Read(): break;
               case 255: continue;
             }
             break;
           }
         }",
        |unit, symbols, codes| {
            assert!(codes.is_empty(), "{codes:?}");
            let test = symbols
                .free_functions()
                .find(|entry| entry.name().to_string() == "Test")
                .unwrap();
            let ir::Stmt::While(condition, _) = &unit.functions[test.key()].block.stmts[0] else {
                panic!("expected while")
            };
            let ir::Stmt::Switch {
                scrutinee,
                branches,
                default_index,
                default,
                ..
            } = &condition.block.stmts[0]
            else {
                panic!("expected switch at execution point")
            };
            assert!(matches!(&**scrutinee, ir::Expr::Call { .. }));
            assert_eq!(*default_index, 1);
            assert!(matches!(
                &branches[0].matcher,
                ir::Expr::Const(ir::Const::U8(0), _)
            ));
            assert!(matches!(&branches[1].matcher, ir::Expr::Call { .. }));
            assert!(matches!(
                &branches[2].matcher,
                ir::Expr::Const(ir::Const::U8(255), _)
            ));
            assert_eq!(branches[0].block.stmts.len(), 1);
            assert!(matches!(&branches[0].block.stmts[0], ir::Stmt::Expr(_)));
            assert!(matches!(
                &default.as_ref().unwrap().stmts[0],
                ir::Stmt::Continue(_)
            ));
            assert!(matches!(&branches[1].block.stmts[0], ir::Stmt::Break(_)));
            assert!(matches!(&branches[2].block.stmts[0], ir::Stmt::Continue(_)));
        },
    );
}
