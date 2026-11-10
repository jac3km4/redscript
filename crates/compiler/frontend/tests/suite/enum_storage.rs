use redscript_community_ast::{EnumStorage, SourceMap};
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, Symbols, TypeInterner, infer_from_sources,
};

#[test]
fn explicit_storage_boundaries_and_queries() {
    for (name, min, max) in [
        ("Int8", -128, 127),
        ("Uint8", 0, 255),
        ("Int16", -32_768, 32_767),
        ("Uint16", 0, 65_535),
        ("Int32", i64::from(i32::MIN), i64::from(i32::MAX)),
        ("Uint32", 0, i64::from(u32::MAX)),
        ("Int64", i64::MIN, i64::MAX),
    ] {
        let sources = SourceMap::new();
        sources.push_back(
            "enum.reds",
            format!("enum E: {name} {{ Min = {min}l, Max = {max}l }}"),
        );

        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(
            reporter.reported().is_empty(),
            "{name}: {:?}",
            reporter.reported()
        );
        let enm = symbols[interner.intern("E")].schema().as_enum().unwrap();
        assert_eq!(enm.storage(), EnumStorage::from_name(name));
        assert_eq!(
            enm.variants().collect::<Vec<_>>(),
            [("Min", min), ("Max", max)]
        );
    }
}

#[test]
fn storage_type_queries_retain_the_written_span() {
    let source = "enum Choice: Uint16 { A }";
    let sources = SourceMap::new();
    let file = sources.push_back("enum.reds", source);
    let (module, errors) = redscript_community_parser::parse_module(source, file);
    assert!(errors.is_empty(), "{errors:?}");
    let module = module.unwrap();
    let position = source.find("Uint16").unwrap() as u32;
    let Some(redscript_community_ast::QueryResult::Type(redscript_community_ast::Type::Named {
        name,
        ..
    })) = module.find_at(position)
    else {
        panic!("missing storage type query")
    };
    assert_eq!(*name, "Uint16");
    let redscript_community_ast::Item::Enum(enm) = &module.items[0].0.item else {
        panic!()
    };
    let (_, span) = enm.storage.as_ref().unwrap();
    assert_eq!(&source[span.start as usize..span.end as usize], "Uint16");
}

#[test]
fn invalid_storage_and_explicit_and_increment_overflow() {
    for storage in [
        "Uint64",
        "Float",
        "Bool",
        "String",
        "Missing",
        "array<Int32>",
        "Int8<Int32>",
    ] {
        let sources = SourceMap::new();
        sources.push_back("enum.reds", format!("enum E: {storage} {{ A }}"));
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(
            matches!(reporter.reported(), [Diagnostic::InvalidEnumStorage(_)]),
            "{storage}: {:?}",
            reporter.reported()
        );
    }
    for (storage, value) in [
        ("Int8", -129),
        ("Int8", 128),
        ("Uint8", -1),
        ("Uint8", 256),
        ("Int16", -32_769),
        ("Int16", 32_768),
        ("Uint16", -1),
        ("Uint16", 65_536),
        ("Int32", i64::from(i32::MAX) + 1),
        ("Uint32", i64::from(u32::MAX) + 1),
    ] {
        let sources = SourceMap::new();
        sources.push_back("enum.reds", format!("enum E: {storage} {{ A = {value}l }}"));
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(
            matches!(
                reporter.reported(),
                [Diagnostic::EnumStorageOverflow(_, _, _)]
            ),
            "{storage}: {:?}",
            reporter.reported()
        );
    }
    for (storage, max) in [
        ("Int8", 127),
        ("Uint8", 255),
        ("Int16", 32_767),
        ("Uint16", 65_535),
        ("Int32", i64::from(i32::MAX)),
        ("Uint32", i64::from(u32::MAX)),
        ("Int64", i64::MAX),
    ] {
        let sources = SourceMap::new();
        sources.push_back(
            "enum.reds",
            format!("enum E: {storage} {{ A = {max}l, B }}"),
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert_eq!(
            reporter.reported().len(),
            1,
            "{storage}: {:?}",
            reporter.reported()
        );
        assert_eq!(reporter.reported()[0].code(), "VAL_OVERFLOW");
    }
}
