use redscript_community_ast::{EnumStorage, SourceMap};
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

#[test]
fn native_storage_boundaries_and_mixed_dependency_queries() {
    for (storage, min, max) in [
        ("Int8", -128, 127),
        ("Uint8", 0, 255),
        ("Int16", -32_768, 32_767),
        ("Uint16", 0, 65_535),
        ("Int32", i64::from(i32::MIN), i64::from(i32::MAX)),
        ("Uint32", 0, i64::from(u32::MAX)),
        ("Int64", -9_223_372_036_854_775_807, i64::MAX),
    ] {
        let sources = SourceMap::new();
        let source = format!(
            "enum E: {storage} {{ Min = {min}, Max = {max} }} function Get() -> Dependency {{ return Dependency.A; }}"
        );
        let file = sources.push_back("enum.script", &source);
        let (parsed, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{storage}: {errors:?}");
        let dep_source = "enum Dependency: Uint16 { A = 65535 }";
        let dep = sources.push_back("dependency.reds", dep_source);
        let (dependency, errors) = redscript_community_parser::parse_module(dep_source, dep);
        assert!(errors.is_empty(), "{errors:?}");

        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, symbols, errors) = process_sources(
            parsed,
            vec![dependency.unwrap()],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        for (name, expected) in [
            ("E", EnumStorage::from_name(storage)),
            ("Dependency", Some(EnumStorage::Uint16)),
        ] {
            let enm = symbols[interner.intern(name)].schema().as_enum().unwrap();
            assert_eq!(enm.storage(), expected);
        }
        assert!(reporter.reported().is_empty(), "{:?}", reporter.reported());
    }
}

#[test]
fn native_explicit_and_implicit_overflow_are_shared_diagnostics() {
    for source in [
        "enum E: Uint8 { A = -1 }",
        "enum E: Int8 { A = 128 }",
        "enum E: Uint8 { A = 255, B }",
        "enum E: Int64 { A = 9223372036854775807, B }",
        "enum E: Uint32 { A = 4294967296 }",
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("enum.script", source);
        let (parsed, errors) = parse_file_with_metadata(source, file);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            parsed,
            vec![],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(
            reporter.reported().len(),
            1,
            "{source}: {:?}",
            reporter.reported()
        );
        assert_eq!(reporter.reported()[0].code(), "VAL_OVERFLOW");
    }
    for storage in ["Uint64", "Float", "Bool", "Missing"] {
        let sources = SourceMap::new();
        let source = format!("enum E: {storage} {{ A }}");
        let file = sources.push_back("enum.script", &source);
        let (parsed, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            parsed,
            vec![],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(reporter.reported().len(), 1);
        assert_eq!(reporter.reported()[0].code(), "INVALID_ENUM_STORAGE");
    }
}
