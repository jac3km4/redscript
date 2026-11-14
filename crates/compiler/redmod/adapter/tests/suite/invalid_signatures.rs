use redscript_community_ast::SourceMap;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner};
use redscript_redmod_adapter::{parse_file_with_metadata, process_sources};

#[test]
fn invalid_override_signatures_and_base_candidates_do_not_generate_adapter_cascades() {
    for (base, child) in [
        ("value: Int32", "id: Missing, value: Int32"),
        ("id: Missing, value: Int32", "value: Int32"),
        (
            "id: Missing, value: Int32",
            "id: OtherMissing, value: Int32",
        ),
    ] {
        let source = format!(
            "class Base {{ public function Test({base}) {{}} }} class Child extends Base {{ public override function Test({child}) {{}} }}"
        );
        let sources = SourceMap::new();
        let file = sources.push_back("invalid.script", source.as_str());
        let (module, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            module,
            [],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{source}: {errors:?}");
        assert!(!reporter.reported().is_empty());
        assert!(
            reporter
                .reported()
                .iter()
                .all(|d| d.to_string().contains("is not a known type")),
            "{:?}",
            reporter.reported()
        );
    }
}

#[test]
fn invalid_generic_arity_does_not_validate_override_or_body_contracts() {
    use redscript_compiler_frontend::{CtxVar, TypeDef, TypeSchema, Variance};

    for (base_return, child_return) in [("GenericType", "Int32"), ("Int32", "GenericType")] {
        let source = format!(
            "class Base {{ public function Test() -> {base_return} {{ return 1; }} }} class Child extends Base {{ public override function Test() -> {child_return} {{ return 1; }} }}"
        );
        let sources = SourceMap::new();
        let file = sources.push_back("arity.script", source.as_str());
        let (module, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut symbols = Symbols::with_default_types();
        symbols.add_type(
            interner.intern("GenericType"),
            TypeDef::new(
                [CtxVar::new("T", Variance::Invariant, None, None).into()],
                TypeSchema::Primitive,
                [],
            ),
        );
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(module, [], symbols, &mut reporter, &interner);
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(reporter.reported().len(), 1, "{:?}", reporter.reported());
        assert!(
            reporter.reported()[0]
                .to_string()
                .contains("type `GenericType` expects 1 type argument, but found 0")
        );
    }
}

#[test]
fn invalid_explicit_returns_do_not_validate_override_or_body_contracts() {
    for (base_return, child_return) in [("Missing", "Int32"), ("Int32", "Missing")] {
        let source = format!(
            "class Base {{ public function Test() -> {base_return} {{ return 1; }} }} class Child extends Base {{ public override function Test() -> {child_return} {{ return 1; }} }}"
        );
        let sources = SourceMap::new();
        let file = sources.push_back("return.script", source.as_str());
        let (module, errors) = parse_file_with_metadata(&source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_, _, errors) = process_sources(
            module,
            [],
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(reporter.reported().len(), 1, "{:?}", reporter.reported());
        assert!(
            reporter.reported()[0]
                .to_string()
                .contains("`Missing` is not a known type")
        );
    }
}
