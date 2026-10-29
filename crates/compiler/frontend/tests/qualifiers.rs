use redscript_community_ast::{ItemQualifiers, SourceMap};
use redscript_compiler_frontend::{
    CompileErrorReporter, Diagnostic, Symbols, TypeInterner, infer_from_sources,
};

#[test]
fn qualifiers_are_checked_per_declaration() {
    for (spelling, qualifier) in [
        ("editable", ItemQualifiers::EDITABLE),
        ("instanceeditable", ItemQualifiers::INSTANCE_EDITABLE),
        ("inlined", ItemQualifiers::INLINE),
        ("replicated", ItemQualifiers::REPLICATED),
        ("testonly", ItemQualifiers::TEST_ONLY),
        ("const", ItemQualifiers::CONST),
    ] {
        for declaration in [
            "class Invalid {}",
            "struct Invalid {}",
            "enum Invalid { A = 0 }",
            "func Invalid() {}",
            "class Owner { QUALIFIER func Invalid() {} }",
            "import Missing",
        ] {
            if qualifier == ItemQualifiers::TEST_ONLY
                && matches!(declaration, "class Invalid {}" | "struct Invalid {}")
            {
                continue;
            }
            let code = if declaration.contains("QUALIFIER") {
                declaration.replace("QUALIFIER", spelling)
            } else {
                format!("{spelling} {declaration}")
            };
            let sources = SourceMap::new();
            sources.push_back("invalid.reds", code);
            let interner = TypeInterner::default();
            let mut reporter = CompileErrorReporter::default();
            infer_from_sources(
                &sources,
                Symbols::with_default_types(),
                &mut reporter,
                &interner,
            );
            let unused = reporter
                .reported()
                .iter()
                .filter_map(|diagnostic| match diagnostic {
                    Diagnostic::UnusedItemQualifiers(flags, _) => Some(*flags),
                    _ => None,
                })
                .collect::<Vec<_>>();
            assert_eq!(unused, [qualifier], "{spelling} {declaration}");
        }
    }
}

#[test]
fn hints_require_one_string_argument_and_a_field() {
    for code in [
        "class Invalid { @hint() let value: Int32; }",
        "class Invalid { @hint(1) let value: Int32; }",
        "class Invalid { @hint(\"a\", \"b\") let value: Int32; }",
        "@hint(\"text\") class Invalid {}",
        "@hint(\"text\") struct Invalid {}",
        "@hint(\"text\") enum Invalid { A = 0 }",
        "@hint(\"text\") func Invalid() {}",
        "@hint(\"text\") import Missing",
        "class Owner { @hint(\"text\") func Invalid() {} }",
    ] {
        let sources = SourceMap::new();
        sources.push_back("invalid.reds", code);
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(
            reporter
                .reported()
                .iter()
                .any(|d| matches!(d, Diagnostic::UnknownAnnotation("hint", _))),
            "{code}: {:?}",
            reporter.reported(),
        );
    }
}
