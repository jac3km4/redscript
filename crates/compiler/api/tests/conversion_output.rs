use redscript_community_ast::SourceMap;
use redscript_compiler_api::{Compilation, FlushError, SourceMapExt};
use redscript_compiler_frontend::TypeInterner;
use redscript_io::ScriptBundle;

#[test]
fn unsupported_provider_bindings_do_not_replace_output() {
    // Community-marked imports do not establish a supported native provider.
    for declaration in [
        "native explicit func Cast(value: Int32) -> Float;",
        "explicit func Cast(value: Int32) -> Float { return 1.0; }",
        "native explicit func Cast(value: Missing) -> Float;",
    ] {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back("provider.reds", declaration);
        sources.push_back(
            "consumer.reds",
            "func Use(value: Int32) -> Float { return Cast<Float>(value); }",
        );
        let interner = TypeInterner::default();
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let compilation = Compilation::builder()
            .bundle(&bytes)
            .sources(&sources)
            .type_interner(&interner)
            .compile()
            .unwrap();
        assert!(compilation.has_fatal_errors());
        let path = std::env::temp_dir().join(format!(
            "redscript-conversion-{}-{}.redscripts",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::write(&path, b"unchanged").unwrap();
        let result = compilation.flush(&path);
        let contents = std::fs::read(&path).unwrap();
        std::fs::remove_file(path).unwrap();
        assert!(matches!(result, Err(FlushError::FatalErrors(..))));
        assert_eq!(contents, b"unchanged");
    }
}
