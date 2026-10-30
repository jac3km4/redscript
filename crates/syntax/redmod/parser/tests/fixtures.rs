use std::fmt::Write;
use std::fs;

use redscript_community_ast::SourceMap;

#[test]
fn source_ast() {
    insta::glob!("data/ast/*.script", |path| {
        let source = fs::read_to_string(path).unwrap().replace("\r\n", "\n");
        let sources = SourceMap::new();
        let file = sources.push_back(path.file_name().unwrap(), &source);
        let (parsed, errors) = redscript_redmod_parser::parse_module(&source, file);
        assert!(errors.is_empty(), "{errors:?}");
        let parsed = parsed.unwrap();
        // Keep spans, grouped syntax and modifiers in the native AST snapshot.
        insta::assert_debug_snapshot!(parsed);
    });
}

#[test]
fn source_diagnostics() {
    insta::glob!("data/diagnostics/*.script", |path| {
        let source = fs::read_to_string(path).unwrap().replace("\r\n", "\n");
        let sources = SourceMap::new();
        let file = sources.push_back(path.file_name().unwrap(), &source);
        let (module, errors) = redscript_redmod_parser::parse_module(&source, file);
        assert!(!errors.is_empty(), "{}", path.display());
        let mut output = String::new();
        for error in errors {
            writeln!(
                output,
                "{}",
                error.display(&sources).unwrap_or_else(|| panic!(
                    "invalid diagnostic span in {}: {error:?}",
                    path.display()
                ))
            )
            .unwrap();
        }
        // Recovery must retain usable siblings, not merely emit diagnostics.
        writeln!(output, "Recovered AST:\n{module:#?}").unwrap();
        insta::assert_snapshot!(output);
    });
}
