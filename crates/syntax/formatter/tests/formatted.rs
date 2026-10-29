use std::env;

use redscript_ast::SourceMap;
use redscript_formatter::{FormatSettings, format_document};
use redscript_parser::parse_module;

#[test]
fn formatted_files() {
    insta::glob!("data/*.reds", |path| {
        let current = env::current_dir().unwrap().canonicalize().unwrap();
        let relative = path.strip_prefix(&current).unwrap();
        let sources = SourceMap::from_files([relative]).unwrap();

        let settings = FormatSettings::default();
        for (id, file) in sources.files() {
            let (module, errors) = format_document(file.source(), id, &settings);
            if let (Some(module), []) = (module, &errors[..]) {
                let formatted = module.to_string();
                let (original_ast, errors) = parse_module(file.source(), id);
                let (Some(original_ast), []) = (original_ast, &errors[..]) else {
                    panic!(
                        "failed to parse original {}: {errors:?}",
                        file.path().display()
                    );
                };
                let (formatted_ast, errors) = parse_module(&formatted, id);
                let (Some(formatted_ast), []) = (formatted_ast, &errors[..]) else {
                    panic!(
                        "failed to parse formatted {}: {errors:?}",
                        file.path().display()
                    );
                };

                let (reformatted, errors) = format_document(&formatted, id, &settings);
                let (Some(reformatted), []) = (reformatted, &errors[..]) else {
                    panic!("failed to reformat {}: {errors:?}", file.path().display());
                };
                assert_eq!(
                    formatted,
                    reformatted.to_string(),
                    "formatting is not idempotent for {}",
                    file.path().display()
                );
                assert_eq!(
                    original_ast.unwrapped(),
                    formatted_ast.unwrapped(),
                    "formatting changed the AST for {}",
                    file.path().display()
                );
                insta::assert_snapshot!(formatted);
            } else {
                panic!("failed to parse {}: {errors:?}", file.path().display());
            }
        }
    });
}
