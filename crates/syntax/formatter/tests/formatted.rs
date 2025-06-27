use std::env;

use redscript_ast::SourceMap;
use redscript_formatter::{FormatSettings, format_document};

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
                insta::assert_snapshot!(module);
            } else {
                panic!("failed to parse {}: {errors:?}", file.path().display());
            }
        }
    });
}
