use std::fmt::Write;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{CompileErrorReporter, FunctionKind, Symbols, TypeInterner};
use redscript_redmod_adapter::{Error, parse_file_with_metadata, process_sources};

fn write_error(output: &mut String, error: &Error) {
    let span = error.span();
    writeln!(
        output,
        "{} [{}..{}]: {}",
        error.code(),
        span.start,
        span.end,
        error
    )
    .unwrap();
}

#[test]
fn semantic_fixtures() {
    insta::glob!("data/*.script", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let file = sources.push_back("fixture.script", source.clone());
        let (parsed, parse_errors) = parse_file_with_metadata(&source, file);
        let mut output = String::new();
        for error in parse_errors
            .iter()
            .filter(|error| matches!(error, Error::Parse(_) | Error::Capability { .. }))
        {
            write_error(&mut output, error);
        }
        let dependencies = sources
            .files()
            .filter(|(id, _)| *id != file)
            .map(|(id, file)| {
                let (module, errors) = redscript_community_parser::parse_module(file.source(), id);
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
        for error in errors {
            write_error(&mut output, &error);
        }
        for error in reporter.into_reported() {
            writeln!(output, "{}: {}", error.code(), error).unwrap();
        }
        let mut declarations = Vec::new();
        for id in unit
            .classes
            .keys()
            .filter(|id| symbols[**id].span().is_some_and(|span| span.file == file))
        {
            if let Some(class) = symbols[*id].schema().as_aggregate() {
                for method in class.methods().iter() {
                    declarations.push(format!(
                        "method {id}.{}: callback={}, return={}",
                        method.name(),
                        method.func().flags().is_callback(),
                        method.func().type_().return_type()
                    ));
                }
                for field in class.fields().iter() {
                    declarations.push(format!(
                        "field {id}.{}: persistent={}, properties={:?}",
                        field.name(),
                        field.field().flags().is_persistent(),
                        field.field().properties()
                    ));
                }
            }
        }
        for id in unit.functions.keys() {
            let function = &symbols[*id];
            if function.span().is_some_and(|span| span.file == file) {
                declarations.push(format!(
                    "function at {}: return={}",
                    function.span().unwrap().start,
                    function.type_().return_type()
                ));
            }
        }
        declarations.sort();
        for declaration in declarations {
            writeln!(output, "{declaration}").unwrap();
        }
        insta::assert_snapshot!(output);
    });
}
