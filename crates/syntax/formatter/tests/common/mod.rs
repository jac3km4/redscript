use std::path::Path;

use redscript_ast::{FileId, SourceMap};
use redscript_formatter::{FormatSettings, format_document};
use redscript_parser::{Token, lex_with_lf_and_comments, parse_module};

#[track_caller]
pub fn format(source: &str, settings: &FormatSettings, path: impl AsRef<Path>) -> String {
    let path = path.as_ref();
    let sources = SourceMap::new();
    let id = sources.push_back(path, source.to_owned());
    let (formatted, errors) = format_document(source, id, settings);
    let (Some(formatted), []) = (formatted, &errors[..]) else {
        panic!(
            "failed to format {}: {errors:?}\nsource:\n{source}",
            path.display()
        );
    };
    formatted.to_string()
}

#[track_caller]
pub fn assert_preserved(source: &str, settings: &FormatSettings, path: impl AsRef<Path>) -> String {
    let path = path.as_ref();
    let formatted = format(source, settings, path);
    let sources = SourceMap::new();
    let id = sources.push_back(path, source.to_owned());
    let (original_ast, errors) = parse_module(source, id);
    let (Some(original_ast), []) = (original_ast, &errors[..]) else {
        panic!(
            "failed to parse original {}: {errors:?}\nsource:\n{source}",
            path.display()
        );
    };
    let (formatted_ast, errors) = parse_module(&formatted, id);
    let (Some(formatted_ast), []) = (formatted_ast, &errors[..]) else {
        panic!(
            "failed to parse formatted {}: {errors:?}\nsource:\n{source}\nformatted:\n{formatted}",
            path.display()
        );
    };
    assert_eq!(
        original_ast.unwrapped(),
        formatted_ast.unwrapped(),
        "formatting changed the AST for {}\nsource:\n{source}\nformatted:\n{formatted}",
        path.display()
    );
    assert_eq!(
        ordered_comments(source, id),
        ordered_comments(&formatted, id),
        "formatting changed the ordered comment text for {}\nsource:\n{source}\nformatted:\n{formatted}",
        path.display()
    );
    assert_eq!(
        formatted,
        format(&formatted, settings, path),
        "formatting is not idempotent for {}\nsource:\n{source}",
        path.display()
    );
    formatted
}

#[track_caller]
pub fn ordered_comments(source: &str, id: FileId) -> Vec<&str> {
    fn collect<'src, S>(tokens: &[(Token<'src, S>, S)], comments: &mut Vec<&'src str>) {
        for (token, _) in tokens {
            match token {
                Token::LineComment(text) | Token::DocComment(text) | Token::BlockComment(text) => {
                    comments.push(text);
                }
                Token::Group(tokens) | Token::InterpStr(tokens) => collect(tokens, comments),
                _ => {}
            }
        }
    }

    let (tokens, errors) = lex_with_lf_and_comments(source, id);
    let (Some(tokens), []) = (tokens, &errors[..]) else {
        panic!("failed to lex comments: {errors:?}\nsource:\n{source}");
    };
    let mut comments = Vec::new();
    collect(&tokens, &mut comments);
    comments
}
