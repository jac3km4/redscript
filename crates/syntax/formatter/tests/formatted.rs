use std::env;

use redscript_ast::{FileId, SourceMap};
use redscript_formatter::{FormatSettings, format_document};
use redscript_parser::{Token, lex_with_lf_and_comments, parse_module};

fn ordered_comments(source: &str, id: FileId) -> Vec<&str> {
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
    assert!(errors.is_empty(), "failed to lex comments: {errors:?}");
    let tokens = tokens.expect("missing tokens when collecting comments");
    let mut comments = Vec::new();
    collect(&tokens, &mut comments);
    comments
}

fn assert_comments_differ(original: &str, changed: &str) {
    let sources = SourceMap::new();
    let id = sources.push_back("test.reds", original.to_owned());
    assert_ne!(
        ordered_comments(original, id),
        ordered_comments(changed, id),
        "comment comparison did not detect a changed sequence"
    );
}

const COMMENTED_CHAIN: &str = "\
/// documentation
func F() {
    return /* chain prefix */ value.Member.Method();
    // before closing brace
}";

#[test]
fn formatting_preserves_chain_and_closing_brace_comments() {
    let sources = SourceMap::new();
    let id = sources.push_back("test.reds", COMMENTED_CHAIN.to_owned());
    let (formatted, errors) = format_document(COMMENTED_CHAIN, id, &FormatSettings::DEFAULT);
    assert!(errors.is_empty(), "failed to format comments: {errors:?}");
    let formatted = formatted.expect("missing formatted document").to_string();
    assert_eq!(
        ordered_comments(COMMENTED_CHAIN, id),
        ordered_comments(&formatted, id),
    );
}

#[test]
fn comment_comparison_detects_loss_before_closing_brace() {
    assert_comments_differ(
        COMMENTED_CHAIN,
        &COMMENTED_CHAIN.replace("    // before closing brace\n", ""),
    );
}

#[test]
fn comment_comparison_detects_duplicated_chain_prefix() {
    assert_comments_differ(
        COMMENTED_CHAIN,
        &COMMENTED_CHAIN.replace(
            "/* chain prefix */",
            "/* chain prefix */ /* chain prefix */",
        ),
    );
}

#[test]
fn comment_comparison_detects_reordering() {
    assert_comments_differ(
        COMMENTED_CHAIN,
        "\
/// documentation
func F() {
    // before closing brace
    return /* chain prefix */ value.Member.Method();
}",
    );
}

#[test]
fn ordered_comments_include_nested_interpolations_but_not_string_contents() {
    let source = r#"
/// documentation
func F() {
    let plain = "// not a comment /* either */";
    return s"/* not a comment */ \(/* outer */ s"\(/* inner */ value)")";
    // before closing brace
}
"#;
    let sources = SourceMap::new();
    let id = sources.push_back("test.reds", source.to_owned());
    assert_eq!(
        ordered_comments(source, id),
        [
            "/// documentation",
            "/* outer */",
            "/* inner */",
            "// before closing brace",
        ]
    );
    assert_eq!(
        ordered_comments(source, id),
        ordered_comments(&source.replace("    ", "  "), id),
        "comment comparison should ignore surrounding whitespace"
    );
}

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
                assert_eq!(
                    ordered_comments(file.source(), id),
                    ordered_comments(&formatted, id),
                    "formatting changed the ordered comment text for {}",
                    file.path().display()
                );
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
