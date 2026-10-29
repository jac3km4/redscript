use redscript_community_ast::{FileId, SourceMap, Span};
use redscript_community_parser::Error;

fn diagnostic(source: &str, highlighted: &str) -> String {
    let sources = SourceMap::new();
    for _ in 0..7 {
        sources.push_back("other.reds", "");
    }
    let file = sources.push_back("interpolation.reds", source);
    assert_eq!(file, FileId::from_i32(7));
    let start = source
        .rfind(highlighted)
        .expect("diagnostic text in source");
    let expected = Span::new(start as u32, (start + highlighted.len()) as u32, file);
    let mut diagnostics = None;
    for lex in [
        redscript_community_parser::lex,
        redscript_community_parser::lex_with_lf_and_comments,
    ] {
        let (tokens, errors) = lex(source, file);
        assert!(tokens.is_some(), "{source}: expected recovered tokens");
        assert_eq!(errors.len(), 1, "{source}: {errors:?}");
        let Error::Lex(_, span) = &errors[0] else {
            panic!("expected lexer diagnostic: {errors:?}");
        };
        assert_eq!(*span, expected, "{source}: {errors:?}");
        assert_eq!(&source[span.start as usize..span.end as usize], highlighted);
        let rendered = errors[0].display(&sources).unwrap().to_string();
        if let Some(previous) = &diagnostics {
            assert_eq!(&rendered, previous, "lexer modes must agree");
        } else {
            diagnostics = Some(rendered);
        }
    }
    diagnostics.unwrap()
}

#[test]
fn invalid_symbols_use_source_byte_offsets() {
    let mut diagnostics = Vec::new();
    for symbol in ["$", "é", "💥"] {
        let source = format!(r#"s"préfixe 💡: \(foo + {symbol} + bar)""#);
        diagnostics.push(diagnostic(&source, symbol));
    }
    insta::assert_snapshot!(diagnostics.join("\n"));
}

#[test]
fn overflowing_literals_use_source_byte_offsets() {
    let mut diagnostics = Vec::new();
    for literal in [
        "2147483649",
        "4294967296u",
        "9223372036854775809l",
        "18446744073709551616ul",
    ] {
        let source = format!(r#"s"préfixe: \(foo + {literal})""#);
        diagnostics.push(diagnostic(&source, literal));
    }
    insta::assert_snapshot!(diagnostics.join("\n"));
}

#[test]
fn invalid_escapes_in_interpolated_expressions_use_source_byte_offsets() {
    let mut diagnostics = Vec::new();
    for (literal, highlighted) in [
        (r#""\q""#, "q"),
        (r#""\u{D800}""#, "{D800}"),
        (r#""\u{xyz}""#, "{xyz}"),
    ] {
        let source = format!(r#"s"préfixe: \(foo + {literal})""#);
        diagnostics.push(diagnostic(&source, highlighted));
    }
    insta::assert_snapshot!(diagnostics.join("\n"));
}

#[test]
fn nested_interpolation_diagnostics_are_rebased_at_each_level() {
    let mut diagnostics = Vec::new();
    for (expression, highlighted) in [
        ("foo + 💥", "💥"),
        ("2147483649", "2147483649"),
        (r#""\q""#, "q"),
        (r#""\u{110000}""#, "{110000}"),
    ] {
        let source = format!(r#"s"extérieur: \(s"intérieur: \({expression})")""#);
        diagnostics.push(diagnostic(&source, highlighted));
    }
    insta::assert_snapshot!(diagnostics.join("\n"));
}

#[test]
fn interpolation_fragment_diagnostics_are_not_shifted_twice() {
    let diagnostics = [
        diagnostic(r#"s"préfixe: \q \(foo)""#, "q"),
        diagnostic(r#"s"extérieur: \(s"intérieur: \q \(foo)")""#, "q"),
    ];
    insta::assert_snapshot!(diagnostics.join("\n"));
}
