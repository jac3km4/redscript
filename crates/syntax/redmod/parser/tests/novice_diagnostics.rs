use redscript_community_ast::{SourceMap, Span};
use redscript_redmod_parser::lex;

#[test]
fn invalid_integer_advice_uses_ranges_not_unsigned_suffixes() {
    let mut diagnostics = Vec::new();
    for source in [
        "1u",
        "1ul",
        "4294967296u",
        "18446744073709551616ul",
        "9223372036854775809l",
        "18446744073709551616l",
        "9223372036854775808",
        "999999999999999999999",
        "1.5l",
        "1e3u",
        ".5ul",
    ] {
        let sources = SourceMap::new();
        let file = sources.push_back("literal.script", source);
        let (_, errors) = lex(source, file);
        assert!(!errors.is_empty(), "accepted {source}");
        for error in errors {
            assert_eq!(error.span(), Span::new(0, source.len() as u32, file));
            let message = error.display(&sources).unwrap().to_string();
            assert!(!message.contains("`u`") && !message.contains("`ul`"));
            diagnostics.push(message);
        }
    }
    insta::assert_snapshot!(diagnostics.join("\n"));
}
