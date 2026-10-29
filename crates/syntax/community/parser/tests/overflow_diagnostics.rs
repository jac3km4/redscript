use redscript_community_ast::{SourceMap, Span};
use redscript_community_parser::{Error, parse_expr};

#[test]
fn positive_signed_min_magnitudes_explain_overflow() {
    let mut diagnostics = Vec::new();
    for (source, literal) in [
        ("2147483648", "2147483648"),
        ("x-2147483648", "2147483648"),
        ("-(2147483648)", "2147483648"),
        ("~2147483648", "2147483648"),
        ("9223372036854775808l", "9223372036854775808l"),
        ("x-9223372036854775808l", "9223372036854775808l"),
    ] {
        let sources = SourceMap::new();
        sources.push_back("other.reds", "");
        let file = sources.push_back("overflow.reds", source);
        let (_, errors) = parse_expr(source, file);
        assert_eq!(errors.len(), 1, "{source}: {errors:?}");
        assert!(matches!(errors[0], Error::Parse(..)), "{errors:?}");
        let start = source.find(literal).unwrap() as u32;
        assert_eq!(
            errors[0].span(),
            Span::new(start, start + literal.len() as u32, file)
        );
        diagnostics.push(errors[0].display(&sources).unwrap().to_string());
    }
    insta::assert_snapshot!(diagnostics.join("\n"));
}
