//! Raw native resource/TweakDBID spelling and opt-in official identity evidence.
mod support;

use redscript_community_ast::SourceMap;
use redscript_redmod_transpiler::LiteralError;

const VALUES: &[&str] = &[
    r"base\gameplay\gui\common\buttonhints.inkwidget",
    r"base\gameplay\gui\common\",
    "café\\widget",
];

use support::try_emit_source as emit;

fn expressions() -> String {
    let mut source = String::new();
    for (i, value) in VALUES.iter().enumerate() {
        for (prefix, ty) in [('r', "ResRef"), ('t', "TweakDBID")] {
            source.push_str(&format!(
                "func Literal{prefix}{i}() -> {ty} {{ return {prefix}\"{}\"; }}\n",
                value.escape_default()
            ));
        }
    }
    source
}

#[test]
fn resource_and_tweak_literals_emit_decoded_backslashes_without_escaping() {
    let text = emit(&expressions()).unwrap();
    for value in VALUES {
        for prefix in ['R', 'T'] {
            let spelling = format!("{prefix}\"{value}\"");
            assert!(text.contains(&format!("return {spelling};")), "{text}");
            let sources = SourceMap::new();
            let file = sources.push_back("literal.script", spelling.clone());
            let (tokens, errors) = redscript_redmod_parser::lex(&spelling, file);
            assert!(errors.is_empty(), "{errors:?}");
            let expected = match prefix {
                'R' => redscript_redmod_parser::Token::ResRef((*value).into()),
                _ => redscript_redmod_parser::Token::TdbId((*value).into()),
            };
            assert_eq!(tokens.unwrap()[0].0, expected);
        }
    }
}

#[test]
fn normalized_resource_paths_are_rejected_but_tweak_ids_stay_raw() {
    for value in [r"base\\gameplay", r"\base\gameplay"] {
        let errors = emit(&format!(
            "func Invalid() -> ResRef {{ return r\"{}\"; }}",
            value.escape_default()
        ))
        .unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| e.literal_error == Some(LiteralError::NormalizedResource)),
            "{errors:?}"
        );
        let text = emit(&format!(
            "func RawTweak() -> TweakDBID {{ return t\"{}\"; }}",
            value.escape_default()
        ))
        .unwrap();
        assert!(text.contains(&format!("T\"{value}\"")), "{text}");
    }
}

#[test]
fn mark_to_sell_resource_argument_preserves_path_identity() {
    let text = emit(
        r#"native func MarkToSell(icon: ResRef) -> Void;
func UseMarkToSell() { MarkToSell(r"base\\gameplay\\gui\\common\\buttonhints.inkwidget"); }"#,
    )
    .unwrap();
    assert!(
        text.contains(r#"MarkToSell(R"base\gameplay\gui\common\buttonhints.inkwidget");"#),
        "{text}"
    );
}

#[test]
fn resource_and_tweak_literals_still_reject_unrepresentable_values() {
    for prefix in ['r', 't'] {
        for (value, kind) in [
            ("say\"hello", LiteralError::EmbeddedDelimiter),
            ("\0", LiteralError::Nul),
            ("first\nsecond", LiteralError::UnverifiedResource),
            ("\r", LiteralError::UnverifiedControl),
            ("\t", LiteralError::UnverifiedControl),
            ("\u{7f}", LiteralError::UnverifiedControl),
            ("\u{85}", LiteralError::UnverifiedControl),
        ] {
            let ty = if prefix == 'r' { "ResRef" } else { "TweakDBID" };
            let errors = emit(&format!(
                "func Invalid() -> {ty} {{ return {prefix}\"{}\"; }}",
                value.escape_default()
            ))
            .unwrap_err();
            assert!(
                errors.iter().any(|error| error.literal_error == Some(kind)),
                "{errors:?}"
            );
        }
    }
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn raw_backslash_identity_with_official_compiler() {
    use redscript_io::Instr;
    use redscript_test_support::{OfficialBundle, OfficialCompiler};

    let source = emit(&expressions()).unwrap();
    let output = OfficialCompiler::from_env().compile("resource-literals", &source);
    output.assert_version("v1.2.2");
    let directory = &output.directory;
    let bytes = output.bundle_bytes();
    let bundle = OfficialBundle::new(&bytes);
    let resources = bundle.literal_pool(68);
    let tweak_ids = bundle.literal_pool(56);
    let mut listing = String::new();
    let mut mismatches = Vec::new();
    for (i, expected) in VALUES.iter().enumerate() {
        for prefix in ['r', 't'] {
            let name = format!("Literal{prefix}{i}");
            let code = bundle.function_code_prefix(&format!("{name};"));
            let ops = code
                .iter()
                .filter(|op| !matches!(op, Instr::Breakpoint(_)))
                .collect::<Vec<_>>();
            let actual = match ops.as_slice() {
                [Instr::Return, Instr::ResourceConst(index), Instr::Nop] if prefix == 'r' => {
                    resources[u32::from(*index) as usize]
                }
                [Instr::Return, Instr::TweakDbIdConst(index), Instr::Nop] if prefix == 't' => {
                    tweak_ids[u32::from(*index) as usize]
                }
                _ => panic!("{name}: unexpected body {code:?}"),
            };
            if actual != *expected {
                mismatches.push(format!("{name}: expected {expected:?}, got {actual:?}"));
            }
            listing.push_str(&format!("{name}: {code:?} => {actual:?}\n"));
        }
    }
    std::fs::write(directory.join("decoded.txt"), &listing).unwrap();
    println!("{listing}");
    assert!(mismatches.is_empty(), "{mismatches:#?}");
}
