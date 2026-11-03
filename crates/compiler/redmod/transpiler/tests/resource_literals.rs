//! Raw native resource/TweakDBID spelling and opt-in official identity evidence.
use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::{LiteralError, TranspileDiagnostic, transpile_sources};

const VALUES: &[&str] = &[
    r"base\gameplay\gui\common\buttonhints.inkwidget",
    r"base\gameplay\gui\common\",
    "café\\widget",
];

fn emit(source: &str) -> Result<String, Vec<TranspileDiagnostic>> {
    let sources = SourceMap::new();
    let selected = sources.push_back("resource-literals.reds", source);
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .map(|output| output[0].module.text.clone())
}

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
    use std::path::{Path, PathBuf};
    use std::process::{Command, Stdio};
    use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

    use byte::BytesExt as _;
    use redscript_io::{BundleReader, Definition, FunctionBody, Instr};

    let executable = PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE"));
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    assert!(executable.is_absolute() && root.is_absolute());
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "resource-literals-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
    let directory = std::path::absolute(directory).unwrap();
    std::fs::write(
        directory.join("r6/scripts/authored.script"),
        emit(&expressions()).unwrap(),
    )
    .unwrap();
    // The executable also writes logs in its working directory, so keep that
    // under target as well as the compiled bundle. The game root is input only.
    let stdout = std::fs::File::create_new(directory.join("stdout.log")).unwrap();
    let stderr = std::fs::File::create_new(directory.join("stderr.log")).unwrap();
    let mut child = Command::new(executable)
        .current_dir(&directory)
        .args(["validate-mod-scripts", "-compile", "-root"])
        .arg(root)
        .arg("-directory")
        .arg(&directory)
        .stdin(Stdio::null())
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
        .unwrap();
    let started = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if started.elapsed() >= Duration::from_secs(60) {
            child.kill().unwrap();
            child.wait().unwrap();
            panic!(
                "official literal validation timed out: {}",
                directory.display()
            );
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    let log = std::fs::read_to_string(directory.join("stdout.log")).unwrap();
    println!("{}\n{log}", directory.display());
    assert!(status.success(), "{status}: {log}");
    assert!(
        log.contains("Commandlet validate-mod-scripts has succeeded"),
        "{log}"
    );
    assert!(log.contains("v1.2.2"), "profiling layout requires v1.2.2");
    let bytes = std::fs::read(directory.join("r6/cache/final.redscripts")).unwrap();
    let reader = BundleReader::new(&bytes).unwrap();
    let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
    let mut listing = String::new();
    let mut mismatches = Vec::new();
    for (i, expected) in VALUES.iter().enumerate() {
        for prefix in ['r', 't'] {
            let name = format!("Literal{prefix}{i}");
            let index = (1..word(84))
                .find(|i| {
                    let at = word(80) as usize + *i as usize * 20;
                    u16::from_le_bytes(bytes[at + 16..at + 18].try_into().unwrap()) == 5
                        && reader.cnames().get(word(at)).unwrap().split(';').next() == Some(&name)
                })
                .unwrap_or_else(|| panic!("missing {name}"));
            let Definition::Function(function) = reader.definitions().get(index).unwrap() else {
                panic!("expected function")
            };
            let FunctionBody::Raw { bytes: raw, .. } = function.body() else {
                panic!("expected raw body")
            };
            // Official v1.2.2 has a 14-byte profiling preamble.
            assert_eq!(raw[0], 0x33);
            let mut at = 14;
            let mut code = Vec::<Instr>::new();
            while at < raw.len() {
                code.push(raw.read(&mut at, byte::LE).unwrap());
            }
            let ops = code
                .iter()
                .filter(|op| !matches!(op, Instr::Breakpoint(_)))
                .collect::<Vec<_>>();
            let actual = match ops.as_slice() {
                [Instr::Return, Instr::ResourceConst(index), Instr::Nop] if prefix == 'r' => {
                    reader.resources().get(*index).unwrap()
                }
                [Instr::Return, Instr::TweakDbIdConst(index), Instr::Nop] if prefix == 't' => {
                    reader.tweakdb_ids().get(*index).unwrap()
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
