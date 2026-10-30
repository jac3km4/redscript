//! Opt-in conformance checks against a locally installed, trusted official compiler.
use std::path::{Path, PathBuf};
use std::time::Duration;

use redscript_community_ast::SourceMap;
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::{Symbols, TypeInterner};
use redscript_redmod_transpiler::package::{ScriptArtifact, publish_mod_with_root};
use redscript_redmod_transpiler::transpile_sources;
use redscript_redmod_transpiler::validate::OfficialValidator;

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn authored_transpiled_sources_compile_with_official_compiler() {
    let executable = PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE"));
    let game_root =
        PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let validator = OfficialValidator {
        executable,
        timeout: Duration::from_secs(60),
    };
    let parent = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!("integration-{}", std::process::id()));
    std::fs::create_dir_all(&parent).unwrap();

    let output = authored_transpiled_sources();
    for ((name, _), source) in FIXTURES.iter().zip(&output) {
        let published = publish_mod_with_root(
            &parent,
            name,
            &[ScriptArtifact {
                path: Path::new("authored.script"),
                source,
            }],
            &validator,
            &game_root,
        )
        .unwrap();
        let report = published.validation.unwrap();
        println!("{name}: {}", published.directory.display());
        println!(
            "{}",
            report.stdout.replace("\r\r\n", "\n").replace("\r\n", "\n")
        );
        let about = std::fs::read_to_string(published.directory.join("about.json")).unwrap();
        assert!(
            about.contains(&format!("\"ModName\": \"{name}\"")),
            "{about}"
        );
        assert!(report.stderr.is_empty(), "{}", report.stderr);
        for artifact in ["final.redscripts", "final.redscripts.h64"] {
            let bytes = std::fs::metadata(published.directory.join("r6/cache").join(artifact))
                .unwrap()
                .len();
            assert!(bytes > 0);
            println!("{artifact}: {bytes} bytes");
        }
        if *name == "Literals" {
            let bytes =
                std::fs::read(published.directory.join("r6/cache/final.redscripts")).unwrap();
            let strings = literal_pool(&bytes, 92);
            for expected in [
                r"assets\widgets\button",
                r"assets\",
                r"\n\t\u{22}\\",
                "first\n  second",
            ] {
                assert!(strings.contains(&expected), "missing String {expected:?}");
            }
            let cnames = literal_pool(&bytes, 44);
            assert!(cnames.contains(&r"raw\name\"));
        }
        assert_eq!(
            validator
                .validate_with_root(&published.directory, &game_root)
                .unwrap_err()
                .kind(),
            std::io::ErrorKind::AlreadyExists
        );
    }
    assert_eq!(std::fs::read_dir(&parent).unwrap().count(), FIXTURES.len());
    // Keep evidence under target, but never accept output from an earlier run.
}

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn raw_inline_local_initializers_compile_with_official_compiler() {
    let executable = PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE"));
    let game_root =
        PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let validator = OfficialValidator {
        executable,
        timeout: Duration::from_secs(60),
    };
    let parent = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "inline-initializers-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    std::fs::create_dir_all(parent.parent().unwrap()).unwrap();
    std::fs::create_dir(&parent).unwrap();

    // Authored REDmod, deliberately bypassing transpilation and snapshot fixtures.
    // Compilation proves acceptance/typing only, not runtime order or side effects.
    let fixtures = [
        (
            "InlineScalars",
            r#"function OfficialInlineTick(out counter: Int32) -> Int32 {
    counter = counter + 1;
    return counter;
}
function OfficialInlineScalars(out counter: Int32) -> Int32 {
    var x: Int32 = OfficialInlineTick(counter);
    counter = counter + 10;
    var y: Int32 = x + OfficialInlineTick(counter);
    return y;
}
"#,
        ),
        (
            "InlineArrayCopy",
            r#"function OfficialInlineArrayCopy() -> Int32 {
    var source: array<Int32>;
    source.Resize(1);
    source[0] = 42;
    var copy: array<Int32> = source;
    return copy[0];
}
"#,
        ),
        (
            "InlineReferences",
            r#"class OfficialInlineWidget extends IScriptable {
}
function OfficialInlineReferences() -> OfficialInlineWidget {
    var value: OfficialInlineWidget = new OfficialInlineWidget;
    var copy: OfficialInlineWidget = value;
    return copy;
}
"#,
        ),
    ];
    let mut failures = Vec::new();
    for (name, source) in fixtures {
        let directory = parent.join(name);
        std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
        std::fs::write(directory.join("r6/scripts/authored.script"), source).unwrap();
        println!("{name}: {}", directory.display());
        match validator.validate_with_root(&directory, &game_root) {
            Ok(report) => {
                println!(
                    "{}",
                    report.stdout.replace("\r\r\n", "\n").replace("\r\n", "\n")
                );
                if !report.stderr.is_empty() {
                    failures.push(format!("{name}: {}", report.stderr));
                }
            }
            Err(error) => failures.push(format!("{name}: {error}")),
        }
    }
    // Retain scripts, logs, and fresh cache artifacts under target as evidence.
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

// Read only bounded version-14 literal tables, not definitions (which may use
// official layouts unsupported by the full decompiler). Offsets follow io/bundle.rs.
fn literal_pool(bytes: &[u8], table: usize) -> Vec<&str> {
    fn word(bytes: &[u8], offset: usize) -> usize {
        u32::from_le_bytes(bytes.get(offset..offset + 4).unwrap().try_into().unwrap()) as usize
    }
    assert_eq!(bytes.get(..4), Some(b"REDS".as_slice()));
    assert_eq!(word(bytes, 4), 14);
    let data_start = word(bytes, 32);
    let data_end = data_start.checked_add(word(bytes, 36)).unwrap();
    let data = bytes.get(data_start..data_end).unwrap();
    let start = word(bytes, table);
    let size = word(bytes, table + 4).checked_mul(4).unwrap();
    bytes
        .get(start..start.checked_add(size).unwrap())
        .unwrap()
        .as_chunks::<4>()
        .0
        .iter()
        .map(|entry| {
            let offset = word(entry, 0);
            let remaining = data.get(offset..).unwrap();
            let end = remaining.iter().position(|&b| b == 0).unwrap();
            std::str::from_utf8(&remaining[..end]).unwrap()
        })
        .collect()
}

#[test]
fn literal_pool_reader_decodes_relative_offsets() {
    let mut bytes = vec![0_u8; 112];
    bytes[..4].copy_from_slice(b"REDS");
    for (offset, value) in [(4, 14_u32), (32, 108), (36, 4), (92, 104), (96, 1)] {
        bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
    }
    bytes[108..].copy_from_slice(b"a\\b\0");
    assert_eq!(literal_pool(&bytes, 92), vec![r"a\b"]);
}

const FIXTURES: &[(&str, &str)] = &[
    (
        "Literals",
        r#"public func OfficialTranspilePath() -> String = "assets\\widgets\\button";
public func OfficialTranspileTrailingSlash() -> String = "assets\\";
public func OfficialTranspileEscapeLooking() -> String = "\\n\\t\\u{22}\\\\";
public func OfficialTranspileMultiline() -> String = "first\n  second";
public func OfficialTranspileName() -> CName = n"raw\\name\\";"#,
    ),
    (
        "Arithmetic",
        "public func OfficialTranspileSum(value: Int32) -> Int32 { return value + 2; }",
    ),
    (
        "Arrays",
        "public func OfficialTranspileArray() -> Int32 { let values: [Int32] = [1, 2]; ArrayPush(values, 3); return ArraySize(values); }",
    ),
    (
        "Floats",
        "public func OfficialTranspileSmallFloat() -> Float = 0.00000001;\npublic func OfficialTranspileLargeFloat() -> Float = 100000000000000000000.0;\npublic func OfficialTranspileNegativeZero() -> Float = -0.0;",
    ),
    (
        "Objects",
        "class OfficialTranspileWidget { private let value: Int32; public func Read() -> Int32 { return this.value; } }\npublic func OfficialTranspileObject() -> ref<OfficialTranspileWidget> { return new OfficialTranspileWidget(); }",
    ),
];

fn authored_transpiled_sources() -> Vec<redscript_redmod_transpiler::TranspiledSource> {
    let sources = SourceMap::new();
    // Analysis-only: the official compiler already knows the native operator.
    sources.push_back(
        "operators.reds",
        "native func OperatorAdd(a: Int32, b: Int32) -> Int32;\nnative func OperatorNeg(value: Float) -> Float;\n",
    );
    let selected: Vec<_> = FIXTURES
        .iter()
        .map(|(name, text)| sources.push_back(format!("{name}.reds"), *text))
        .collect();
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    transpile_sources(
        &sources,
        &selected,
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap()
}

#[test]
fn authored_transpile_snapshots() {
    for ((name, _), source) in FIXTURES.iter().zip(authored_transpiled_sources()) {
        insta::assert_snapshot!(*name, source.module.text);
    }
}
