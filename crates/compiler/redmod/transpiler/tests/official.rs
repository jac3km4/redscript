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
fn semicolon_event_contracts_with_official_compiler() {
    let validator = OfficialValidator {
        executable: PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE")),
        timeout: Duration::from_secs(60),
    };
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let parent = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/parity-integration/official-events")
        .join(format!(
            "{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    let sources = SourceMap::new();
    let selected = sources.push_back("callback.reds", "class ReviewBoolCallback { protected cb func OnReview(value: Bool) -> Bool { if value { return false; } return value; } }");
    sources.populate_boot_lib();
    let interner = TypeInterner::default();
    let output = transpile_sources(
        &sources,
        &[selected],
        Symbols::with_default_types(),
        &interner,
    )
    .unwrap();
    for (name, source, accepted) in [
        ("TranspiledBoolEvent", output[0].module.text.as_str(), true),
        (
            "ScriptEvent",
            "class ReviewScriptEvent extends IScriptable { protected event OnReview(); }",
            true,
        ),
        (
            "ImportOnlyEvent",
            "importonly class ReviewImportOnlyEvent extends IScriptable { protected event OnReview(); }",
            false,
        ),
        ("FreePrototype", "function ReviewFreePrototype();", true),
        (
            "FreeVoidPrototype",
            "function ReviewFreeVoidPrototype(): Void;",
            true,
        ),
        (
            "FreeValuePrototype",
            "function ReviewFreeValuePrototype(): Bool;",
            false,
        ),
    ] {
        let directory = parent.join(name);
        std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
        std::fs::write(directory.join("r6/scripts/authored.script"), source).unwrap();
        let result = validator.validate_with_root(&directory, &root);
        println!("{name}: {}\n{result:?}", directory.display());
        assert_eq!(result.is_ok(), accepted, "{name}: {result:?}");
        if name == "FreeValuePrototype" {
            assert!(
                result
                    .as_ref()
                    .unwrap_err()
                    .to_string()
                    .contains("must return a value")
            );
        }
        if name == "ImportOnlyEvent" {
            assert!(
                result
                    .unwrap_err()
                    .to_string()
                    .contains("cannot have scripted properties nor methods")
            );
        }
    }
}

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
        let inspected = matches!(
            *name,
            "FieldArrayArguments"
                | "FieldArrayUnary"
                | "IndexedMethodReceivers"
                | "ScriptTemporaryBorrow"
                | "IndexedBorrow"
                | "StructFieldPlaces"
                | "ScriptFieldBorrow"
                | "IndexedFieldReads"
                | "FieldArrayIndex"
                | "Literals"
        );
        if inspected {
            let bytes =
                std::fs::read(published.directory.join("r6/cache/final.redscripts")).unwrap();
            match *name {
                "FieldArrayArguments" => {
                    use redscript_io::Instr;
                    let code = official_function_code(&bytes, "OfficialFieldArguments");
                    let is_array_op = |op: &Instr| {
                        matches!(
                            op,
                            Instr::ArrayResize(_)
                                | Instr::ArrayGrow(_)
                                | Instr::ArrayCountFast(_)
                                | Instr::ArrayFindFirstFast(_)
                                | Instr::ArrayFindLastFast(_)
                                | Instr::ArrayContainsFast(_)
                                | Instr::ArrayPush(_)
                                | Instr::ArrayInsert(_)
                                | Instr::ArrayRemoveFast(_)
                                | Instr::ArrayErase(_)
                        )
                    };
                    let arrays = code.iter().filter(|op| is_array_op(op)).count();
                    assert_eq!(arrays, 10, "{code:?}");
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::ObjectField(_)))
                            .count(),
                        10,
                        "{code:?}"
                    );
                    assert!(
                        !code.iter().any(|op| matches!(
                            op,
                            Instr::Assign
                                | Instr::InvokeStatic { .. }
                                | Instr::InvokeVirtual { .. }
                        )),
                        "{code:?}"
                    );
                    let weak = official_function_code(&bytes, "AddWeak");
                    assert_eq!(
                        weak.iter()
                            .filter(|op| matches!(op, Instr::WeakRefToRef))
                            .count(),
                        1,
                        "{weak:?}"
                    );
                    let listener = official_function_code(&bytes, "Add");
                    assert_eq!(
                        listener
                            .iter()
                            .filter(|op| matches!(op, Instr::RefToWeakRef))
                            .count(),
                        3,
                        "{listener:?}"
                    );
                    assert!(
                        listener.iter().any(|op| matches!(op, Instr::ArrayPush(_))),
                        "{listener:?}"
                    );
                    assert!(
                        !listener
                            .iter()
                            .any(|op| matches!(op, Instr::Assign | Instr::Local(_))),
                        "{listener:?}"
                    );
                    for pair in code.windows(2) {
                        if is_array_op(&pair[0]) {
                            assert!(matches!(pair[1], Instr::Context(_)), "{pair:?}");
                        }
                    }
                }
                "FieldArrayUnary" => {
                    use redscript_io::Instr;
                    for function in [
                        "OfficialFieldClear",
                        "OfficialFieldPop",
                        "OfficialFieldLast",
                    ] {
                        let code = official_function_code(&bytes, function);
                        assert_eq!(
                            code.iter()
                                .filter(|op| matches!(op, Instr::ObjectField(_)))
                                .count(),
                            1,
                            "{code:?}"
                        );
                        assert_eq!(
                            code.iter()
                                .filter(|op| matches!(
                                    op,
                                    Instr::ArrayClear(_) | Instr::ArrayPop(_) | Instr::ArrayLast(_)
                                ))
                                .count(),
                            1,
                            "{code:?}"
                        );
                        assert!(
                            !code
                                .iter()
                                .any(|op| matches!(op, Instr::Assign | Instr::Local(_))),
                            "{code:?}"
                        );
                    }
                }
                "IndexedMethodReceivers" => {
                    use redscript_io::Instr;
                    let code = official_function_code(&bytes, "OfficialIndexedMethod");
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::ArrayElement(_)))
                            .count(),
                        2,
                        "{code:?}"
                    );
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::WeakRefToRef))
                            .count(),
                        1,
                        "{code:?}"
                    );
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::InvokeStatic { .. }))
                            .count(),
                        3,
                        "{code:?}"
                    );
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::InvokeVirtual { .. }))
                            .count(),
                        0,
                        "{code:?}"
                    );
                    assert!(
                        !code
                            .iter()
                            .any(|op| matches!(op, Instr::Assign | Instr::Local(_))),
                        "{code:?}"
                    );
                }
                "ScriptTemporaryBorrow" => {
                    use redscript_io::{Instr, InvokeFlags};
                    let code = official_function_code(&bytes, "OfficialTemporaryProbe");
                    let calls = code
                        .iter()
                        .filter_map(|op| match op {
                            Instr::InvokeStatic { flags, .. } => Some(*flags),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    let mut temporary = InvokeFlags::default();
                    temporary.set_is_rvalue_ref(0);
                    assert_eq!(calls, [temporary, InvokeFlags::default()], "{code:?}");
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::AsRef(_)))
                            .count(),
                        1,
                        "{code:?}"
                    );
                    assert!(
                        matches!(code.first(), Some(Instr::JumpIfFalse(_))),
                        "{code:?}"
                    );
                    assert!(
                        !code
                            .iter()
                            .any(|op| matches!(op, Instr::Assign | Instr::Local(_))),
                        "{code:?}"
                    );
                }
                "IndexedBorrow" => {
                    use redscript_io::{Instr, InvokeFlags};
                    let code = official_function_code(&bytes, "OfficialIndexedBorrow");
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::ArrayElement(_)))
                            .count(),
                        1,
                        "{code:?}"
                    );
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::AsRef(_)))
                            .count(),
                        1,
                        "{code:?}"
                    );
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, Instr::InvokeStatic { .. }))
                            .count(),
                        2,
                        "{code:?}"
                    );
                    for op in &code {
                        if let Instr::InvokeStatic { flags, .. } = op {
                            assert_eq!(*flags, InvokeFlags::default(), "{code:?}");
                        }
                    }
                    assert!(
                        !code
                            .iter()
                            .any(|op| matches!(op, Instr::Assign | Instr::Local(_))),
                        "{code:?}"
                    );
                }
                "StructFieldPlaces" => {
                    let code = official_function_code(&bytes, "OfficialStructPlaces");
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, redscript_io::Instr::Assign))
                            .count(),
                        2,
                        "{code:?}"
                    );
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, redscript_io::Instr::StructField(_)))
                            .count(),
                        8,
                        "{code:?}"
                    );
                    assert!(
                        !code
                            .iter()
                            .any(|op| matches!(op, redscript_io::Instr::Local(_))),
                        "{code:?}"
                    );
                }
                "ScriptFieldBorrow" => {
                    let code = official_function_code(&bytes, "OfficialScriptBorrow");
                    for op in &code {
                        if let redscript_io::Instr::InvokeStatic { flags, .. } = op {
                            assert_eq!(*flags, redscript_io::InvokeFlags::default(), "{code:?}");
                        }
                    }
                    let callee = official_function_code(&bytes, "OfficialBorrowSet");
                    assert!(
                        callee
                            .iter()
                            .any(|op| matches!(op, redscript_io::Instr::Assign)),
                        "{callee:?}"
                    );
                    assert!(
                        callee
                            .iter()
                            .any(|op| matches!(op, redscript_io::Instr::Deref(_))),
                        "{callee:?}"
                    );
                    assert!(
                        code.iter()
                            .any(|op| matches!(op, redscript_io::Instr::AsRef(_))),
                        "{code:?}"
                    );
                    assert!(
                        code.iter()
                            .any(|op| matches!(op, redscript_io::Instr::ObjectField(_))),
                        "{code:?}"
                    );
                    assert!(
                        !code.iter().any(|op| matches!(
                            op,
                            redscript_io::Instr::Local(_) | redscript_io::Instr::Assign
                        )),
                        "{code:?}"
                    );
                }
                "IndexedFieldReads" => {
                    for function in ["OfficialIndexStruct", "OfficialIndexClass"] {
                        let code = official_function_code(&bytes, function);
                        assert_eq!(
                            code.iter()
                                .filter(|op| matches!(op, redscript_io::Instr::ArrayElement(_)))
                                .count(),
                            1,
                            "{code:?}"
                        );
                        assert!(
                            !code.iter().any(|op| matches!(
                                op,
                                redscript_io::Instr::Local(_) | redscript_io::Instr::Assign
                            )),
                            "{code:?}"
                        );
                    }
                }
                "FieldArrayIndex" => {
                    let code = official_function_code(&bytes, "OfficialFieldIndex");
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, redscript_io::Instr::ArrayElement(_)))
                            .count(),
                        2,
                        "{code:?}"
                    );
                    assert_eq!(
                        code.iter()
                            .filter(|op| matches!(op, redscript_io::Instr::Assign))
                            .count(),
                        1,
                        "{code:?}"
                    );
                    assert!(
                        !code
                            .iter()
                            .any(|op| matches!(op, redscript_io::Instr::Local(_))),
                        "{code:?}"
                    );
                }
                "Literals" => {
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
                _ => unreachable!(),
            }
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

#[test]
#[ignore = "requires REDMOD_EXE and REDMOD_GAME_ROOT pointing to a trusted local installation"]
fn authored_enum_storage_matches_official_bounds_and_bundle_representation() {
    let validator = OfficialValidator {
        executable: PathBuf::from(std::env::var_os("REDMOD_EXE").expect("set REDMOD_EXE")),
        timeout: Duration::from_secs(60),
    };
    let root = PathBuf::from(std::env::var_os("REDMOD_GAME_ROOT").expect("set REDMOD_GAME_ROOT"));
    let parent = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-official")
        .join(format!(
            "enum-storage-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
    let cases = [
        (
            "Legal",
            "enum I8: Int8 { A = -128, B = 127 } enum U8: Uint8 { A = 0, B = 255 } enum I16: Int16 { A = -32768, B = 32767 } enum U16: Uint16 { A = 65535 } enum I32: Int32 { A = -2147483648, B = 2147483647 } enum U32: Uint32 { A = 4294967295 } enum I64: Int64 { A = -9223372036854775807, B = 9223372036854775807 } enum Default { A = 128 } enum Signed: Int8 { A = 0 } enum Unsigned: Uint8 { A = 0 }",
            None,
        ),
        (
            "NegativeUnsigned",
            "enum E: Uint8 { A = -1 }",
            Some("Cannot add enum option"),
        ),
        (
            "ExplicitOverflow",
            "enum E: Int8 { A = 128 }",
            Some("Cannot add enum option"),
        ),
        (
            "IncrementOverflow",
            "enum E: Uint8 { A = 255, B }",
            Some("Cannot add enum option"),
        ),
        (
            "Unsigned64",
            "enum E: Uint64 { A = 0 }",
            Some("Cannot define enum"),
        ),
        (
            "NonInteger",
            "enum E: Float { A = 0 }",
            Some("Cannot define enum"),
        ),
        (
            "Minimum64",
            "enum E: Int64 { A = -9223372036854775808 }",
            Some("Could not convert script token"),
        ),
    ];
    for (name, source, expected_error) in cases {
        let directory = parent.join(name);
        std::fs::create_dir_all(directory.join("r6/scripts")).unwrap();
        std::fs::write(directory.join("r6/scripts/authored.script"), source).unwrap();
        println!("{name}: {}", directory.display());
        let result = validator.validate_with_root(&directory, &root);
        if let Some(expected) = expected_error {
            let message = result.unwrap_err().to_string();
            assert!(message.contains(expected), "{message}");
        } else {
            result.unwrap();
            let bytes = std::fs::read(directory.join("r6/cache/final.redscripts")).unwrap();
            let names = literal_pool(&bytes, 44);
            let word = |offset| {
                u32::from_le_bytes(bytes.get(offset..offset + 4).unwrap().try_into().unwrap())
                    as usize
            };
            let start = word(80);
            let count = word(84);
            let enums = (0..count)
                .filter_map(|i| {
                    let header = start + i * 20;
                    let tag = u16::from_le_bytes(
                        bytes
                            .get(header + 16..header + 18)
                            .unwrap()
                            .try_into()
                            .unwrap(),
                    );
                    (tag == 3).then(|| {
                        let offset = word(header + 8);
                        (
                            names[word(header)],
                            bytes.get(offset..offset + word(header + 12)).unwrap(),
                        )
                    })
                })
                .collect::<std::collections::BTreeMap<_, _>>();
            for (name, width) in [
                ("I8", 1),
                ("U8", 1),
                ("I16", 2),
                ("U16", 2),
                ("I32", 4),
                ("U32", 4),
                ("I64", 8),
                ("Default", 4),
            ] {
                assert_eq!(enums[name][1], width, "{name}");
            }
            // The member index differs; visibility, size, count and native flag do not.
            assert_eq!(&enums["Signed"][..6], &enums["Unsigned"][..6]);
            assert_eq!(enums["Signed"].last(), enums["Unsigned"].last());
        }
    }
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

// Decode only the named function: official external records need not be supported
// by the full bundle reader. v1.2.2 prepends a 14-byte profiling instruction.
fn official_function_code(bytes: &[u8], name: &str) -> Vec<redscript_io::Instr> {
    use byte::BytesExt as _;
    use redscript_io::{BundleReader, Definition, FunctionBody, Instr};
    let reader = BundleReader::new(bytes).unwrap();
    let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
    let index = (1..word(84))
        .find(|i| {
            let at = word(80) as usize + *i as usize * 20;
            u16::from_le_bytes(bytes[at + 16..at + 18].try_into().unwrap()) == 5
                && reader.cnames().get(word(at)).unwrap().split(';').next() == Some(name)
        })
        .unwrap_or_else(|| panic!("missing {name}"));
    let Definition::Function(function) = reader.definitions().get(index).unwrap() else {
        panic!("expected function")
    };
    let FunctionBody::Raw { bytes: raw, .. } = function.body() else {
        panic!("expected raw body")
    };
    assert_eq!(raw[0], 0x33);
    let mut at = 14;
    let mut code = Vec::<Instr>::new();
    while at < raw.len() {
        code.push(raw.read(&mut at, byte::LE).unwrap());
    }
    code.retain(|op| !matches!(op, Instr::Breakpoint(_)));
    code
}

const FIXTURES: &[(&str, &str)] = &[
    (
        "FieldArrayArguments",
        r#"class OfficialArgumentArrayOwner {
            public let values: [Int32];
            public let listeners: [wref<OfficialArgumentArrayOwner>];
            public let strong: [ref<OfficialArgumentArrayOwner>];
            public func AddWeak(listener: wref<OfficialArgumentArrayOwner>) {
                ArrayPush(this.strong, listener);
            }
            public func Add(listener: ref<OfficialArgumentArrayOwner>) {
                ArrayPush(this.listeners, listener);
                ArrayContains(this.listeners, listener);
                ArrayRemove(this.listeners, listener);
            }
        }
func OfficialFieldArguments(owner: ref<OfficialArgumentArrayOwner>, value: Int32) {
    ArrayResize(owner.values, 3);
    ArrayGrow(owner.values, value);
    ArrayCount(owner.values, value);
    ArrayFindFirst(owner.values, value);
    ArrayFindLast(owner.values, value);
    ArrayContains(owner.values, value);
    ArrayPush(owner.values, value);
    ArrayInsert(owner.values, 0, value);
    ArrayRemove(owner.values, value);
    ArrayErase(owner.values, 0);
}"#,
    ),
    (
        "FieldArrayUnary",
        r#"class OfficialUnaryArrayOwner { public let values: [Int32]; }
func OfficialFieldClear(owner: ref<OfficialUnaryArrayOwner>) { ArrayClear(owner.values); }
func OfficialFieldPop(owner: ref<OfficialUnaryArrayOwner>) -> Int32 { return ArrayPop(owner.values); }
func OfficialFieldLast(owner: ref<OfficialUnaryArrayOwner>) -> Int32 { return ArrayLast(owner.values); }"#,
    ),
    (
        "IndexedMethodReceivers",
        r#"class OfficialIndexedReceiver {
    public let objects: [ref<OfficialIndexedReceiver>];
    public let weakObjects: [wref<OfficialIndexedReceiver>];
    public func Read(value: Int32) -> Int32 { return value; }
}
func OfficialReceiverIndex(out index: Int32) -> Int32 { index = 1; return 0; }
func OfficialIndexedMethod(owner: ref<OfficialIndexedReceiver>, index: Int32) -> Int32 {
    owner.objects[OfficialReceiverIndex(index)].Read(42);
    return owner.weakObjects[index].Read(43);
}"#,
    ),
    (
        "ScriptTemporaryBorrow",
        r#"struct OfficialTemporaryValue { public let count: Int32; }
func OfficialTemporaryMake() -> OfficialTemporaryValue { return OfficialTemporaryValue(42); }
func OfficialTemporaryTake(value: script_ref<OfficialTemporaryValue>) -> Int32 { return Deref(value).count; }
func OfficialTemporaryProbe(enabled: Bool) -> Int32 {
    if enabled { return OfficialTemporaryTake(OfficialTemporaryMake()); }
    return 0;
}"#,
    ),
    (
        "IndexedBorrow",
        r#"struct OfficialIndexedValue { public let count: Int32; }
class OfficialIndexedBorrowOwner { public let values: [OfficialIndexedValue]; }
func OfficialIndexedSet(value: script_ref<OfficialIndexedValue>) { Deref(value).count = 42; }
func OfficialBorrowIndex(out index: Int32) -> Int32 { index = 1; return 0; }
func OfficialIndexedBorrow(owner: ref<OfficialIndexedBorrowOwner>, index: Int32) {
    OfficialIndexedSet(owner.values[OfficialBorrowIndex(index)]);
}"#,
    ),
    (
        "StructFieldPlaces",
        r#"struct OfficialPlaceInner { public let count: Int32; }
struct OfficialPlaceOuter { public let inner: OfficialPlaceInner; }
class OfficialPlaceOwner { public let value: OfficialPlaceOuter; }
func OfficialPlaceSet(out value: Int32) { value = 42; }
func OfficialStructPlaces(owner: ref<OfficialPlaceOwner>, value: script_ref<OfficialPlaceOuter>) {
    owner.value.inner.count = 41;
    OfficialPlaceSet(owner.value.inner.count);
    Deref(value).inner.count = 43;
    OfficialPlaceSet(Deref(value).inner.count);
}"#,
    ),
    (
        "CastFieldReads",
        r#"class OfficialFieldBase {}
class OfficialFieldDerived extends OfficialFieldBase { public let count: Int32; }
func OfficialCastField(owner: ref<OfficialFieldBase>, enabled: Bool) -> Int32 {
    if enabled { return (owner as OfficialFieldDerived).count; }
    return (null as OfficialFieldDerived).count;
}"#,
    ),
    (
        "ScriptFieldBorrow",
        r#"struct OfficialBorrowValue { public let count: Int32; }
class OfficialBorrowOwner { public let value: OfficialBorrowValue; }
func OfficialBorrowSet(value: script_ref<OfficialBorrowValue>) { Deref(value).count = 42; }
func OfficialScriptBorrow(owner: ref<OfficialBorrowOwner>) { OfficialBorrowSet(owner.value); }"#,
    ),
    (
        "IndexedFieldReads",
        r#"struct OfficialIndexValue { public let count: Int32; }
class OfficialIndexContainer {
    public let objects: [ref<OfficialIndexContainer>];
    public let values: [OfficialIndexValue];
    public let count: Int32;
}
func OfficialIndexStruct(owner: ref<OfficialIndexContainer>, index: Int32) -> Int32 {
    return owner.values[index].count;
}
func OfficialIndexClass(owner: ref<OfficialIndexContainer>, index: Int32) -> Int32 {
    return owner.objects[index].count;
}"#,
    ),
    (
        "FieldArrayIndex",
        r#"class OfficialIndexOwner {
    public let values: [Int32];
    public let child: ref<OfficialIndexOwner>;
}
func OfficialFieldIndex(owner: ref<OfficialIndexOwner>, index: Int32) -> Int32 {
    owner.child.values[index] = 42;
    return owner.values[index];
}"#,
    ),
    (
        "ArrayGrowCount",
        r#"func OfficialGrowCount(values: [Int32]) -> Int32 {
    ArrayGrow(values, 2);
    return ArrayCount(values, 42);
}"#,
    ),
    (
        "FieldOutArguments",
        r#"class OfficialOutOwner {
    public let count: Int32;
    public let child: ref<OfficialOutOwner>;
    public func Next() -> ref<OfficialOutOwner> { return this.child; }
}
func OfficialSetOut(out value: Int32) { value = 42; }
func OfficialFieldOut(owner: ref<OfficialOutOwner>, handle: wref<OfficialOutOwner>) {
    OfficialSetOut(owner.count);
    OfficialSetOut(owner.child.count);
    OfficialSetOut(handle.count);
    OfficialSetOut(owner.Next().count);
}"#,
    ),
    (
        "FieldArraySize",
        r#"class OfficialArrayOwner {
    public let values: [Int32];
    public let child: ref<OfficialArrayOwner>;
    public func Next() -> ref<OfficialArrayOwner> { return this.child; }
}
func OfficialFieldArraySize(owner: ref<OfficialArrayOwner>, enabled: Bool) -> Int32 {
    if enabled { return ArraySize(owner.child.values); }
    return ArraySize(owner.values);
}"#,
    ),
    (
        "WeakFieldReads",
        r#"class OfficialWeakFieldOwner {
    public let handle: wref<OfficialWeakFieldOwner>;
    public let count: Int32;
    public func Next() -> wref<OfficialWeakFieldOwner> { return this.handle; }
}
func OfficialWeakFieldRead(owner: ref<OfficialWeakFieldOwner>, enabled: Bool) -> Int32 {
    if enabled { return owner.handle.count; }
    return owner.Next().count;
}"#,
    ),
    (
        "CastReceivers",
        r#"class OfficialCastBase {}
class OfficialCastOwner extends OfficialCastBase {
    public func Read(value: Int32) -> Int32 { return value; }
}
func OfficialCastReceiver(owner: ref<OfficialCastBase>, enabled: Bool) -> Int32 {
    if enabled { return (owner as OfficialCastOwner).Read(42); }
    return (null as OfficialCastOwner).Read(43);
}"#,
    ),
    (
        "PersistentReceivers",
        r#"class OfficialPersistentOwner {
    public persistent let count: Int32;
    public func Next() -> ref<OfficialPersistentOwner> { return this; }
    public func Read(owner: ref<OfficialPersistentOwner>) -> Int32 {
        owner.count = 42;
        return owner.Next().count;
    }
}"#,
    ),
    (
        "NestedAssignments",
        r#"class OfficialNestedOwner {
    public let child: ref<OfficialNestedOwner>;
    public let count: Int32;
}
func OfficialNestedAssign(owner: ref<OfficialNestedOwner>, handle: wref<OfficialNestedOwner>) {
    owner.child.count = 42;
    handle.child.count = 43;
}"#,
    ),
    (
        "ReferenceUpcasts",
        r#"class OfficialRefBase {
    public static func Take(value: ref<OfficialRefBase>, other: ref<OfficialRefBase>) -> Bool { return true; }
}
class OfficialRefDerived extends OfficialRefBase {}
func OfficialStrongBase(value: wref<OfficialRefDerived>) -> ref<OfficialRefBase> { return WeakRefToRef(value); }
func OfficialWeakBase(value: ref<OfficialRefDerived>) -> wref<OfficialRefBase> { return RefToWeakRef(value); }
func OfficialUpcastCall(value: wref<OfficialRefDerived>, other: wref<OfficialRefBase>) -> Bool {
    let base: ref<OfficialRefBase> = WeakRefToRef(value);
    base = WeakRefToRef(value);
    return OfficialRefBase.Take(value, other);
}"#,
    ),
    (
        "WeakIsDefined",
        r#"class OfficialWeakOwner { public let handle: wref<OfficialWeakOwner>; }
func OfficialGetWeak(values: [Int32]) -> wref<OfficialWeakOwner> { return null; }
func OfficialWeakIsDefined(handle: wref<OfficialWeakOwner>, owner: ref<OfficialWeakOwner>) -> Bool {
    IsDefined(handle);
    IsDefined(WeakRefToRef(handle));
    IsDefined(owner.handle);
    while IsDefined(OfficialGetWeak([1])) { continue; }
    return !IsDefined(WeakRefToRef(owner.handle));
}"#,
    ),
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
        "native func OperatorAdd(a: Int32, b: Int32) -> Int32;\nnative func OperatorNeg(value: Float) -> Float;\nnative func OperatorLogicNot(value: Bool) -> Bool;\n",
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
