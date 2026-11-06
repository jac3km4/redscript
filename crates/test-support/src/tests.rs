use std::borrow::Cow;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::process::Command;

use redscript_io::{Function, FunctionFlags, ScriptBundle, Visibility};

use super::*;

fn fixture(functions: &[(&str, &[u8])]) -> Vec<u8> {
    let mut bundle = ScriptBundle::default();
    for (name, body) in functions {
        let name = bundle.cnames_mut().add(*name);
        bundle.define(
            Function::new(name, Visibility::Public, FunctionFlags::default()).with_body(
                FunctionBody::Raw {
                    max_offset: 1,
                    bytes: Cow::Owned(body.to_vec()),
                },
            ),
        );
    }
    bundle.into_writeable().to_bytes().unwrap()
}

fn body(code: &[u8]) -> Vec<u8> {
    let mut bytes = vec![0; 14];
    bytes[0] = 0x33;
    bytes.extend_from_slice(code);
    bytes
}

fn put(bytes: &mut [u8], at: usize, value: u32) {
    bytes[at..at + 4].copy_from_slice(&value.to_le_bytes());
}

fn rejects(f: impl FnOnce()) {
    assert!(catch_unwind(AssertUnwindSafe(f)).is_err());
}

#[test]
fn reserves_absolute_distinct_directories() {
    let first = unique_directory("reservation");
    let second = unique_directory("reservation");
    assert!(first.is_absolute() && first.is_dir());
    assert!(second.is_absolute() && second.is_dir());
    assert_ne!(first, second);
    assert_eq!(first.parent(), second.parent());
    assert!(first.parent().unwrap().ends_with("target/redmod-official"));
    rejects(|| {
        unique_directory("../escape");
    });
}

#[test]
fn exact_mangled_lookup_and_prefix_ambiguity() {
    let raw = body(&[0x27]);
    let bytes = fixture(&[("Probe;Int32", &raw), ("Probe;Bool", &raw)]);
    let bundle = OfficialBundle::new(&bytes);
    assert_eq!(bundle.definition_index("Probe;Int32", 5), 1);
    assert_eq!(bundle.definition_name(2), "Probe;Bool");
    assert_eq!(bundle.function_code("Probe;Int32"), vec![Instr::Return]);
    assert_eq!(
        bundle.function_code_prefix("Probe;Int"),
        vec![Instr::Return]
    );
    assert_eq!(
        bundle.definition("Probe;Int32", 5),
        bundle.definition_prefix("Probe;Int", 5)
    );
    rejects(|| {
        bundle.definition("Probe", 5);
    });
    rejects(|| {
        bundle.definition_prefix("Probe", 5);
    });
    rejects(|| {
        bundle.function_code_prefix("Probe");
    });
    rejects(|| {
        bundle.definition("Probe;Int32", 1);
    });
    rejects(|| {
        bundle.definition_name(0);
    });
    rejects(|| {
        bundle.definition_name(3);
    });

    let bytes = fixture(&[("Duplicate;", &raw), ("Duplicate;", &raw)]);
    rejects(|| {
        OfficialBundle::new(&bytes).definition("Duplicate;", 5);
    });
}

#[test]
fn rejects_malformed_tables_names_and_records() {
    let valid = fixture(&[("Probe;", &body(&[0x27]))]);
    for (at, value) in [
        (4, 15),
        (44, u32::MAX),
        (48, u32::MAX),
        (84, u32::MAX),
        (36, u32::MAX),
    ] {
        let mut bytes = valid.clone();
        put(&mut bytes, at, value);
        rejects(|| {
            OfficialBundle::new(&bytes);
        });
    }
    let bundle = OfficialBundle::new(&valid);
    let table = bundle.word(44) as usize;
    let header = bundle.header(1);
    for (at, value) in [
        (table, u32::MAX),
        (header, u32::MAX),
        (header + 8, u32::MAX),
        (header + 12, u32::MAX),
    ] {
        let mut bytes = valid.clone();
        put(&mut bytes, at, value);
        rejects(|| {
            OfficialBundle::new(&bytes);
        });
    }
    rejects(|| {
        OfficialBundle::new(&valid[..103]);
    });
    rejects(|| {
        bundle.literal_pool(80);
    });
    let mut bytes = valid.clone();
    let end = bundle.word(32) as usize + bundle.word(36) as usize;
    bytes[end - 1] = b'x';
    rejects(|| {
        OfficialBundle::new(&bytes);
    });
}

#[test]
fn decodes_only_named_physical_record_and_retains_breakpoints() {
    let mut code = vec![0x15];
    code.extend_from_slice(&[0; 19]); // Serialized Breakpoint payload.
    code.push(0x27);
    let bytes = fixture(&[("Probe;", &body(&code)), ("Unrelated;", &[0xff])]);
    let bundle = OfficialBundle::new(&bytes);
    let decoded = bundle.function_code("Probe;");
    assert!(matches!(
        decoded.as_slice(),
        [Instr::Breakpoint(_), Instr::Return]
    ));
    // max_offset is deliberately 1: decoding follows the physical record, not
    // that virtual length, and does not attempt to decode Unrelated.
    assert_eq!(decoded.len(), 2);
    rejects(|| {
        bundle.function_code("Unrelated;");
    });

    // A truncated I32Const must not consume bytes from the following record.
    let bytes = fixture(&[("Truncated;", &body(&[0x06, 1])), ("Next;", &body(&[0x27]))]);
    rejects(|| {
        OfficialBundle::new(&bytes).function_code("Truncated;");
    });
    for raw in [&[0x33][..], &body(&[0xff]), &body(&[0x33])] {
        let bytes = fixture(&[("Unsupported;", raw)]);
        rejects(|| {
            OfficialBundle::new(&bytes).function_code("Unsupported;");
        });
    }
}

#[test]
fn literal_pool_offsets_are_relative_to_string_data() {
    let mut bytes = vec![0; 112];
    bytes[..4].copy_from_slice(b"REDS");
    for (at, value) in [(4, 14), (32, 108), (36, 4), (92, 104), (96, 1)] {
        put(&mut bytes, at, value);
    }
    bytes[108..].copy_from_slice(b"a\\b\0");
    assert_eq!(OfficialBundle::new(&bytes).literal_pool(92), vec![r"a\b"]);
}

#[test]
fn mock_compiler_distinguishes_rejection_from_infrastructure_errors() {
    let tools = unique_directory("mock-tools");
    let source = tools.join("mock.rs");
    fs::write(&source, r#"
fn main() {
    let args: Vec<_> = std::env::args().collect();
    assert_eq!(args[1], "validate-mod-scripts");
    assert_eq!(args[2], "-compile");
    assert_eq!(args[3], "-root");
    assert_eq!(args[5], "-directory");
    let directory = std::path::Path::new(&args[6]);
    assert_eq!(std::fs::canonicalize(std::env::current_dir().unwrap()).unwrap(), std::fs::canonicalize(directory).unwrap());
    println!("Official mock v1.2.2");
    let scripts = directory.join("r6/scripts");
    let mut files: Vec<_> = std::fs::read_dir(&scripts).unwrap().map(|entry| entry.unwrap().path()).collect();
    files.sort();
    let sources: Vec<_> = files.iter().map(|path| std::fs::read_to_string(path).unwrap()).collect();
    let source = &sources[0];
    if sources.len() > 1 {
        assert!(!scripts.join("authored.script").exists());
        for (index, path) in files.iter().enumerate() {
            assert_eq!(path.file_name().unwrap().to_str().unwrap(), format!("authored-{index}.script"));
        }
        std::fs::write(directory.join("observed-sources"), sources.join("\0")).unwrap();
    }
    if source == "timeout" { std::thread::sleep(std::time::Duration::from_secs(10)); }
    if source == "reject" || source == "bad-artifact" {
        if source == "bad-artifact" { std::fs::create_dir(directory.join("r6/cache/final.redscripts")).unwrap(); }
        eprintln!("expected diagnostic");
        std::process::exit(1);
    }
    if source != "missing-artifact" {
        for name in ["final.redscripts", "final.redscripts.h64"] {
            std::fs::write(directory.join("r6/cache").join(name), b"bundle").unwrap();
        }
    }
    println!("Commandlet validate-mod-scripts has succeeded");
}
"#).unwrap();
    let executable = tools.join(format!("mock{}", std::env::consts::EXE_SUFFIX));
    assert!(
        Command::new("rustc")
            .arg(&source)
            .arg("-o")
            .arg(&executable)
            .status()
            .unwrap()
            .success()
    );
    let compiler = OfficialCompiler {
        validator: OfficialValidator {
            executable,
            timeout: Duration::from_secs(60),
        },
        game_root: tools,
    };
    let output = compiler.compile("mock-accept", "exact authored source\r\n");
    assert_eq!(
        fs::read(output.directory.join("r6/scripts/authored.script")).unwrap(),
        b"exact authored source\r\n"
    );
    assert_eq!(output.bundle_bytes(), b"bundle");
    output.assert_version("v1.2.2");
    assert!(
        output
            .directory
            .join("redmod-validation.stdout.log")
            .is_file()
    );
    let sources = ["module First\r\n// café\\path\n", "module Second\n\n", ""];
    let output = compiler.compile_files("mock-multi-accept", &sources);
    assert_eq!(
        fs::read(output.directory.join("observed-sources")).unwrap(),
        sources.join("\0").as_bytes()
    );
    for (index, source) in sources.iter().enumerate() {
        assert_eq!(
            fs::read(
                output
                    .directory
                    .join(format!("r6/scripts/authored-{index}.script"))
            )
            .unwrap(),
            source.as_bytes()
        );
    }
    assert!(!output.directory.join("r6/scripts/authored.script").exists());
    let sources = ["reject", "unchanged second source\r\n"];
    let output = compiler.reject_files("mock-multi-reject", &sources, "expected diagnostic");
    assert_eq!(
        fs::read(output.directory.join("observed-sources")).unwrap(),
        sources.join("\0").as_bytes()
    );
    assert_eq!(
        fs::read(output.directory.join("r6/scripts/authored-1.script")).unwrap(),
        sources[1].as_bytes()
    );
    rejects(|| {
        compiler.compile_files("mock-empty", &[]);
    });
    rejects(|| {
        compiler.reject_files("mock-empty-reject", &[], "expected diagnostic");
    });
    rejects(|| {
        compiler.reject_files(
            "mock-multi-wrong-diagnostic",
            &sources,
            "different diagnostic",
        );
    });
    let output = compiler.reject("mock-reject", "reject", "expected diagnostic");
    assert!(output.stderr.contains("expected diagnostic"));
    rejects(|| {
        compiler.reject("mock-wrong-diagnostic", "reject", "different diagnostic");
    });
    rejects(|| {
        compiler.reject("mock-missing-artifact", "missing-artifact", "succeeded");
    });
    rejects(|| {
        compiler.reject("mock-bad-artifact", "bad-artifact", "expected diagnostic");
    });
    rejects(|| {
        compiler.compile("mock-compile-rejection", "reject");
    });

    let directory = unique_directory("mock-timeout");
    fs::create_dir_all(directory.join("r6/scripts")).unwrap();
    fs::write(directory.join("r6/scripts/authored.script"), "timeout").unwrap();
    let validator = OfficialValidator {
        timeout: Duration::from_millis(100),
        ..compiler.validator.clone()
    };
    assert_eq!(
        validator
            .invoke_with_root(&directory, &compiler.game_root)
            .unwrap_err()
            .kind(),
        std::io::ErrorKind::TimedOut
    );
    assert!(directory.join("redmod-validation.stdout.log").is_file());
    let missing = OfficialCompiler {
        validator: OfficialValidator {
            executable: compiler.game_root.join("missing.exe"),
            timeout: Duration::from_secs(60),
        },
        game_root: compiler.game_root.clone(),
    };
    rejects(|| {
        missing.reject("mock-spawn-error", "reject", "expected diagnostic");
    });
}
