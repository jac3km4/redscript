use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

use redscript_compiler_api::ScriptBundle;
use redscript_compiler_api::ast::SourceMap;

struct TestDir(PathBuf);

impl TestDir {
    fn new() -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let path = std::env::temp_dir().join(format!(
            "redscript-cli-test-{}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&path).unwrap();
        Self(path)
    }

    fn command(&self) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_redscript-cli"));
        command.current_dir(&self.0);
        command
    }
}

impl Drop for TestDir {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.0).unwrap();
    }
}

fn stderr(output: &Output) -> String {
    String::from_utf8(output.stderr.clone()).unwrap()
}

fn assert_failure(output: &Output, expected: &[&str]) {
    assert!(!output.status.success(), "{output:?}");
    let stderr = stderr(output);
    for text in expected {
        assert!(stderr.contains(text), "missing {text:?} in {stderr}");
    }
}

fn write_bundle(dir: &TestDir) {
    fs::write(
        dir.0.join("input.redscripts"),
        ScriptBundle::default().into_writeable().to_bytes().unwrap(),
    )
    .unwrap();
}

fn source_command(dir: &TestDir, subcommand: &str) -> Command {
    let mut command = dir.command();
    command.args([subcommand, "-b", "input.redscripts"]);
    if subcommand == "compile" {
        command.args(["-o", "output.redscripts"]);
    }
    command
}

#[test]
fn warning_help_and_parse_errors_list_accepted_values() {
    let dir = TestDir::new();
    fs::write(dir.0.join("test.reds"), "func Test() {}\n").unwrap();
    for subcommand in ["compile", "lint"] {
        let output = dir.command().args([subcommand, "--help"]).output().unwrap();
        let help = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            stderr(&output)
        );
        for text in [
            "unused_locals",
            "default: none",
            "repeatable",
            "pedantic, warning",
            "default: warning",
            "--runtime-compatible",
            "match SCC",
            "hard errors still",
        ] {
            assert!(help.contains(text), "{help}");
        }
        for (flag, value, expected) in [
            (
                "-W",
                "unused",
                "accepted values for -W/--warn-on: unused_locals",
            ),
            (
                "-L",
                "error",
                "accepted values for -L/--warn-level: pedantic, warning (default: warning)",
            ),
        ] {
            let output = source_command(&dir, subcommand)
                .args(["-s", "test.reds", flag, value])
                .output()
                .unwrap();
            assert_failure(&output, &[value, expected]);
        }
        for level in ["pedantic", "warning"] {
            let output = source_command(&dir, subcommand)
                .args(["-s", "test.reds", "-W", "unused_locals", "-L", level])
                .output()
                .unwrap();
            assert_failure(
                &output,
                &["failed to read baseline bundle input.redscripts"],
            );
        }
    }
}

#[test]
fn compile_and_lint_require_explicit_existing_sources() {
    let dir = TestDir::new();
    write_bundle(&dir);
    fs::write(dir.0.join("test.reds"), "func Test() {}\n").unwrap();
    fs::write(dir.0.join("output.redscripts"), "keep me").unwrap();
    for subcommand in ["compile", "lint"] {
        let output = source_command(&dir, subcommand).output().unwrap();
        assert_failure(&output, &["no source paths provided", "-s/--src"]);
        // A valid first root must not hide a missing later root.
        let output = source_command(&dir, subcommand)
            .args(["-s", "test.reds", "-s", "missing-sources"])
            .output()
            .unwrap();
        assert_failure(&output, &["failed to access source root missing-sources"]);
        assert_eq!(
            fs::read_to_string(dir.0.join("output.redscripts")).unwrap(),
            "keep me"
        );
    }
}

#[test]
fn compile_and_lint_baseline_errors_include_paths() {
    let dir = TestDir::new();
    fs::write(dir.0.join("test.reds"), "func Test() {}\n").unwrap();
    for subcommand in ["compile", "lint"] {
        let output = source_command(&dir, subcommand)
            .args(["-s", "test.reds"])
            .output()
            .unwrap();
        assert_failure(
            &output,
            &["failed to read baseline bundle input.redscripts"],
        );
    }
    fs::write(dir.0.join("input.redscripts"), "not a bundle").unwrap();
    for subcommand in ["compile", "lint"] {
        let output = source_command(&dir, subcommand)
            .args(["-s", "test.reds"])
            .output()
            .unwrap();
        assert_failure(&output, &["baseline bundle input.redscripts"]);
    }
}

#[test]
fn compile_output_errors_include_paths() {
    let dir = TestDir::new();
    write_bundle(&dir);
    fs::write(dir.0.join("test.reds"), "func Test() {}\n").unwrap();
    fs::create_dir(dir.0.join("output.redscripts")).unwrap();
    let output = source_command(&dir, "compile")
        .args(["-s", "test.reds"])
        .output()
        .unwrap();
    assert_failure(
        &output,
        &["failed to write output bundle output.redscripts"],
    );
}

#[test]
fn decompile_io_errors_include_paths() {
    let dir = TestDir::new();
    let run = || {
        dir.command()
            .args(["decompile", "-b", "input.redscripts", "-o", "output.reds"])
            .output()
            .unwrap()
    };
    assert_failure(&run(), &["failed to read input bundle input.redscripts"]);
    fs::write(dir.0.join("input.redscripts"), "not a bundle").unwrap();
    assert_failure(&run(), &["failed to decode input bundle input.redscripts"]);
    write_bundle(&dir);
    fs::create_dir(dir.0.join("output.reds")).unwrap();
    assert_failure(&run(), &["failed to create decompiled output output.reds"]);
}

#[cfg(windows)]
#[test]
fn formatter_save_errors_include_paths() {
    let dir = TestDir::new();
    let path = dir.0.join("test.reds");
    fs::write(&path, "func Test(){}\n").unwrap();
    let original = fs::metadata(&path).unwrap().permissions();
    let mut readonly = original.clone();
    readonly.set_readonly(true);
    fs::set_permissions(&path, readonly).unwrap();
    let output = dir
        .command()
        .args(["format", "-s", "test.reds", "--save"])
        .output()
        .unwrap();
    fs::set_permissions(&path, original).unwrap();
    assert_failure(&output, &["failed to save formatted source", "test.reds"]);
    assert_eq!(fs::read_to_string(path).unwrap(), "func Test(){}\n");
}

#[test]
fn compilation_and_lint_diagnostics_use_stderr() {
    let dir = TestDir::new();
    fs::write(
        dir.0.join("input.redscripts"),
        ScriptBundle::default().into_writeable().to_bytes().unwrap(),
    )
    .unwrap();

    for subcommand in ["compile", "lint"] {
        for (source, success) in [
            ("func Test() {}\n", true),
            ("func Test() { MissingFunction(); }\n", false),
        ] {
            fs::write(dir.0.join("test.reds"), source).unwrap();
            let mut command = dir.command();
            command.args([subcommand, "-s", "test.reds", "-b", "input.redscripts"]);
            if subcommand == "compile" {
                command.args(["-o", "output.redscripts"]);
            }
            let output = command.output().unwrap();
            let stderr = stderr(&output);
            assert_eq!(output.status.success(), success, "{stderr}");
            assert!(output.stdout.is_empty(), "{output:?}");
            assert!(stderr.contains("Completed with "), "{stderr}");
            assert!(
                stderr.contains(if success {
                    "Compilation successful"
                } else {
                    "Compilation failed"
                }),
                "{stderr}"
            );
            if !success {
                assert!(stderr.contains("error["), "{stderr}");
            }
        }
    }
}

#[test]
fn formatter_continues_after_parse_errors() {
    for mode in ["--save", "--check", "stdout"] {
        let dir = TestDir::new();
        let sources = dir.0.join("sources");
        fs::create_dir(&sources).unwrap();
        fs::write(sources.join("a.reds"), "").unwrap();
        fs::write(sources.join("b.reds"), "").unwrap();
        let map = SourceMap::from_paths_recursively([&sources]).unwrap();
        let paths = map
            .files()
            .map(|(_, file)| file.path().to_owned())
            .collect::<Vec<_>>();
        let invalid = &paths[0];
        let valid = &paths[1];
        fs::write(invalid, "func Broken(\n").unwrap();
        fs::write(valid, "func Valid(){}\n").unwrap();
        let map = SourceMap::from_paths_recursively([&sources]).unwrap();
        assert_eq!(map.files().next().unwrap().1.path(), invalid);

        let mut command = dir.command();
        command.args(["format", "-s", "sources"]);
        if mode != "stdout" {
            command.arg(mode);
        }
        let output = command.output().unwrap();
        let stderr = stderr(&output);
        assert!(!output.status.success(), "{stderr}");
        assert!(stderr.contains("error:"), "{stderr}");
        assert!(!stderr.contains("[ERROR] error:"), "{stderr}");
        assert_eq!(fs::read_to_string(invalid).unwrap(), "func Broken(\n");
        match mode {
            "--save" => {
                assert!(output.stdout.is_empty());
                assert_eq!(
                    fs::read_to_string(valid).unwrap(),
                    "\nfunc Valid() {\n}\n\n"
                );
            }
            "--check" => {
                assert!(output.stdout.is_empty());
                assert!(stderr.contains("format --save -s"), "{stderr}");
                assert!(
                    stderr.contains("file is not formatted correctly"),
                    "{stderr}"
                );
                assert!(
                    stderr.contains(valid.file_name().unwrap().to_str().unwrap()),
                    "{stderr}"
                );
            }
            _ => assert!(
                String::from_utf8(output.stdout)
                    .unwrap()
                    .contains("func Valid() {\n}")
            ),
        }
    }
}

#[test]
fn runtime_compatibility_allows_runtime_diagnostics_but_not_hard_errors() {
    let dir = TestDir::new();
    write_bundle(&dir);
    for (source, diagnostic, compatible) in [
        (
            "class Example { private let value: Int32; }
func Test(value: ref<Example>) -> Int32 { return value.value; }
",
            "INACCESSIBLE_FIELD",
            true,
        ),
        (
            "class Parent { public func Method() {} }
class Child extends Parent { private func Method() {} }
",
            "LESS_VISIBLE_OVERRIDE",
            true,
        ),
        (
            "func Test() { MissingFunction(); }
",
            "MissingFunction",
            false,
        ),
    ] {
        fs::write(dir.0.join("test.reds"), source).unwrap();
        for subcommand in ["compile", "lint"] {
            for runtime_compatible in [false, true] {
                fs::write(dir.0.join("output.redscripts"), "keep me").unwrap();
                let mut command = source_command(&dir, subcommand);
                command.args(["-s", "test.reds"]);
                if runtime_compatible {
                    command.arg("--runtime-compatible");
                }
                let output = command.output().unwrap();
                let success = runtime_compatible && compatible;
                assert_eq!(output.status.success(), success, "{}", stderr(&output));
                assert!(stderr(&output).contains(diagnostic), "{output:?}");
                let bytes = fs::read(dir.0.join("output.redscripts")).unwrap();
                if success && subcommand == "compile" {
                    ScriptBundle::from_bytes(&bytes).unwrap();
                    assert_ne!(bytes, b"keep me");
                } else {
                    assert_eq!(bytes, b"keep me");
                }
            }
        }
    }
}
