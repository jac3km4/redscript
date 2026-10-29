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
