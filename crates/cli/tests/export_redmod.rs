use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};

use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{Compilation, ScriptBundle, SourceMapExt, TypeInterner};

static NEXT: AtomicU64 = AtomicU64::new(0);

struct Fixture(PathBuf);

impl Fixture {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "redscript-cli-test-{}-{}",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir(&path).unwrap();
        Self(path)
    }

    fn write(&self, path: &str, text: &str) {
        let path = self.0.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }

    fn run(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_redscript-cli"))
            .current_dir(&self.0)
            .arg("export-redmod")
            .args(args)
            .output()
            .unwrap()
    }

    fn assert_clean(&self) {
        assert!(!self.0.join("MyMod").exists());
        for entry in fs::read_dir(&self.0).unwrap() {
            let name = entry.unwrap().file_name();
            let name = name.to_string_lossy();
            assert!(
                !name.starts_with(".MyMod") && !name.starts_with("MyMod-stage-"),
                "leaked packaging artifact: {name}"
            );
        }
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.0).unwrap();
    }
}

fn assert_success(output: &Output) {
    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn normalize_diagnostic(diagnostic: &str, root: &str) -> String {
    let root = root.replace('\\', "/");
    let root = root.strip_prefix("//?/").unwrap_or(&root);
    diagnostic
        .replace("\r\n", "\n")
        .replace('\\', "/")
        .replace(root, "[ROOT]")
        .replace("//?/[ROOT]", "[ROOT]")
}

#[test]
fn diagnostic_paths_are_normalized_before_root_replacement() {
    for (root, path) in [
        ("/tmp/fixture", "/tmp/fixture/main.reds"),
        (r"C:\Temp\fixture", r"C:\Temp\fixture\main.reds"),
        (r"C:\Temp\fixture", "//?/C:/Temp/fixture/main.reds"),
        (r"\\?\C:\Temp\fixture", "//?/C:/Temp/fixture/main.reds"),
        (r"\\?\C:\Temp\fixture", "C:/Temp/fixture/main.reds"),
    ] {
        assert_eq!(
            normalize_diagnostic(&format!(" --> {path}:1:25\r\n"), root),
            " --> [ROOT]/main.reds:1:25\n"
        );
    }
}

#[test]
fn exports_only_authored_roots_and_honors_dotfile() {
    let f = Fixture::new();
    f.write("project/.redscript", "source_roots = [\"src\"]");
    f.write(
        "project/src/nested/main.reds",
        "module Example\npublic func Answer() -> Int32 { return Helper(); }",
    );
    f.write("project/ignored.reds", "not valid source");
    f.write(
        "deps/helper.reds",
        "public func Helper() -> Int32 { return 42; }",
    );
    assert_success(&f.run(&[
        "--src",
        "project",
        "--src",
        "project/src",
        "--dependency",
        "deps",
        "--dependency",
        "project/src",
        "--output",
        "MyMod",
    ]));
    let scripts = f.0.join("MyMod/r6/scripts");
    assert!(scripts.join("nested/main.script").is_file());
    assert_eq!(fs::read_dir(&scripts).unwrap().count(), 1);
    assert!(
        fs::read_to_string(scripts.join("nested/main.script"))
            .unwrap()
            .contains("Answer")
    );
}

#[test]
fn source_failure_has_readable_diagnostics_and_no_output() {
    let f = Fixture::new();
    f.write(
        "main.reds",
        "public func Broken() -> MissingType { return 1; }",
    );
    let output = f.run(&["--src", "main.reds", "--output", "MyMod"]);
    assert!(!output.status.success());
    let root = fs::canonicalize(&f.0).unwrap();
    let diagnostic = normalize_diagnostic(
        &String::from_utf8(output.stderr).unwrap(),
        root.to_str().unwrap(),
    );
    insta::assert_snapshot!(diagnostic);
    f.assert_clean();
}

#[test]
fn unsupported_association_fails_without_partial_export() {
    let f = Fixture::new();
    f.write("good.reds", "public func Good() -> Int32 { return 1; }");
    f.write("bad.reds", "public func Inferred() = 1;");
    let output = f.run(&["--src", ".", "--output", "MyMod"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("REDMOD_EXPORT"));
    f.assert_clean();
}

#[test]
fn collisions_fail_without_publishing() {
    let f = Fixture::new();
    f.write("a/main.reds", "public func A() {}");
    f.write("b/main.reds", "public func B() {}");
    let output = f.run(&["--src", "a", "--src", "b", "--output", "MyMod"]);
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("unique portable"));
    f.assert_clean();
}

#[test]
fn existing_destination_is_untouched_and_names_are_validated() {
    let f = Fixture::new();
    f.write("main.reds", "public func Main() {}");
    f.write("MyMod/keep", "original");
    let output = f.run(&["--src", "main.reds", "--output", "MyMod"]);
    assert!(!output.status.success());
    assert_eq!(
        fs::read_to_string(f.0.join("MyMod/keep")).unwrap(),
        "original"
    );
    assert_eq!(fs::read_dir(f.0.join("MyMod")).unwrap().count(), 1);
    for name in ["Bad.Mod", "CON"] {
        let output = f.run(&["--src", "main.reds", "--output", name]);
        assert!(!output.status.success());
        assert!(!f.0.join(name).exists());
    }
}

#[test]
fn missing_sources_and_invalid_options_do_not_publish() {
    let f = Fixture::new();
    f.write("main.reds", "public func Main() {}");
    for args in [
        vec!["--output", "MyMod"],
        vec![
            "--src",
            "main.reds",
            "--output",
            "MyMod",
            "--timeout",
            "NaN",
        ],
        vec![
            "--src",
            "main.reds",
            "--output",
            "MyMod",
            "--redmod",
            "relative.exe",
        ],
    ] {
        assert!(!f.run(&args).status.success());
        f.assert_clean();
    }
}

#[test]
fn game_root_requires_validator_and_absolute_path() {
    let f = Fixture::new();
    f.write("main.reds", "public func Main() {}");
    let executable = f.0.join("missing-redmod.exe");
    for (extra, expected) in [
        (
            vec!["--game-root", f.0.to_str().unwrap()],
            "--game-root requires --redmod",
        ),
        (
            vec![
                "--redmod",
                executable.to_str().unwrap(),
                "--game-root",
                "relative",
            ],
            "--game-root must be an absolute path",
        ),
    ] {
        let mut args = vec!["--src", "main.reds", "--output", "MyMod"];
        args.extend(extra);
        let output = f.run(&args);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains(expected));
        f.assert_clean();
    }
}

#[test]
fn invalid_source_inputs_are_not_silently_skipped() {
    let f = Fixture::new();
    f.write("main.reds", "public func Main() {}");
    f.write("other.txt", "public func Other() {}");
    f.write("project/.redscript", "source_roots = [\"missing.reds\"]");
    for (extra, expected) in [
        (
            vec!["--src", "missing.reds"],
            "loading source root missing.reds",
        ),
        (
            vec!["--dependency", "missing.reds"],
            "loading source root missing.reds",
        ),
        (
            vec!["--src", "other.txt"],
            "source file must have a .reds extension",
        ),
        (
            vec!["--dependency", "other.txt"],
            "source file must have a .reds extension",
        ),
        (vec!["--src", "project"], "missing.reds"),
    ] {
        let mut args = vec!["--src", "main.reds", "--output", "MyMod"];
        args.extend(extra);
        let output = f.run(&args);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains(expected));
        f.assert_clean();
    }
}

#[test]
fn directories_with_reds_extension_are_not_read_as_files() {
    let f = Fixture::new();
    f.write("src/nested.reds/main.reds", "public func Main() {}");
    assert_success(&f.run(&["--src", "src", "--output", "MyMod"]));
    assert!(
        f.0.join("MyMod/r6/scripts/nested.reds/main.script")
            .is_file()
    );
}

#[test]
fn validator_spawn_failure_rolls_back_staging() {
    let f = Fixture::new();
    f.write("main.reds", "public func Main() {}");
    let executable = f.0.join("missing-redmod.exe");
    for extra in [vec![], vec!["--game-root", f.0.to_str().unwrap()]] {
        let mut args = vec![
            "--src",
            "main.reds",
            "--output",
            "MyMod",
            "--redmod",
            executable.to_str().unwrap(),
            "--timeout",
            "0.1",
        ];
        args.extend(extra);
        let output = f.run(&args);
        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).contains("publishing REDmod package"));
        f.assert_clean();
    }
}

#[test]
fn bundle_symbols_replace_boot_sources() {
    let f = Fixture::new();
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    sources.push_back(
        "engine.reds",
        "public func EngineValue() -> Int32 { return 42; }",
    );
    let interner = TypeInterner::default();
    let baseline = ScriptBundle::default().into_writeable().to_bytes().unwrap();
    let artifact = Compilation::builder()
        .bundle(&baseline)
        .sources(&sources)
        .type_interner(&interner)
        .compile()
        .unwrap()
        .emit()
        .unwrap();
    fs::write(
        f.0.join("baseline.redscripts"),
        artifact.bundle.to_bytes().unwrap(),
    )
    .unwrap();
    f.write(
        "main.reds",
        "public func Main() -> Int32 { return EngineValue(); }",
    );
    assert_success(&f.run(&[
        "--src",
        "main.reds",
        "--bundle",
        "baseline.redscripts",
        "--output",
        "MyMod",
    ]));
    assert!(f.0.join("MyMod/r6/scripts/main.script").is_file());
    assert_eq!(
        fs::read_dir(f.0.join("MyMod/r6/scripts")).unwrap().count(),
        1
    );
}
