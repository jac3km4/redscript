use std::fs;
use std::fs::{File, OpenOptions};
use std::path::Path;
use std::process::Command;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use scc_lib::timestamp::*;

#[test]
fn no_args() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("scc")?;
    cmd.assert().failure().stderr(predicate::str::contains(
        "Expected <-compile>, pass --help for usage information",
    ));
    Ok(())
}

#[test]
fn help() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("scc")?;
    cmd.arg("--help");
    cmd.assert().failure().stderr(predicate::str::contains("not compile\n"));
    Ok(())
}

#[test]
fn bundle_result() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let predef = Path::new("../../resources/predef.redscripts");
    let predef_cmp = Path::new("../../resources/predef.redscripts.cmp");
    let bundle_path = temp.child("final.redscripts");
    fs::copy(predef, &bundle_path).expect("should copy predef.redscripts to bundle path");

    let script_file = temp.child("scripts/test.reds");
    script_file.write_str("class TestClass {}")?;

    let mut cmd = Command::cargo_bin("scc")?;
    cmd.arg("-compile")
        .arg(temp.child("scripts").path())
        .arg(bundle_path.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Output successfully saved"));

    bundle_path.assert(predicate::path::eq_file(predef_cmp));
    temp.close()?;
    Ok(())
}

#[test]
fn timestamp_migration() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let predef = Path::new("../../resources/predef.redscripts");
    let bundle_path = temp.child("final.redscripts");
    let backup_path = temp.child("final.redscripts.bk");
    let new_ts_path = temp.child("final.redscripts.ts");
    fs::copy(predef, &bundle_path).expect("Could not copy predef.redscripts to bundle path");
    fs::copy(predef, backup_path).expect("Could not copy predef.redscripts to backup path");

    let ts_path = temp.child("redscript.ts");
    let bundle_file = File::open(&bundle_path)?;
    let mut ts_file = OpenOptions::new().read(true).write(true).create(true).open(&ts_path)?;
    CompileTimestamp::of_cache_file(&bundle_file)?.write(&mut ts_file)?;

    let script_path = temp.child("test.reds");
    script_path.write_str("class TestClass {}")?;

    assert!(ts_path.exists(), "Old timestamp file doesn't exist");
    assert!(!new_ts_path.exists(), "New timestamp file already exists");

    let mut cmd = Command::cargo_bin("scc")?;
    cmd.arg("-compile").arg(script_path.path()).arg(bundle_path.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Output successfully saved"));

    assert!(!ts_path.exists(), "Old timestamp file still exists");
    assert!(new_ts_path.exists(), "New timestamp file doesn't exist");

    temp.close()?;
    Ok(())
}

#[test]
fn custom_output_clean() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let predef = Path::new("../../resources/predef.redscripts");
    let predef_cmp = Path::new("../../resources/predef.redscripts.cmp");
    let bundle_path = temp.child("final.redscripts");
    let output_path = temp.child("final.redscripts.modded");
    fs::copy(predef, &bundle_path).expect("should copy predef.redscripts to bundle path");

    let script_file = temp.child("scripts/test.reds");
    script_file.write_str("class TestClass {}")?;

    let mut cmd = Command::cargo_bin("scc")?;
    cmd.arg("-compile")
        .arg(temp.child("scripts").path())
        .arg("-outputCacheFile")
        .arg(output_path.path())
        .arg(bundle_path.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Output successfully saved"));

    output_path.assert(predicate::path::eq_file(predef_cmp));
    temp.close()?;
    Ok(())
}

#[test]
fn custom_output_with_backup() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let predef = Path::new("../../resources/predef.redscripts");
    let predef_cmp = Path::new("../../resources/predef.redscripts.cmp");
    let bundle_path = temp.child("final.redscripts");
    let backup_path = temp.child("final.redscripts.bk");
    let output_path = temp.child("final.redscripts.modded");
    fs::copy(predef, &bundle_path).expect("should copy predef.redscripts to bundle path");
    fs::copy(predef, &backup_path).expect("should copy predef.redscripts to backup path");

    let script_file = temp.child("scripts/test.reds");
    script_file.write_str("class TestClass {}")?;

    let mut cmd = Command::cargo_bin("scc")?;
    cmd.arg("-compile")
        .arg(temp.child("scripts").path())
        .arg("-outputCacheFile")
        .arg(output_path.path())
        .arg(bundle_path.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Output successfully saved"));

    backup_path.assert(predicate::path::missing());
    output_path.assert(predicate::path::eq_file(predef_cmp));
    temp.close()?;
    Ok(())
}
