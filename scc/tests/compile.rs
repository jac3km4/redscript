use std::fs;
use std::fs::{File, OpenOptions};
use std::process::Command;

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use scc::timestamp::*;

mod compile {

    use super::*;

    #[test]
    fn no_args() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("scc")?;
        cmd.assert()
            .failure()
            .stderr(predicate::str::contains("r6/scripts directory is required"));
        Ok(())
    }

    #[test]
    fn help() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("scc")?;
        cmd.arg("--help");
        cmd.assert()
            .success()
            .stdout(predicate::str::contains("Available positional items"));
        Ok(())
    }

    #[test]
    fn bundle_result() -> Result<(), Box<dyn std::error::Error>> {
        let temp = assert_fs::TempDir::new().unwrap();
        let scc_dir = std::env::current_dir().unwrap();
        let project_dir = scc_dir.parent().unwrap();

        let predef = project_dir.join("resources/predef.redscripts");
        let predef_cmp = project_dir.join("resources/predef.redscripts.cmp");
        let bundle_path = temp.child("final.redscripts");
        fs::copy(predef, &bundle_path).expect("Could not copy predef.redscripts");

        let script_file = temp.child("test.reds");
        script_file.write_str("class TestClass {}")?;

        let mut cmd = Command::cargo_bin("scc")?;
        cmd.arg("-compile").arg(script_file.path()).arg(bundle_path.path());
        cmd.assert()
            .success()
            .stdout(predicate::str::contains("Output successfully saved"));

        bundle_path.assert(predicate::path::eq_file(predef_cmp.as_path()));
        temp.close().unwrap();
        Ok(())
    }

    #[test]
    fn timestamp_migration() -> Result<(), Box<dyn std::error::Error>> {
        let temp = assert_fs::TempDir::new().unwrap();
        let scc_dir = std::env::current_dir().unwrap();
        let project_dir = scc_dir.parent().unwrap();

        let predef = project_dir.join("resources/predef.redscripts");
        let bundle_path = temp.child("final.redscripts");
        let backup_path = temp.child("final.redscripts.bk");
        let new_ts_path = temp.child("final.redscripts.ts");
        fs::copy(&predef, &bundle_path).expect("Could not copy predef.redscripts to bundle path");
        fs::copy(&predef, &backup_path).expect("Could not copy predef.redscripts to backup path");

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

        temp.close().unwrap();
        Ok(())
    }
}
