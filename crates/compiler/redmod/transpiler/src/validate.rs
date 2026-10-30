//! Optional official compiler invocation. Successful validation is not a runtime guarantee.
use std::fs::{self, OpenOptions};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};
use std::{io, thread};

/// Explicit installation path and bounded execution; no discovery or download occurs.
#[derive(Debug, Clone)]
pub struct OfficialValidator {
    pub executable: PathBuf,
    pub timeout: Duration,
}

#[derive(Debug, Clone)]
pub struct ValidationReport {
    pub stdout: String,
    pub stderr: String,
}

impl OfficialValidator {
    /// Run against a dedicated staging directory. Existing output/cache logs are
    /// rejected, so stale artifacts cannot be mistaken for this invocation's result.
    /// The supplied executable is trusted and may read the local game installation.
    pub fn validate(&self, mod_directory: &Path) -> io::Result<ValidationReport> {
        self.validate_inner(mod_directory, None)
    }

    /// Use an explicit game root for installations without base-game blobs beside
    /// the executable. The compiler reads this root; outputs remain in the mod.
    pub fn validate_with_root(
        &self,
        mod_directory: &Path,
        game_root: &Path,
    ) -> io::Result<ValidationReport> {
        if !game_root.is_absolute() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "validator game root must be absolute",
            ));
        }
        self.validate_inner(mod_directory, Some(game_root))
    }

    fn validate_inner(
        &self,
        mod_directory: &Path,
        game_root: Option<&Path>,
    ) -> io::Result<ValidationReport> {
        if !self.executable.is_absolute() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "validator executable must be absolute",
            ));
        }
        let directory = compiler_path(&fs::canonicalize(mod_directory)?);
        let cache = directory.join("r6/cache");
        fs::create_dir_all(&cache)?;
        for name in ["final.redscripts", "final.redscripts.h64"] {
            if fs::symlink_metadata(cache.join(name)).is_ok() {
                return Err(io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    "validation requires fresh cache artifacts",
                ));
            }
        }
        let stdout_path = directory.join("redmod-validation.stdout.log");
        let stderr_path = directory.join("redmod-validation.stderr.log");
        let stdout = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&stdout_path)?;
        let stderr = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&stderr_path)?;
        // Files, not unread pipes: large compiler logs cannot deadlock the child.
        let mut command = Command::new(&self.executable);
        command
            .current_dir(
                self.executable
                    .parent()
                    .expect("absolute executable parent"),
            )
            .arg("validate-mod-scripts")
            .arg("-compile");
        if let Some(root) = game_root {
            // Unlike -directory, the official tool does not accept -root=<path>.
            command.arg("-root").arg(compiler_path(root));
        }
        let mut child = command
            .arg("-directory")
            .arg(&directory)
            .stdin(Stdio::null())
            .stdout(stdout)
            .stderr(stderr)
            .spawn()?;
        let started = Instant::now();
        let status = loop {
            if let Some(status) = child.try_wait()? {
                break status;
            }
            if started.elapsed() >= self.timeout {
                drop(child.kill());
                drop(child.wait());
                return Err(io::Error::new(
                    io::ErrorKind::TimedOut,
                    "official REDmod validation timed out",
                ));
            }
            thread::sleep(Duration::from_millis(20));
        };
        let report = ValidationReport {
            stdout: String::from_utf8_lossy(&fs::read(stdout_path)?).into_owned(),
            stderr: String::from_utf8_lossy(&fs::read(stderr_path)?).into_owned(),
        };
        let fresh_outputs = ["final.redscripts", "final.redscripts.h64"]
            .iter()
            .all(|name| {
                fs::symlink_metadata(cache.join(name))
                    .is_ok_and(|m| m.file_type().is_file() && m.len() > 0)
            });
        if !status.success()
            || !report
                .stdout
                .lines()
                .chain(report.stderr.lines())
                .any(final_success)
            || !fresh_outputs
        {
            return Err(io::Error::other(format!(
                "official REDmod validation failed: status={status}; final success marker and fresh nonempty cache artifacts required\n{}\n{}",
                report.stdout, report.stderr
            )));
        }
        Ok(report)
    }
}

// REDmod's file APIs reject Rust's canonical Windows verbatim paths (error 3).
fn compiler_path(path: &Path) -> PathBuf {
    #[cfg(windows)]
    {
        use std::path::{Component, Prefix};
        let mut components = path.components();
        if let Some(Component::Prefix(prefix)) = components.next() {
            let ordinary = match prefix.kind() {
                Prefix::VerbatimDisk(drive) => {
                    Some(PathBuf::from(format!("{}:\\", char::from(drive))))
                }
                Prefix::VerbatimUNC(server, share) => {
                    let mut root = std::ffi::OsString::from("\\\\");
                    root.push(server);
                    root.push("\\");
                    root.push(share);
                    root.push("\\");
                    Some(PathBuf::from(root))
                }
                _ => None,
            };
            if let Some(mut ordinary) = ordinary {
                if matches!(components.clone().next(), Some(Component::RootDir)) {
                    components.next();
                }
                ordinary.push(components.as_path());
                return ordinary;
            }
        }
    }
    path.to_path_buf()
}

fn final_success(line: &str) -> bool {
    let line = line.trim();
    let line = line.strip_suffix('.').unwrap_or(line);
    let marker = "Commandlet validate-mod-scripts has succeeded";
    line == marker
        || line
            .strip_suffix(marker)
            .is_some_and(|prefix| prefix.ends_with(char::is_whitespace) || prefix.ends_with(']'))
}

#[cfg(test)]
mod tests {
    use super::final_success;

    #[cfg(windows)]
    #[test]
    fn compiler_paths_remove_only_verbatim_disk_and_unc_prefixes() {
        use std::path::Path;
        for (input, expected) in [
            (r"\\?\C:\mods\Example", r"C:\mods\Example"),
            (r"\\?\UNC\server\share\Example", r"\\server\share\Example"),
            (r"C:\mods\Example", r"C:\mods\Example"),
        ] {
            assert_eq!(super::compiler_path(Path::new(input)), Path::new(expected));
        }
    }

    #[test]
    fn final_marker_allows_log_prefixes_but_not_trailing_text() {
        for line in [
            "Commandlet validate-mod-scripts has succeeded",
            "Commandlet validate-mod-scripts has succeeded.",
            "[INFO] Commandlet validate-mod-scripts has succeeded.",
            "[INFO] Commandlet validate-mod-scripts has succeeded",
            "[2026-10-29 12:00:00][INFO]Commandlet validate-mod-scripts has succeeded",
        ] {
            assert!(final_success(line), "{line}");
        }
        for line in [
            "Validate mod scripts success!",
            "Commandlet validate-mod-scripts has failed.",
            "Commandlet validate-mod-scripts has succeeded (not really)",
            "NotCommandlet validate-mod-scripts has succeeded",
        ] {
            assert!(!final_success(line), "{line}");
        }
    }
}
