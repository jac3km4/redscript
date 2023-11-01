use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::Context;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct UserHint {
    pub id: String,
    pub message: String,
    pub file: Option<PathBuf>,
    pub span_starts_with: Option<String>,
    pub line_contains: Option<String>,
}

#[derive(Debug, Default)]
pub struct UserHints {
    pub hints: HashMap<String, Vec<UserHint>>,
}

impl UserHints {
    pub fn load(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let mut hints = HashMap::new();
        if !path.as_ref().exists() {
            return Ok(Self { hints });
        }

        let dir = fs::read_dir(path).context("Failed to read the hints directory")?;
        for entry in dir {
            let entry = entry.context("Failed to read a hint directory entry")?;
            if entry.path().extension() == Some(OsStr::new("toml")) {
                let contents = fs::read_to_string(entry.path()).context("Failed to read a hint file")?;
                let contents: HashMap<String, Vec<UserHint>> =
                    toml::from_str(&contents).context("Failed to parse a hint file")?;
                for (key, val) in contents {
                    hints.entry(key).or_default().extend(val);
                }
            }
        }
        Ok(Self { hints })
    }

    pub fn get_by_error(&self, error_code: &str, path: &Path, source: &str, source_line: &str) -> Option<&UserHint> {
        self.hints.get(error_code)?.iter().find(|a| {
            a.file.as_ref().map_or(true, |p| p == path)
                && (a.span_starts_with.as_deref().is_some_and(|str| source.starts_with(str))
                    || a.line_contains.as_deref().is_some_and(|str| source_line.contains(str)))
        })
    }
}
