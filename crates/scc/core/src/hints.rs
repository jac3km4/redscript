use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::Path;

use anyhow::Context;
use hashbrown::HashMap;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct UserHint {
    id: String,
    message: String,
    file_name: Option<OsString>,
    span_starts_with: Option<String>,
    line_contains: Option<String>,
}

impl UserHint {
    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn matches(&self, file_name: Option<&OsStr>, source: &str, source_line: &str) -> bool {
        self.file_name
            .as_ref()
            .is_none_or(|p| Some(p.as_os_str()) == file_name)
            && (self
                .span_starts_with
                .as_deref()
                .is_some_and(|str| source.starts_with(str))
                || self
                    .line_contains
                    .as_deref()
                    .is_some_and(|str| source_line.contains(str)))
    }
}

#[derive(Debug, Default)]
pub struct UserHints {
    hints: HashMap<String, Vec<UserHint>>,
}

impl UserHints {
    pub fn load(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let mut hints = HashMap::new();
        if !path.as_ref().exists() {
            return Ok(Self { hints });
        }

        let dir = fs::read_dir(path).context("failed to read the hints directory")?;
        for entry in dir {
            let entry = entry.context("failed to read a hint directory entry")?;
            if entry.path().extension() == Some(OsStr::new("toml")) {
                let contents =
                    fs::read_to_string(entry.path()).context("failed to read a hint file")?;
                let contents: HashMap<String, Vec<UserHint>> =
                    toml::from_str(&contents).context("failed to parse a hint file")?;
                for (key, val) in contents {
                    hints.entry(key).or_default().extend(val);
                }
            }
        }
        Ok(Self { hints })
    }

    pub fn get_by_code(&self, error_code: &str) -> Option<&[UserHint]> {
        self.hints.get(error_code).map(Vec::as_slice)
    }
}
