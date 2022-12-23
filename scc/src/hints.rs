use std::collections::HashMap;
use std::error::Error;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};

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
    pub fn load(path: impl AsRef<Path>) -> Result<Self, Box<dyn Error>> {
        let mut hints = HashMap::new();
        if !path.as_ref().exists() {
            return Ok(Self { hints });
        }
        let dir = std::fs::read_dir(path)?;
        for entry in dir {
            let entry = entry?;
            if entry.path().extension() == Some(OsStr::new("toml")) {
                let contents = std::fs::read_to_string(entry.path())?;
                let contents: HashMap<String, Vec<UserHint>> = toml::from_str(&contents)?;
                hints.extend(contents);
            }
        }
        Ok(Self { hints })
    }

    pub fn get_by_error(&self, error_code: &str, path: &Path, source: &str, source_line: &str) -> Option<&UserHint> {
        self.hints.get(error_code)?.iter().find(|a| {
            a.file.as_ref().map_or(true, |p| p == path)
                && (matches!(&a.span_starts_with, Some(str) if source.starts_with(str))
                    || matches!(&a.line_contains, Some(str) if source_line.contains(str)))
        })
    }
}
