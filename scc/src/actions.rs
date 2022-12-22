use std::collections::HashMap;
use std::error::Error;
use std::ffi::OsStr;
use std::path::Path;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct UserAction {
    pub id: String,
    pub message: String,
    pub span_starts_with: Option<String>,
    pub line_contains: Option<String>,
}

#[derive(Debug)]
pub struct UserActions {
    pub actions: HashMap<String, Vec<UserAction>>,
}

impl UserActions {
    pub fn load(path: impl AsRef<Path>) -> Result<Self, Box<dyn Error>> {
        let mut actions = HashMap::new();
        if !path.as_ref().exists() {
            return Ok(Self { actions });
        }
        let dir = std::fs::read_dir(path)?;
        for entry in dir {
            let entry = entry?;
            if entry.path().extension() == Some(OsStr::new("toml")) {
                let contents = std::fs::read_to_string(entry.path())?;
                let contents: HashMap<String, Vec<UserAction>> = toml::from_str(&contents)?;
                actions.extend(contents);
            }
        }
        Ok(Self { actions })
    }

    pub fn get_by_error(&self, error_code: &str, source: &str, source_line: &str) -> Option<&UserAction> {
        self.actions.get(error_code)?.iter().find(|a| {
            matches!(&a.span_starts_with, Some(str) if source.starts_with(str))
                || matches!(&a.line_contains, Some(str) if source_line.contains(str))
        })
    }
}
