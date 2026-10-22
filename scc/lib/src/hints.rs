use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::Context;
use hashbrown::HashMap;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct UserHint {
    id: String,
    message: String,
    file: Option<PathBuf>,
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

    pub fn get_by_error(
        &self,
        error_code: &str,
        path: Option<&Path>,
        source: &str,
        source_line: &str,
    ) -> Option<&UserHint> {
        self.hints.get(error_code)?.iter().find(|a| {
            a.file.as_ref().is_none_or(|p| Some(p.as_path()) == path)
                && (matches!(&a.span_starts_with, Some(str) if source.starts_with(str))
                    || matches!(&a.line_contains, Some(str) if source_line.contains(str)))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loads_and_matches_user_hint_files() {
        let dir = assert_fs::TempDir::new().unwrap();
        fs::write(
            dir.path().join("example.toml"),
            r#"
[[UNRESOLVED_REF]]
id = "example"
message = "Install the required mod — проверка"
file = "example.reds"
span_starts_with = "MissingType"

[[UNRESOLVED_REF]]
id = "line-match"
message = "Check this declaration"
line_contains = "OtherType"
"#,
        )
        .unwrap();
        let hints = UserHints::load(dir.path()).unwrap();
        let hint = hints
            .get_by_error(
                "UNRESOLVED_REF",
                Some(Path::new("example.reds")),
                "MissingType value",
                "",
            )
            .unwrap();
        assert_eq!(hint.id(), "example");
        assert_eq!(hint.message(), "Install the required mod — проверка");
        assert!(
            hints
                .get_by_error("UNRESOLVED_REF", Some(Path::new("other.reds")), "MissingType value", "")
                .is_none()
        );
        assert_eq!(
            hints
                .get_by_error("UNRESOLVED_REF", None, "", "let value: OtherType;")
                .unwrap()
                .id(),
            "line-match"
        );
        assert!(
            hints
                .get_by_error("OTHER_ERROR", None, "MissingType", "OtherType")
                .is_none()
        );
    }

    #[test]
    fn rejects_malformed_hint_files_with_context() {
        let dir = assert_fs::TempDir::new().unwrap();
        fs::write(dir.path().join("broken.toml"), "[[UNRESOLVED_REF]\n").unwrap();
        assert!(
            UserHints::load(dir.path())
                .unwrap_err()
                .to_string()
                .contains("Failed to parse a hint file")
        );
    }
}
