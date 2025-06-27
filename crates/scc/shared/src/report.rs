use std::fmt;
use std::path::PathBuf;

use hashbrown::{HashMap, HashSet};
use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{Diagnostic, Diagnostics};
use thiserror::Error;

use crate::SccSettings;
use crate::hints::UserHints;

#[derive(Debug)]
pub struct ErrorReport<'ctx> {
    cause: &'ctx anyhow::Error,
}

impl<'ctx> ErrorReport<'ctx> {
    pub fn new(cause: &'ctx anyhow::Error) -> Self {
        Self { cause }
    }
}

impl fmt::Display for ErrorReport<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "REDscript compilation has failed.")?;
        writeln!(f)?;

        if let Some(source) = self.cause.downcast_ref::<CompilationFailure>() {
            writeln!(f, "Fatal errors were found in the files listed below:")?;
            for file in &source.failing_files {
                writeln!(f, "{}", file.display())?;
            }
            writeln!(f)?;

            if !source.hints.is_empty() {
                writeln!(
                    f,
                    "One or more of the errors found has a known solution. Here are the \
                    recommended steps:"
                )?;
                for hint in &source.hints {
                    writeln!(f, "- {hint}")?;
                }
            } else {
                writeln!(
                    f,
                    "You should check if these mods are outdated and update them if possible. They may \
                 also be incompatible with the current version of the game, in which case you \
                 should remove them and try again."
                )?;
            }
        } else {
            writeln!(f, "Reason: {}", self.cause)?;
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
#[error("fatal errors found")]
pub struct CompilationFailure {
    failing_files: Vec<PathBuf>,
    hints: Vec<String>,
}

impl CompilationFailure {
    pub fn new(
        diagnostics: Diagnostics<'_>,
        sources: &SourceMap,
        settings: &SccSettings,
    ) -> anyhow::Result<Self> {
        let fatal = diagnostics
            .into_iter()
            .filter(Diagnostic::is_fatal)
            .collect::<Vec<_>>();

        let failing_files = fatal
            .iter()
            .map(|d| d.span().file)
            .collect::<HashSet<_>>()
            .into_iter()
            .filter_map(|f| {
                let path = sources.get(f)?.path();
                let path = path.strip_prefix(settings.root_dir()).unwrap_or(path);
                Some(path.to_owned())
            })
            .collect();

        let hints_config = UserHints::load(settings.user_hints_dir()).unwrap_or_else(|err| {
            log::warn!("Failed to parse one of the user hints TOML files: {err}");
            UserHints::default()
        });

        let mut hints = HashMap::new();
        for hint in fatal.iter().filter_map(|d| {
            let candidates = hints_config.get_by_code(d.code())?;
            let span = d.span();
            let file = sources.get(span.file)?;
            let source = file.span_contents(span);
            let (line, _) = file.line_and_offset(span.start);
            let line_source = file.line_contents(line)?;
            candidates
                .iter()
                .find(|hint| hint.does_match(file.path().file_name(), source, line_source))
        }) {
            hints
                .entry(hint.id())
                .or_insert_with(|| hint.message().to_owned());
        }

        Ok(Self {
            failing_files,
            hints: hints.into_values().collect(),
        })
    }
}
