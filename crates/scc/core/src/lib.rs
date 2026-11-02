use std::borrow::Cow;
use std::path::PathBuf;
use std::{env, fs, io};

use anyhow::Context;
use output::extract_refs;
pub use output::{SccOutput, SourceRef, SourceRefType};
use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{
    Compilation, Diagnostic, DiagnosticFilter, DiagnosticLevel, Diagnostics, FlushError, SaveError,
    SourceMapExt, Symbols, TypeInterner,
};
use report::{CompilationFailure, ErrorReport};
pub use settings::SccSettings;
use settings::{BACKUP_FILE_EXT, TIMESTAMP_FILE_EXT};
use timestamp::CompileTimestamp;
use vmap::Map;

use crate::lock::FileLock;

mod hints;
mod lock;
mod logger;
mod migration;
mod output;
mod report;
mod settings;
mod timestamp;

const FATAL_DIAGNOSTIC_LEVEL: DiagnosticLevel = DiagnosticLevel::Error;

pub fn compile(settings: &SccSettings) -> anyhow::Result<SccOutput> {
    logger::setup(settings.root_dir());
    log::info!("Running REDscript {}", env!("CARGO_PKG_VERSION"));
    compile_impl(settings)
}

fn compile_impl(settings: &SccSettings) -> Result<SccOutput, anyhow::Error> {
    #[derive(Debug, PartialEq, Eq)]
    enum State {
        Uninitialized,
        Retry,
        Retrying,
    }

    let mut state = State::Uninitialized;

    loop {
        match lock_and_compile(settings, |diagnostics, symbols, sources| {
            if state == State::Uninitialized
                && migration::migrate_if_needed(diagnostics.as_ref(), &symbols, sources)?
            {
                state = State::Retry;
            } else {
                diagnostics.dump(
                    sources,
                    DiagnosticFilter::Predicate(Box::new(diagnostic_filter)),
                )?;
            }
            Ok(())
        }) {
            Ok(output) => {
                log::info!("Compilation successful");
                return Ok(output);
            }
            Err(err) => {
                if state == State::Retry {
                    log::info!("Retrying compilation after a migration");
                    state = State::Retrying;
                    continue;
                }

                log::error!("Compilation failed: {err:#}");
                if settings.should_show_error_report() {
                    let report = ErrorReport::new(&err, settings).to_string();
                    tfd::MessageBox::new("REDscript error", &report)
                        .with_icon(tfd::MessageBoxIcon::Error)
                        .run_modal();
                }
                return Err(err);
            }
        }
    }
}

fn lock_and_compile(
    settings: &SccSettings,
    mut on_error: impl for<'ctx> FnMut(
        &Diagnostics<'ctx>,
        Symbols<'ctx>,
        &'ctx SourceMap,
    ) -> anyhow::Result<()>,
) -> anyhow::Result<SccOutput> {
    let cache_file = settings.cache_file_path();

    let ts_path = cache_file.with_extension(TIMESTAMP_FILE_EXT);
    let mut ts_file = FileLock::new(
        fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&ts_path)
            .with_context(|| format!("failed to open timestamp file '{}'", ts_path.display()))?,
    )
    .with_context(|| {
        format!(
            "failed to acquire a write lock on timestamp file '{}'",
            ts_path.display()
        )
    })?;

    let input_file = prepare_input_cache(settings, &mut ts_file)?;
    let output_file = settings.output_cache_file_path();

    let sources = SourceMap::from_paths_recursively(settings.script_paths())?;
    log::info!(
        "Compiling {} files:\n{}",
        sources.len(),
        sources.display_at(settings.root_dir())
    );
    sources.populate_boot_lib();

    let interner = TypeInterner::default();
    let (mmap, _f) = Map::with_options()
        .open(&input_file)
        .with_context(|| format!("failed to map input cache file '{}'", input_file.display()))?;

    let refs = match Compilation::builder()
        .bundle(&mmap)
        .sources(&sources)
        .type_interner(&interner)
        .diagnostics(&[])
        .type_flags(Cow::Borrowed(settings.type_flags()))
        .fatal_diagnostic_level(FATAL_DIAGNOSTIC_LEVEL)
        .compile()?
        .flush(&output_file)
    {
        Err(FlushError::Write(SaveError::Mmap(err)))
            if err.kind() == io::ErrorKind::PermissionDenied =>
        {
            return Err(anyhow::Error::new(err).context(format!(
                "could not write to '{}', make sure the file is not read-only",
                output_file.display()
            )));
        }
        Err(FlushError::FatalErrors(diagnostics, symbols)) => {
            on_error(&diagnostics, symbols, &sources)?;
            return Err(CompilationFailure::new(diagnostics, &sources, settings)?.into());
        }
        Err(FlushError::Write(err)) => {
            return Err(anyhow::Error::new(err).context(format!(
                "failed to write output cache file '{}'",
                output_file.display()
            )));
        }
        Err(err) => anyhow::bail!("{err}"),
        Ok((syms, diagnostics)) => {
            diagnostics.dump(
                &sources,
                DiagnosticFilter::Predicate(Box::new(diagnostic_filter)),
            )?;

            log::info!("Succesfully written '{}'", output_file.display());
            extract_refs(&syms, &interner)
        }
    };

    let metadata = output_file.metadata().with_context(|| {
        format!(
            "failed to read output cache metadata '{}'",
            output_file.display()
        )
    })?;
    CompileTimestamp::try_from(&metadata)
        .with_context(|| {
            format!(
                "failed to obtain a timestamp of '{}'",
                output_file.display()
            )
        })?
        .write(&mut *ts_file)
        .with_context(|| format!("failed to write timestamp file '{}'", ts_path.display()))?;

    Ok(SccOutput::new(sources, interner, refs))
}

fn prepare_input_cache(settings: &SccSettings, ts_file: &mut fs::File) -> anyhow::Result<PathBuf> {
    let cache_file = settings.cache_file_path();
    if !cache_file.exists() {
        let cache_dir = cache_file
            .parent()
            .context("provided cache file path has no parent directory")?;
        fs::create_dir_all(cache_dir).with_context(|| {
            format!("failed to create cache directory '{}'", cache_dir.display())
        })?;

        let base = settings.unmodified_cache_file_path();
        fs::copy(&base, &cache_file).with_context(|| {
            format!(
                "could not copy base cache file '{}' to '{}'",
                base.display(),
                cache_file.display()
            )
        })?;
    }

    let backup_file = cache_file.with_extension(BACKUP_FILE_EXT);
    if !backup_file.exists() {
        let base = settings.default_backup_cache_file_path();
        if base.exists() {
            log::info!("Re-initializing the backup file from '{}'", base.display());

            fs::copy(&base, &backup_file).with_context(|| {
                format!(
                    "could not copy base redscript backup file '{}' to '{}'",
                    base.display(),
                    backup_file.display()
                )
            })?;
        }
    }

    let write_ts = cache_file
        .metadata()
        .and_then(|meta| CompileTimestamp::try_from(&meta))
        .with_context(|| {
            format!(
                "failed to obtain a timestamp of cache file '{}'",
                cache_file.display()
            )
        })?;
    let saved_ts = CompileTimestamp::read(&mut *ts_file).with_context(|| {
        format!(
            "failed to read existing timestamp file '{}'",
            cache_file.with_extension(TIMESTAMP_FILE_EXT).display()
        )
    })?;

    if settings
        .does_have_separate_output_cache()
        .with_context(|| {
            format!(
                "failed to compare output cache '{}' with input cache '{}'",
                settings.output_cache_file_path().display(),
                cache_file.display()
            )
        })?
    {
        match saved_ts {
            saved_ts if saved_ts != Some(write_ts) && backup_file.exists() => {
                log::info!(
                    "Removing a stale backup file at '{}'",
                    backup_file.display()
                );

                fs::remove_file(&backup_file).with_context(|| {
                    format!(
                        "failed to remove stale backup file '{}'",
                        backup_file.display()
                    )
                })?;
            }
            _ if backup_file.exists() => {
                log::info!("Restoring the backup file to '{}'", cache_file.display());

                fs::rename(&backup_file, &cache_file).with_context(|| {
                    format!(
                        "failed to restore backup file '{}' to '{}'",
                        backup_file.display(),
                        cache_file.display()
                    )
                })?;
            }
            _ => {}
        }
        Ok(cache_file.into_owned())
    } else {
        match saved_ts {
            None if backup_file.exists() => {
                log::info!("Previous cache backup file found");
            }
            saved_ts if saved_ts != Some(write_ts) => {
                log::info!(
                    "The final.redscripts file is not ours, copying it to '{}'",
                    backup_file.display()
                );

                fs::copy(&cache_file, &backup_file).with_context(|| {
                    format!(
                        "failed to copy cache file '{}' to '{}'",
                        cache_file.display(),
                        backup_file.display()
                    )
                })?;
            }
            Some(_) if !backup_file.exists() => {
                anyhow::bail!(
                    "a compilation timestamp was found, but backup files are missing, your \
                     installation might be corrupted, you should remove the contents of 'r6/cache/'
                     directory and verify game files with Steam/GOG"
                );
            }
            _ => {}
        }
        Ok(backup_file)
    }
}

fn diagnostic_filter(dia: &Diagnostic<'_>) -> bool {
    dia.level() == DiagnosticLevel::Error || dia.level() == DiagnosticLevel::Warning
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timestamp_open_failure_includes_path_and_io_cause() {
        let root = env::temp_dir().join(format!(
            "redscript-missing-cache-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let settings = SccSettings::new(root);
        let error = lock_and_compile(&settings, |_, _, _| Ok(()))
            .err()
            .expect("missing cache directory should fail");
        let path = settings
            .cache_file_path()
            .with_extension(TIMESTAMP_FILE_EXT);
        assert_eq!(
            error.to_string(),
            format!("failed to open timestamp file '{}'", path.display())
        );
        let cause = error
            .downcast_ref::<io::Error>()
            .expect("I/O cause is preserved");
        assert_eq!(cause.kind(), io::ErrorKind::NotFound);
        assert!(format!("{error:#}").contains(&cause.to_string()));
    }
}
