pub mod api;
pub mod hints;
pub mod timestamp;

use std::fs::{self, File, OpenOptions};
use std::path::{Path, PathBuf};
use std::{fmt, io, iter, vec};

use anyhow::Context;
use api::{SccOutput, SccResult, SccSettings};
use fd_lock::RwLock;
use flexi_logger::{Age, Cleanup, Criterion, Duplicate, FileSpec, LogSpecBuilder, Logger, Naming};
use hashbrown::{HashMap, HashSet};
use hints::UserHints;
use log::LevelFilter;
use redscript::ast::Span;
use redscript::bundle::ScriptBundle;
use redscript_compiler::error::Error;
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::unit::CompilationUnit;
use timestamp::CompileTimestamp;

const BUNDLE_FILE_NAME: &str = "final.redscripts";
const BACKUP_FILE_NAME: &str = "final.redscripts.bk";
const LEGACY_TIMESTAMP_FILE_NAME: &str = "redscript.ts";

const BACKUP_FILE_EXT: &str = "redscripts.bk";
const TIMESTAMP_FILE_EXT: &str = "redscripts.ts";

const USER_HINTS_DIR: &str = "redsUserHints";

pub fn compile(settings: &SccSettings) -> Box<SccResult> {
    setup_logger(&settings.r6_dir);

    match try_run(settings) {
        Ok(output) => Box::new(output),
        Err(err) => {
            log::error!("{}", err);
            Box::new(SccResult::Error(err))
        }
    }
}

fn try_run(settings: &SccSettings) -> anyhow::Result<SccResult> {
    let default_cache_dir = settings.r6_dir.join("cache");
    let cache_file = settings
        .custom_cache_file
        .as_deref()
        .map(PathBuf::from)
        .unwrap_or_else(|| default_cache_dir.join(BUNDLE_FILE_NAME));
    let script_paths = iter::once(settings.r6_dir.join("scripts").into_boxed_path())
        .chain(settings.additional_script_paths.iter().cloned())
        .collect::<Vec<_>>();

    if !cache_file.exists() {
        let base_cache_file = get_base_bundle_path(&default_cache_dir);
        let cache_dir = cache_file.parent().context("Provided cache file path has no parent")?;
        fs::create_dir_all(cache_dir).context("Failed to create the cache directory")?;
        fs::copy(base_cache_file, &cache_file).context("Could not copy the base script cache file")?;
    }

    let backup_path = cache_file.with_extension(BACKUP_FILE_EXT);
    let fallback_backup_path = settings.r6_dir.join("cache").join(BACKUP_FILE_NAME);

    if fallback_backup_path.exists() && !backup_path.exists() {
        log::info!("Re-initializing backup file from {}", fallback_backup_path.display());
        fs::copy(fallback_backup_path, &backup_path).context("Failed to copy the backup file")?;
    }

    let files = Files::from_dirs(&script_paths, &SourceFilter::None).context("Could not load script sources")?;

    match compile_at(&settings.r6_dir, &cache_file, files) {
        Ok(output) => {
            log::info!("Output successfully saved to {}", cache_file.display());
            Ok(output)
        }
        Err(err) => {
            log::error!("{err}");

            #[cfg(feature = "popup")]
            {
                let content = format!(
                    "{err}\n\
                    If you need more information, consult the logs."
                );
                msgbox::create("Compilation error", &content, msgbox::IconType::Error).ok();
            }
            Err(err)
        }
    }
}

fn setup_logger(r6_dir: &Path) {
    let file = FileSpec::default().directory(r6_dir.join("logs")).basename("redscript");
    Logger::with(LogSpecBuilder::new().default(LevelFilter::Info).build())
        .log_to_file(file)
        .duplicate_to_stdout(Duplicate::All)
        .rotate(Criterion::Age(Age::Day), Naming::Timestamps, Cleanup::KeepLogFiles(4))
        .format(|out, time, msg| write!(out, "[{} - {}] {}", msg.level(), time.now().to_rfc2822(), msg.args()))
        .start()
        .ok();
}

fn compile_at(r6_dir: &Path, cache_file: &Path, files: Files) -> anyhow::Result<SccResult> {
    let backup_path = cache_file.with_extension(BACKUP_FILE_EXT);
    let timestamp_path = cache_file.with_extension(TIMESTAMP_FILE_EXT);

    let fallback_timestamp_path = cache_file
        .parent()
        .expect("script cache path should have a parent")
        .join(LEGACY_TIMESTAMP_FILE_NAME);

    if !timestamp_path.exists() && fallback_timestamp_path.exists() {
        fs::rename(&fallback_timestamp_path, &timestamp_path).context("Failed to rename the legacy timestamp file")?;
    }

    let mut ts_lock = RwLock::new(
        OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&timestamp_path)
            .context("Failed to open the timestamp file")?,
    );

    let mut ts_file = ts_lock
        .write()
        .context("Failed to acquire a write lock on the timestamp file")?;

    let write_timestamp = File::open(cache_file)
        .and_then(|f| CompileTimestamp::of_cache_file(&f))
        .context("Failed to obtain a timestamp of the cache file")?;
    let saved_timestamp =
        CompileTimestamp::read(&mut *ts_file).context("Failed to read the existing timestamp file")?;

    match saved_timestamp {
        None if backup_path.exists() => {
            log::info!("Previous cache backup file found");
        }
        saved_timestamp if saved_timestamp != Some(write_timestamp) => {
            log::info!(
                "Redscript cache file is not ours, copying it to {}",
                backup_path.display()
            );
            fs::copy(cache_file, &backup_path).context("Failed to copy the cache file")?;
        }
        Some(_) if !backup_path.exists() => {
            log::warn!(
                "A compiler timestamp was found but not the backup file, your installation might be corrupted, \
                 try removing redscript.ts and verifying game files"
            );
        }
        _ => {}
    }

    #[cfg(feature = "mmap")]
    let mut bundle = {
        let (map, _) = vmap::Map::with_options()
            .open(backup_path)
            .context("Failed to open the original script cache file")?;
        ScriptBundle::load(&mut io::Cursor::new(map.as_ref())).context("Failed to load the original script cache")?
    };
    #[cfg(not(feature = "mmap"))]
    let mut bundle = {
        let file = File::open(backup_path).context("Failed to open the original script cache file")?;
        ScriptBundle::load(&mut io::BufReader::new(file)).context("Failed to load the original script cache")?
    };

    let default_scripts_dir = r6_dir.join("scripts");
    if !files.is_empty() {
        log::info!(
            "Compiling files in {}:\n{}",
            default_scripts_dir.display(),
            files.display(&default_scripts_dir)
        );
    }
    match CompilationUnit::new(&mut bundle.pool, vec![])
        .map_err(|err| anyhow::anyhow!("Failed to create the compilation unit: {err}"))?
        .compile_and_report(&files)
    {
        Ok(compilation) => {
            log::info!("Compilation complete");

            let mut file = File::create(cache_file)?;
            bundle.save(&mut io::BufWriter::new(&mut file))?;
            file.sync_all()?;

            CompileTimestamp::of_cache_file(&file)?.write(&mut *ts_file)?;

            let output = SccOutput::new(compilation, bundle, files);
            Ok(SccResult::Success(Box::new(output)))
        }
        Err(err) => {
            let hints = UserHints::load(r6_dir.join("config").join(USER_HINTS_DIR)).unwrap_or_else(|err| {
                log::error!("Failed to parse one of the user hints TOML files: {}", err);
                UserHints::default()
            });

            Err(ErrorReport::from_error(err, default_scripts_dir.clone(), files, hints)?.into())
        }
    }
}

fn get_base_bundle_path(cache_dir: &Path) -> PathBuf {
    let bk_path = cache_dir.join(BACKUP_FILE_NAME);
    if bk_path.exists() {
        bk_path
    } else {
        cache_dir.join(BUNDLE_FILE_NAME)
    }
}

#[derive(Debug)]
struct ErrorReport {
    scripts_dir: PathBuf,
    files: Files,
    hints: UserHints,
    spans: Vec<(&'static str, Span)>,
}

impl ErrorReport {
    fn from_error(error: Error, scripts_dir: PathBuf, files: Files, hints: UserHints) -> anyhow::Result<Self> {
        let spans = match error {
            Error::CompileError(code, span) => {
                vec![(code.code(), span)]
            }
            Error::SyntaxError(_, span) => vec![("SYNTAX_ERR", span)],
            Error::CteError(_, span) => vec![("CTE_ERR", span)],
            Error::MultipleErrors(spans) => spans,
            Error::IoError(err) => anyhow::bail!("There's been an I/O error: {err}"),
            Error::PoolError(err) => anyhow::bail!("There's been a constant pool error: {err}"),
        };

        Ok(Self {
            scripts_dir,
            files,
            hints,
            spans,
        })
    }
}

impl fmt::Display for ErrorReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut offending_mods = HashSet::new();
        let mut hints_matched = HashMap::new();

        for &(code, span) in &self.spans {
            let loc = self.files.lookup(span).expect("span should point to a source map file");
            let Ok(rel_path) = loc.file.path().strip_prefix(&self.scripts_dir) else {
                continue;
            };
            let cause = rel_path
                .iter()
                .next()
                .unwrap_or_else(|| loc.file.path().as_os_str())
                .to_string_lossy();

            offending_mods.insert(cause);
            if let Some(act) =
                self.hints
                    .get_by_error(code, rel_path, loc.file.source_slice(span), loc.enclosing_line())
            {
                hints_matched.entry(act.id()).or_insert(act);
            }
        }

        writeln!(
            f,
            "REDScript compilation failed. \
            The game will start, but none of the scripts will take effect."
        )?;

        if !offending_mods.is_empty() {
            writeln!(f, "This error has been caused by:")?;
            for mod_ in &offending_mods {
                writeln!(f, "- {}", mod_)?;
            }
        }
        if !hints_matched.is_empty() {
            writeln!(
                f,
                "One or more of the errors found has a known solution. \
                Here are the recommended steps:"
            )?;
            for act in hints_matched.values() {
                writeln!(f, "- {}", act.message())?;
            }
        } else {
            writeln!(
                f,
                "You should check if your REDScript mods are outdated and update them if necessary. \
                They may also be incompatible with the current version of the game, \
                in which case you should remove them and try again."
            )?;
        }
        Ok(())
    }
}

impl std::error::Error for ErrorReport {}
