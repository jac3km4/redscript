use std::collections::{HashMap, HashSet};
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead};
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use bpaf::ParseFailure;
use fd_lock::RwLock;
use flexi_logger::{Age, Cleanup, Criterion, Duplicate, FileSpec, LevelFilter, LogSpecBuilder, Logger, Naming};
use opts::{fix_args, Opts};
use redscript::ast::Span;
use redscript::bundle::ScriptBundle;
use redscript_compiler::error::Error;
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::unit::CompilationUnit;
use scc::timestamp::CompileTimestamp;
use serde::Deserialize;

use crate::hints::UserHints;

mod hints;
mod opts;

const BUNDLE_FILE_NAME: &str = "final.redscripts";
const BACKUP_FILE_NAME: &str = "final.redscripts.bk";
const LEGACY_TIMESTAMP_FILE_NAME: &str = "redscript.ts";

const BACKUP_FILE_EXT: &str = "redscripts.bk";
const TIMESTAMP_FILE_EXT: &str = "redscripts.ts";

const USER_HINTS_DIR: &str = "redsUserHints";

fn main() -> ExitCode {
    let opts = match Opts::load(
        fix_args(std::env::args().skip(1).collect())
            .iter()
            .map(String::as_str)
            .collect::<Vec<&str>>()
            .as_slice(),
    ) {
        Ok(opts) => opts,
        Err(ParseFailure::Stdout(out)) => {
            println!("{}", out);
            return ExitCode::SUCCESS;
        },
        Err(ParseFailure::Stderr(out)) => {
            eprintln!("{}", out);
            return ExitCode::FAILURE;
        }
    };

    let scripts_dir = if let Some(dir) = opts.scripts_dir {
        dir
    } else {
        eprintln!("Error: r6/scripts directory is required");
        return ExitCode::FAILURE;
    };

    let r6_dir = scripts_dir.parent().unwrap();
    let default_cache_dir = r6_dir.join("cache");

    setup_logger(r6_dir);

    let manifest = ScriptManifest::load(&scripts_dir).unwrap_or_else(|err| {
        log::info!("Using defaults for the script manifest ({err})");
        ScriptManifest::default()
    });

    let mut script_paths = vec![scripts_dir.clone()];
    match opts.script_paths_file.as_deref().map(load_script_paths).transpose() {
        Ok(loaded_paths) => script_paths.extend(loaded_paths.unwrap_or_default()),
        Err(err) => log::warn!("An invalid script paths file was provided: {err}, it will be ignored"),
    };

    let (bundle_path, cache_dir, fallback_dir) = match (opts.cache_file.as_deref(), opts.cache_dir.as_deref()) {
        (Some(file), _) => {
            log::info!("Bundle path provided: {}", file.to_str().unwrap());
            if opts.cache_dir.is_some() {
                log::warn!("Custom cache directory also provided - ignoring");
            }
            (
                file.to_path_buf(),
                file.parent().unwrap().to_path_buf(),
                Some(default_cache_dir.clone()),
            )
        }
        (None, Some(dir)) => {
            log::info!("Custom cache directory provided: {}", dir.to_str().unwrap());
            (
                dir.join(BUNDLE_FILE_NAME),
                dir.to_path_buf(),
                Some(default_cache_dir.clone()),
            )
        }
        (None, None) => (
            default_cache_dir.join(BUNDLE_FILE_NAME),
            default_cache_dir.clone(),
            None,
        ),
    };

    if !bundle_path.exists() {
        let base = get_base_bundle_path(&default_cache_dir);
        fs::create_dir_all(cache_dir).expect("Could not create the custom cache directory");
        fs::copy(base, &bundle_path).expect("Could not copy base bundle");
    }

    let files = Files::from_dirs(&script_paths, &manifest.source_filter()).expect("Could not load script sources");

    match compile_scripts(&scripts_dir, &bundle_path, fallback_dir.as_deref(), &files) {
        Ok(_) => {
            log::info!("Output successfully saved to {}", bundle_path.display());
            ExitCode::SUCCESS
        }
        Err(err) => {
            let content = error_message(err, &files, r6_dir);
            #[cfg(feature = "popup")]
            msgbox::create("Compilation error", &content, msgbox::IconType::Error).unwrap();

            log::error!("Compilation error: {}", content);
            ExitCode::FAILURE
        }
    }
}

fn load_script_paths(script_paths_file: &Path) -> io::Result<Vec<PathBuf>> {
    io::BufReader::new(File::open(script_paths_file)?)
        .lines()
        .map(|line| line.map(PathBuf::from))
        .collect()
}

fn get_base_bundle_path(cache_dir: &Path) -> PathBuf {
    let bk_path = cache_dir.join(BACKUP_FILE_NAME);
    if bk_path.exists() {
        bk_path
    } else {
        cache_dir.join(BUNDLE_FILE_NAME)
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
        .expect("Failed to initialize the logger");
}

fn compile_scripts(
    script_dir: &Path,
    bundle_path: &Path,
    fallback_cache_dir: Option<&Path>,
    files: &Files,
) -> Result<(), Error> {
    let backup_path = bundle_path.with_extension(BACKUP_FILE_EXT);
    let fallback_backup_path = fallback_cache_dir.map(|dir| dir.join(BACKUP_FILE_NAME));
    let timestamp_path = bundle_path.with_extension(TIMESTAMP_FILE_EXT);
    let fallback_timestamp_path = bundle_path.parent().unwrap().join(LEGACY_TIMESTAMP_FILE_NAME);

    if !timestamp_path.exists() && fallback_timestamp_path.exists() {
        fs::rename(&fallback_timestamp_path, &timestamp_path).expect("Error renaming timestamp");
    }

    let mut ts_lock = RwLock::new(
        OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&timestamp_path)?,
    );
    let mut ts_file = ts_lock.write()?;

    if let Some(fallback_path) = fallback_backup_path.filter(|fallback| fallback.exists() && !backup_path.exists()) {
        log::info!("Re-initializing backup file from {}", fallback_path.display());
        fs::copy(fallback_path, &backup_path)?;
    }

    let write_timestamp = CompileTimestamp::of_cache_file(&File::open(bundle_path)?)?;
    let saved_timestamp = CompileTimestamp::read(&mut *ts_file).ok();

    match saved_timestamp {
        None if backup_path.exists() => {
            log::info!("Previous cache backup file found");
        }
        saved_timestamp if saved_timestamp != Some(write_timestamp) => {
            log::info!(
                "Redscript cache file is not ours, copying it to {}",
                backup_path.display()
            );
            fs::copy(bundle_path, &backup_path)?;
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
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
        ScriptBundle::load(&mut io::Cursor::new(map.as_ref()))?
    };
    #[cfg(not(feature = "mmap"))]
    let mut bundle = ScriptBundle::load(&mut io::BufReader::new(File::open(backup_path)?))?;

    if !files.is_empty() {
        log::info!(
            "Compiling files in {}:\n{}",
            script_dir.display(),
            files.display(script_dir)
        );
    }
    CompilationUnit::new(&mut bundle.pool, vec![])?.compile_and_report(files)?;
    log::info!("Compilation complete");

    let mut file = File::create(bundle_path)?;
    bundle.save(&mut io::BufWriter::new(&mut file))?;
    file.sync_all()?;

    CompileTimestamp::of_cache_file(&file)?.write(&mut *ts_file)?;

    Ok(())
}

#[derive(Debug, Deserialize, Default)]
struct ScriptManifest {
    exclusions: HashSet<String>,
}

impl ScriptManifest {
    pub fn load(script_dir: &Path) -> Result<Self, String> {
        let path = script_dir.join("redscript.toml");
        let contents = fs::read_to_string(path).map_err(|err| match err.kind() {
            io::ErrorKind::NotFound => "manifest not present".to_owned(),
            _ => err.to_string(),
        })?;

        let manifest = toml::from_str(&contents).map_err(|err| format!("manifest parse error: {err}"))?;
        Ok(manifest)
    }

    pub fn source_filter(self) -> SourceFilter {
        SourceFilter::Exclude(self.exclusions)
    }
}

fn error_message(error: Error, files: &Files, r6_dir: &Path) -> String {
    fn detailed_message(spans: &[(&'static str, Span)], files: &Files, r6_dir: &Path) -> Option<String> {
        let scripts_dir = r6_dir.join("scripts");
        let hints = UserHints::load(r6_dir.join("config").join(USER_HINTS_DIR)).unwrap_or_else(|err| {
            log::error!("Failed to parse one of the user actions TOML files: {}", err);
            UserHints::default()
        });
        let mut offending_mods = HashSet::new();
        let mut hints_matched = HashMap::new();

        for &(code, span) in spans {
            let loc = files.lookup(span)?;
            let Ok(rel_path) = loc.file.path().strip_prefix(&scripts_dir) else { continue };
            let cause = rel_path
                .iter()
                .next()
                .unwrap_or_else(|| loc.file.path().as_os_str())
                .to_string_lossy();

            offending_mods.insert(cause);
            if let Some(act) = hints.get_by_error(code, rel_path, loc.file.source_slice(span), loc.enclosing_line()) {
                hints_matched.entry(&act.id).or_insert(act);
            }
        }

        let offending_mods_msg: String = offending_mods.iter().flat_map(|file| ["- ", file, "\n"]).collect();
        let hints_msg: Option<String> = hints_matched
            .is_empty()
            .not()
            .then(|| hints_matched.values().flat_map(|a| ["- ", &a.message, "\n"]).collect());

        let res = if let Some(hints_msg) = hints_msg {
            format!(
                "This is caused by errors in:\n\
                {offending_mods_msg}\
                Based on the errors found, the suggested actions are:\n\
                {hints_msg}\
                If you need more information, consult the logs."
            )
        } else {
            format!(
                "This is caused by errors in:\n\
                {offending_mods_msg}\
                You can try updating or removing these scripts to resolve the issue. If you need more information, consult the logs."
            )
        };
        Some(res)
    }

    let str = match error {
        Error::CompileError(code, span) => detailed_message(&[(code.code(), span)], files, r6_dir).unwrap_or_default(),
        Error::SyntaxError(_, span) => detailed_message(&[("SYNTAX_ERR", span)], files, r6_dir).unwrap_or_default(),
        Error::MultipleErrors(spans) => detailed_message(&spans, files, r6_dir).unwrap_or_default(),
        Error::IoError(err) => format!("This is caused by an I/O error: {err}"),
        Error::PoolError(err) => format!("This is caused by a constant pool error: {err}"),
        _ => String::new(),
    };

    format!("REDScript compilation failed. The game will start, but none of the scripts will take effect. {str}")
}
