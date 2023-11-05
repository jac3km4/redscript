use std::fmt;
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use anyhow::Context;
use bpaf::ParseFailure;
use fd_lock::RwLock;
use flexi_logger::{Age, Cleanup, Criterion, Duplicate, FileSpec, LevelFilter, LogSpecBuilder, Logger, Naming};
use hashbrown::{HashMap, HashSet};
use redscript::ast::Span;
use redscript::bundle::ScriptBundle;
use redscript_compiler::compiler::{CompilationResources, Compiler};
use redscript_compiler::error::{CompileError, ParseError};
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::StringInterner;
use scc::hints::UserHints;
use scc::opts::{fix_args, Opts};
use scc::timestamp::CompileTimestamp;
use serde::Deserialize;

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
        }
        Err(ParseFailure::Stderr(out)) => {
            eprintln!("{}", out);
            return ExitCode::FAILURE;
        }
    };

    let r6_dir = opts
        .scripts_dir
        .parent()
        .expect("r6/scripts directory must have a parent")
        .to_path_buf();

    setup_logger(&r6_dir);

    if let Err(err) = run(opts, r6_dir) {
        log::error!("{}", err);
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn run(opts: Opts, r6_dir: PathBuf) -> anyhow::Result<()> {
    let manifest = ScriptManifest::load(&opts.scripts_dir)?.unwrap_or_default();

    let mut script_paths = vec![opts.scripts_dir.clone()];
    match opts.script_paths_file.as_deref().map(load_script_paths).transpose() {
        Ok(loaded_paths) => script_paths.extend(loaded_paths.unwrap_or_default()),
        Err(err) => log::warn!("An invalid script paths file was provided: {err}, it will be ignored"),
    };

    let default_cache_dir = r6_dir.join("cache");
    let (bundle_path, cache_dir, fallback_dir) = match (opts.cache_file.as_deref(), opts.cache_dir.as_deref()) {
        (Some(file), _) => {
            log::info!("Script cache file path provided: {}", file.display());
            if opts.cache_dir.is_some() {
                log::warn!("Custom cache directory also provided - ignoring");
            }
            (
                file.to_path_buf(),
                file.parent()
                    .ok_or_else(|| anyhow::anyhow!("script cache path should have a parent"))?
                    .to_path_buf(),
                Some(default_cache_dir.clone()),
            )
        }
        (None, Some(dir)) => {
            log::info!("Custom cache directory provided: {}", dir.display());
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
        fs::copy(base, &bundle_path).expect("Could not copy the base script cache file");
    }

    let mut files = Files::from_dirs(&script_paths, &manifest.source_filter()).expect("Could not load script sources");
    files.include_std();

    match compile_scripts(&r6_dir, &opts.scripts_dir, &bundle_path, fallback_dir.as_deref(), files) {
        Ok(_) => {
            log::info!("Output successfully saved to {}", bundle_path.display());
            Ok(())
        }
        Err(err) => {
            let content = format!(
                "REDScript compilation failed. The game will start, but none of the scripts will take effect. \
                Read below for an error report.\n\n\
                {err}\n\
                If you need more information, consult the logs."
            );
            #[cfg(feature = "popup")]
            msgbox::create("Compilation error", &content, msgbox::IconType::Error).ok();

            log::error!("{}", content);
            Ok(())
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
        .expect("the logger should always start");
}

fn compile_scripts(
    r6_dir: &Path,
    script_dir: &Path,
    bundle_path: &Path,
    fallback_cache_dir: Option<&Path>,
    files: Files,
) -> anyhow::Result<()> {
    let backup_path = bundle_path.with_extension(BACKUP_FILE_EXT);
    let fallback_backup_path = fallback_cache_dir.map(|dir| dir.join(BACKUP_FILE_NAME));
    let timestamp_path = bundle_path.with_extension(TIMESTAMP_FILE_EXT);
    let fallback_timestamp_path = bundle_path
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

    if let Some(fallback_path) = fallback_backup_path.filter(|fallback| fallback.exists() && !backup_path.exists()) {
        log::info!("Re-initializing backup file from {}", fallback_path.display());
        fs::copy(fallback_path, &backup_path).context("Failed to copy the backup file")?;
    }

    let write_timestamp = File::open(bundle_path)
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
            fs::copy(bundle_path, &backup_path).context("Failed to copy the cache file")?;
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

    if !files.is_empty() {
        log::info!(
            "Compiling files in {}:\n{}",
            script_dir.display(),
            files.display(script_dir)
        );
    }

    let interner: StringInterner = StringInterner::default();
    let mut res = CompilationResources::load(&bundle.pool, &interner);
    let output = Compiler::new(res.type_repo, &interner).run(&files);

    match output {
        Ok(output) if !output.reporter().is_compilation_failed() => {
            log::info!("Compilation complete");

            output.commit(&mut res.db, &mut res.type_cache, &mut bundle.pool);
            let mut file = File::create(bundle_path).with_context(|| {
                format!(
                    "Failed to write the output script cache file, navigate to '{}', check its \
                    properties and make sure that it's not marked as read-only",
                    bundle_path.display()
                )
            })?;
            bundle
                .save(&mut io::BufWriter::new(&mut file))
                .context("Failed to save the script cache")?;
            file.sync_all()?;

            CompileTimestamp::of_cache_file(&file)
                .context("Failed to obtain the new compile timestamp")?
                .write(&mut *ts_file)
                .context("Failed to write the new compile timestamp")?;
            Ok(())
        }
        Ok(failed) => {
            let errors = failed.into_errors();
            for error in &errors {
                log::error!("{}", error.display(&files));
            }

            let hints = UserHints::load(r6_dir.join("config").join(USER_HINTS_DIR)).unwrap_or_else(|err| {
                log::error!("Failed to parse one of the user hints TOML files: {}", err);
                UserHints::default()
            });

            Err(ErrorReport::from_errors(errors, script_dir.to_path_buf(), files, hints).into())
        }
        Err(error) => {
            Err(ErrorReport::from_parse_error(error, script_dir.to_path_buf(), files, UserHints::default()).into())
        }
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
    fn from_errors<'id>(
        errors: impl IntoIterator<Item = CompileError<'id>>,
        scripts_dir: PathBuf,
        files: Files,
        hints: UserHints,
    ) -> Self {
        let spans = errors.into_iter().map(|err| (err.code(), err.span())).collect();
        Self {
            scripts_dir,
            files,
            hints,
            spans,
        }
    }

    fn from_parse_error(err: ParseError, scripts_dir: PathBuf, files: Files, hints: UserHints) -> Self {
        Self {
            scripts_dir,
            files,
            hints,
            spans: vec![("SYNTAX_ERR", err.span())],
        }
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
                hints_matched.entry(&act.id).or_insert(act);
            }
        }

        if !offending_mods.is_empty() {
            writeln!(f, "Errors have been found in:")?;
            for mod_ in &offending_mods {
                writeln!(f, "- {}", mod_)?;
            }
        }
        if !hints_matched.is_empty() {
            writeln!(
                f,
                "One or more of the errors found has a known solution. \
                Please follow the instructions below to resolve it:"
            )?;
            for act in hints_matched.values() {
                writeln!(f, "- {}", act.message)?;
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

#[derive(Debug, Deserialize, Default)]
struct ScriptManifest {
    exclusions: HashSet<String>,
}

impl ScriptManifest {
    pub fn load(script_dir: &Path) -> anyhow::Result<Option<Self>> {
        let path = script_dir.join("redscript.toml");
        match fs::read_to_string(path) {
            Ok(contents) => {
                let manifest = toml::from_str(&contents).context("Failed to parse the redscript manifest TOML file")?;
                Ok(Some(manifest))
            }
            Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(None),
            Err(err) => anyhow::bail!("Failed to read the redscript manifest TOML file: {err}"),
        }
    }

    pub fn source_filter(self) -> SourceFilter {
        SourceFilter::Exclude(self.exclusions)
    }
}
