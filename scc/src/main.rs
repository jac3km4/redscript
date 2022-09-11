use std::collections::HashSet;
use std::fs::{self, File, OpenOptions};
use std::io;
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::SystemTime;

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use fd_lock::RwLock;
use redscript::ast::Span;
use redscript::bundle::ScriptBundle;
use redscript_compiler::error::Error;
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::unit::CompilationUnit;
use serde::Deserialize;
use time::format_description::well_known::Rfc3339 as Rfc3339Format;
use time::format_description::FormatItem;
use time::macros::format_description;
use time::OffsetDateTime;

fn main() -> ExitCode {
    // the way cyberpunk passes CLI args is broken, this is a workaround
    let mut iter = std::env::args().skip(1);
    match (iter.next().as_deref(), iter.next()) {
        (Some("-compile"), Some(path_str)) => {
            let script_dir = PathBuf::from(path_str.split('"').next().unwrap());
            let r6_dir = script_dir.parent().unwrap();

            // load manifest without fallback
            let manifest = ScriptManifest::load(&script_dir);

            // set up logger with an optional manifest
            let append_date_to_logfiles = manifest
                .as_ref()
                .ok()
                .and_then(|m| m.append_date_to_logfiles)
                .unwrap_or(false);
            setup_logger(r6_dir, append_date_to_logfiles).expect("Could not set up the logger");

            // get manifest or fallback
            let manifest = manifest.unwrap_or_else(|err| {
                log::info!(
                    "Could not load the manifest, falling back to defaults (caused by {})",
                    err
                );
                ScriptManifest::default()
            });

            let (cache_dir, bundle_dir_override) = match (iter.next().as_deref(), iter.next()) {
                (Some("-customCacheDir"), Some(custom_path)) => {
                    log::info!("Custom cache directory provided: {}", custom_path);
                    let cache_dir = PathBuf::from(custom_path);
                    if !cache_dir.exists() {
                        fs::create_dir_all(&cache_dir).expect("Could not create the custom cache directory");
                        (cache_dir, Some(r6_dir.join("cache")))
                    } else {
                        (cache_dir, None)
                    }
                }
                _ => (r6_dir.join("cache"), None),
            };

            let files = Files::from_dir(&script_dir, manifest.source_filter()).expect("Could not load script sources");

            match compile_scripts(&cache_dir, bundle_dir_override.as_deref(), &files) {
                Ok(_) => {
                    log::info!("Output successfully saved in {}", cache_dir.display());
                    ExitCode::SUCCESS
                }
                Err(err) => {
                    let content = error_message(err, &files, &script_dir);
                    #[cfg(feature = "popup")]
                    msgbox::create("Compilation error", &content, msgbox::IconType::Error).unwrap();

                    log::error!("Compilation error: {}", content);
                    ExitCode::FAILURE
                }
            }
        }
        _ => {
            log::error!("Invalid command-line arguments");
            ExitCode::FAILURE
        }
    }
}

fn setup_logger(r6_dir: &Path, include_date_in_filename: bool) -> Result<(), Error> {
    let log_file_name = if include_date_in_filename {
        const DATE_FORMAT: &[FormatItem] = format_description!("[year].[month].[day]_[hour]-[minute]-[second]");
        let date = OffsetDateTime::now_utc().format(&DATE_FORMAT).unwrap();
        format!("redscript-{date}.log")
    } else {
        "redscript.log".to_owned()
    };

    let log_dir = &r6_dir.join("logs");

    if !log_dir.exists() {
        fs::create_dir(log_dir)?;
    }

    fern::Dispatch::new()
        .format(move |out, message, rec| {
            let time = OffsetDateTime::now_local().unwrap().format(&Rfc3339Format).unwrap();
            out.finish(format_args!("{} [{}] {}", time, rec.level(), message));
        })
        .level(log::LevelFilter::Info)
        .chain(io::stdout())
        .chain(fern::log_file(log_dir.join(log_file_name))?)
        .apply()
        .expect("Failed to initialize the logger");
    Ok(())
}

fn compile_scripts(cache_dir: &Path, bundle_dir_override: Option<&Path>, files: &Files) -> Result<(), Error> {
    let input_dir = bundle_dir_override.unwrap_or(cache_dir);
    let bundle_input_path = input_dir.join("final.redscripts");
    let bundle_output_path = cache_dir.join("final.redscripts");
    let backup_path = input_dir.join("final.redscripts.bk");
    let timestamp_path = input_dir.join("redscript.ts");
    let mut ts_lock = RwLock::new(
        OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&timestamp_path)?,
    );
    let mut ts_file = ts_lock.write()?;
    let write_timestamp = CompileTimestamp::of_cache_file(&File::open(&bundle_input_path)?)?;
    let saved_timestamp = CompileTimestamp::read(ts_file.deref_mut()).ok();

    match saved_timestamp {
        None if backup_path.exists() => {
            log::info!("Previous redscripts.bk file found")
        }
        saved_timestamp if saved_timestamp != Some(write_timestamp) => {
            log::info!(
                "Redscript cache file is not ours, copying it to {}",
                backup_path.display()
            );
            fs::copy(&bundle_input_path, &backup_path)?;
        }
        Some(_) if !backup_path.exists() => {
            log::warn!("A compiler timestamp was found but not the backup file, your installation might be corrupted, try removing redscript.ts and verifying game files");
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

    CompilationUnit::new(&mut bundle.pool, vec![])?.compile_and_report(files)?;

    let mut file = File::create(&bundle_output_path)?;
    bundle.save(&mut io::BufWriter::new(&mut file))?;
    file.sync_all()?;

    CompileTimestamp::of_cache_file(&file)?.write(ts_file.deref_mut())?;

    Ok(())
}

#[derive(Debug, PartialEq, Eq)]
struct CompileTimestamp {
    nanos: u128,
}

impl CompileTimestamp {
    fn read<R: io::Read + io::Seek>(input: &mut R) -> Result<Self, Error> {
        input.seek(io::SeekFrom::Start(0))?;
        let nanos = input.read_u128::<LittleEndian>()?;
        Ok(CompileTimestamp { nanos })
    }

    fn write<W: io::Write + io::Seek>(&self, output: &mut W) -> Result<(), Error> {
        output.seek(io::SeekFrom::Start(0))?;
        output.write_u128::<LittleEndian>(self.nanos)?;
        Ok(())
    }

    fn of_cache_file(file: &File) -> Result<Self, Error> {
        let nanos = file
            .metadata()?
            .modified()?
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        Ok(CompileTimestamp { nanos })
    }
}

#[derive(Debug, Deserialize, Default)]
struct ScriptManifest {
    exclusions: HashSet<String>,
    append_date_to_logfiles: Option<bool>,
}

impl ScriptManifest {
    pub fn load(script_dir: &Path) -> Result<Self, Error> {
        let path = script_dir.join("redscript.toml");
        let contents = std::fs::read_to_string(&path)?;
        let manifest =
            toml::from_str(&contents).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;
        Ok(manifest)
    }

    pub fn source_filter(self) -> SourceFilter {
        SourceFilter::Exclude(self.exclusions)
    }
}

#[allow(dead_code)]
fn error_message(error: Error, files: &Files, scripts_dir: &Path) -> String {
    fn detailed_message(spans: Vec<Span>, files: &Files, scripts_dir: &Path) -> Option<String> {
        let mut causes = HashSet::new();

        for pos in spans {
            let file = files.lookup_file(pos.low)?;
            let cause = file
                .path()
                .strip_prefix(scripts_dir)
                .ok()
                .and_then(|p| p.iter().next())
                .unwrap_or_else(|| file.path().as_os_str())
                .to_string_lossy();

            causes.insert(cause);
        }

        let causes = causes
            .iter()
            .map(|file| format!("- {file}\n"))
            .fold(String::new(), |acc, el| acc + &el);

        let msg = format!(
            "This is caused by errors in:\n{causes}You can try updating or removing these scripts to resolve the issue. If you need more information, consult the logs."
        );
        Some(msg)
    }

    let str = match error {
        Error::SyntaxError(_, pos) => detailed_message(vec![pos], files, scripts_dir).unwrap_or_default(),
        Error::CompileError(_, pos) => detailed_message(vec![pos], files, scripts_dir).unwrap_or_default(),
        Error::MultipleErrors(spans) => detailed_message(spans, files, scripts_dir).unwrap_or_default(),
        Error::IoError(err) => format!("This is caused by an I/O error: {err}"),
        Error::PoolError(err) => format!("This is caused by a constant pool error: {err}"),
        _ => String::new(),
    };

    format!("REDScript compilation failed. The game will start, but none of the scripts will take effect. {str}")
}
