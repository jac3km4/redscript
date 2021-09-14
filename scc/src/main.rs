use std::collections::HashSet;
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufReader, BufWriter, SeekFrom};
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use fd_lock::RwLock;
use log::LevelFilter;
use redscript::ast::Pos;
use redscript::bundle::ScriptBundle;
use redscript::error::Error;
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::unit::CompilationUnit;
use serde::Deserialize;
use simplelog::{CombinedLogger, Config as LoggerConfig, SimpleLogger, WriteLogger};

fn main() -> Result<(), Error> {
    // the way cyberpunk passes CLI args is broken, this is a workaround
    let args: Vec<String> = std::env::args().skip(1).collect();
    match &args[..] {
        [cmd, path_str, ..] if cmd == "-compile" => {
            let script_dir = PathBuf::from(path_str.split('"').next().unwrap());
            let cache_dir = script_dir.parent().unwrap().join("cache");
            start_logger(&cache_dir)?;
            let manifest = ScriptManifest::load_with_fallback(&script_dir);
            let files = Files::from_dir(&script_dir, manifest.source_filter())?;

            match load_scripts(&cache_dir, &files) {
                Ok(_) => {
                    log::info!("Output successfully saved in {}", cache_dir.display());
                }
                Err(err) => {
                    let content = error_message(err, &files, &script_dir);
                    msgbox::create("Compilation error", &content, msgbox::IconType::Error).unwrap();
                }
            }
        }
        _ => {
            log::error!("Invalid arguments");
        }
    }
    Ok(())
}

fn start_logger(cache_dir: &Path) -> Result<(), Error> {
    let log_path = cache_dir.join("redscript.log");
    CombinedLogger::init(vec![
        SimpleLogger::new(LevelFilter::Info, LoggerConfig::default()),
        WriteLogger::new(LevelFilter::Info, LoggerConfig::default(), File::create(log_path)?),
    ])
    .expect("Failed to initialize the logger");
    Ok(())
}

fn load_scripts(cache_dir: &Path, files: &Files) -> Result<(), Error> {
    let bundle_path = cache_dir.join("final.redscripts");
    let backup_path = cache_dir.join("final.redscripts.bk");
    let timestamp_path = cache_dir.join("redscript.ts");

    let mut ts_lock = RwLock::new(
        OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&timestamp_path)?,
    );
    let mut ts_file = ts_lock.write()?;
    let write_timestamp = CompileTimestamp::of_cache_file(&File::open(&bundle_path)?)?;
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
            fs::copy(&bundle_path, &backup_path)?;
        }
        _ => {}
    }

    let mut bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(&backup_path)?))?;

    CompilationUnit::new(&mut bundle.pool)?.compile(files)?;

    let mut file = File::create(&bundle_path)?;
    bundle.save(&mut BufWriter::new(&mut file))?;
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
        input.seek(SeekFrom::Start(0))?;
        let nanos = input.read_u128::<LittleEndian>()?;
        Ok(CompileTimestamp { nanos })
    }

    fn write<W: io::Write + io::Seek>(&self, output: &mut W) -> Result<(), Error> {
        output.seek(SeekFrom::Start(0))?;
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
}

impl ScriptManifest {
    pub fn load(script_dir: &Path) -> Result<Self, Error> {
        let path = script_dir.join("redscript.toml");
        let contents = std::fs::read_to_string(&path)?;
        log::info!("Loaded script manfiest from {}", path.display());
        let manifest =
            toml::from_str(&contents).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;
        Ok(manifest)
    }

    pub fn load_with_fallback(script_dir: &Path) -> Self {
        Self::load(script_dir).unwrap_or_else(|err| {
            log::info!("Could not load the manifest: {:?}, falling back to defaults", err);
            Self::default()
        })
    }

    pub fn source_filter(self) -> SourceFilter {
        SourceFilter::Exclude(self.exclusions)
    }
}

fn error_message(error: Error, files: &Files, scripts_dir: &Path) -> String {
    fn detailed_message(msg: String, pos: Pos, files: &Files, scripts_dir: &Path) -> Option<String> {
        let loc = files.lookup(pos)?;
        let file = loc
            .file
            .path()
            .strip_prefix(scripts_dir)
            .ok()
            .and_then(|p| p.iter().next())
            .unwrap_or_else(|| loc.file.path().as_os_str());

        let msg = format!(
            "This is caused by an error in '{}', you can try updating or removing it to resolve the issue. The error message was: '{}'. You can consult the logs for more information.",
            file.to_string_lossy(),
            msg
        );
        Some(msg)
    }

    let str = match error {
        Error::SyntaxError(msg, pos) => detailed_message(msg, pos, files, scripts_dir).unwrap_or_default(),
        Error::CompileError(msg, pos) => detailed_message(msg, pos, files, scripts_dir).unwrap_or_default(),
        Error::TypeError(msg, pos) => detailed_message(msg, pos, files, scripts_dir).unwrap_or_default(),
        Error::ResolutionError(msg, pos) => detailed_message(msg, pos, files, scripts_dir).unwrap_or_default(),
        Error::IoError(err) => format!("This is caused by an encoding error: {}", err),
        Error::PoolError(err) => format!("This is caused by a constant pool error: {}", err),
        _ => String::new(),
    };

    format!(
        "REDScript compilation failed. The game will start, but none of the scripts will take effect. {}",
        str
    )
}
