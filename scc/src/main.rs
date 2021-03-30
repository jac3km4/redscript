use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, BufReader, BufWriter};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use log::LevelFilter;
use redscript::bundle::ScriptBundle;
use redscript::error::Error;
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::Compiler;
use serde_derive::Deserialize;
use simplelog::{CombinedLogger, Config as LoggerConfig, SimpleLogger, WriteLogger};

fn main() -> Result<(), Error> {
    // the way cyberpunk passes CLI args is broken, this is a workaround
    let args: Vec<String> = std::env::args().skip(1).collect();
    match &args[..] {
        [cmd, path_str, ..] if cmd == "-compile" => {
            let script_dir = PathBuf::from(path_str.split('"').next().unwrap());
            let cache_dir = script_dir.parent().unwrap().join("cache");
            start_logger(&cache_dir)?;
            load_scripts(&script_dir, &cache_dir)
        }
        _ => {
            log::error!("Invalid arguments");
            Ok(())
        }
    }
}

fn start_logger(cache_dir: &Path) -> Result<(), Error> {
    let log_path = cache_dir.join("redscript.log");
    CombinedLogger::init(vec![
        SimpleLogger::new(LevelFilter::Warn, LoggerConfig::default()),
        WriteLogger::new(LevelFilter::Info, LoggerConfig::default(), File::create(log_path)?),
    ])
    .expect("Failed to initialize the logger");
    Ok(())
}

fn load_scripts(script_dir: &Path, cache_dir: &Path) -> Result<(), Error> {
    let bundle_path = cache_dir.join("final.redscripts");
    let backup_path = cache_dir.join("final.redscripts.bk");
    let timestamp_path = cache_dir.join("redscript.ts");

    let manifest = ScriptManifest::load_with_fallback(script_dir)?;
    let write_timestamp = CompileTimestamp::of_cache_file(&File::open(&bundle_path)?)?;
    let saved_timestamp = CompileTimestamp::read(&timestamp_path).ok();

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
    let mut compiler = Compiler::new(&mut bundle.pool)?;

    let files = Files::from_dir(script_dir, manifest.source_filter())?;
    compiler.compile(&files)?;
    let mut file = File::create(&bundle_path)?;
    bundle.save(&mut BufWriter::new(&mut file))?;
    file.sync_all()?;

    CompileTimestamp::of_cache_file(&file)?.write(&timestamp_path)?;

    log::info!("Output successfully saved to {}", bundle_path.display());
    Ok(())
}

#[derive(Debug, PartialEq, Eq)]
struct CompileTimestamp {
    nanos: u128,
}

impl CompileTimestamp {
    fn read(path: &Path) -> Result<Self, Error> {
        let nanos = File::open(path)?.read_u128::<LittleEndian>()?;
        Ok(CompileTimestamp { nanos })
    }

    fn write(&self, path: &Path) -> Result<(), Error> {
        File::create(path)?.write_u128::<LittleEndian>(self.nanos)?;
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

    pub fn load_with_fallback(script_dir: &Path) -> Result<Self, Error> {
        Ok(Self::load(script_dir).unwrap_or_default())
    }

    pub fn source_filter(self) -> SourceFilter {
        SourceFilter::Exclude(self.exclusions)
    }
}
