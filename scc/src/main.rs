use std::collections::HashSet;
use std::fs::{self, File, OpenOptions};
use std::io;
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
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
use time::{OffsetDateTime};
#[cfg(feature = "mmap")]
use vmap::Map;

fn main() -> Result<(), Error> {
    // the way cyberpunk passes CLI args is broken, this is a workaround
    let args: Vec<String> = std::env::args().skip(1).collect(); 
    match &args[..] {
        [cmd, path_str, ..] if cmd == "-compile" => {
            let script_dir = PathBuf::from(path_str.split('"').next().unwrap());
            let cache_dir = script_dir.parent().unwrap().join("cache");

            // load manifest without fallback
            let manifest = ScriptManifest::load(&script_dir);

            // set up logger with an optional manifest
            setup_logger(&cache_dir, manifest.as_ref().ok().unwrap().append_date_to_logfiles.unwrap())?;

            // get manifest or fallback
            let manifest = manifest.unwrap_or_else(|err| {
                log::info!("Could not load the manifest, falling back to defaults (caused by {})", err);
                ScriptManifest::default()
            });

            let files = Files::from_dir(&script_dir, manifest.source_filter())?;

            match load_scripts(&cache_dir, &files) {
                Ok(_) => {
                    log::info!("Output successfully saved in {}", cache_dir.display());
                }
                #[cfg(feature = "popup")]
                Err(err) => {
                    let content = error_message(err, &files, &script_dir);
                    msgbox::create("Compilation error", &content, msgbox::IconType::Error).unwrap();
                }
                #[cfg(not(feature = "popup"))]
                Err(_) => {}
            }
        }
        _ => {
            log::error!("Invalid arguments");
        }
    }
    Ok(())
}

pub fn exists(file : &Path) -> bool {
    fs::metadata(file).is_ok()
}

fn setup_logger(cache_dir: &Path, date_time_logger : bool) -> Result<(), Error> 
{
    let parent_dir  = cache_dir.parent().unwrap();
    let mut log_name : String = "redscript".to_owned();
    
    if date_time_logger == true
    {
        log_name.push_str("_");
        let time_create = OffsetDateTime::now_local().unwrap().format(&Rfc3339Format).unwrap();
        log_name.push_str(&time_create);

        log_name = log_name.replace("-", ".").replace(":", "-")[..29].to_string();
        log_name.replace_range(20..21, "_");
    }
    
    log_name.push_str(".log");

    // Log directory make
    let log_dir = parent_dir.join("logs");

    if !exists(&log_dir)
    {
        fs::create_dir(log_dir)?;
    }

    fern::Dispatch::new()
        .format(move |out, message, rec| 
        {
            let time = OffsetDateTime::now_local().unwrap().format(&Rfc3339Format).unwrap();
                out.finish(format_args!("{} [{}] {}", time, rec.level(), message));
        })
        .level(log::LevelFilter::Info)
        .chain(io::stdout())
        .chain(fern::log_file(parent_dir.join("logs").join(log_name))?)
        .apply()
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

    #[cfg(feature = "mmap")]
    let mut bundle = {
        let (map, _) = Map::with_options()
            .open(backup_path)
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
        ScriptBundle::load(&mut io::Cursor::new(map.as_ref()))?
    };
    #[cfg(not(feature = "mmap"))]
    let mut bundle = ScriptBundle::load(&mut io::BufReader::new(File::open(backup_path)?))?;

    CompilationUnit::new(&mut bundle.pool, vec![])?.compile_and_report(files)?;

    let mut file = File::create(&bundle_path)?;
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
    append_date_to_logfiles: Option<bool>
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
