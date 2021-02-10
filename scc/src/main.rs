use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use redscript::bundle::ScriptBundle;
use redscript::error::Error;
use redscript_compiler::Compiler;

fn main() -> Result<(), Error> {
    // the way cyberpunk passes CLI args is broken, this is a workaround
    let args: Vec<String> = std::env::args().skip(1).collect();
    match &args[..] {
        [cmd, path_str, ..] if cmd == "-compile" => {
            let script_dir_path = PathBuf::from(path_str.split('"').next().unwrap());

            let cache_dir_path = script_dir_path.parent().unwrap().join("cache");
            let bundle_path = cache_dir_path.join("final.redscripts");
            let backup_path = cache_dir_path.join("final.redscripts.bk");
            let timestamp_path = cache_dir_path.join("redscript.ts");

            let write_timestamp = CompileTimestamp::of_cache_file(&File::open(&bundle_path)?)?;
            let saved_timestamp = CompileTimestamp::read(&timestamp_path).ok();

            match saved_timestamp {
                None if backup_path.exists() => {
                    println!("Previous redscripts.bk file found")
                }
                saved_timestamp if saved_timestamp != Some(write_timestamp) => {
                    println!(
                        "Redscript cache file is not ours, copying it to {}",
                        backup_path.display()
                    );
                    fs::copy(&bundle_path, &backup_path)?;
                }
                _ => {}
            }

            let mut bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(&backup_path)?))?;
            let mut compiler = Compiler::new(&mut bundle.pool)?;

            compiler.compile_all(&script_dir_path)?;
            let mut file = File::create(&bundle_path)?;
            bundle.save(&mut BufWriter::new(&mut file))?;
            file.sync_all()?;

            CompileTimestamp::of_cache_file(&file)?.write(&timestamp_path)?;

            println!("Output successfully saved in {:?}", cache_dir_path);
            Ok(())
        }
        _ => {
            println!("Invalid arguments: {:?}", args);
            Ok(())
        }
    }
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
