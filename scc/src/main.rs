use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;

use redscript::bundle::ScriptBundle;
use redscript::error::Error;
use redscript_compiler::Compiler;

fn main() -> Result<(), Error> {
    // the way cyberpunk passes CLI args is broken, this is a workaround
    let args: Vec<String> = std::env::args().skip(1).collect();
    match &args[..] {
        [cmd, path_str, ..] if cmd == "-compile" => {
            let script_path = PathBuf::from(path_str.split('"').next().unwrap());

            let cache_path = script_path.parent().unwrap().join("cache");
            let bundle_path = cache_path.join("final.redscripts");
            let backup_path = cache_path.join("final.redscripts.bk");
            if !backup_path.exists() {
                std::fs::rename(&bundle_path, &backup_path)?;
            }

            let mut bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(&backup_path)?))?;
            let mut compiler = Compiler::new(&mut bundle.pool)?;

            compiler.compile_all(&script_path)?;
            bundle.save(&mut BufWriter::new(File::create(&bundle_path)?))?;

            println!("Output successfully saved in {:?}", cache_path);
            Ok(())
        }
        _ => {
            println!("Invalid arguments: {:?}", args);
            Ok(())
        }
    }
}
