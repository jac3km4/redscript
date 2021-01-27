use std::ffi::OsStr;
use std::io::{BufReader, BufWriter};
use std::{fs::File, path::PathBuf};

use redscript::bundle::ScriptBundle;
use redscript::error::Error;
use redscript_compiler::parser;
use redscript_compiler::Compiler;
use walkdir::WalkDir;

fn main() -> Result<(), Error> {
    // the way cyberpunk passes CLI args is broken, this is a workaround
    let args: Vec<String> = std::env::args().skip(1).collect();
    match &args[..] {
        [cmd, path_str, ..] if cmd == "-compile" => {
            let script_path = PathBuf::from(path_str.split("\"").next().unwrap());

            let mut defs = Vec::new();
            for entry in WalkDir::new(&script_path)
                .into_iter()
                .filter_map(Result::ok)
                .filter(|e| e.path().extension() == Some(OsStr::new("reds")))
            {
                let sources = std::fs::read_to_string(entry.path())?;
                let mut entries =
                    parser::parse(&sources).map_err(|err| Error::CompileError(format!("Syntax error: {}", err)))?;
                defs.append(&mut entries);
            }

            let cache_path = script_path.parent().unwrap().join("cache");
            let bundle_path = cache_path.join("final.redscripts");
            let backup_path = cache_path.join("final.redscripts.bk");
            if !backup_path.exists() {
                std::fs::copy(&bundle_path, backup_path)?;
            }

            let mut bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(&bundle_path)?))?;
            let mut compiler = Compiler::new(&mut bundle.pool)?;

            compiler.compile(defs)?;
            bundle.save(&mut BufWriter::new(File::create(&bundle_path)?))?;

            println!("Output successfully saved in {:?}", cache_path);
            Ok(())
        }
        _ => Err(Error::CompileError(format!("Invalid arguments: {:?}", args))),
    }
}
