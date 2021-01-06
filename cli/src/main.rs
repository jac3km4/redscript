use redscript::bundle::ScriptBundle;
use redscript::definition::DefinitionValue;
use redscript::error::Error;
use redscript::print::{write_definition, OutputMode};

use gumdrop::Options;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;

#[derive(Debug, Options)]
struct Configuration {
    #[options(required, short = "i", help = "input file")]
    input: PathBuf,
    #[options(required, short = "o", help = "output file or directory")]
    output: PathBuf,
    #[options(short = "m", help = "dump mode (one of: 'ast', 'bytecode' or 'code')")]
    mode: String,
    #[options(short = "f", help = "split into individual files (doesn't work for everything yet)")]
    dump_files: bool,
}

fn main() -> Result<(), Error> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let config: Configuration = match Configuration::parse_args_default(&args) {
        Ok(res) => res,
        Err(err) => {
            println!("{}", err);
            println!("{}", Configuration::usage());
            return Ok(());
        }
    };

    let cache: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(config.input)?))?;

    let mode = match config.mode.as_str() {
        "ast" => OutputMode::SyntaxTree,
        "bytecode" => OutputMode::Bytecode,
        _ => OutputMode::Code,
    };

    let pool = cache.pool();

    if config.dump_files {
        for entry in pool.files().iter() {
            let path = config.output.as_path().join(&entry.file.path);

            std::fs::create_dir_all(path.parent().unwrap())?;
            let mut output = BufWriter::new(File::create(path)?);
            for def in entry.definitions {
                if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                    println!("Failed to process definition at {}: {:?}", def.file_offset, err);
                }
            }
        }
    } else {
        let mut output = BufWriter::new(File::create(&config.output)?);

        for (_, def) in pool.roots().filter(|(_, def)| {
            matches!(&def.value, DefinitionValue::Class(_))
                || matches!(&def.value, DefinitionValue::Enum(_))
                || matches!(&def.value, DefinitionValue::Function(_))
        }) {
            if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                println!("Failed to process definition at {}: {:?}", def.file_offset, err);
            }
        }
    }
    println!("Output successfully saved to {:?}", config.output);
    Ok(())
}
