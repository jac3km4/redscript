use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;

use gumdrop::Options;
use redscript::bundle::ScriptBundle;
use redscript::definition::DefinitionValue;
use redscript::error::Error;
use redscript_compiler::{parser, Compiler};
use redscript_decompiler::print;

#[derive(Debug, Options)]
enum Command {
    #[options(help = "[opts]")]
    Decompile(DecompileOpts),
    #[options(help = "[opts]")]
    Compile(CompileOpts),
}

#[derive(Debug, Options)]
struct DecompileOpts {
    #[options(required, short = "i", help = "input file")]
    input: PathBuf,
    #[options(required, short = "o", help = "output file or directory")]
    output: PathBuf,
    #[options(short = "m", help = "dump mode (one of: 'ast', 'bytecode' or 'code')")]
    mode: String,
    #[options(short = "f", help = "split into individual files (doesn't work for everything yet)")]
    dump_files: bool,
}

#[derive(Debug, Options)]
struct CompileOpts {
    #[options(required, short = "i", help = "input source file")]
    input: PathBuf,
    #[options(required, short = "b", help = "redscript bundle file to use")]
    bundle: PathBuf,
    #[options(required, short = "o", help = "redscript bundle file to write")]
    output: PathBuf,
}

fn main() -> Result<(), Error> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let command: Command = match Command::parse_args_default(&args) {
        Ok(res) => res,
        Err(err) => {
            println!("{}", err);
            println!("Usage:");
            println!("{}", Command::usage());
            println!("Compiler options:");
            println!("{}", CompileOpts::usage());
            println!("Decompiler options:");
            println!("{}", DecompileOpts::usage());
            return Ok(());
        }
    };

    match command {
        Command::Decompile(opts) => decompile(opts),
        Command::Compile(opts) => compile(opts),
    }
}

fn compile(opts: CompileOpts) -> Result<(), Error> {
    let sources = std::fs::read_to_string(opts.input)?;
    let entries = parser::parse(&sources).map_err(|err| Error::CompileError(format!("Syntax error: {}", err)))?;

    let mut bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(opts.bundle)?))?;
    let mut compiler = Compiler::new(&mut bundle.pool)?;

    compiler.compile(entries)?;
    bundle.save(&mut BufWriter::new(File::create(&opts.output)?))?;

    println!("Output successfully saved to {:?}", opts.output);
    Ok(())
}

fn decompile(opts: DecompileOpts) -> Result<(), Error> {
    let bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(opts.input)?))?;
    let pool = &bundle.pool;

    let mode = match opts.mode.as_str() {
        "ast" => print::OutputMode::SyntaxTree,
        "bytecode" => print::OutputMode::Bytecode,
        _ => print::OutputMode::Code,
    };

    if opts.dump_files {
        for entry in pool.files().iter() {
            let path = opts.output.as_path().join(&entry.file.path);

            std::fs::create_dir_all(path.parent().unwrap())?;
            let mut output = BufWriter::new(File::create(path)?);
            for def in entry.definitions {
                if let Err(err) = print::write_definition(&mut output, def, pool, 0, mode) {
                    println!("Failed to process definition at {:?}: {:?}", def, err);
                }
            }
        }
    } else {
        let mut output = BufWriter::new(File::create(&opts.output)?);

        for (_, def) in pool.roots().filter(|(_, def)| {
            matches!(&def.value, DefinitionValue::Class(_))
                || matches!(&def.value, DefinitionValue::Enum(_))
                || matches!(&def.value, DefinitionValue::Function(_))
        }) {
            if let Err(err) = print::write_definition(&mut output, def, pool, 0, mode) {
                println!("Failed to process definition at {:?}: {:?}", def, err);
            }
        }
    }
    println!("Output successfully saved to {:?}", opts.output);
    Ok(())
}
