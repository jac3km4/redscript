use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;

use gumdrop::Options;
use log::LevelFilter;
use redscript::bundle::ScriptBundle;
use redscript::definition::DefinitionValue;
use redscript::error::Error;
use redscript_compiler::Compiler;
use redscript_decompiler::files::FileIndex;
use redscript_decompiler::print::{write_definition, OutputMode};
use simplelog::{TermLogger, TerminalMode};

#[derive(Debug, Options)]
enum Command {
    #[options(help = "[opts]")]
    Decompile(DecompileOpts),
    #[options(help = "[opts]")]
    Compile(CompileOpts),
    #[options(help = "[opts]")]
    Lint(LintOpts),
}

#[derive(Debug, Options)]
struct DecompileOpts {
    #[options(required, short = "i", help = "input redscripts bundle file")]
    input: PathBuf,
    #[options(required, short = "o", help = "output file or directory")]
    output: PathBuf,
    #[options(short = "m", help = "dump mode (one of: 'ast', 'bytecode' or 'code')")]
    mode: String,
    #[options(short = "f", help = "split output into individual files")]
    dump_files: bool,
    #[options(short = "v", help = "verbose output (include implicit conversions)")]
    verbose: bool,
}

#[derive(Debug, Options)]
struct CompileOpts {
    #[options(required, short = "s", help = "source file or directory")]
    src: PathBuf,
    #[options(required, short = "b", help = "redscript bundle file to use")]
    bundle: PathBuf,
    #[options(required, short = "o", help = "redscript bundle file to write")]
    output: PathBuf,
}

#[derive(Debug, Options)]
struct LintOpts {
    #[options(required, short = "s", help = "source file or directory")]
    src: PathBuf,
    #[options(short = "b", help = "redscript bundle file to use, optional")]
    bundle: Option<PathBuf>,
}

fn main() {
    let log_config = simplelog::ConfigBuilder::new().set_time_format_str("").build();
    TermLogger::init(LevelFilter::Info, log_config, TerminalMode::Mixed).unwrap();

    run().unwrap_or_else(|err| log::error!("{:?}", err));
}

fn run() -> Result<(), Error> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let command: Command = match Command::parse_args_default(&args) {
        Ok(res) => res,
        Err(err) => {
            log::info!(
                "{} \n\
                 Usage: \n\
                 {} \n\
                 Compiler options: \n\
                 {} \n\
                 Decompiler options: \n\
                 {} \n\
                 Lint options \n\
                 {}",
                err,
                Command::usage(),
                CompileOpts::usage(),
                DecompileOpts::usage(),
                LintOpts::usage()
            );
            return Ok(());
        }
    };

    match command {
        Command::Decompile(opts) => decompile(opts),
        Command::Compile(opts) => compile(opts),
        Command::Lint(opts) => lint(opts),
    }
}

fn compile(opts: CompileOpts) -> Result<(), Error> {
    let mut bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(opts.bundle)?))?;
    let mut compiler = Compiler::new(&mut bundle.pool)?;

    match compiler.compile_all(&opts.src) {
        Ok(()) => {
            bundle.save(&mut BufWriter::new(File::create(&opts.output)?))?;
            log::info!("Output successfully saved to {}", opts.output.display());
        }
        Err(_) => {
            log::error!("Build failed");
        }
    }
    Ok(())
}

fn decompile(opts: DecompileOpts) -> Result<(), Error> {
    let bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(opts.input)?))?;
    let pool = &bundle.pool;

    let mode = match opts.mode.as_str() {
        "ast" => OutputMode::SyntaxTree,
        "bytecode" => OutputMode::Bytecode,
        _ => OutputMode::Code { verbose: opts.verbose },
    };

    if opts.dump_files {
        for entry in FileIndex::from_pool(pool).iter() {
            let path = opts.output.as_path().join(&entry.path);

            std::fs::create_dir_all(path.parent().unwrap())?;
            let mut output = BufWriter::new(File::create(path)?);
            for def in entry.definitions {
                if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                    log::error!("Failed to process definition at {:?}: {:?}", def, err);
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
            if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                log::error!("Failed to process definition at {:?}: {:?}", def, err);
            }
        }
    }
    log::info!("Output successfully saved to {}", opts.output.display());
    Ok(())
}

fn lint(opts: LintOpts) -> Result<(), Error> {
    match opts.bundle {
        Some(bundle_path) => {
            let mut bundle: ScriptBundle = ScriptBundle::load(&mut BufReader::new(File::open(bundle_path)?))?;
            let mut compiler = Compiler::new(&mut bundle.pool)?;
            if compiler.compile_all(&opts.src).is_ok() {
                log::info!("Lint successful");
            }
            Ok(())
        }
        None => Ok(()),
    }
}
