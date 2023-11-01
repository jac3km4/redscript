use std::fs::{self, File};
use std::io;
use std::path::{Path, PathBuf};

use argh::FromArgs;
use flexi_logger::{LevelFilter, LogSpecBuilder, Logger};
use redscript::bundle::ScriptBundle;
use redscript::definition::AnyDefinition;
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::unit::CompilationUnit;
use redscript_decompiler::files::FileIndex;
use redscript_decompiler::print::{write_definition, OutputMode};
use vmap::Map;

/// redscript command line interface
#[derive(Debug, FromArgs)]
struct Args {
    #[argh(subcommand)]
    command: Command,
}

#[derive(Debug, FromArgs)]
#[argh(subcommand)]
enum Command {
    Decompile(DecompileOpts),
    Compile(CompileOpts),
    Lint(LintOpts),
}

/// decompile a .redscripts file
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "decompile")]
struct DecompileOpts {
    /// path to an input .redscripts file
    #[argh(option, short = 'i')]
    input: PathBuf,
    /// path to an output file or directory
    #[argh(option, short = 'o')]
    output: PathBuf,
    /// output mode, use 'code' to print redscript code, 'ast' to print a direct representation
    /// of the AST, 'bytecode' to print individual bytecode instructions
    #[argh(option, short = 'm', default = "String::from(\"code\")")]
    mode: String,
    /// write individual files based on the stored file index instead of a single file
    #[argh(switch, short = 'f')]
    dump_files: bool,
    /// include implicit operations in the output (conversions etc.)
    #[argh(switch, short = 'v')]
    verbose: bool,
}

/// compile redscript source code into a .redscripts file
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "compile")]
struct CompileOpts {
    /// path to an input source file or directory
    #[argh(option, short = 's')]
    src: Vec<PathBuf>,
    /// path to a .redscripts file to use for incremental compilation
    #[argh(option, short = 'b')]
    bundle: PathBuf,
    /// path to an output .redscripts file
    #[argh(option, short = 'o')]
    output: PathBuf,
}

/// lint redscript source code
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "lint")]
struct LintOpts {
    /// path to an input source file or directory
    #[argh(option, short = 's')]
    src: Vec<PathBuf>,
    /// path to a .redscripts file to use for incremental compilation
    #[argh(option, short = 'b')]
    bundle: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setup_logger();

    run().map_err(|err| {
        log::error!("{}", err);
        err
    })
}

fn setup_logger() {
    Logger::with(LogSpecBuilder::new().default(LevelFilter::Info).build())
        .log_to_stdout()
        .start()
        .expect("Failed to initialize the logger");
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let args: Args = argh::from_env();

    match args.command {
        Command::Decompile(opts) => Ok(decompile(opts)?),
        Command::Compile(opts) => Ok(compile(opts)?),
        Command::Lint(opts) => Ok(lint(opts)?),
    }
}

fn compile(opts: CompileOpts) -> Result<(), redscript_compiler::error::Error> {
    let mut bundle = load_bundle(&opts.bundle)?;

    let files = Files::from_dirs(&opts.src, &SourceFilter::None)?;

    match CompilationUnit::new_with_defaults(&mut bundle.pool)?.compile_and_report(&files) {
        Ok(()) => {
            bundle.save(&mut io::BufWriter::new(File::create(&opts.output)?))?;
            log::info!("Output successfully saved to {}", opts.output.display());
        }
        Err(_) => {
            log::error!("Build failed");
        }
    }
    Ok(())
}

fn decompile(opts: DecompileOpts) -> Result<(), redscript_decompiler::error::Error> {
    let bundle = load_bundle(&opts.input)?;
    let pool = &bundle.pool;

    let mode = match opts.mode.as_str() {
        "ast" => OutputMode::SyntaxTree,
        "bytecode" => OutputMode::Bytecode,
        _ => OutputMode::Code { verbose: opts.verbose },
    };

    if opts.dump_files {
        for entry in FileIndex::from_pool(pool).iter() {
            let path = opts.output.as_path().join(entry.path);

            fs::create_dir_all(path.parent().unwrap())?;
            let mut output = io::BufWriter::new(File::create(path)?);
            for def in entry.definitions {
                if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                    log::error!("Failed to process definition at {:?}: {}", def, err);
                }
            }
        }
    } else {
        let mut output = io::BufWriter::new(File::create(&opts.output)?);

        for (_, def) in pool.roots().filter(|(_, def)| {
            matches!(&def.value, AnyDefinition::Class(_))
                || matches!(&def.value, AnyDefinition::Enum(_))
                || matches!(&def.value, AnyDefinition::Function(_))
        }) {
            if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                log::error!("Failed to process definition at {:?}: {}", def, err);
            }
        }
    }
    log::info!("Output successfully saved to {}", opts.output.display());
    Ok(())
}

fn lint(opts: LintOpts) -> Result<(), redscript_compiler::error::Error> {
    match opts.bundle {
        Some(bundle_path) => {
            let mut bundle = load_bundle(&bundle_path)?;

            let files = Files::from_dirs(&opts.src, &SourceFilter::None)?;

            if CompilationUnit::new_with_defaults(&mut bundle.pool)?
                .compile_and_report(&files)
                .is_ok()
            {
                log::info!("Lint successful");
            }
            Ok(())
        }
        None => Ok(()),
    }
}

fn load_bundle(path: &Path) -> Result<ScriptBundle, io::Error> {
    let (map, _) = Map::with_options()
        .open(path)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let mut reader = io::Cursor::new(map.as_ref());
    ScriptBundle::load(&mut reader)
}
