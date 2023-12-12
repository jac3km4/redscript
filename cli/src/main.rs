use std::fs::{self, File};
use std::io;
use std::path::{Path, PathBuf};

use anyhow::Context;
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

fn main() -> anyhow::Result<()> {
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
        .expect("info logger should always start");
}

fn run() -> anyhow::Result<()> {
    let args: Args = argh::from_env();

    match args.command {
        Command::Decompile(opts) => Ok(decompile(opts)?),
        Command::Compile(opts) => Ok(compile(opts)?),
        Command::Lint(opts) => Ok(lint(opts)?),
    }
}

fn compile(opts: CompileOpts) -> anyhow::Result<()> {
    let mut bundle = load_bundle(&opts.bundle)?;

    let files = Files::from_dirs(&opts.src, &SourceFilter::None)
        .map_err(|err| anyhow::anyhow!("Failed to load the source files: {err}"))?;

    match CompilationUnit::new_with_defaults(&mut bundle.pool)
        .map_err(|err| anyhow::anyhow!("Failed to create the compilation unit: {err}"))?
        .compile_and_report(&files)
    {
        Ok(_) => {
            let file = File::create(&opts.output).context("Failed to create a file at the specified output path")?;
            bundle
                .save(&mut io::BufWriter::new(file))
                .context("Failed to write the script cache")?;

            log::info!("Output successfully saved to {}", opts.output.display());
        }
        Err(_) => {
            log::error!("Build failed");
        }
    }
    Ok(())
}

fn decompile(opts: DecompileOpts) -> anyhow::Result<()> {
    let bundle = load_bundle(&opts.input)?;
    let pool = &bundle.pool;

    let mode = match opts.mode.as_str() {
        "ast" => OutputMode::SyntaxTree,
        "bytecode" => OutputMode::Bytecode,
        "code" => OutputMode::Code { verbose: opts.verbose },
        _ => anyhow::bail!("Invalid output mode: {}", opts.mode),
    };

    if opts.dump_files {
        for entry in FileIndex::from_pool(pool).iter() {
            let path = opts.output.as_path().join(entry.path);

            fs::create_dir_all(path.parent().expect("entry path should have at least one component"))?;

            let file = File::create(path).context("Failed to create a file at the specified output path")?;
            let mut output = io::BufWriter::new(file);
            for def in entry.definitions {
                if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                    log::error!("Failed to process a definition: {err}");
                }
            }
        }
    } else {
        let file = File::create(&opts.output).context("Failed to create a file at the specified output path")?;
        let mut output = io::BufWriter::new(file);

        for (_, def) in pool.roots().filter(|(_, def)| {
            matches!(&def.value, AnyDefinition::Class(_))
                || matches!(&def.value, AnyDefinition::Enum(_))
                || matches!(&def.value, AnyDefinition::Function(_))
        }) {
            if let Err(err) = write_definition(&mut output, def, pool, 0, mode) {
                log::error!("Failed to process a definition: {err}");
            }
        }
    }
    log::info!("Output successfully saved to {}", opts.output.display());
    Ok(())
}

fn lint(opts: LintOpts) -> anyhow::Result<()> {
    match opts.bundle {
        Some(bundle_path) => {
            let mut bundle = load_bundle(&bundle_path)?;
            let files = Files::from_dirs(&opts.src, &SourceFilter::None)
                .map_err(|err| anyhow::anyhow!("Failed to load the source files: {err}"))?;

            if CompilationUnit::new_with_defaults(&mut bundle.pool)
                .map_err(|err| anyhow::anyhow!("Failed to create the compilation unit: {err}"))?
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

fn load_bundle(path: &Path) -> anyhow::Result<ScriptBundle> {
    let (map, _) = Map::with_options()
        .open(path)
        .context("Failed to open the script cache")?;
    let mut reader = io::Cursor::new(map.as_ref());
    ScriptBundle::load(&mut reader).context("Failed to load the script cache")
}
