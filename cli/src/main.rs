use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use fern::colors::ColoredLevelConfig;
use gumdrop::Options;
use redscript::bundle::ScriptBundle;
use redscript::definition::AnyDefinition;
use redscript::error::Error;
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::unit::CompilationUnit;
use redscript_decompiler::files::FileIndex;
use redscript_decompiler::print::{write_definition, OutputMode};
use vmap::Map;

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

fn main() -> Result<(), Error> {
    setup_logger();

    run().map_err(|err| {
        log::error!("{}", err);
        err
    })
}

fn setup_logger() {
    let colors = ColoredLevelConfig::new();
    fern::Dispatch::new()
        .format(move |out, message, rec| {
            out.finish(format_args!("[{}] {}", colors.color(rec.level()), message));
        })
        .level(log::LevelFilter::Info)
        .chain(io::stdout())
        .apply()
        .expect("Failed to initialize the logger");
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
    let mut bundle = load_bundle(&opts.bundle)?;

    let files = Files::from_dir(&opts.src, SourceFilter::None)?;

    match CompilationUnit::new(&mut bundle.pool)?.compile_and_print(&files) {
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

fn decompile(opts: DecompileOpts) -> Result<(), Error> {
    let bundle = load_bundle(&opts.input)?;
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

fn lint(opts: LintOpts) -> Result<(), Error> {
    match opts.bundle {
        Some(bundle_path) => {
            let mut bundle = load_bundle(&bundle_path)?;

            let files = Files::from_dir(&opts.src, SourceFilter::None)?;

            if CompilationUnit::new(&mut bundle.pool)?
                .compile_and_print(&files)
                .is_ok()
            {
                log::info!("Lint successful");
            }
            Ok(())
        }
        None => Ok(()),
    }
}

fn load_bundle(path: &Path) -> Result<ScriptBundle, Error> {
    let (map, _) = Map::with_options()
        .open(path)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let mut reader = io::Cursor::new(map.as_ref());
    ScriptBundle::load(&mut reader)
}
