use std::fs::File;
use std::io;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use flexi_logger::{LevelFilter, LogSpecBuilder, Logger};
use gumdrop::Options;
use redscript::bundle::ScriptBundle;
use redscript::definition::AnyDefinition;
use redscript_compiler::compiler::{CompilationResources, Compiler};
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::StringInterner;
use redscript_decompiler::display::{display_definition, OutputMode};
use redscript_decompiler::files::FileIndex;
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
    src: Vec<PathBuf>,
    #[options(required, short = "b", help = "redscript bundle file to use")]
    bundle: PathBuf,
    #[options(required, short = "o", help = "redscript bundle file to write")]
    output: PathBuf,
}

#[derive(Debug, Options)]
struct LintOpts {
    #[options(required, short = "s", help = "source file or directory")]
    src: Vec<PathBuf>,
    #[options(short = "b", help = "redscript bundle file to use, optional")]
    bundle: Option<PathBuf>,
}

fn main() -> ExitCode {
    setup_logger();

    match run() {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::FAILURE,
        Err(err) => {
            log::error!("{}", err);
            ExitCode::FAILURE
        }
    }
}

fn setup_logger() {
    Logger::with(LogSpecBuilder::new().default(LevelFilter::Info).build())
        .log_to_stdout()
        .start()
        .expect("Failed to initialize the logger");
}

fn run() -> Result<bool, Box<dyn std::error::Error>> {
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
            return Ok(false);
        }
    };

    match command {
        Command::Decompile(opts) => Ok(decompile(opts)?),
        Command::Compile(opts) => Ok(compile(opts)?),
        Command::Lint(opts) => Ok(lint(opts)?),
    }
}

fn compile(opts: CompileOpts) -> io::Result<bool> {
    let mut bundle = load_bundle(&opts.bundle)?;
    let mut files = Files::from_dirs(&opts.src, &SourceFilter::None)?;
    files.include_std();
    let interner: StringInterner = StringInterner::default();
    let mut res = CompilationResources::load(&bundle.pool, &interner);
    let output = Compiler::new(res.type_repo, &interner).run(&files);

    match output {
        Ok(output) if !output.reporter().is_compilation_failed() => {
            output.commit(&mut res.db, &mut res.type_cache, &mut bundle.pool);
            bundle.save(&mut io::BufWriter::new(File::create(&opts.output)?))?;
            log::info!("Output successfully saved to {}", opts.output.display());
            Ok(true)
        }
        Ok(failed) => {
            for error in failed.into_errors() {
                log::error!("{}", error.display(&files));
            }
            Ok(false)
        }
        Err(error) => {
            log::error!("{}", error);
            Ok(false)
        }
    }
}

fn decompile(opts: DecompileOpts) -> Result<bool, redscript_decompiler::error::Error> {
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

            std::fs::create_dir_all(path.parent().unwrap())?;
            let mut output = io::BufWriter::new(File::create(path)?);
            for def in entry.definitions {
                if let Err(err) = write!(output, "{}", display_definition(def, pool, mode)) {
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
            if let Err(err) = write!(output, "{}", display_definition(def, pool, mode)) {
                log::error!("Failed to process definition at {:?}: {}", def, err);
            }
        }
    }
    log::info!("Output successfully saved to {}", opts.output.display());
    Ok(true)
}

fn lint(opts: LintOpts) -> io::Result<bool> {
    match opts.bundle {
        Some(bundle_path) => {
            let bundle = load_bundle(&bundle_path)?;
            let mut files = Files::from_dirs(&opts.src, &SourceFilter::None)?;
            files.include_std();
            let interner: StringInterner = StringInterner::default();
            let res = CompilationResources::load(&bundle.pool, &interner);
            let output = Compiler::new(res.type_repo, &interner).run(&files);

            match output {
                Ok(output) if !output.reporter().is_compilation_failed() => {
                    log::info!("Lint successful");
                    Ok(true)
                }
                Ok(failed) => {
                    for error in failed.into_errors() {
                        log::error!("{}", error.display(&files));
                    }
                    Ok(false)
                }
                Err(error) => {
                    log::error!("{}", error);
                    Ok(false)
                }
            }
        }
        None => Ok(true),
    }
}

fn load_bundle(path: &Path) -> Result<ScriptBundle, io::Error> {
    let (map, _) = Map::with_options()
        .open(path)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, err))?;
    let mut reader = io::Cursor::new(map.as_ref());
    ScriptBundle::load(&mut reader)
}
