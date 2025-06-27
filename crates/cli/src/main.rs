use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::process::ExitCode;
use std::str::FromStr;

use anyhow::Context;
use argh::FromArgs;
use hashbrown::HashMap;
use mimalloc::MiMalloc;
use redscript_compiler_api::ast::SourceMap;
use redscript_compiler_api::{
    Compilation, FlushError, ScriptBundle, SourceMapExt, TypeInterner, pass,
};
use redscript_decompiler::{Settings, decompile_all};
use redscript_dotfile::Dotfile;
use redscript_formatter::{FormatCtx, FormatSettings, SyntaxOps, format_document};
use vmap::Map;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

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
    Format(FormatOpts),
}

/// decompile a .redscripts file
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "decompile")]
struct DecompileOpts {
    /// path to an input .redscripts file
    #[argh(option, short = 'b')]
    bundle: PathBuf,
    /// path to an output file
    #[argh(option, short = 'o')]
    output: PathBuf,
    /// include implicit operations in the output (conversions etc.)
    #[argh(switch, short = 'v')]
    verbose: bool,
    /// size of the indentation
    #[argh(option)]
    indent: Option<u16>,
    /// max line width
    #[argh(option)]
    max_width: Option<u16>,
    /// max fields in a chain
    #[argh(option)]
    max_chain_fields: Option<u8>,
    /// max calls in a chain
    #[argh(option)]
    max_chain_calls: Option<u8>,
    /// max operators in a chain
    #[argh(option)]
    max_chain_operators: Option<u8>,
    /// max total chain length
    #[argh(option)]
    max_chain_total: Option<u8>,
    /// max significant digits in floating point numbers
    #[argh(option)]
    max_sig_digits: Option<u8>,
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
    /// enable specific warnings
    #[argh(option, short = 'W')]
    warn_on: Vec<WarnOn>,
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
    bundle: PathBuf,
    /// enable specific warnings
    #[argh(option, short = 'W')]
    warn_on: Vec<WarnOn>,
}

/// format redscript source code
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "format")]
struct FormatOpts {
    /// path to an input source file or directory
    #[argh(option, short = 's')]
    src: PathBuf,
    /// write the formatted source code to the input files
    #[argh(switch)]
    save: bool,
    /// return exit code 1 if the source code is not formatted correctly
    #[argh(switch, short = 'c')]
    check: bool,
    /// size of the indentation
    #[argh(option)]
    indent: Option<u16>,
    /// max line width
    #[argh(option)]
    max_width: Option<u16>,
    /// max fields in a chain
    #[argh(option)]
    max_chain_fields: Option<u8>,
    /// max calls in a chain
    #[argh(option)]
    max_chain_calls: Option<u8>,
    /// max operators in a chain
    #[argh(option)]
    max_chain_operators: Option<u8>,
    /// max total chain length
    #[argh(option)]
    max_chain_total: Option<u8>,
    /// max significant digits in floating point numbers
    #[argh(option)]
    max_sig_digits: Option<u8>,
}

#[derive(Debug)]
enum WarnOn {
    UnusedLocals,
}

impl WarnOn {
    pub fn to_pass<'ctx>(&self) -> Box<dyn pass::DiagnosticPass<'ctx>> {
        match self {
            Self::UnusedLocals => Box::new(pass::UnusedLocals),
        }
    }
}

impl FromStr for WarnOn {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "unused_locals" => Ok(Self::UnusedLocals),
            _ => Err(format!("unknown warning: {s}")),
        }
    }
}

fn main() -> anyhow::Result<ExitCode> {
    let colors = fern::colors::ColoredLevelConfig::default();
    fern::Dispatch::new()
        .format(move |out, message, record| {
            let level = colors.color(record.level());
            out.finish(format_args!("[{level}] {message}"));
        })
        .chain(std::io::stdout())
        .apply()?;

    let args: Args = argh::from_env();

    match args.command {
        Command::Decompile(opts) => decompile(opts),
        Command::Compile(opts) => compile(opts),
        Command::Lint(opts) => lint(opts),
        Command::Format(opts) => format(opts),
    }
}

fn compile(opts: CompileOpts) -> anyhow::Result<ExitCode> {
    let (map, _f) = Map::with_options().open(opts.bundle)?;
    let interner = TypeInterner::default();
    let sources = load_sources(&opts.src)?;
    let passes = opts
        .warn_on
        .into_iter()
        .map(|w| w.to_pass())
        .collect::<Vec<_>>();

    match Compilation::new_with(&map, &sources, &interner, &passes)?.flush(opts.output) {
        Ok((_, diagnostics)) => {
            diagnostics.dump(&sources)?;
            log::info!("Compilation successful");
            Ok(ExitCode::SUCCESS)
        }
        Err(FlushError::CompilationErrors(diagnostics)) => {
            diagnostics.dump(&sources)?;
            log::info!("Compilation failed");
            Ok(ExitCode::FAILURE)
        }
        Err(err) => anyhow::bail!("{err}"),
    }
}

fn lint(opts: LintOpts) -> anyhow::Result<ExitCode> {
    let (map, _f) = Map::with_options().open(opts.bundle)?;
    let interner = TypeInterner::default();
    let sources = load_sources(&opts.src)?;
    let passes = opts
        .warn_on
        .into_iter()
        .map(|w| w.to_pass())
        .collect::<Vec<_>>();

    let comp = Compilation::new_with(&map, &sources, &interner, &passes)?;
    comp.diagnostics().dump(&sources)?;
    if comp.diagnostics().has_fatal_errors() {
        log::info!("Compilation failed");
        Ok(ExitCode::FAILURE)
    } else {
        log::info!("Compilation successful");
        Ok(ExitCode::SUCCESS)
    }
}

fn decompile(opts: DecompileOpts) -> anyhow::Result<ExitCode> {
    let mut settings = FormatSettings::default();
    if let Some(indent) = opts.indent {
        settings.indent = indent;
    }
    if let Some(max_width) = opts.max_width {
        settings.max_width = max_width;
    }
    if let Some(max_chain_calls) = opts.max_chain_calls {
        settings.max_chain_calls = max_chain_calls;
    }
    if let Some(max_chain_fields) = opts.max_chain_fields {
        settings.max_chain_fields = max_chain_fields;
    }
    if let Some(max_chain_operators) = opts.max_chain_operators {
        settings.max_chain_operators = max_chain_operators;
    }
    if let Some(max_chain_total) = opts.max_chain_total {
        settings.max_chain_total = max_chain_total;
    }
    settings.trunc_sig_digits = opts.max_sig_digits;

    let prefix_map = HashMap::default();
    let ctx = FormatCtx::new(&settings, &prefix_map);

    let (map, _f) = Map::with_options().open(opts.bundle)?;
    let bundle = ScriptBundle::from_bytes(&map)?;

    let mut output = BufWriter::new(File::create(opts.output)?);
    let settings = Settings::default();
    let settings = if opts.verbose {
        settings.verbose()
    } else {
        settings
    };
    for item in decompile_all(&bundle, &settings) {
        writeln!(output, "{}", item?.as_fmt(ctx))?;
    }

    Ok(ExitCode::SUCCESS)
}

fn format(opts: FormatOpts) -> anyhow::Result<ExitCode> {
    let dotfile = Dotfile::load_or_default(&opts.src)?;

    let mut settings = FormatSettings::default();
    if let Some(indent) = opts.indent.or(dotfile.format.indent) {
        settings.indent = indent;
    }
    if let Some(max_width) = opts.max_width.or(dotfile.format.max_width) {
        settings.max_width = max_width;
    }
    if let Some(max_chain_calls) = opts.max_chain_calls.or(dotfile.format.max_chain_calls) {
        settings.max_chain_calls = max_chain_calls;
    }
    if let Some(max_chain_fields) = opts.max_chain_fields.or(dotfile.format.max_chain_fields) {
        settings.max_chain_fields = max_chain_fields;
    }
    if let Some(max_chain_operators) = opts
        .max_chain_operators
        .or(dotfile.format.max_chain_operators)
    {
        settings.max_chain_operators = max_chain_operators;
    }
    if let Some(max_chain_total) = opts.max_chain_total.or(dotfile.format.max_chain_total) {
        settings.max_chain_total = max_chain_total;
    }
    settings.trunc_sig_digits = opts.max_sig_digits;

    let mut errors = vec![];
    let mut failed = false;

    let map = SourceMap::from_paths_recursively([opts.src])?;
    for (id, file) in map.files() {
        let (module, e) = format_document(file.source(), id, &settings);
        errors.extend(e);

        if !errors.is_empty() {
            failed = true;
            continue;
        }
        let Some(module) = module else {
            continue;
        };

        let formatted = module.to_string();

        if opts.check {
            if formatted.trim() != file.source().trim() {
                failed = true;
                eprintln!("[ERROR] At {}", file.path().display());
                eprintln!("file is not formatted correctly");
            }
            continue;
        }

        if opts.save {
            fs::write(file.path(), formatted)?;
        } else {
            println!("{module}");
        }
    }

    for err in &errors {
        eprintln!("[ERROR] {}", err.display(&map).context("unknown source")?);
    }

    if failed {
        Ok(ExitCode::FAILURE)
    } else {
        Ok(ExitCode::SUCCESS)
    }
}

fn load_sources(src: &[PathBuf]) -> anyhow::Result<SourceMap> {
    let mut roots = vec![];
    for src in src {
        let dotfile = Dotfile::load_or_default(src)?;
        roots.extend(dotfile.source_roots.into_iter().map(|p| src.join(p)));
    }
    let sources = SourceMap::from_paths_recursively(&roots)?;
    sources.populate_boot_lib();
    Ok(sources)
}
