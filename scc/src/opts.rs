use std::collections::VecDeque;
use std::path::{Path, PathBuf};

use bpaf::*;

pub fn fix_args(args: Vec<String>) -> Vec<String> {
    let mut fixed_args: Vec<String> = vec![];
    let mut broken = false;

    // when cyberpunk's args are processed, the \" messes up the grouping, so we need to fix the args that have quotes & spaces in them
    for arg in args {
        let contains_quote = arg.contains('"');
        if !contains_quote && !broken {
            fixed_args.push(arg);
        } else if !broken {
            let slashful = arg.replace('"', "\\\"").replace("\" ", "\"");
            let mut parts = slashful.split('"').collect::<VecDeque<&str>>();
            fixed_args.push(parts.pop_front().unwrap().to_string());
            for part in parts {
                if part.is_empty() {
                    continue;
                }
                for p in part.split(' ') {
                    fixed_args.push(p.to_string());
                }
            }
        } else {
            // fixes args that come after the first
            let broken_arg = if contains_quote { arg.replace('"', "\\") } else { arg };
            if broken_arg.contains(' ') {
                let mut parts = broken_arg.split(' ').collect::<VecDeque<&str>>();
                let last = fixed_args.last_mut().unwrap();
                *last = format!("{} {}", last, &parts.pop_front().unwrap());
                for part in parts {
                    fixed_args.push(part.to_string());
                }
            } else {
                let last = fixed_args.last_mut().unwrap();
                *last = format!("{} {}", last, broken_arg);
            }
        }
        if contains_quote {
            broken = !broken;
        }
    }

    fixed_args.retain(|s| !s.is_empty());

    fixed_args
}

#[derive(Clone, Debug)]
pub struct Opts {
    pub scripts_dir: PathBuf,
    pub cache_dir: Option<PathBuf>,
    pub optimize: bool,
    pub threads: u8,
    pub warnings: Vec<String>,
    pub no_testonly: bool,
    pub no_breakpoint: bool,
    pub profile: bool,
    pub script_paths_file: Option<PathBuf>,
    pub cache_file: Option<PathBuf>,
    pub no_exec: bool,
    pub no_debug: bool,
}

impl Opts {
    pub const DEFAULT_NO_BREAKPOINT: bool = false;
    pub const DEFAULT_NO_DEBUG: bool = false;
    pub const DEFAULT_NO_EXEC: bool = false;
    pub const DEFAULT_NO_PROFILE: bool = true;
    pub const DEFAULT_NO_TESTONLY: bool = false;
    pub const DEFAULT_OPTIMIZE: bool = false;
    pub const DEFAULT_THREADS: u8 = 1;
}

fn is_not_slong<P: AsRef<Path>>(p: &P) -> bool {
    p.as_ref().to_str().is_some_and(|str| !str.starts_with('-'))
}

fn no_space(name: &'static str, help: &'static str) -> impl Parser<String> {
    any::<String>(name)
        .help(help)
        .guard(move |s| s.starts_with(name), "no_space doesn't start with -<name>")
        .parse(move |s| s.strip_prefix(name).map(str::to_owned).ok_or("could not extract name"))
        .anywhere()
}

fn toggle_options(name: &'static str, help: &'static str) -> impl Parser<Option<bool>> {
    any::<String>(name)
        .help(help)
        .guard(move |s| s.as_str() == name, "toggle_option isn't -<name>")
        .map(|_| true)
        .adjacent()
        .anywhere()
        .optional()
        .catch()
}

fn equals_sign(name: &'static str, help: &'static str) -> impl Parser<Option<String>> {
    any::<String>(name)
        .help(help)
        .guard(
            move |s| s.strip_prefix(name).and_then(|s| s.strip_prefix('=')).is_some(),
            "equals_sign doesn't start with -<name>=",
        )
        .parse(move |s| {
            let (_, val) = s.split_once('=').ok_or("equals_sign does not contain =")?;
            Ok::<_, &str>(val.to_owned())
        })
        .anywhere()
        .optional()
        .catch()
}

fn slong(name: &'static str, arg_name: &'static str, help: &'static str) -> impl Parser<Option<String>> {
    let tag = any::<String>(name)
        .help(help)
        .guard(move |s| s.as_str() == name, "slong isn't -<name>");
    let value = positional::<String>(arg_name).guard(|s| !s.starts_with('-'), "starts with -");
    construct!(tag, value)
        .adjacent()
        .anywhere()
        .map(|pair| pair.1)
        .optional()
        .catch()
}

fn scripts_dir() -> impl Parser<PathBuf> {
    let tag = any::<String>("-compile")
        .help("Compile scripts to blob")
        .guard(|s| s == "-compile", "not compile");
    let value = positional::<PathBuf>("SCRIPT_PATH").guard(is_not_slong, "starts with -");
    construct!(tag, value).adjacent().map(|pair| pair.1)
}

fn script_paths_file() -> impl Parser<Option<PathBuf>> {
    let tag = any::<String>("-compilePathsFile")
        .help("File containing a newline-delimited list of redscript paths to compile")
        .guard(|s| s == "-compilePathsFile", "not compilePathsFile");
    let value = positional::<PathBuf>("SCRIPT_PATHS_FILE").guard(is_not_slong, "starts with -");
    construct!(tag, value)
        .adjacent()
        .anywhere()
        .map(|pair| pair.1)
        .optional()
        .catch()
}

fn cache_dir() -> impl Parser<Option<PathBuf>> {
    let tag = any::<String>("-customCacheDir")
        .help("A custom cache dir to write the final.redscripts to")
        .guard(|s| s == "-customCacheDir", "not customCacheDir");
    let value = positional::<PathBuf>("CACHE_DIR").guard(is_not_slong, "starts with -");
    construct!(tag, value)
        .adjacent()
        .anywhere()
        .map(|pair| pair.1)
        .optional()
        .catch()
}

impl Opts {
    pub fn get_parser() -> OptionParser<Opts> {
        let optimize = toggle_options("-optimize", "Enable optimiziations. Off by default")
            .map(|s| s.unwrap_or(Opts::DEFAULT_OPTIMIZE));
        let no_exec = toggle_options("-no-exec", "Unknown").map(|s| s.unwrap_or(Opts::DEFAULT_NO_EXEC));
        let no_debug = toggle_options("-no-debug", "Unknown").map(|s| s.unwrap_or(Opts::DEFAULT_NO_DEBUG));
        let threads = slong("-threads", "THREADS", "Set number of internal compilation threads")
            .parse(|s| s.map(|s| s.parse::<u8>()).unwrap_or(Ok(Opts::DEFAULT_THREADS)));
        let no_testonly = toggle_options("-no-testonly", "Skips testonly code. Off by default")
            .map(|s| s.unwrap_or(Opts::DEFAULT_NO_TESTONLY));
        let no_breakpoint = toggle_options(
            "-no-breakpoint",
            "Skips generation of breakpoint opcoes. Off by default",
        )
        .map(|s| s.unwrap_or(Opts::DEFAULT_NO_BREAKPOINT));
        let warnings = no_space("-W", "Warnings enabled").many();
        let profile = equals_sign("-profile", "Introduces profiling opcodes. On by default").parse(|s| {
            if let Some(str) = s {
                match str.as_str() {
                    "on" => Ok(true),
                    "off" => Ok(false),
                    _ => Err("Profile option parse error"),
                }
            } else {
                Ok(Opts::DEFAULT_NO_PROFILE)
            }
        });
        let cache_file = positional::<PathBuf>("CACHE_FILE")
            .guard(is_not_slong, "starts with -")
            .anywhere()
            .optional()
            .catch();

        let parser = construct!(Opts {
            scripts_dir(),
            cache_dir(),
            optimize,
            threads,
            warnings,
            no_testonly,
            no_breakpoint,
            profile,
            script_paths_file(),
            cache_file,
            no_exec,
            no_debug
        });
        parser.to_options()
    }

    pub fn load(args: &[&str]) -> Result<Self, ParseFailure> {
        Self::get_parser().run_inner(bpaf::Args::from(args))
    }
}
