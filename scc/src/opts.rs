use std::collections::VecDeque;
use std::path::PathBuf;

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

    fixed_args
}

#[derive(Clone, Debug)]
pub struct Opts {
    pub script_paths: Vec<PathBuf>,
    pub script_paths_file: Option<PathBuf>,
    pub cache_dir: Option<PathBuf>,
    pub optimize: bool,
    pub threads: u8,
    pub no_warnings: bool,
    pub no_testonly: bool,
    pub no_breakpoint: bool,
    pub profile_off: bool,
    pub cache_file: Option<PathBuf>,
}

fn toggle_options(name: &'static str, help: &'static str) -> impl Parser<bool> {
    any::<String>(name)
        .help(help)
        .guard(|s| s.starts_with('-'), "doesn't start with -")
        .parse(move |s| {
            let Some(cur_name) = s.strip_prefix('-') else {
                return Err(format!("{} is not a toggle option", s));
            };
            if cur_name != name {
                Err(format!("{} is not a known toggle option name", cur_name))
            } else {
                Ok(true)
            }
        })
        .anywhere()
        .fallback(false)
}

fn slong(tag_str: &'static str, arg_str: &'static str) -> impl Parser<String> {
    let tag = any::<String>(tag_str).guard(move |s| s.as_str() == tag_str, "not arg name");
    let value = positional::<String>(arg_str);
    construct!(tag, value).anywhere().map(|pair| pair.1)
}

fn script_paths() -> impl Parser<Vec<PathBuf>> {
    let tag = any::<String>("-compile").guard(|s| s == "-compile", "not compile");
    let value = positional::<PathBuf>("SCRIPT_PATH");
    construct!(tag, value).anywhere().map(|pair| pair.1).many().catch()
}

fn script_paths_file() -> impl Parser<Option<PathBuf>> {
    let tag = any::<String>("-compilePathsFile")
        .help("File listing all of the script paths")
        .guard(|s| s == "-compilePathsFile", "not compilePathsFile");
    let value = positional::<PathBuf>("FILE");
    construct!(tag, value).anywhere().map(|pair| pair.1).optional().catch()
}

fn cache_dir() -> impl Parser<Option<PathBuf>> {
    let tag = any::<String>("-customCacheDir").guard(|s| s == "-customCacheDir", "not customCacheDir");
    let value = positional::<PathBuf>("CACHE_DIR");
    construct!(tag, value).map(|pair| pair.1).optional().catch()
}

impl Opts {
    pub fn load(args: &[&str]) -> Self {
        let optimize = toggle_options("optimize", "Optimize the redscripts");
        let threads = slong("-threads", "Number of theads to script_paths on")
            .parse(|s| s.parse::<u8>())
            .fallback(1);
        let no_warnings = toggle_options("Wnone", "No warnings");
        let no_testonly = toggle_options("no-testonly", "No testonly classes");
        let no_breakpoint = toggle_options("no-breakpoint", "No breakpoints");
        let profile_off = toggle_options("profile=off", "Profile off");
        let cache_file = any::<PathBuf>("CACHE_FILE").optional();

        let parser = construct!(Opts {
            script_paths(),
            script_paths_file(),
            cache_dir(),
            optimize,
            threads,
            no_warnings,
            no_testonly,
            no_breakpoint,
            profile_off,
            cache_file,
        });
        parser.to_options().run_inner(bpaf::Args::from(args)).unwrap()
    }
}
