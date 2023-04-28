use bpaf::*;
use std::ffi::OsString;
use std::path::PathBuf;
use std::collections::VecDeque;

pub fn fix_args(args: Vec<String>) -> Vec<String> {
    let mut fixed_args: Vec<String> = vec![];
    let mut broken = false;

    log::debug!("{:#?}", args);

    // when cyberpunk's args are processed, the \" messes up the grouping, so we need to fix the args that have quotes & spaces in them
    for arg in args {
        if !arg.contains("\"") && !broken {
            fixed_args.push(arg.to_owned());
        } else {
            if !broken {
                broken = true;
                let spaceless = arg.replace("\"", "\\\"");
                for part in spaceless.split("\"") {
                    fixed_args.push(part.to_string());
                }
            } else {
                // fixes args that come after the first
                let broken_arg = if arg.contains("\"") {
                    arg.replace("\"", "\\")
                } else {
                    arg
                };
                if broken_arg.contains(" ") {
                    let mut parts = broken_arg.split(' ').collect::<VecDeque<&str>>();
                    let last = fixed_args.last().unwrap();
                    *fixed_args.last_mut().unwrap() = format!("{} {}", last, &parts.pop_front().unwrap());
                    for part in parts {
                        fixed_args.push(part.to_string());
                    }
                } else {
                    let last = fixed_args.last().unwrap();
                    *fixed_args.last_mut().unwrap() = format!("{} {}", last, broken_arg);
                }
            }
        }
    }

    log::debug!("{:#?}", fixed_args);

    fixed_args
}

#[derive(Clone, Debug)]
pub struct Opts {
    pub script_paths: Vec<PathBuf>,
    pub cache_file: PathBuf,
    pub cache_dir: Option<PathBuf>,
    pub optimize: bool,
    pub threads: Option<u8>,
    pub no_warnings: bool,
    pub no_testonly: bool,
    pub no_breakpoint: bool,
    pub profile_off: bool
}

fn toggle_options(name: &'static str, help: &'static str) -> impl Parser<bool> {
    any::<String>(name)
        .help(help)
        .guard(|s| s.starts_with("-"), "doesn't start with -")
        .parse(move |s| {
            let (state, cur_name) = if let Some(rest) = s.strip_prefix('-') {
                (true, rest)
            } else {
                return Err(format!("{} is not a toggle option", s));
            };
            if cur_name != name {
                Err(format!("{} is not a known toggle option name", cur_name))
            } else {
                Ok(state)
            }
        })
        .anywhere()
}

fn slong(tag_str: &'static str, arg_str: &'static str) -> impl Parser<String> {
    let tag = any::<String>(tag_str)
        .guard(|s| s.as_str() == tag_str.to_owned(), "not arg name");
    let value = positional::<String>(arg_str);
    construct!(tag, value)
        .anywhere()
        .map(|pair| pair.1)
}

fn script_paths() -> impl Parser<Vec<PathBuf>> {
    let tag = any::<String>("-compile")
        .guard(|s| s == "-compile", "not compile");
    let value = positional::<OsString>("SCRIPT_PATH");
    construct!(tag, value)
        .anywhere()
        .map(|pair| pair.1)
        .map(PathBuf::from)
        .many()
        .catch()
}

fn cache_dir() -> impl Parser<Option<PathBuf>> {
    let tag = any::<String>("-customCacheDir")
        .guard(|s| s == "-customCacheDir", "not customCacheDir");
    let value = positional::<OsString>("CACHE_DIR");
    construct!(tag, value)
        .anywhere()
        .map(|pair| pair.1)
        .map(PathBuf::from)
        .optional()
        .catch()
}

impl Opts {
    pub fn load(args: Vec<String>) -> Self {
        let cache_file = any::<OsString>("CACHE_FILE")
            .anywhere()
            .map(PathBuf::from);
        let optimize = toggle_options("optimize", "Optimize the redscripts");
        let threads = slong("-threads", "Number of theads to script_paths on").map(|s| s.parse::<u8>().unwrap()).optional();
        let no_warnings = toggle_options("Wnone", "No warnings");
        let no_testonly = toggle_options("no-testonly", "No testonly classes");
        let no_breakpoint = toggle_options("no-breakpoint", "No breakpoints");
        let profile_off = toggle_options("profile=off", "Profile off");

        let parser = construct!(Opts {
            script_paths(),
            cache_file,
            cache_dir(),
            optimize,
            threads,
            no_warnings,
            no_testonly,
            no_breakpoint,
            profile_off
        });
        let fixed_args = fix_args(args);
        let bpaf_args = fixed_args.iter().map(|s| s.as_str()).collect::<Vec<&str>>();
        parser.to_options().run_inner(bpaf::Args::from(bpaf_args.as_slice())).unwrap()
    }
}