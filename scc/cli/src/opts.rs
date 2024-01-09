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

fn is_not_slong(p: &PathBuf) -> bool {
    matches!((*p).to_str(), Some(str) if !str.starts_with('-'))
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

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use rstest_reuse::{self, *};

    use super::*;

    pub fn test_fix_args(arg_str: &str, expected_args: &[&str]) -> Result<Vec<String>, String> {
        let args = fix_args(winsplit::split(arg_str));
        (args == expected_args.iter().map(|&s| s.into()).collect::<Vec<String>>())
            .then_some(args.clone())
            .ok_or("Does not match expected output".to_owned())
    }

    #[derive(Debug, Clone)]
    pub enum Arg {
        Path(PathBuf),
        String(String),
        Number(u8),
    }

    impl Arg {
        pub fn to_command_str(&self) -> String {
            match self {
                Self::Path(p) => format!("\"{}\"", p.as_path().to_str().unwrap_or_default()),
                Self::String(s) => s.clone(),
                Self::Number(n) => n.to_string(),
            }
        }

        pub fn to_arg_str(&self) -> String {
            match self {
                Self::Path(p) => p.as_path().to_str().unwrap_or_default().to_string(),
                Self::String(s) => s.clone(),
                Self::Number(n) => n.to_string(),
            }
        }
    }

    impl From<String> for Arg {
        fn from(item: String) -> Self {
            Self::String(item)
        }
    }

    impl From<PathBuf> for Arg {
        fn from(item: PathBuf) -> Self {
            Self::Path(item)
        }
    }

    impl From<&PathBuf> for Arg {
        fn from(item: &PathBuf) -> Self {
            Self::Path(item.clone())
        }
    }

    impl From<u8> for Arg {
        fn from(item: u8) -> Self {
            Self::Number(item)
        }
    }

    impl From<&str> for Arg {
        fn from(item: &str) -> Self {
            Self::String(item.to_owned())
        }
    }

    const SCRIPTS_DIR: &str = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\";

    const PATHS_FILE: &str =
        "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\redscript_paths.txt";

    const CACHE_FILE: &str =
        "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts";

    const CACHE_DIR: &str = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\modded";

    #[template]
    #[rstest]
    fn file_directory_orders(
        #[values(SCRIPTS_DIR)] scripts_dir: &str,
        #[values(Some(CACHE_FILE), None)] cache_file: Option<&str>,
        #[values(Some(CACHE_DIR), None)] cache_dir: Option<&str>,
        #[values(Some(PATHS_FILE), None)] script_paths_file: Option<&str>,
        #[values(&["none"], &[])] warnings: &[&str],
        #[values(Some(true), None)] optimize: Option<bool>,
        #[values(Some(4), None)] threads: Option<u8>,
        #[values(Some(false), None)] no_testonly: Option<bool>,
        #[values(Some(false), None)] no_breakpoint: Option<bool>,
        #[values(Some(false), None)] no_exec: Option<bool>,
        #[values(Some(false), None)] no_debug: Option<bool>,
        #[values(Some(true), Some(false), None)] profile: Option<bool>,
    ) {
    }

    #[test]
    fn get_help() {
        assert_eq!(
            "not compile",
            Opts::get_parser()
                .run_inner(bpaf::Args::from(&["--help"]))
                .unwrap_err()
                .unwrap_stderr()
        );
    }

    #[test]
    fn check_options() {
        Opts::get_parser().check_invariants(false);
    }

    #[apply(file_directory_orders)]
    fn standard(
        scripts_dir: &str,
        cache_file: Option<&str>,
        cache_dir: Option<&str>,
        script_paths_file: Option<&str>,
        warnings: &[&str],
        optimize: Option<bool>,
        threads: Option<u8>,
        no_testonly: Option<bool>,
        no_breakpoint: Option<bool>,
        no_exec: Option<bool>,
        no_debug: Option<bool>,
        profile: Option<bool>,
    ) {
        let mut args = Vec::<Arg>::new();
        args.push("-compile".into());
        args.push(PathBuf::from(scripts_dir).into());
        if let Some(value) = cache_file {
            args.push(PathBuf::from(value).into());
        }
        if let Some(value) = cache_dir {
            args.push("-customCacheDir".into());
            args.push(PathBuf::from(value).into());
        }
        if let Some(value) = script_paths_file {
            args.push("-compilePathsFile".into());
            args.push(PathBuf::from(value).into());
        }
        for warning in warnings {
            args.push(["-W", warning].join("").into());
        }
        if let Some(n) = threads {
            args.push("-threads".into());
            args.push(n.to_string().into());
        }
        if no_testonly == Some(true) {
            args.push("-no-testonly".into());
        }
        if no_breakpoint == Some(true) {
            args.push("-no-breakpoint".into());
        }
        if no_exec == Some(true) {
            args.push("-no-exec".into());
        }
        if no_debug == Some(true) {
            args.push("-no-debug".into());
        }
        match profile {
            Some(false) => args.push("-profile=off".into()),
            Some(true) => args.push("-profile=on".into()),
            None => {}
        };
        if optimize == Some(true) {
            args.push("-optimize".to_owned().into());
        }
        let expected_args = args.iter().map(Arg::to_arg_str).collect::<Vec<_>>();
        let fixed_args = test_fix_args(
            args.iter()
                .map(Arg::to_command_str)
                .collect::<Vec<_>>()
                .join(" ")
                .as_str(),
            expected_args.iter().map(String::as_str).collect::<Vec<_>>().as_slice(),
        )
        .unwrap();
        let opts = Opts::load(&fixed_args.iter().map(String::as_str).collect::<Vec<&str>>()).unwrap();

        self::assert_eq!(opts.scripts_dir, PathBuf::from(scripts_dir));
        self::assert_eq!(opts.cache_file, cache_file.map(PathBuf::from));
        self::assert_eq!(opts.cache_dir, cache_dir.map(PathBuf::from));
        self::assert_eq!(opts.script_paths_file, script_paths_file.map(PathBuf::from));
        self::assert_eq!(opts.warnings, warnings);
        self::assert_eq!(opts.threads, threads.unwrap_or(Opts::DEFAULT_THREADS));
        self::assert_eq!(opts.no_testonly, no_testonly.unwrap_or(Opts::DEFAULT_NO_TESTONLY));
        self::assert_eq!(opts.no_breakpoint, no_breakpoint.unwrap_or(Opts::DEFAULT_NO_BREAKPOINT));
        self::assert_eq!(opts.profile, profile.unwrap_or(Opts::DEFAULT_NO_PROFILE));
        self::assert_eq!(opts.optimize, optimize.unwrap_or(Opts::DEFAULT_OPTIMIZE));
        self::assert_eq!(opts.no_exec, no_exec.unwrap_or(Opts::DEFAULT_NO_EXEC));
        self::assert_eq!(opts.no_debug, no_debug.unwrap_or(Opts::DEFAULT_NO_DEBUG));
    }
}
