use std::path::PathBuf;

use pretty_assertions::assert_eq;
use rstest::rstest;
use rstest_reuse::{self, *};
use scc::opts::*;
use winsplit;

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

pub struct TestArg {
    pub raw: &'static str,
    pub parsed: &'static str,
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
    println!(
        "{}",
        Opts::get_parser()
            .run_inner(bpaf::Args::from(&["--help"]))
            .unwrap_err()
            .unwrap_stdout()
    );
}

#[test]
fn check_options() {
    Opts::get_parser().check_invariants(false)
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
    for warning in warnings.iter() {
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
    let expected_args = args.iter().map(|a| a.to_arg_str()).collect::<Vec<_>>();
    let fixed_args = test_fix_args(
        args.iter()
            .map(Arg::to_command_str)
            .collect::<Vec<_>>()
            .join(" ")
            .as_str(),
        expected_args.iter().map(|a| a.as_str()).collect::<Vec<_>>().as_slice(),
    )
    .unwrap();
    let opts = Opts::load(&fixed_args.iter().map(String::as_str).collect::<Vec<&str>>()).unwrap();

    self::assert_eq!(opts.scripts_dir, Some(PathBuf::from(scripts_dir)));
    self::assert_eq!(opts.cache_file, cache_file.map(|s| PathBuf::from(s)));
    self::assert_eq!(opts.cache_dir, cache_dir.map(|s| PathBuf::from(s)));
    self::assert_eq!(opts.script_paths_file, script_paths_file.map(|s| PathBuf::from(s)));
    self::assert_eq!(opts.warnings, warnings);
    self::assert_eq!(opts.threads, threads.unwrap_or(Opts::DEFAULT_THREADS));
    self::assert_eq!(opts.no_testonly, no_testonly.unwrap_or(Opts::DEFAULT_NO_TESTONLY));
    self::assert_eq!(opts.no_breakpoint, no_breakpoint.unwrap_or(Opts::DEFAULT_NO_BREAKPOINT));
    self::assert_eq!(opts.profile, profile.unwrap_or(Opts::DEFAULT_NO_PROFILE));
    self::assert_eq!(opts.optimize, optimize.unwrap_or(Opts::DEFAULT_OPTIMIZE));
    self::assert_eq!(opts.no_exec, no_exec.unwrap_or(Opts::DEFAULT_NO_EXEC));
    self::assert_eq!(opts.no_debug, no_debug.unwrap_or(Opts::DEFAULT_NO_DEBUG));
}
