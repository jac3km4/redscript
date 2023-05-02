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

const TEST_SCRIPT_PATHS: &str =
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\redscript_paths.txt";

const TEST_FILE: &str =
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\packed.reds";

const TEST_DIRECTORY: &str =
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\extra\\";

const TEST_CACHE_FILE: &str =
    "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts";

const TEST_CACHE_DIR: &str = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\modded";

#[template]
#[rstest]
#[case(&[])]
// #[case(&[TEST_FILE])]
// #[case(&[TEST_DIRECTORY])]
#[case(&[TEST_FILE, TEST_DIRECTORY])]
// #[case(&[TEST_FILE, TEST_FILE, TEST_FILE])]
// #[case(&[TEST_FILE, TEST_FILE, TEST_DIRECTORY])]
// #[case(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY])]
// #[case(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE])]
// #[case(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE])]
// #[case(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY])]
// #[case(&[TEST_DIRECTORY, TEST_FILE])]
// #[case(&[TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY])]
// #[case(&[TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE])]
// #[case(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE])]
// #[case(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY])]
// #[case(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY])]
// #[case(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY, TEST_FILE])]
fn file_directory_orders(
    #[case] f_args: &[&str],
    #[values(Some(TEST_CACHE_FILE), None)] cache_file: Option<&str>,
    #[values(Some(TEST_CACHE_DIR), None)] cache_dir: Option<&str>,
    #[values(Some(TEST_SCRIPT_PATHS), None)] script_paths_file: Option<&str>,
    #[values(Some("none"), None)] warnings: Option<&str>,
    #[values(Some(true), None)] optimize: Option<bool>,
    #[values(Some(4), None)] threads: Option<u8>,
    #[values(Some(false), None)] no_testonly: Option<bool>,
    #[values(Some(false), None)] no_breakpoint: Option<bool>,
    #[values(Some(true), Some(false), None)] profile: Option<bool>,
) {
}

#[cfg(test)]
mod tests {
    use super::*;

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
        f_args: &[&str],
        cache_file: Option<&str>,
        cache_dir: Option<&str>,
        script_paths_file: Option<&str>,
        warnings: Option<&str>,
        optimize: Option<bool>,
        threads: Option<u8>,
        no_testonly: Option<bool>,
        no_breakpoint: Option<bool>,
        profile: Option<bool>,
    ) {
        let mut args = Vec::<Arg>::new();
        args.push("-compile".into());
        args.push(
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\").into(),
        );
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
        if let Some(value) = warnings {
            args.push(["-W", value].join("").into());
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
        match profile {
            Some(false) => args.push("-profile=off".into()),
            Some(true) => args.push("-profile=on".into()),
            None => {}
        };
        if optimize == Some(true) {
            args.push("-optimize".to_owned().into());
        }
        args.extend(f_args.iter().flat_map(|a| ["-compile".into(), PathBuf::from(a).into()]));
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
        let opts = Opts::load(&fixed_args.iter().map(String::as_str).collect::<Vec<&str>>());

        self::assert_eq!(
            opts.script_paths,
            [PathBuf::from(
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\"
            )]
            .into_iter()
            .chain(f_args.iter().map(|s| PathBuf::from(s)))
            .collect::<Vec<_>>()
        );
        self::assert_eq!(opts.cache_file, cache_file.map(|s| PathBuf::from(s)));
        self::assert_eq!(opts.cache_dir, cache_dir.map(|s| PathBuf::from(s)));
        self::assert_eq!(opts.script_paths_file, script_paths_file.map(|s| PathBuf::from(s)));
        self::assert_eq!(opts.warnings.as_deref(), warnings.as_deref());
        self::assert_eq!(opts.threads, threads.unwrap_or(Opts::default().threads));
        self::assert_eq!(opts.no_testonly, no_testonly.unwrap_or(Opts::default().no_testonly));
        self::assert_eq!(
            opts.no_breakpoint,
            no_breakpoint.unwrap_or(Opts::default().no_breakpoint)
        );
        self::assert_eq!(opts.profile, profile.unwrap_or(Opts::default().profile));
        self::assert_eq!(opts.optimize, optimize.unwrap_or(Opts::default().optimize));
    }
}
