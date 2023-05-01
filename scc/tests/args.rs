use std::path::PathBuf;

use rstest::rstest;
use rstest_reuse::{self, *};
use scc::opts::*;
use winsplit;

pub fn test_fix_args(arg_str: &str, expected_args: &[&str]) -> Result<Vec<String>, String> {
    let args = fix_args(winsplit::split(arg_str));
    (args == expected_args.iter().map(|&s| s.into()).collect::<Vec<String>>())
        .then_some(args)
        .ok_or({
            // println!("fixed: {:#?}", args);
            // println!("expected: {:#?}", expected_args);
            "Does not match expected output".to_owned()
        })
}

pub struct TestArg {
    pub raw: &'static str,
    pub parsed: &'static str,
}

const TEST_SCRIPT_PATHS: TestArg = TestArg {
    raw: r#" -compilePathsFile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\redscript_paths.txt""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\redscript_paths.txt",
};

const TEST_FILE: TestArg = TestArg {
    raw: r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\packed.reds""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\packed.reds",
};

const TEST_DIRECTORY: TestArg = TestArg {
    raw: r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\extra\""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\extra\\",
};

const TEST_CACHE_FILE: TestArg = TestArg {
    raw: r#" "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts",
};

const TEST_CACHE_DIR: TestArg = TestArg {
    raw: r#" -customCacheDir "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\modded""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\modded",
};

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
    #[case] f_args: &[TestArg],
    #[values(Some(&TEST_CACHE_FILE), None)] cache_file: Option<&TestArg>,
    #[values(Some(&TEST_CACHE_DIR), None)] cache_dir: Option<&TestArg>,
    #[values(Some(&TEST_SCRIPT_PATHS), None)] script_paths_file: Option<&TestArg>,
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
        f_args: &[TestArg],
        cache_file: Option<&TestArg>,
        cache_dir: Option<&TestArg>,
        script_paths_file: Option<&TestArg>,
        warnings: Option<&str>,
        optimize: Option<bool>,
        threads: Option<u8>,
        no_testonly: Option<bool>,
        no_breakpoint: Option<bool>,
        profile: Option<bool>,
    ) {
        std::panic::set_hook(Box::new(|a| println!("{:#?}", a)));
        let mut expected_args = vec![
            "-compile",
            "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\",
        ];
        let mut arg_str =
            String::from(r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\""#);
        if let Some(value) = cache_file {
            expected_args.push(value.parsed);
            arg_str.push_str(value.raw);
        }
        if let Some(value) = cache_dir {
            expected_args.push("-customCacheDir");
            expected_args.push(value.parsed);
            arg_str.push_str(value.raw);
        }
        if let Some(value) = script_paths_file {
            expected_args.push("-compilePathsFile");
            expected_args.push(value.parsed);
            arg_str.push_str(value.raw);
        }
        let warning_str;
        if let Some(value) = warnings {
            warning_str = ["-W", value].join("");
            expected_args.push(&warning_str);
            arg_str.push_str(" -W");
            arg_str.push_str(value);
        }
        let thread_str;
        if let Some(n) = threads {
            thread_str = n.to_string();
            expected_args.push("-threads");
            expected_args.push(&thread_str);
            arg_str.push_str(" -threads ");
            arg_str.push_str(thread_str.as_str());
        }
        if no_testonly == Some(true) {
            expected_args.push("-no-testonly");
            arg_str.push_str(" -no-testonly");
        }
        if no_breakpoint == Some(true) {
            expected_args.push("-no-breakpoint");
            arg_str.push_str(" -no-breakpoint");
        }
        match profile {
            Some(false) => {
                expected_args.push("-profile=off");
                arg_str.push_str(" -profile=off");
            }
            Some(true) => {
                expected_args.push("-profile=on");
                arg_str.push_str(" -profile=on");
            }
            None => {}
        };
        if optimize == Some(true) {
            expected_args.push("-optimize");
            arg_str.push_str(" -optimize");
        }
        expected_args.extend(f_args.iter().flat_map(|a| ["-compile", a.parsed]));
        f_args.iter().for_each(|a| arg_str.push_str(a.raw));
        let args = test_fix_args(arg_str.as_str(), expected_args.as_slice()).unwrap();
        let opts = Opts::load(&args.iter().map(String::as_str).collect::<Vec<&str>>());

        assert_eq!(
            opts.script_paths,
            [PathBuf::from(
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\"
            )]
            .into_iter()
            .chain(f_args.iter().map(|a| PathBuf::from(a.parsed)))
            .collect::<Vec<_>>()
        );
        assert_eq!(opts.cache_file, cache_file.map(|f| f.parsed.into()));
        assert_eq!(opts.cache_dir, cache_dir.map(|f| f.parsed.into()));
        assert_eq!(opts.script_paths_file, script_paths_file.map(|f| f.parsed.into()));
        assert_eq!(opts.warnings.as_deref(), warnings.as_deref());
        assert_eq!(opts.threads, threads.unwrap_or(Opts::default().threads));
        assert_eq!(opts.no_testonly, no_testonly.unwrap_or(Opts::default().no_testonly));
        assert_eq!(
            opts.no_breakpoint,
            no_breakpoint.unwrap_or(Opts::default().no_breakpoint)
        );
        assert_eq!(opts.profile, profile.unwrap_or(Opts::default().profile));
        assert_eq!(opts.optimize, optimize.unwrap_or(Opts::default().optimize));
    }
}
