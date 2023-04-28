use scc::opts::*;
use std::path::PathBuf;
use winsplit;
use std::println;

pub fn test_fix_args(arg_str: &str, expected_args: &[&str]) -> Result<Vec<String>, String> {
    let args = fix_args(winsplit::split(arg_str));
    (args == expected_args.iter().map(|&s| s.into()).collect::<Vec<String>>()).then_some(args).ok_or("Does not match expected output".to_owned())
}

pub struct TestArg {
    pub raw: &'static str,
    pub parsed: &'static str,
}

const TEST_FILE: TestArg = TestArg {
    raw: r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\packed.reds""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\packed.reds"
};

const TEST_DIRECTORY: TestArg = TestArg {
    raw: r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\extra\""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\extra\\"
};

fn test_order(f_args: &[TestArg]) {
    let args = test_fix_args(&[concat!(
        r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\""#,
        r#" "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
        r#" -Wnone -threads 4 -no-testonly -no-breakpoint -profile=off -optimize"#), &f_args.iter().map(|a| a.raw).collect::<Vec<_>>().join("")].join(""),
        &[&[
            "-compile",
            "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\",
            "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts",
            "-Wnone",
            "-threads",
            "4",
            "-no-testonly",
            "-no-breakpoint",
            "-profile=off",
            "-optimize",
        ], f_args.iter().flat_map(|a| ["-compile", a.parsed]).collect::<Vec<_>>().as_slice()].concat()
    ).unwrap();
    let opts = Opts::load(args.iter().map(String::as_str).collect::<Vec<&str>>().as_slice());
    println!("{:#?}", opts);

    assert!(opts.script_paths == [vec![PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\")],
        f_args.iter().map(|a| PathBuf::from(a.parsed)).collect()
    ].concat());
    assert!(opts.cache_file == PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts"));
    assert!(opts.cache_dir == None);
    assert!(opts.threads == Some(4));
}

mod tests {
    use super::*;

    #[test] fn test_standard() { test_order(&[]); }
    #[test] fn test_fd() { test_order(&[TEST_FILE, TEST_DIRECTORY]); }
    #[test] fn test_fdf() { test_order(&[TEST_FILE, TEST_FILE, TEST_FILE]); }
    #[test] fn test_ffd() { test_order(&[TEST_FILE, TEST_FILE, TEST_DIRECTORY]); }
    #[test] fn test_fdd() { test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY]); }
    #[test] fn test_fddf() { test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]); }
    #[test] fn test_fdfd() { test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]); }
    #[test] fn test_fddfd() { test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY]); }
    #[test] fn test_df() { test_order(&[TEST_DIRECTORY, TEST_FILE]); }
    #[test] fn test_dfd() { test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY]); }
    #[test] fn test_ddf() { test_order(&[TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]); }
    #[test] fn test_dff() { test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE]); }
    #[test] fn test_dffd() { test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY]); }
    #[test] fn test_dfdf() { test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY]); }
    #[test] fn test_dffdf() { test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY, TEST_FILE]); }
}