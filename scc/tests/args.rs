use std::path::PathBuf;

use scc::opts::*;
use winsplit;

pub fn test_fix_args(arg_str: &str, expected_args: &[&str]) -> Result<Vec<String>, String> {
    let args = fix_args(winsplit::split(arg_str));
    (args == expected_args.iter().map(|&s| s.into()).collect::<Vec<String>>())
        .then_some(args)
        .ok_or("Does not match expected output".to_owned())
}

pub struct TestArg {
    pub raw: &'static str,
    pub parsed: &'static str,
}

const TEST_FILE: TestArg = TestArg {
    raw: r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\packed.reds""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\packed.reds",
};

const TEST_DIRECTORY: TestArg = TestArg {
    raw: r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\extra\""#,
    parsed: "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\extra\\",
};

fn test_order(f_args: &[TestArg]) {
    let args = test_fix_args(
        &[
            concat!(
                r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\""#,
                r#" "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
                r#" -Wnone -threads 4 -no-testonly -no-breakpoint -profile=off -optimize"#
            ),
            &f_args.iter().map(|a| a.raw).collect::<Vec<_>>().join(""),
        ]
        .join(""),
        &[
            &[
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
            ],
            f_args
                .iter()
                .flat_map(|a| ["-compile", a.parsed])
                .collect::<Vec<_>>()
                .as_slice(),
        ]
        .concat(),
    )
    .unwrap();
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
    assert_eq!(opts.cache_dir, None);
    assert_eq!(opts.threads, 4);
}

fn test_order_cyber(f_args: &[TestArg]) {
    let args = test_fix_args(
        &[
            concat!(
                r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts""#,
                r#" -customCacheDir "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\modded""#,
            ),
            &f_args.iter().map(|a| a.raw).collect::<Vec<_>>().join(""),
        ]
        .join(""),
        &[
            &[
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts",
                "-customCacheDir",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\modded",
            ],
            f_args
                .iter()
                .flat_map(|a| ["-compile", a.parsed])
                .collect::<Vec<_>>()
                .as_slice(),
        ]
        .concat(),
    )
    .unwrap();
    let opts = Opts::load(args.iter().map(String::as_str).collect::<Vec<&str>>().as_slice());

    assert_eq!(
        opts.script_paths,
        [PathBuf::from(
            "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts"
        )]
        .into_iter()
        .chain(f_args.iter().map(|a| PathBuf::from(a.parsed)))
        .collect::<Vec<_>>(),
        "Testing script_paths"
    );
    assert_eq!(opts.cache_file, None, "Testings cache_file");
    assert_eq!(
        opts.cache_dir,
        Some("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\modded".into()),
        "Testing cache_dir"
    );
}

mod standard {
    use super::*;

    #[test]
    fn test_empty() {
        test_order(&[]);
    }
    #[test]
    fn test_fd() {
        test_order(&[TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_fdf() {
        test_order(&[TEST_FILE, TEST_FILE, TEST_FILE]);
    }
    #[test]
    fn test_ffd() {
        test_order(&[TEST_FILE, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_fdd() {
        test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY]);
    }
    #[test]
    fn test_fddf() {
        test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_fdfd() {
        test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_fddfd() {
        test_order(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_df() {
        test_order(&[TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_dfd() {
        test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_ddf() {
        test_order(&[TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_dff() {
        test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE]);
    }
    #[test]
    fn test_dffd() {
        test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_dfdf() {
        test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_dffdf() {
        test_order(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY, TEST_FILE]);
    }
}

mod cybercmd {
    use super::*;

    #[test]
    fn test_empty() {
        test_order_cyber(&[]);
    }
    #[test]
    fn test_fd() {
        test_order_cyber(&[TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_fdf() {
        test_order_cyber(&[TEST_FILE, TEST_FILE, TEST_FILE]);
    }
    #[test]
    fn test_ffd() {
        test_order_cyber(&[TEST_FILE, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_fdd() {
        test_order_cyber(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY]);
    }
    #[test]
    fn test_fddf() {
        test_order_cyber(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_fdfd() {
        test_order_cyber(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_fddfd() {
        test_order_cyber(&[TEST_FILE, TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_df() {
        test_order_cyber(&[TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_dfd() {
        test_order_cyber(&[TEST_DIRECTORY, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_ddf() {
        test_order_cyber(&[TEST_DIRECTORY, TEST_DIRECTORY, TEST_FILE]);
    }
    #[test]
    fn test_dff() {
        test_order_cyber(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE]);
    }
    #[test]
    fn test_dffd() {
        test_order_cyber(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_dfdf() {
        test_order_cyber(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY]);
    }
    #[test]
    fn test_dffdf() {
        test_order_cyber(&[TEST_DIRECTORY, TEST_FILE, TEST_FILE, TEST_DIRECTORY, TEST_FILE]);
    }
}
