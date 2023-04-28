use scc::opts::*;
use std::path::PathBuf;
use winsplit;
use std::{println as info};

pub fn test_fix_args(arg_str: &str, expected_args: &[&str]) -> Result<Vec<String>, String> {
    let args = fix_args(winsplit::split(arg_str));
    (args == expected_args.iter().map(|&s| s.into()).collect::<Vec<String>>()).then_some(args).ok_or("Does not match expected output".to_owned())
}

mod tests {
    use super::*;

    #[test]
    fn test_standard() {
        let args = test_fix_args(concat!(
            r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\" "#,
            r#""C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
            r#" -Wnone -threads 4 -no-testonly -no-breakpoint -profile=off -optimize"#),
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
                "-optimize"
            ]
        ).unwrap();
        let opts = Opts::load(args);
        info!("{:#?}", opts);

        assert!(opts.script_paths == vec![PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\")]);
        assert!(opts.cache_file == PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts"));
        assert!(opts.cache_dir == None);
        assert!(opts.threads == Some(4));
    }

    #[test]
    fn test_custom_files() {
        let args = test_fix_args(concat!(
            r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\" "#,
            r#""C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
            r#" -Wnone -threads 4 -no-testonly -no-breakpoint -profile=off -optimize"#,
            r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\my_mod.module.reds""#,
            r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\my_mod.packed.reds""#),
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
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.module.reds",
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.packed.reds"
            ]
        ).unwrap();
        let opts = Opts::load(args);
        info!("{:#?}", opts);

        assert!(opts.script_paths == vec![
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\"),
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.module.reds"),
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.packed.reds")
        ]);
        assert!(opts.cache_file == PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts"));
        assert!(opts.cache_dir == None);
        assert!(opts.threads == Some(4));
    }

    #[test]
    fn test_custom_folder() {
        let args = test_fix_args(concat!(
            r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\" "#,
            r#""C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
            r#" -Wnone -threads 4 -no-testonly -no-breakpoint -profile=off -optimize"#,
            r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\""#),
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
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\"
            ]
        ).unwrap();
        let opts = Opts::load(args);
        info!("{:#?}", opts);

        assert!(opts.script_paths == vec![
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\"),
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\")
        ]);
        assert!(opts.cache_file == PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts"));
        assert!(opts.cache_dir == None);
        assert!(opts.threads == Some(4));
    }

    #[test]
    fn test_custom_folder_file() {
        let args = test_fix_args(concat!(
            r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\" "#,
            r#""C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
            r#" -Wnone -threads 4 -no-testonly -no-breakpoint -profile=off -optimize"#,
            r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\""#,
            r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\my_mod.packed.reds""#),
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
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\",
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.packed.reds"
            ]
        ).unwrap();
        let opts = Opts::load(args);
        info!("{:#?}", opts);

        assert!(opts.script_paths == vec![
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\"),
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\"),
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.packed.reds")
        ]);
        assert!(opts.cache_file == PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts"));
        assert!(opts.cache_dir == None);
        assert!(opts.threads == Some(4));
    }

    #[test]
    fn test_custom_file_folder() {
        let args = test_fix_args(concat!(
            r#"-compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\scripts\" "#,
            r#""C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\r6\cache\final.redscripts""#,
            r#" -Wnone -threads 4 -no-testonly -no-breakpoint -profile=off -optimize"#,
            r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\my_mod.packed.reds""#,
            r#" -compile "C:\Program Files (x86)\Steam\steamapps\common\Cyberpunk 2077\red4ext\plugins\my_mod\extra\""#,
        ),
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
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.packed.reds",
                "-compile",
                "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\extra\\",
            ]
        ).unwrap();
        let opts = Opts::load(args);
        info!("{:#?}", opts);

        assert!(opts.script_paths == vec![
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\scripts\\"),
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\my_mod.packed.reds"),
            PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\red4ext\\plugins\\my_mod\\extra\\"),
        ]);
        assert!(opts.cache_file == PathBuf::from("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Cyberpunk 2077\\r6\\cache\\final.redscripts"));
        assert!(opts.cache_dir == None);
        assert!(opts.threads == Some(4));
    }
}