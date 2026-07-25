use std::env;
use std::ffi::CString;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};

use anyhow::{Context, anyhow};
use arguments::Arguments;

mod arguments;
mod capi;
mod raw;

fn main() -> anyhow::Result<()> {
    let (r6_dir, args) = match Arguments::from_env_args(std::env::args()) {
        Ok(args) => {
            let r6_dir = args
                .scripts_dir
                .parent()
                .context("provided scripts directory has no parent directory")?
                .to_owned();
            (r6_dir, Some(args))
        }
        Err(err) => {
            let current_dir = env::current_dir()?;
            let r6_dir = current_dir
                .parent()
                .and_then(Path::parent)
                .map(|path| path.join("r6"))
                .filter(|path| path.exists())
                .ok_or_else(|| anyhow!("could not run scc: {err}"))?;
            (r6_dir, None)
        }
    };
    run(&r6_dir, args)
}

fn run(r6_dir: &Path, args: Option<Arguments>) -> anyhow::Result<()> {
    let api = capi::load()?;

    let settings_new = api.settings_new.context("missing 'settings_new'")?;
    let set_custom_cache_file = api
        .settings_set_custom_cache_file
        .context("missing 'settings_set_custom_cache_file'")?;
    let add_script_path = api
        .settings_add_script_path
        .context("missing 'settings_add_script_path'")?;
    let compile = api.compile.context("missing 'compile'")?;
    let free_result = api.free_result.context("missing 'free_result'")?;
    let get_success = api.get_success.context("missing 'get_success'")?;

    let root = c_path(r6_dir)?;

    unsafe {
        let settings = settings_new(root.as_ptr());

        if let Some(args) = args {
            if let Some(cache_file) = args.cache_file {
                let cache_file = c_path(&cache_file)?;
                set_custom_cache_file(settings, cache_file.as_ptr());
            }

            if let Some(script_paths_file) = args.script_paths_file {
                for script_path in io::BufReader::new(File::open(script_paths_file)?)
                    .lines()
                    .map(|line| Ok(PathBuf::from(line?)))
                    .collect::<io::Result<Vec<_>>>()?
                {
                    let script_path = c_path(&script_path)?;
                    add_script_path(settings, script_path.as_ptr());
                }
            }
        }

        let res = compile(settings);
        let succeeded = !get_success(res).is_null();
        free_result(res);

        if !succeeded {
            std::process::exit(1);
        }
    }

    Ok(())
}

fn c_path(path: &Path) -> anyhow::Result<CString> {
    Ok(CString::new(path.to_string_lossy().as_ref())?)
}
