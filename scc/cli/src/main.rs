use std::fs::File;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use anyhow::Context;
use bpaf::ParseFailure;
use opts::{fix_args, Opts};
#[cfg(test)]
use rstest_reuse;
use scc_lib::api::{SccOutput, SccResult, SccSettings};

mod opts;

fn main() -> ExitCode {
    let opts = match Opts::load(
        fix_args(std::env::args().skip(1).collect())
            .iter()
            .map(String::as_str)
            .collect::<Vec<&str>>()
            .as_slice(),
    ) {
        Ok(opts) => opts,
        Err(ParseFailure::Stdout(out)) => {
            println!("{}", out);
            return ExitCode::SUCCESS;
        }
        Err(ParseFailure::Stderr(out)) => {
            eprintln!("{}", out);
            return ExitCode::FAILURE;
        }
    };

    let r6_dir = opts
        .scripts_dir
        .parent()
        .expect("r6/scripts directory must have a parent")
        .to_path_buf();

    match run(opts, &r6_dir) {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::FAILURE,
        Err(err) => {
            eprintln!("{}", err);

            // any error reported here is an unexpected critical failure, and this CLI wrapper
            // has no access to the logger, so we just write the error to a new log file
            std::fs::write(r6_dir.join("logs").join("scc_rCURRENT.log"), format!("{err}")).ok();
            ExitCode::FAILURE
        }
    }
}

fn run(opts: Opts, r6_dir: &Path) -> anyhow::Result<bool> {
    const BUNDLE_FILE_NAME: &str = "final.redscripts";

    let additional_script_paths = opts
        .script_paths_file
        .as_deref()
        .map(load_script_paths)
        .transpose()
        .unwrap_or_default()
        .unwrap_or_default();

    let custom_cache_file = match (opts.cache_file.as_deref(), opts.cache_dir.as_deref()) {
        (Some(file), _) => file.to_path_buf(),
        (None, Some(dir)) => dir.join(BUNDLE_FILE_NAME),
        (None, None) => r6_dir.join("cache").join(BUNDLE_FILE_NAME),
    };

    let settings = SccSettings {
        r6_dir: r6_dir.into(),
        custom_cache_file: Some(custom_cache_file.into()),
        additional_script_paths,
    };

    let is_success = SccApi::load()?.compile(settings.into());
    Ok(is_success)
}

fn load_script_paths(script_paths_file: &Path) -> io::Result<Vec<Box<Path>>> {
    io::BufReader::new(File::open(script_paths_file)?)
        .lines()
        .map(|line| Ok(PathBuf::from(line?).into_boxed_path()))
        .collect()
}

struct SccApi {
    compile: unsafe extern "C" fn(settings: Box<SccSettings>) -> Box<SccResult>,
    get_success: unsafe extern "C" fn(result: &SccResult) -> Option<&SccOutput>,
    free_result: unsafe extern "C" fn(result: Box<SccResult>),
}

impl SccApi {
    fn load() -> anyhow::Result<Self> {
        use minidl::*;

        let lib_path = std::env::current_exe()
            .context("Could not get current exe path")?
            .with_file_name("scc_lib.dll");

        let lib = Library::load(lib_path).context("Could not load the scc shared library")?;
        unsafe {
            Ok(SccApi {
                compile: lib
                    .sym("scc_compile\0")
                    .context("Could not load the scc_compile function")?,
                get_success: lib
                    .sym("scc_get_success\0")
                    .context("Could not load the scc_get_success function")?,
                free_result: lib
                    .sym("scc_free_result\0")
                    .context("Could not load the scc_free_result function")?,
            })
        }
    }

    fn compile(&self, settings: Box<SccSettings>) -> bool {
        unsafe {
            let result = (self.compile)(settings);
            let output = (self.get_success)(&result);
            let is_success = output.is_some();
            (self.free_result)(result);
            is_success
        }
    }
}
