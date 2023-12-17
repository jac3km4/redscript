use std::fs::File;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use bpaf::ParseFailure;
use opts::{fix_args, Opts};
#[cfg(test)]
use rstest_reuse;
use scc_lib::api::{SccResult, SccSettings};

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

    if let Err(_err) = run(opts, r6_dir) {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn run(opts: Opts, r6_dir: PathBuf) -> anyhow::Result<()> {
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

    SccApi::load().compile(settings.into());
    Ok(())
}

fn load_script_paths(script_paths_file: &Path) -> io::Result<Vec<Box<Path>>> {
    io::BufReader::new(File::open(script_paths_file)?)
        .lines()
        .map(|line| Ok(PathBuf::from(line?).into_boxed_path()))
        .collect()
}

struct SccApi {
    compile: unsafe extern "C" fn(settings: Box<SccSettings>) -> Box<SccResult>,
    free_result: unsafe extern "C" fn(result: Box<SccResult>),
}

impl SccApi {
    fn load() -> Self {
        use minidl::*;

        let dll_path = std::env::current_exe()
            .expect("should be able to get current exe path")
            .with_file_name("scc_lib.dll");

        let dll = Library::load(dll_path).expect("should be able to load scc.dll");
        unsafe {
            SccApi {
                compile: dll.sym("scc_compile\0").expect("should be able to get scc_compile"),
                free_result: dll
                    .sym("scc_free_result\0")
                    .expect("should be able to get scc_free_result"),
            }
        }
    }

    fn compile(&self, settings: Box<SccSettings>) {
        unsafe { (self.free_result)((self.compile)(settings)) }
    }
}
