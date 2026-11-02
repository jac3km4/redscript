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
    let copy_error = api.copy_error.context("missing 'copy_error'")?;

    let root = c_path(r6_dir)?;

    unsafe {
        let settings = settings_new(root.as_ptr());

        if let Some(args) = args {
            if let Some(cache_file) = args.cache_file {
                let cache_file = c_path(&cache_file)?;
                set_custom_cache_file(settings, cache_file.as_ptr());
            }

            if let Some(script_paths_file) = args.script_paths_file {
                for script_path in
                    io::BufReader::new(File::open(&script_paths_file).with_context(|| {
                        format!(
                            "failed to open script paths file '{}'",
                            script_paths_file.display()
                        )
                    })?)
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
        finish_result(res, get_success, copy_error, free_result)
    }
}

unsafe fn finish_result(
    result: *mut raw::SccResult,
    get_success: unsafe extern "C" fn(*mut raw::SccResult) -> *mut raw::SccOutput,
    copy_error: unsafe extern "C" fn(*mut raw::SccResult, *mut i8, usize) -> usize,
    free_result: unsafe extern "C" fn(*mut raw::SccResult),
) -> anyhow::Result<()> {
    let outcome = if unsafe { get_success(result) }.is_null() {
        let mut buffer = vec![0u8; 1024];
        let copied = loop {
            let copied = unsafe { copy_error(result, buffer.as_mut_ptr().cast(), buffer.len()) };
            // A full buffer may be truncated: copy_error returns bytes copied, not required size.
            if copied < buffer.len() - 1 {
                break copied;
            }
            buffer.resize(buffer.len() * 2, 0);
        };
        Err(anyhow!(
            "REDscript compilation failed: {}",
            String::from_utf8_lossy(&buffer[..copied])
        ))
    } else {
        Ok(())
    };
    unsafe { free_result(result) };
    outcome
}

fn c_path(path: &Path) -> anyhow::Result<CString> {
    Ok(CString::new(path.to_string_lossy().as_ref())?)
}

#[cfg(test)]
mod tests {
    use std::ptr;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use super::*;

    struct MockResult {
        error: Option<String>,
        frees: Arc<AtomicUsize>,
    }

    unsafe extern "C" fn get_success(result: *mut raw::SccResult) -> *mut raw::SccOutput {
        let result = unsafe { &*result.cast::<MockResult>() };
        if result.error.is_some() {
            ptr::null_mut()
        } else {
            ptr::dangling_mut()
        }
    }

    unsafe extern "C" fn copy_error(
        result: *mut raw::SccResult,
        buffer: *mut i8,
        size: usize,
    ) -> usize {
        let result = unsafe { &*result.cast::<MockResult>() };
        let error = result.error.as_ref().expect("must not copy on success");
        let copied = error.len().min(size - 1);
        unsafe {
            ptr::copy_nonoverlapping(error.as_ptr(), buffer.cast(), copied);
            buffer.add(copied).write(0);
        }
        copied
    }

    unsafe extern "C" fn free_result(result: *mut raw::SccResult) {
        let result = unsafe { Box::from_raw(result.cast::<MockResult>()) };
        result.frees.fetch_add(1, Ordering::SeqCst);
    }

    fn finish(error: Option<String>) -> anyhow::Result<()> {
        let frees = Arc::new(AtomicUsize::new(0));
        let result = Box::into_raw(Box::new(MockResult {
            error,
            frees: frees.clone(),
        }))
        .cast();
        let outcome = unsafe { finish_result(result, get_success, copy_error, free_result) };
        assert_eq!(frees.load(Ordering::SeqCst), 1);
        outcome
    }

    #[test]
    fn successful_result_is_freed() {
        finish(None).unwrap();
    }

    #[test]
    fn failure_is_copied_in_full_before_freeing() {
        for error in [
            "context: underlying error".to_owned(),
            "é".repeat(2048),
            "x".repeat(1023),
        ] {
            let outcome = finish(Some(error.clone())).unwrap_err();
            assert_eq!(
                outcome.to_string(),
                format!("REDscript compilation failed: {error}")
            );
        }
    }
}
