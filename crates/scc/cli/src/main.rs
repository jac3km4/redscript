use std::env;
use std::ffi::{CString, c_char};
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
                .scripts_dirs
                .first()
                .and_then(|dir| dir.parent())
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
    run_with_api(r6_dir, args, &capi::load()?)
}

/// Runs the CLI against a loaded API. Kept separate from [`run`] so tests can
/// substitute a mock API and observe settings/result ownership.
fn run_with_api(r6_dir: &Path, args: Option<Arguments>, api: &raw::SccApi) -> anyhow::Result<()> {
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

    // Resolve every fallible input before creating `settings`. Once it exists,
    // `compile` consumes it and an early `?` would leak it.
    let mut custom_cache_file: Option<CString> = None;
    let mut additional_script_dirs: Vec<CString> = Vec::new();
    let mut script_paths: Vec<CString> = Vec::new();
    if let Some(args) = args {
        if let Some(cache_file) = args.cache_file {
            custom_cache_file = Some(c_path(&cache_file)?);
        }

        for script_dir in args.scripts_dirs.into_iter().skip(1) {
            additional_script_dirs.push(c_path(&script_dir)?);
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
                script_paths.push(c_path(&script_path)?);
            }
        }
    }

    unsafe {
        let settings = settings_new(root.as_ptr());

        if let Some(custom_cache_file) = &custom_cache_file {
            set_custom_cache_file(settings, custom_cache_file.as_ptr());
        }
        for script_dir in &additional_script_dirs {
            add_script_path(settings, script_dir.as_ptr());
        }
        for script_path in &script_paths {
            add_script_path(settings, script_path.as_ptr());
        }

        let res = compile(settings);
        finish_result(res, get_success, copy_error, free_result)
    }
}

unsafe fn finish_result(
    result: *mut raw::SccResult,
    get_success: unsafe extern "C" fn(*mut raw::SccResult) -> *mut raw::SccOutput,
    copy_error: unsafe extern "C" fn(*mut raw::SccResult, *mut c_char, usize) -> usize,
    free_result: unsafe extern "C" fn(*mut raw::SccResult),
) -> anyhow::Result<()> {
    let outcome = if unsafe { get_success(result) }.is_null() {
        let mut buffer = vec![0u8; 1024];
        let copied = loop {
            let copied = unsafe { copy_error(result, buffer.as_mut_ptr().cast(), buffer.len()) };
            // A full buffer may be truncated: copy_error returns bytes copied, not
            // required size. `scc_copy_error` backs up to a character boundary, so
            // a truncated message can report up to 3 bytes below the buffer length.
            if copied.saturating_add(4) < buffer.len() {
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
        buffer: *mut c_char,
        size: usize,
    ) -> usize {
        let result = unsafe { &*result.cast::<MockResult>() };
        let error = result.error.as_ref().expect("must not copy on success");
        if size == 0 {
            return 0;
        }
        // Mirror `scc_copy_error`: truncate to `size - 1` bytes but never split a
        // multi-byte character, so a truncated message can fall short of the
        // buffer length by up to 3 bytes.
        let mut copied = error.len().min(size - 1);
        while !error.is_char_boundary(copied) {
            copied -= 1;
        }
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
            "😀".repeat(2048),
            "x".repeat(1023),
        ] {
            let outcome = finish(Some(error.clone())).unwrap_err();
            assert_eq!(
                outcome.to_string(),
                format!("REDscript compilation failed: {error}")
            );
        }
    }

    // ---- Mock API for `run_with_api` ownership tests ------------------------

    use std::cell::Cell;
    use std::thread::LocalKey;

    // Each test runs on its own thread, so these counters are isolated per test.
    thread_local! {
        static SETTINGS_CREATED: Cell<usize> = const { Cell::new(0) };
        static SETTINGS_CONSUMED: Cell<usize> = const { Cell::new(0) };
        static RESULTS_FREED: Cell<usize> = const { Cell::new(0) };
        static COMPILE_FAILS: Cell<bool> = const { Cell::new(false) };
    }

    fn counter(key: &'static LocalKey<Cell<usize>>) -> usize {
        key.with(Cell::get)
    }

    fn args() -> Arguments {
        Arguments {
            scripts_dirs: vec![PathBuf::from("/r6/scripts")],
            optimize: false,
            threads: 1,
            warnings: Vec::new(),
            no_testonly: false,
            no_breakpoint: false,
            profile: false,
            script_paths_file: None,
            cache_file: None,
            no_exec: false,
            no_debug: false,
        }
    }

    unsafe extern "C" fn api_settings_new(_r6_dir: *const c_char) -> *mut raw::SccSettings {
        SETTINGS_CREATED.with(|count| count.set(count.get() + 1));
        ptr::dangling_mut()
    }

    unsafe extern "C" fn api_set_custom_cache_file(
        _settings: *mut raw::SccSettings,
        _path: *const c_char,
    ) {
    }

    unsafe extern "C" fn api_add_script_path(
        _settings: *mut raw::SccSettings,
        _path: *const c_char,
    ) {
    }

    unsafe extern "C" fn api_compile(_settings: *mut raw::SccSettings) -> *mut raw::SccResult {
        SETTINGS_CONSUMED.with(|count| count.set(count.get() + 1));
        ptr::dangling_mut()
    }

    unsafe extern "C" fn api_get_success(_result: *mut raw::SccResult) -> *mut raw::SccOutput {
        if COMPILE_FAILS.with(Cell::get) {
            ptr::null_mut()
        } else {
            ptr::dangling_mut()
        }
    }

    unsafe extern "C" fn api_copy_error(
        _result: *mut raw::SccResult,
        buffer: *mut c_char,
        size: usize,
    ) -> usize {
        if size == 0 {
            return 0;
        }
        let message = b"mock failure";
        let copied = message.len().min(size - 1);
        unsafe {
            ptr::copy_nonoverlapping(message.as_ptr(), buffer.cast(), copied);
            buffer.add(copied).write(0);
        }
        copied
    }

    unsafe extern "C" fn api_free_result(_result: *mut raw::SccResult) {
        RESULTS_FREED.with(|count| count.set(count.get() + 1));
    }

    fn mock_api() -> raw::SccApi {
        raw::SccApi {
            settings_new: Some(api_settings_new),
            settings_set_custom_cache_file: Some(api_set_custom_cache_file),
            settings_set_output_cache_file: None,
            settings_add_script_path: Some(api_add_script_path),
            settings_disable_error_popup: None,
            settings_register_never_ref_type: None,
            settings_register_mixed_ref_type: None,
            compile: Some(api_compile),
            free_result: Some(api_free_result),
            get_success: Some(api_get_success),
            copy_error: Some(api_copy_error),
            output_get_source_ref: None,
            output_source_ref_count: None,
            source_ref_type: None,
            source_ref_is_native: None,
            source_ref_name: None,
            source_ref_parent_name: None,
            source_ref_path: None,
            source_ref_line: None,
        }
    }

    #[test]
    fn early_input_failure_does_not_create_settings() {
        let mut options = args();
        options.script_paths_file = Some(PathBuf::from("/definitely/missing/script_paths.txt"));

        let error = run_with_api(Path::new("/r6"), Some(options), &mock_api()).unwrap_err();

        assert!(
            error
                .to_string()
                .contains("failed to open script paths file"),
            "{error}"
        );
        assert_eq!(counter(&SETTINGS_CREATED), 0);
        assert_eq!(counter(&RESULTS_FREED), 0);
    }

    #[test]
    fn interior_nul_cache_file_is_rejected_before_settings() {
        let mut options = args();
        options.cache_file = Some(PathBuf::from("bad\0cache"));

        let error = run_with_api(Path::new("/r6"), Some(options), &mock_api()).unwrap_err();

        assert!(!error.to_string().is_empty());
        assert_eq!(counter(&SETTINGS_CREATED), 0);
        assert_eq!(counter(&RESULTS_FREED), 0);
    }

    #[test]
    fn successful_run_balances_settings_and_result() {
        let script_paths_file =
            std::env::temp_dir().join(format!("scc-script-paths-{}.txt", std::process::id()));
        std::fs::write(
            &script_paths_file,
            "/r6/scripts/a.reds\n/r6/scripts/b.reds\n",
        )
        .unwrap();

        let mut options = args();
        options.scripts_dirs = vec![
            PathBuf::from("/r6/scripts"),
            PathBuf::from("/r6/scripts-extra"),
        ];
        options.cache_file = Some(PathBuf::from("/r6/cache"));
        options.script_paths_file = Some(script_paths_file.clone());

        let outcome = run_with_api(Path::new("/r6"), Some(options), &mock_api());
        std::fs::remove_file(&script_paths_file).ok();

        outcome.unwrap();
        assert_eq!(counter(&SETTINGS_CREATED), 1);
        assert_eq!(counter(&SETTINGS_CONSUMED), 1);
        assert_eq!(counter(&RESULTS_FREED), 1);
    }

    #[test]
    fn failed_compile_frees_settings_and_result() {
        COMPILE_FAILS.with(|flag| flag.set(true));

        let error = run_with_api(Path::new("/r6"), Some(args()), &mock_api()).unwrap_err();

        COMPILE_FAILS.with(|flag| flag.set(false));
        assert!(error.to_string().contains("mock failure"), "{error}");
        assert_eq!(counter(&SETTINGS_CREATED), 1);
        assert_eq!(counter(&SETTINGS_CONSUMED), 1);
        assert_eq!(counter(&RESULTS_FREED), 1);
    }
}
