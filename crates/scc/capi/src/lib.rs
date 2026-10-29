use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::ptr;

use mimalloc::MiMalloc;
use scc_core::{SccOutput, SccSettings, SourceRef, SourceRefType};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// # Safety
/// The caller must ensure that `r6_dir` is a valid null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scc_settings_new(r6_dir: *const i8) -> Box<SccSettings> {
    let r6_dir = unsafe { c_path(r6_dir) };
    let root_dir = r6_dir.parent().map(Path::to_owned).unwrap_or(r6_dir);
    Box::new(SccSettings::new(root_dir))
}

/// # Safety
/// The caller must ensure that `settings` is a valid pointer to a `SccSettings` struct and
/// `path` is a valid null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scc_settings_set_custom_cache_file(
    settings: &mut SccSettings,
    path: *const i8,
) {
    settings.set_custom_cache_file(unsafe { c_path(path) });
}

/// # Safety
/// The caller must ensure that `settings` is a valid pointer to a `SccSettings` struct and
/// `path` is a valid null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scc_settings_set_output_cache_file(
    settings: &mut SccSettings,
    path: *const i8,
) {
    settings.set_output_cache_file(unsafe { c_path(path) });
}

/// # Safety
/// The caller must ensure that `settings` is a valid pointer to a `SccSettings` struct and
/// `path` is a valid null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scc_settings_add_script_path(settings: &mut SccSettings, path: *const i8) {
    settings.add_script_path(unsafe { c_path(path) });
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_settings_disable_error_popup(settings: &mut SccSettings) {
    settings.set_show_error_report(false);
}

/// # Safety
/// The caller must ensure that `settings` is a valid pointer to a `SccSettings` struct and
/// `name` is a valid null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scc_settings_register_never_ref_type(
    settings: &mut SccSettings,
    name: *const i8,
) {
    settings.register_never_ref_type(
        unsafe { c_str(name) }
            .into_string()
            .expect("name should be valid UTF-8"),
    );
}

/// # Safety
/// The caller must ensure that `settings` is a valid pointer to a `SccSettings` struct and
/// `name` is a valid null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scc_settings_register_mixed_ref_type(
    settings: &mut SccSettings,
    name: *const i8,
) {
    settings.register_mixed_ref_type(
        unsafe { c_str(name) }
            .into_string()
            .expect("name should be valid UTF-8"),
    );
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_compile(settings: Box<SccSettings>) -> Box<SccResult> {
    let res = match scc_core::compile(&settings) {
        Ok(output) => SccResult::Success(Box::new(output)),
        Err(err) => SccResult::Error(err),
    };
    Box::new(res)
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_free_result(_: Box<SccResult>) {}

#[unsafe(no_mangle)]
pub extern "C" fn scc_get_success(output: &SccResult) -> Option<&SccOutput> {
    match output {
        SccResult::Success(success) => Some(success),
        SccResult::Error(_) => None,
    }
}

/// Copies the error message, returning the number of bytes copied excluding the null terminator.
/// On error, a nonempty buffer is null-terminated and the message is truncated to fit.
/// Returns zero without touching the buffer on success or when `buffer_size` is zero.
///
/// # Safety
/// If `buffer_size` is nonzero, `buffer` must be writable for at least `buffer_size` bytes.
/// If `buffer_size` is zero, `buffer` may be null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scc_copy_error(
    output: &SccResult,
    buffer: *mut u8,
    buffer_size: usize,
) -> usize {
    if buffer_size == 0 {
        return 0;
    }
    match output {
        SccResult::Success(_) => 0,
        SccResult::Error(error) => {
            let error = error.to_string();
            let max_len = error.len().min(buffer_size - 1);
            unsafe {
                ptr::copy_nonoverlapping(error.as_ptr(), buffer, max_len);
                buffer.add(max_len).write(0);
            }
            max_len
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_output_get_source_ref(output: &SccOutput, i: usize) -> *const SourceRef {
    &output.refs()[i]
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_output_source_ref_count(output: &SccOutput) -> usize {
    output.refs().len()
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_type(_output: &SccOutput, link: &SourceRef) -> SourceRefType {
    link.type_()
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_is_native(_output: &SccOutput, _link: &SourceRef) -> bool {
    true
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_name<'a>(
    output: &'a SccOutput,
    link: &'a SourceRef,
) -> StrWithLen<'a> {
    output.name(link).unwrap_or_default().into()
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_parent_name<'a>(
    output: &'a SccOutput,
    link: &SourceRef,
) -> StrWithLen<'a> {
    output.parent_name(link).unwrap_or_default().into()
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_path<'a>(
    output: &'a SccOutput,
    link: &SourceRef,
) -> StrWithLen<'a> {
    output
        .path(link)
        .and_then(|path| path.to_str())
        .unwrap_or_default()
        .into()
}

#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_line(output: &SccOutput, link: &SourceRef) -> usize {
    output.line(link).unwrap_or(0)
}

unsafe fn c_str(str: *const i8) -> CString {
    unsafe { CStr::from_ptr(str).to_owned() }
}

unsafe fn c_path(r6_dir: *const i8) -> PathBuf {
    let cstr = unsafe { CStr::from_ptr(r6_dir) };
    PathBuf::from(cstr.to_string_lossy().as_ref())
}

pub enum SccResult {
    Success(Box<SccOutput>),
    Error(anyhow::Error),
}

#[derive(Debug)]
#[repr(C)]
pub struct StrWithLen<'a> {
    ptr: *const u8,
    len: usize,
    _marker: PhantomData<&'a str>,
}

impl<'a> From<&'a str> for StrWithLen<'a> {
    fn from(value: &str) -> Self {
        Self {
            ptr: value.as_ptr(),
            len: value.len(),
            _marker: PhantomData,
        }
    }
}

impl Default for StrWithLen<'_> {
    fn default() -> Self {
        Self {
            ptr: std::ptr::null(),
            len: 0,
            _marker: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copy_error_accepts_null_zero_sized_buffer() {
        let result = SccResult::Error(anyhow::anyhow!("test error"));
        assert_eq!(unsafe { scc_copy_error(&result, ptr::null_mut(), 0) }, 0);
    }

    #[test]
    fn copy_error_leaves_zero_sized_buffer_untouched() {
        let result = SccResult::Error(anyhow::anyhow!("test error"));
        let mut buffer = [0xff];
        assert_eq!(
            unsafe { scc_copy_error(&result, buffer.as_mut_ptr(), 0) },
            0
        );
        assert_eq!(buffer, [0xff]);
    }

    #[test]
    fn copy_error_terminates_one_byte_buffer() {
        let result = SccResult::Error(anyhow::anyhow!("test error"));
        let mut buffer = [0xff; 2];
        assert_eq!(
            unsafe { scc_copy_error(&result, buffer.as_mut_ptr(), 1) },
            0
        );
        assert_eq!(buffer, [0, 0xff]);
    }

    #[test]
    fn copy_error_copies_message_and_terminator() {
        let result = SccResult::Error(anyhow::anyhow!("test error"));
        let mut buffer = [0xff; 12];
        let copied = unsafe { scc_copy_error(&result, buffer.as_mut_ptr(), buffer.len()) };
        assert_eq!(copied, 10);
        assert_eq!(&buffer[..11], b"test error\0");
        assert_eq!(buffer[11], 0xff);
    }

    #[test]
    fn copy_error_truncates_and_terminates_message() {
        let result = SccResult::Error(anyhow::anyhow!("test error"));
        let mut buffer = [0xff; 6];
        let copied = unsafe { scc_copy_error(&result, buffer.as_mut_ptr(), 5) };
        assert_eq!(copied, 4);
        assert_eq!(&buffer[..5], b"test\0");
        assert_eq!(buffer[5], 0xff);
    }

    #[test]
    fn str_with_len_has_correct_layout() {
        let mut data = [0u8; 16];
        unsafe {
            data.as_mut_ptr()
                .cast::<*const u8>()
                .write(b"test".as_ptr());
            data.as_mut_ptr().add(8).cast::<usize>().write(4);
            let str = data.as_ptr().cast::<StrWithLen<'_>>().read();
            assert_eq!(std::slice::from_raw_parts(str.ptr, str.len), b"test");
        }
    }

    #[test]
    fn source_ref_type_is_stable() {
        assert_eq!(SourceRefType::Undefined as u8, 0);
        assert_eq!(SourceRefType::Class as u8, 1);
        assert_eq!(SourceRefType::Field as u8, 2);
        assert_eq!(SourceRefType::Function as u8, 3);
        assert_eq!(SourceRefType::Enum as u8, 4);
    }
}
