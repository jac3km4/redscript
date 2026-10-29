//! C API ownership contracts.
//!
//! Settings are owned handles: [`scc_compile`] consumes and frees them on both success and failure.
//! Setting functions only borrow settings and copy their input strings; callers retain ownership
//! of those strings and may release them after the call.
//!
//! A compilation result is owned by the caller and must be passed to [`scc_free_result`] exactly
//! once. Outputs, source references, and strings obtained from it are read-only borrows, valid
//! only until that result is freed. They must not be freed separately. Source reference functions
//! require a reference from the same output. All C handle arguments must be non-null and live;
//! settings mutations require exclusive access. No handles or borrows may be used after being
//! consumed or freed. See [`scc_copy_error`] for its zero-sized buffer exception.

use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::ptr;

use mimalloc::MiMalloc;
use scc_core::{SccOutput, SccSettings, SourceRef, SourceRefType};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// Creates owned settings to be consumed exactly once by [`scc_compile`].
/// The input string is copied and remains owned by the caller.
///
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

/// Borrows settings exclusively to disable error popups; ownership stays with the caller.
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

/// Runs the compiler, consuming and freeing `settings` on both success and failure.
/// C callers must pass an owned, non-null handle from [`scc_settings_new`] exactly once
/// and must not access it afterward. The returned result is owned by the caller and must
/// be released exactly once with [`scc_free_result`].
#[unsafe(no_mangle)]
pub extern "C" fn scc_compile(settings: Box<SccSettings>) -> Box<SccResult> {
    let res = match scc_core::compile(&settings) {
        Ok(output) => SccResult::Success(Box::new(output)),
        Err(err) => SccResult::Error(err),
    };
    Box::new(res)
}

/// Consumes and frees an owned result returned by [`scc_compile`].
/// C callers must pass a live, non-null result exactly once. All outputs, source references,
/// and string views borrowed from it become invalid and must not be accessed afterward.
#[unsafe(no_mangle)]
pub extern "C" fn scc_free_result(_: Box<SccResult>) {}

/// Borrows the successful output, or returns `None` (NULL in C) on failure.
/// The output must not be modified or freed separately and is valid until [`scc_free_result`].
#[unsafe(no_mangle)]
pub extern "C" fn scc_get_success(output: &SccResult) -> Option<&SccOutput> {
    match output {
        SccResult::Success(success) => Some(success),
        SccResult::Error(_) => None,
    }
}

/// Copies the error message into caller-owned storage without consuming `output`.
/// The copied bytes remain valid independently of the result.
/// Returns the number of bytes copied excluding the null terminator.
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

/// Borrows the source reference at `i`, which must be less than the source reference count.
/// The reference must not be modified or freed and is valid until [`scc_free_result`].
#[unsafe(no_mangle)]
pub extern "C" fn scc_output_get_source_ref(output: &SccOutput, i: usize) -> *const SourceRef {
    &output.refs()[i]
}

/// Returns the source reference count for a live, borrowed output.
#[unsafe(no_mangle)]
pub extern "C" fn scc_output_source_ref_count(output: &SccOutput) -> usize {
    output.refs().len()
}

/// Returns the entity type. `link` must be a live reference borrowed from `output`.
#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_type(_output: &SccOutput, link: &SourceRef) -> SourceRefType {
    link.type_()
}

/// Reports whether the entity is native. Both arguments must be live borrows from the same result.
#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_is_native(_output: &SccOutput, _link: &SourceRef) -> bool {
    true
}

/// Borrows the entity name as UTF-8 bytes without a null terminator.
/// `link` must belong to `output`; the view must not be modified or freed and is valid only
/// until the owning result is passed to [`scc_free_result`].
#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_name<'a>(
    output: &'a SccOutput,
    link: &'a SourceRef,
) -> StrWithLen<'a> {
    output.name(link).unwrap_or_default().into()
}

/// Borrows the parent name as UTF-8 bytes without a null terminator.
/// `link` must belong to `output`; the view must not be modified or freed and is valid only
/// until the owning result is passed to [`scc_free_result`].
#[unsafe(no_mangle)]
pub extern "C" fn scc_source_ref_parent_name<'a>(
    output: &'a SccOutput,
    link: &SourceRef,
) -> StrWithLen<'a> {
    output.parent_name(link).unwrap_or_default().into()
}

/// Borrows the source path as UTF-8 bytes without a null terminator.
/// `link` must belong to `output`; the view must not be modified or freed and is valid only
/// until the owning result is passed to [`scc_free_result`].
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

/// Returns the source line. `link` must be a live reference borrowed from `output`.
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

/// An owned compilation result. C callers must release it with [`scc_free_result`].
pub enum SccResult {
    Success(Box<SccOutput>),
    Error(anyhow::Error),
}

/// A borrowed UTF-8 string view, not null-terminated. Read exactly `len` bytes from `ptr`.
/// An empty view may have a null pointer; do not dereference it when `len` is zero.
/// Views returned by the C API are read-only and valid until [`scc_free_result`] frees their
/// owning result. Neither the pointer nor the view owns an allocation to free.
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
