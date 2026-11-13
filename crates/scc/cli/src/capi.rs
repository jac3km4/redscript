use anyhow::Context;
use minidl::Library;

use crate::raw;

pub(super) fn load() -> anyhow::Result<raw::SccApi> {
    let lib_path = std::env::current_exe().context("Could not get current exe path")?;
    let lib_path = if cfg!(target_os = "windows") {
        lib_path.with_file_name("scc_lib.dll")
    } else if cfg!(target_os = "macos") {
        lib_path.with_file_name("libscc_lib.dylib")
    } else {
        lib_path.with_file_name("libscc_lib.so")
    };
    let lib = Library::load(lib_path).context("could not load the scc shared library")?;

    Ok(unsafe {
        raw::SccApi {
            settings_new: lib.sym("scc_settings_new\0")?,
            settings_set_custom_cache_file: lib.sym("scc_settings_set_custom_cache_file\0")?,
            settings_add_script_path: lib.sym("scc_settings_add_script_path\0")?,
            settings_set_output_cache_file: lib.sym("scc_settings_set_output_cache_file\0")?,
            settings_disable_error_popup: lib.sym("scc_settings_disable_error_popup\0")?,
            settings_register_never_ref_type: lib.sym("scc_settings_register_never_ref_type\0")?,
            settings_register_mixed_ref_type: lib.sym("scc_settings_register_mixed_ref_type\0")?,
            compile: lib.sym("scc_compile\0")?,
            free_result: lib.sym("scc_free_result\0")?,
            get_success: lib.sym("scc_get_success\0")?,
            copy_error: lib.sym("scc_copy_error\0")?,
            output_get_source_ref: lib.sym("scc_output_get_source_ref\0")?,
            output_source_ref_count: lib.sym("scc_output_source_ref_count\0")?,
            source_ref_type: lib.sym("scc_source_ref_type\0")?,
            source_ref_is_native: lib.sym("scc_source_ref_is_native\0")?,
            source_ref_name: lib.sym("scc_source_ref_name\0")?,
            source_ref_parent_name: lib.sym("scc_source_ref_parent_name\0")?,
            source_ref_path: lib.sym("scc_source_ref_path\0")?,
            source_ref_line: lib.sym("scc_source_ref_line\0")?,
        }
    })
}

#[cfg(test)]
mod tests {
    use std::ffi::c_char;
    use std::mem;

    use raw::*;

    use super::*;

    // this test just ensures that the API types haven't changed
    // existing function signatures are not allowed to change due to backwards compatibility
    #[test]
    fn api_functions_are_stable() {
        let api = SccApi {
            settings_new: None,
            settings_set_custom_cache_file: None,
            settings_set_output_cache_file: None,
            settings_add_script_path: None,
            settings_disable_error_popup: None,
            settings_register_never_ref_type: None,
            settings_register_mixed_ref_type: None,
            compile: None,
            free_result: None,
            get_success: None,
            copy_error: None,
            output_get_source_ref: None,
            output_source_ref_count: None,
            source_ref_type: None,
            source_ref_is_native: None,
            source_ref_name: None,
            source_ref_parent_name: None,
            source_ref_path: None,
            source_ref_line: None,
        };

        let _settings_new: Option<unsafe extern "C" fn(*const c_char) -> *mut SccSettings> =
            api.settings_new;
        let _settings_set_custom_cache_file: Option<
            unsafe extern "C" fn(*mut SccSettings, *const c_char),
        > = api.settings_set_custom_cache_file;
        let _settings_set_output_cache_file: Option<
            unsafe extern "C" fn(*mut SccSettings, *const c_char),
        > = api.settings_set_output_cache_file;
        let _settings_add_script_path: Option<
            unsafe extern "C" fn(*mut SccSettings, *const c_char),
        > = api.settings_add_script_path;
        let _settings_disable_error_popup: Option<unsafe extern "C" fn(*mut SccSettings)> =
            api.settings_disable_error_popup;
        let _settings_register_never_ref_type: Option<
            unsafe extern "C" fn(*mut SccSettings, *const c_char),
        > = api.settings_register_never_ref_type;
        let _settings_register_mixed_ref_type: Option<
            unsafe extern "C" fn(*mut SccSettings, *const c_char),
        > = api.settings_register_mixed_ref_type;
        let _compile: Option<unsafe extern "C" fn(*mut SccSettings) -> *mut SccResult> =
            api.compile;
        let _free_result: Option<unsafe extern "C" fn(*mut SccResult)> = api.free_result;
        let _get_success: Option<unsafe extern "C" fn(*mut SccResult) -> *mut SccOutput> =
            api.get_success;
        let _copy_error: Option<unsafe extern "C" fn(*mut SccResult, *mut c_char, usize) -> usize> =
            api.copy_error;
        let _output_get_source_ref: Option<
            unsafe extern "C" fn(*mut SccOutput, usize) -> *mut SccSourceRef,
        > = api.output_get_source_ref;
        let _output_source_ref_count: Option<unsafe extern "C" fn(*mut SccOutput) -> usize> =
            api.output_source_ref_count;
        let _source_ref_type: Option<
            unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> u8,
        > = api.source_ref_type;
        let _source_ref_is_native: Option<
            unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> bool,
        > = api.source_ref_is_native;
        let _source_ref_name: Option<
            unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> StrWithLen,
        > = api.source_ref_name;
        let _source_ref_parent_name: Option<
            unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> StrWithLen,
        > = api.source_ref_parent_name;
        let _source_ref_path: Option<
            unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> StrWithLen,
        > = api.source_ref_path;
        let _source_ref_line: Option<
            unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> usize,
        > = api.source_ref_line;
    }

    #[test]
    fn api_types_are_stable() {
        assert_eq!(mem::size_of::<StrWithLen>(), 16);
        assert_eq!(mem::offset_of!(StrWithLen, str_), 0);
        assert_eq!(mem::offset_of!(StrWithLen, len), 8);

        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_UNDEFINED, 0);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_CLASS, 1);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_FIELD, 2);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_FUNCTION, 3);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_ENUM, 4);
    }
}
