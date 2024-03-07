use std::ffi::CString;
use std::{fs, mem};

use assert_fs::prelude::*;
use minidl::*;
use offset::offset_of;
use scc_lib_tests::*;

#[test]
fn api_functions_are_stable() {
    // this test just ensures that the API types haven't changed
    // existing function signatures are not allowed to change due to backwards compatibility

    let SccApi {
        settings_new,
        settings_set_custom_cache_file,
        settings_set_output_cache_file,
        settings_add_script_path,
        compile,
        free_result,
        get_success,
        copy_error,
        output_get_source_ref,
        output_source_ref_count,
        source_ref_type,
        source_ref_is_native,
        source_ref_name,
        source_ref_parent_name,
        source_ref_path,
        source_ref_line,
    } = load_api();

    let _settings_new: unsafe extern "C" fn(*const i8) -> *mut SccSettings = settings_new.unwrap();
    let _settings_set_custom_cache_file: unsafe extern "C" fn(*mut SccSettings, *const i8) =
        settings_set_custom_cache_file.unwrap();
    let _settings_set_output_cache_file: unsafe extern "C" fn(*mut SccSettings, *const i8) =
        settings_set_output_cache_file.unwrap();
    let _settings_add_script_path: unsafe extern "C" fn(*mut SccSettings, *const i8) =
        settings_add_script_path.unwrap();
    let _compile: unsafe extern "C" fn(*mut SccSettings) -> *mut SccResult = compile.unwrap();
    let _free_result: unsafe extern "C" fn(*mut SccResult) = free_result.unwrap();
    let _get_success: unsafe extern "C" fn(*mut SccResult) -> *mut SccOutput = get_success.unwrap();
    let _copy_error: unsafe extern "C" fn(*mut SccResult, *mut i8, usize) -> usize = copy_error.unwrap();
    let _output_get_source_ref: unsafe extern "C" fn(*mut SccOutput, usize) -> *mut SccSourceRef =
        output_get_source_ref.unwrap();
    let _output_source_ref_count: unsafe extern "C" fn(*mut SccOutput) -> usize = output_source_ref_count.unwrap();
    let _source_ref_type: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> u8 = source_ref_type.unwrap();
    let _source_ref_is_native: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> bool =
        source_ref_is_native.unwrap();
    let _source_ref_name: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> StrWithLen =
        source_ref_name.unwrap();
    let _source_ref_parent_name: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> StrWithLen =
        source_ref_parent_name.unwrap();
    let _source_ref_path: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> StrWithLen =
        source_ref_path.unwrap();
    let _source_ref_line: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> usize = source_ref_line.unwrap();
}

#[test]
fn api_types_are_stable() {
    assert_eq!(mem::size_of::<StrWithLen>(), 16);
    assert_eq!(offset_of!(StrWithLen::str_).as_u32(), 0);
    assert_eq!(offset_of!(StrWithLen::len).as_u32(), 8);

    assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_UNDEFINED, 0);
    assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_CLASS, 1);
    assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_FIELD, 2);
    assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_FUNCTION, 3);
    assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_ENUM, 4);
}

#[test]
fn compiles_successfully() {
    let temp = assert_fs::TempDir::new().unwrap();
    let bundle_path = temp.child("final.redscripts");
    let script_path = temp.child("test.reds");

    fs::copy("../../resources/predef.redscripts", &bundle_path)
        .expect("Could not copy predef.redscripts to bundle path");

    script_path
        .write_str("native class TestClass { func Dummy() {} }")
        .unwrap();

    let api = load_api();
    unsafe {
        let r6_dir_cstr = CString::new(temp.path().to_string_lossy().as_bytes()).unwrap();
        let script_path_cstr = CString::new(script_path.path().to_string_lossy().as_bytes()).unwrap();
        let bundle_path_cstr = CString::new(bundle_path.path().to_string_lossy().as_bytes()).unwrap();

        let settings = (api.settings_new.unwrap())(r6_dir_cstr.as_ptr() as _);
        (api.settings_add_script_path.unwrap())(settings, script_path_cstr.as_ptr() as _);
        (api.settings_set_custom_cache_file.unwrap())(settings, bundle_path_cstr.as_ptr() as _);
        (api.settings_set_output_cache_file.unwrap())(settings, bundle_path_cstr.as_ptr() as _);

        let result = (api.compile.unwrap())(settings);
        let output = (api.get_success.unwrap())(result);
        assert!(!output.is_null());

        let count = (api.output_source_ref_count.unwrap())(output);
        assert_eq!(count, 2);

        let source_ref = (api.output_get_source_ref.unwrap())(output, 0);
        assert!(!source_ref.is_null());
        let type_ = (api.source_ref_type.unwrap())(output, source_ref);
        assert_eq!(type_, 3);
        let name = (api.source_ref_name.unwrap())(output, source_ref);
        assert_eq!(
            std::str::from_utf8(std::slice::from_raw_parts(name.str_ as _, name.len)).unwrap(),
            "Dummy;"
        );
        let parent_name = (api.source_ref_parent_name.unwrap())(output, source_ref);
        assert_eq!(
            std::str::from_utf8(std::slice::from_raw_parts(parent_name.str_ as _, parent_name.len)).unwrap(),
            "TestClass"
        );
        let path = (api.source_ref_path.unwrap())(output, source_ref);
        assert_eq!(
            std::str::from_utf8(std::slice::from_raw_parts(path.str_ as _, path.len)).unwrap(),
            script_path.path().to_string_lossy()
        );
        let line = (api.source_ref_line.unwrap())(output, source_ref);
        assert_eq!(line, 0);
        let is_native = (api.source_ref_is_native.unwrap())(output, source_ref);
        assert!(!is_native);

        let source_ref = (api.output_get_source_ref.unwrap())(output, 1);
        assert!(!source_ref.is_null());
        let type_ = (api.source_ref_type.unwrap())(output, source_ref);
        assert_eq!(type_, 1);
        let name = (api.source_ref_name.unwrap())(output, source_ref);
        assert_eq!(
            std::str::from_utf8(std::slice::from_raw_parts(name.str_ as _, name.len)).unwrap(),
            "TestClass"
        );
        let parent_name = (api.source_ref_parent_name.unwrap())(output, source_ref);
        assert_eq!(
            std::str::from_utf8(std::slice::from_raw_parts(parent_name.str_ as _, parent_name.len)).unwrap(),
            ""
        );
        let path = (api.source_ref_path.unwrap())(output, source_ref);
        assert_eq!(
            std::str::from_utf8(std::slice::from_raw_parts(path.str_ as _, path.len)).unwrap(),
            script_path.path().to_string_lossy()
        );
        let line = (api.source_ref_line.unwrap())(output, source_ref);
        assert_eq!(line, 0);
        let is_native = (api.source_ref_is_native.unwrap())(output, source_ref);
        assert!(is_native);

        api.free_result.unwrap()(result);
    }
    temp.close().unwrap();
}

#[test]
fn receives_an_error() {
    let temp = assert_fs::TempDir::new().unwrap();
    let bundle_path = temp.child("final.redscripts");
    let script_path = temp.child("test.reds");

    fs::copy("../../resources/predef.redscripts", &bundle_path)
        .expect("Could not copy predef.redscripts to bundle path");

    script_path.write_str("func Dummy()").unwrap();

    let api = load_api();
    unsafe {
        let r6_dir_cstr = CString::new(temp.path().to_string_lossy().as_bytes()).unwrap();
        let script_path_cstr = CString::new(script_path.path().to_string_lossy().as_bytes()).unwrap();
        let bundle_path_cstr = CString::new(bundle_path.path().to_string_lossy().as_bytes()).unwrap();

        let settings = (api.settings_new.unwrap())(r6_dir_cstr.as_ptr() as _);
        (api.settings_add_script_path.unwrap())(settings, script_path_cstr.as_ptr() as _);
        (api.settings_set_custom_cache_file.unwrap())(settings, bundle_path_cstr.as_ptr() as _);

        let result = (api.compile.unwrap())(settings);
        let output = (api.get_success.unwrap())(result);
        assert!(output.is_null());

        let mut error = [0u8; 1024];
        let error_len = (api.copy_error.unwrap())(result, error.as_mut_ptr() as _, error.len());
        let error = std::str::from_utf8(&error[..error_len]).unwrap();
        assert_eq!(
            error,
            "REDScript compilation has failed.\n\
            This error has been caused by mods listed below:\n\
            - test.reds\n\
            \n\
            You should check if these mods are outdated and update them if possible. \
            They may also be incompatible with the current version of the game, in which case you \
            should remove them and try again.\n"
        );

        api.free_result.unwrap()(result);
    }
    temp.close().unwrap();
}

fn load_api() -> SccApi {
    let lib = Library::load("scc_lib.dll").unwrap();
    unsafe {
        SccApi {
            settings_new: lib.sym("scc_settings_new\0").unwrap(),
            settings_set_custom_cache_file: lib.sym("scc_settings_set_custom_cache_file\0").unwrap(),
            settings_add_script_path: lib.sym("scc_settings_add_script_path\0").unwrap(),
            settings_set_output_cache_file: lib.sym("scc_settings_set_output_cache_file\0").unwrap(),
            compile: lib.sym("scc_compile\0").unwrap(),
            free_result: lib.sym("scc_free_result\0").unwrap(),
            get_success: lib.sym("scc_get_success\0").unwrap(),
            copy_error: lib.sym("scc_copy_error\0").unwrap(),
            output_get_source_ref: lib.sym("scc_output_get_source_ref\0").unwrap(),
            output_source_ref_count: lib.sym("scc_output_source_ref_count\0").unwrap(),
            source_ref_type: lib.sym("scc_source_ref_type\0").unwrap(),
            source_ref_is_native: lib.sym("scc_source_ref_is_native\0").unwrap(),
            source_ref_name: lib.sym("scc_source_ref_name\0").unwrap(),
            source_ref_parent_name: lib.sym("scc_source_ref_parent_name\0").unwrap(),
            source_ref_path: lib.sym("scc_source_ref_path\0").unwrap(),
            source_ref_line: lib.sym("scc_source_ref_line\0").unwrap(),
        }
    }
}
