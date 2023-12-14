use std::ffi::CStr;
use std::marker::PhantomData;
use std::path::PathBuf;

use redscript::bundle::ScriptBundle;
use redscript::definition::AnyDefinition;
use redscript_compiler::source_map::Files;
use redscript_compiler::unit::{CompilationOutput, SourceRef};

use crate::compile;

/// # Safety
/// The caller must ensure that `r6_dir` is a valid null-terminated UTF-8 string.
#[no_mangle]
pub unsafe extern "C" fn scc_settings_new(r6_dir: *const i8) -> Box<SccSettings> {
    Box::new(SccSettings {
        r6_dir: PathBuf::from(CStr::from_ptr(r6_dir).to_string_lossy().as_ref()),
        custom_cache_file: None,
        additional_script_paths: Vec::new(),
    })
}

/// # Safety
/// The caller must ensure that `settings` is a valid pointer to a `SccSettings` struct and
/// `path` is a valid null-terminated UTF-8 string.
#[no_mangle]
pub unsafe extern "C" fn scc_settings_set_custom_cache_file(settings: &mut SccSettings, path: *const i8) {
    settings.custom_cache_file = Some(PathBuf::from(CStr::from_ptr(path).to_string_lossy().as_ref()));
}

/// # Safety
/// The caller must ensure that `settings` is a valid pointer to a `SccSettings` struct and
/// `path` is a valid null-terminated UTF-8 string.
#[no_mangle]
pub unsafe extern "C" fn scc_settings_add_script_path(settings: &mut SccSettings, path: *const i8) {
    settings
        .additional_script_paths
        .push(PathBuf::from(CStr::from_ptr(path).to_string_lossy().as_ref()));
}

#[no_mangle]
pub extern "C" fn scc_compile(settings: Box<SccSettings>) -> Box<SccResult> {
    compile(&settings)
}

#[no_mangle]
pub extern "C" fn scc_free_result(_: Box<SccResult>) {}

#[no_mangle]
pub extern "C" fn scc_get_success(output: &SccResult) -> Option<&SccOutput> {
    match output {
        SccResult::Success(success) => Some(success),
        SccResult::Error(_) => None,
    }
}

#[no_mangle]
pub extern "C" fn scc_output_get_source_ref(output: &SccOutput, i: usize) -> *const SourceRef {
    &output.compilation.source_refs()[i] as *const SourceRef
}

#[no_mangle]
pub extern "C" fn scc_output_source_ref_count(output: &SccOutput) -> usize {
    output.compilation.source_refs().len()
}

#[no_mangle]
pub extern "C" fn scc_source_ref_type(output: &SccOutput, link: &SourceRef) -> SourceRefType {
    let Ok(def) = output.bundle.pool.definition(link.index()) else {
        return SourceRefType::Undefined;
    };
    match def.value {
        AnyDefinition::Class(_) => SourceRefType::Class,
        AnyDefinition::Enum(_) => SourceRefType::Enum,
        AnyDefinition::Function(_) => SourceRefType::Function,
        AnyDefinition::Field(_) => SourceRefType::Field,
        _ => SourceRefType::Undefined,
    }
}

#[no_mangle]
pub extern "C" fn scc_source_ref_is_native(output: &SccOutput, link: &SourceRef) -> bool {
    let Ok(def) = output.bundle.pool.definition(link.index()) else {
        return false;
    };
    match &def.value {
        AnyDefinition::Function(f) => f.flags.is_native(),
        AnyDefinition::Class(c) => c.flags.is_native(),
        AnyDefinition::Field(f) => f.flags.is_native(),
        _ => false,
    }
}

#[no_mangle]
pub extern "C" fn scc_source_ref_name<'a>(output: &'a SccOutput, link: &SourceRef) -> StrWithLen<'a> {
    let Ok(name) = output.bundle.pool.def_name(link.index()) else {
        return StrWithLen::default();
    };
    name.as_ref().into()
}

#[no_mangle]
pub extern "C" fn scc_source_ref_parent_name<'a>(output: &'a SccOutput, link: &SourceRef) -> StrWithLen<'a> {
    let Ok(def) = output.bundle.pool.definition(link.index()) else {
        return StrWithLen::default();
    };
    if def.parent.is_undefined() {
        return StrWithLen::default();
    }
    let Ok(name) = output.bundle.pool.def_name(def.parent) else {
        return StrWithLen::default();
    };
    name.as_ref().into()
}

#[no_mangle]
pub extern "C" fn scc_source_ref_path<'a>(output: &'a SccOutput, link: &SourceRef) -> StrWithLen<'a> {
    let Some(file) = output.files.lookup_file(link.pos()) else {
        return StrWithLen::default();
    };
    file.path().to_string_lossy().as_ref().into()
}

#[no_mangle]
pub extern "C" fn scc_source_ref_line(output: &SccOutput, link: &SourceRef) -> usize {
    let Some(pos) = output.files.lookup_file(link.pos()).and_then(|f| f.lookup(link.pos())) else {
        return usize::MAX;
    };
    pos.line
}

#[derive(Debug)]
pub struct SccSettings {
    pub r6_dir: PathBuf,
    pub custom_cache_file: Option<PathBuf>,
    pub additional_script_paths: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct SccOutput {
    compilation: CompilationOutput,
    bundle: ScriptBundle,
    files: Files,
}

impl SccOutput {
    pub fn new(compilation: CompilationOutput, bundle: ScriptBundle, files: Files) -> Self {
        Self {
            compilation,
            bundle,
            files,
        }
    }
}

#[derive(Debug)]
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

impl From<&str> for StrWithLen<'_> {
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

#[repr(u8)]
pub enum SourceRefType {
    Undefined = 0,
    Class = 1,
    Field = 2,
    Function = 3,
    Enum = 4,
}
