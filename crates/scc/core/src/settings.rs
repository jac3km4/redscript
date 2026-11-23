use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::{io, iter};

use file_id::get_file_id;
use redscript_compiler_api::TypeFlagRegistry;

const BUNDLE_FILE: &str = "final.redscripts";
const BACKUP_BUNDLE_FILE: &str = "final.redscripts.bk";

const R6_DIR: &str = "r6";
const CACHE_DIR: &str = "cache";
const SCRIPTS_DIR: &str = "scripts";
const CONFIG_DIR: &str = "config";
const USER_HINTS_DIR: &str = "redsUserHints";

pub(crate) const CACHE_FILE_EXT: &str = "redscripts";
pub(crate) const BACKUP_FILE_EXT: &str = "redscripts.bk";
pub(crate) const TIMESTAMP_FILE_EXT: &str = "redscripts.ts";

#[derive(Debug)]
#[repr(C)]
pub struct SccSettings {
    root_dir: PathBuf,
    custom_cache_file: Option<PathBuf>,
    output_cache_file: Option<PathBuf>,
    additional_script_paths: Vec<PathBuf>,
    show_error_report: bool,
    type_flags: TypeFlagRegistry,
}

impl SccSettings {
    pub fn new(root_dir: PathBuf) -> Self {
        Self {
            root_dir,
            custom_cache_file: None,
            output_cache_file: None,
            additional_script_paths: vec![],
            show_error_report: true,
            type_flags: TypeFlagRegistry::default(),
        }
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub fn user_hints_dir(&self) -> PathBuf {
        self.root_dir
            .join(R6_DIR)
            .join(CONFIG_DIR)
            .join(USER_HINTS_DIR)
    }

    fn default_cache_file_path(&self) -> PathBuf {
        self.root_dir.join(R6_DIR).join(CACHE_DIR).join(BUNDLE_FILE)
    }

    fn default_scripts_dir_path(&self) -> PathBuf {
        self.root_dir.join(R6_DIR).join(SCRIPTS_DIR)
    }

    pub fn script_paths(&self) -> impl Iterator<Item = Cow<'_, Path>> {
        iter::once(self.default_scripts_dir_path().into())
            .chain(self.additional_script_paths.iter().map(Into::into))
    }

    pub fn cache_file_path(&self) -> Cow<'_, Path> {
        self.custom_cache_file
            .as_ref()
            .map_or_else(|| self.default_cache_file_path().into(), Into::into)
    }

    pub fn output_cache_file_path(&self) -> Cow<'_, Path> {
        self.output_cache_file
            .as_ref()
            .map_or_else(|| self.cache_file_path(), Into::into)
    }

    pub fn default_backup_cache_file_path(&self) -> PathBuf {
        self.root_dir
            .join(R6_DIR)
            .join(CACHE_DIR)
            .join(BACKUP_BUNDLE_FILE)
    }

    pub fn unmodified_cache_file_path(&self) -> PathBuf {
        let path = self.default_backup_cache_file_path();
        if path.exists() {
            path
        } else {
            path.with_extension(CACHE_FILE_EXT)
        }
    }

    pub fn has_separate_output_cache(&self) -> io::Result<bool> {
        let Some(output) = self.output_cache_file.as_ref() else {
            return Ok(false);
        };
        match (get_file_id(output), get_file_id(self.cache_file_path())) {
            (Ok(id1), Ok(id2)) => Ok(id1 != id2),
            (Err(err), Ok(_)) if err.kind() == io::ErrorKind::NotFound => Ok(true),
            (Err(err), _) | (_, Err(err)) => Err(err),
        }
    }

    pub fn should_show_error_report(&self) -> bool {
        self.show_error_report
    }

    pub fn type_flags(&self) -> &TypeFlagRegistry {
        &self.type_flags
    }

    pub fn set_custom_cache_file(&mut self, path: PathBuf) {
        self.custom_cache_file = Some(path);
    }

    pub fn set_output_cache_file(&mut self, path: PathBuf) {
        self.output_cache_file = Some(path);
    }

    pub fn set_show_error_report(&mut self, show: bool) {
        self.show_error_report = show;
    }

    pub fn add_script_path(&mut self, path: PathBuf) {
        self.additional_script_paths.push(path);
    }

    pub fn register_never_ref_type(&mut self, name: impl Into<Cow<'static, str>>) {
        self.type_flags
            .entry_or_default_mut(name)
            .set_is_never_ref(true);
    }

    pub fn register_mixed_ref_type(&mut self, name: impl Into<Cow<'static, str>>) {
        self.type_flags
            .entry_or_default_mut(name)
            .set_is_mixed_ref(true);
    }
}
