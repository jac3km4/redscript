//! Stage generated scripts on the destination filesystem and publish a new mod.
//! Existing mods are never replaced. Assets, tweaks and manifests are not converted.
use std::collections::HashSet;
use std::fs::{self, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use redscript_community_ast::Span;

use crate::validate::{OfficialValidator, ValidationReport};
use crate::{ExportedSource, SourceMapping};

static NEXT_STAGE: AtomicU64 = AtomicU64::new(0);

pub struct ScriptArtifact<'a> {
    /// Portable relative path below `r6/scripts`, ending in `.script`.
    pub path: &'a Path,
    pub source: &'a ExportedSource,
}

#[derive(Debug)]
pub struct PublishedScript {
    pub path: PathBuf,
    pub file: redscript_community_ast::FileId,
    pub mappings: Vec<SourceMapping>,
}

#[derive(Debug)]
pub struct PublishedMod {
    pub directory: PathBuf,
    pub scripts: Vec<PublishedScript>,
    pub validation: Option<ValidationReport>,
}

fn component(name: &str) -> bool {
    let stem = name.split('.').next().unwrap_or("").to_ascii_uppercase();
    !name.is_empty()
        && !matches!(name, "." | "..")
        && !name.ends_with(['.', ' '])
        && !name
            .chars()
            .any(|c| c.is_control() || "<>:\"/\\|?*".contains(c))
        && !matches!(stem.as_str(), "CON" | "PRN" | "AUX" | "NUL")
        && !(stem.len() == 4
            && (stem.starts_with("COM") || stem.starts_with("LPT"))
            && stem.as_bytes()[3].is_ascii_digit())
}

fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, message)
}

/// Publish a new mod via a same-parent directory rename. All callers must honor
/// the sibling reservation lock; unrelated processes must not create the destination
/// during publication (portable std has no atomic directory rename-no-replace).
/// On error staging is removed; existing destinations are never intentionally modified.
pub fn publish_mod(
    parent: &Path,
    name: &str,
    scripts: &[ScriptArtifact<'_>],
    validator: Option<&OfficialValidator>,
) -> io::Result<PublishedMod> {
    publish_mod_inner(parent, name, scripts, validator, None)
}

/// Like [`publish_mod`], with an explicit read-only base-game root for validation.
/// The compiler may still write its own process logs beside its executable.
pub fn publish_mod_with_root(
    parent: &Path,
    name: &str,
    scripts: &[ScriptArtifact<'_>],
    validator: &OfficialValidator,
    game_root: &Path,
) -> io::Result<PublishedMod> {
    publish_mod_inner(parent, name, scripts, Some(validator), Some(game_root))
}

fn publish_mod_inner(
    parent: &Path,
    name: &str,
    scripts: &[ScriptArtifact<'_>],
    validator: Option<&OfficialValidator>,
    game_root: Option<&Path>,
) -> io::Result<PublishedMod> {
    if !component(name) || name.contains('.') {
        return Err(invalid(
            "mod name must be a portable component without periods",
        ));
    }
    let mut paths = HashSet::new();
    for script in scripts {
        let text = script
            .path
            .to_str()
            .ok_or_else(|| invalid("script path must be UTF-8"))?;
        if !text.split('/').all(component)
            || text.contains('\\')
            || !text.ends_with(".script")
            || !paths.insert(text.to_lowercase())
        {
            return Err(invalid(
                "script paths must be unique portable relative .script paths (case-insensitive)",
            ));
        }
    }
    let parent = fs::canonicalize(parent)?;
    let destination = parent.join(name);
    let lock_path = parent.join(format!(".{name}.redscript-redmod-export.lock"));
    let lock = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&lock_path)?;
    struct Cleanup {
        stage: Option<PathBuf>,
        lock_path: PathBuf,
        lock: Option<fs::File>,
    }
    impl Drop for Cleanup {
        fn drop(&mut self) {
            if let Some(stage) = &self.stage {
                drop(fs::remove_dir_all(stage));
            }
            self.lock.take();
            drop(fs::remove_file(&self.lock_path));
        }
    }
    let mut cleanup = Cleanup {
        stage: None,
        lock_path,
        lock: Some(lock),
    };
    if fs::symlink_metadata(&destination).is_ok() {
        return Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            "mod destination already exists",
        ));
    }
    let stage = loop {
        let candidate = parent.join(format!(
            "{name}-stage-{}-{}",
            std::process::id(),
            NEXT_STAGE.fetch_add(1, Ordering::Relaxed)
        ));
        match fs::create_dir(&candidate) {
            Ok(()) => break candidate,
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(e),
        }
    };
    cleanup.stage = Some(stage.clone());
    // REDmod derives about.json's ModName from this folder and may create sibling
    // Dependencies. Keep both inside the reservation container until publication.
    let mod_stage = stage.join(name);
    fs::create_dir(&mod_stage)?;
    for script in scripts {
        let path = mod_stage.join("r6/scripts").join(script.path);
        fs::create_dir_all(path.parent().expect("script parent"))?;
        let mut file = OpenOptions::new().write(true).create_new(true).open(path)?;
        file.write_all(script.source.module.text.as_bytes())?;
        file.sync_all()?;
    }
    let validation = validator
        .map(|v| match game_root {
            Some(root) => v.validate_with_root(&mod_stage, root),
            None => v.validate(&mod_stage),
        })
        .transpose()?;
    if fs::symlink_metadata(&destination).is_ok() {
        return Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            "mod destination appeared during staging",
        ));
    }
    fs::rename(&mod_stage, &destination)?;
    Ok(PublishedMod {
        scripts: scripts
            .iter()
            .map(|s| PublishedScript {
                path: destination.join("r6/scripts").join(s.path),
                file: s.source.file,
                mappings: s.source.module.mappings.clone(),
            })
            .collect(),
        directory: destination,
        validation,
    })
}

/// Resolve a generated UTF-8 byte offset to the tightest retained source range.
/// Statement mappings take precedence over enclosing declaration mappings.
pub fn source_at(mappings: &[SourceMapping], offset: usize) -> Option<Span> {
    mappings
        .iter()
        .filter(|m| m.generated.contains(&offset))
        .min_by_key(|m| m.generated.len())
        .map(|m| m.source)
}
