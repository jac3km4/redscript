use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::Duration;

use anyhow::{Context, ensure};
use argh::FromArgs;
use redscript_compiler_api::ast::{FileId, SourceMap};
use redscript_compiler_api::{
    CompilationInputs, ScriptBundle, SourceMapExt, Symbols, TypeFlagRegistry, TypeInterner,
};
use redscript_dotfile::Dotfile;
use redscript_redmod_export::package::{ScriptArtifact, publish_mod, publish_mod_with_root};
use redscript_redmod_export::validate::OfficialValidator;
use redscript_redmod_export::{ExportDiagnostic, analyze_sources};

/// export community redscript sources as a new REDmod script package
#[derive(Debug, FromArgs)]
#[argh(subcommand, name = "export-redmod")]
pub(crate) struct ExportOpts {
    /// authored source file or directory to export (repeatable; honors .redscript roots)
    #[argh(option, short = 's')]
    src: Vec<PathBuf>,
    /// community source file or directory for analysis only (repeatable)
    #[argh(option)]
    dependency: Vec<PathBuf>,
    /// existing baseline .redscripts for engine symbols (replaces built-in boot sources)
    #[argh(option, short = 'b')]
    bundle: Option<PathBuf>,
    /// new mod directory; parent must exist, name must not contain periods
    #[argh(option, short = 'o')]
    output: PathBuf,
    /// absolute path to a trusted official REDmod executable for optional validation
    #[argh(option)]
    redmod: Option<PathBuf>,
    /// absolute game installation root for base blobs (requires --redmod)
    #[argh(option)]
    game_root: Option<PathBuf>,
    /// official validation timeout in positive, finite seconds (default: 60)
    #[argh(
        option,
        default = "Duration::from_secs(60)",
        from_str_fn(parse_timeout)
    )]
    timeout: Duration,
}

fn parse_timeout(value: &str) -> Result<Duration, String> {
    let seconds = value
        .parse::<f64>()
        .map_err(|_| "timeout must be positive, finite seconds".to_owned())?;
    let duration = Duration::try_from_secs_f64(seconds)
        .map_err(|_| "timeout must be positive, finite seconds".to_owned())?;
    if duration.is_zero() {
        return Err("timeout must be positive, finite seconds".to_owned());
    }
    Ok(duration)
}

pub(crate) fn export(opts: ExportOpts) -> anyhow::Result<ExitCode> {
    ensure!(!opts.src.is_empty(), "at least one --src is required");
    let name = opts
        .output
        .file_name()
        .and_then(|n| n.to_str())
        .context("--output must name a new mod directory")?;
    ensure!(!name.contains('.'), "mod name must not contain periods");
    ensure!(
        fs::symlink_metadata(&opts.output).is_err_and(|e| e.kind() == std::io::ErrorKind::NotFound),
        "mod destination already exists or cannot be accessed: {}",
        opts.output.display()
    );
    let parent = opts
        .output
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    ensure!(parent.is_dir(), "output parent directory must exist");
    if let Some(root) = &opts.game_root {
        ensure!(opts.redmod.is_some(), "--game-root requires --redmod");
        ensure!(root.is_absolute(), "--game-root must be an absolute path");
    }
    let validator = opts
        .redmod
        .map(|executable| {
            ensure!(
                executable.is_absolute(),
                "--redmod must be an absolute path"
            );
            Ok(OfficialValidator {
                executable,
                timeout: opts.timeout,
            })
        })
        .transpose()?;

    let sources = SourceMap::new();
    let mut seen = HashSet::new();
    let mut selected = Vec::new();
    let mut paths = Vec::new();
    for (file, path) in load_roots(&sources, &opts.src, &mut seen)? {
        selected.push(file);
        paths.push(path);
    }
    ensure!(!selected.is_empty(), "--src contains no .reds source files");
    load_roots(&sources, &opts.dependency, &mut seen)?;

    let interner = TypeInterner::default();
    let bytes = opts
        .bundle
        .as_ref()
        .map(|path| fs::read(path).with_context(|| format!("loading bundle {}", path.display())))
        .transpose()?;
    let bundle = bytes
        .as_deref()
        .map(ScriptBundle::from_bytes)
        .transpose()
        .context("decoding baseline bundle")?;
    let symbols = if let Some(bundle) = &bundle {
        CompilationInputs::load_without_mapping(bundle, &interner, &TypeFlagRegistry::default())
            .context("loading baseline symbols")?
    } else {
        sources.populate_boot_lib();
        Symbols::with_default_types()
    };

    let analyzed = match analyze_sources(&sources, symbols, &interner) {
        Ok(analyzed) => analyzed,
        Err(diagnostics) => return report_errors(&diagnostics, &sources),
    };
    for warning in analyzed.warnings() {
        eprintln!("warning{warning}");
    }
    let exported = match analyzed.export_selected(&selected) {
        Ok(exported) => exported,
        Err(diagnostics) => return report_errors(&diagnostics, &sources),
    };
    let scripts: Vec<_> = paths
        .iter()
        .zip(&exported)
        .map(|(path, source)| ScriptArtifact { path, source })
        .collect();
    let published = match (&opts.game_root, &validator) {
        (Some(root), Some(validator)) => {
            publish_mod_with_root(parent, name, &scripts, validator, root)
        }
        _ => publish_mod(parent, name, &scripts, validator.as_ref()),
    }
    .with_context(|| format!("publishing REDmod package {}", opts.output.display()))?;
    log::info!(
        "Exported {} script(s) to {}",
        published.scripts.len(),
        published.directory.display()
    );
    if published.validation.is_none() {
        log::info!(
            "Official REDmod validation was not requested; runtime compatibility is not guaranteed"
        );
    }
    Ok(ExitCode::SUCCESS)
}

fn report_errors(
    diagnostics: &[ExportDiagnostic],
    sources: &SourceMap,
) -> anyhow::Result<ExitCode> {
    for diagnostic in diagnostics {
        eprintln!("{}", diagnostic.display(sources));
    }
    Ok(ExitCode::FAILURE)
}

/// Keep authored FileIds separate from dependencies/boot, deduplicating overlapping roots.
/// Relative paths are preserved per expanded root; packaging rejects output collisions.
fn load_roots(
    sources: &SourceMap,
    roots: &[PathBuf],
    seen: &mut HashSet<PathBuf>,
) -> anyhow::Result<Vec<(FileId, PathBuf)>> {
    let mut selected = Vec::new();
    for root in roots {
        let root = fs::canonicalize(root)
            .with_context(|| format!("loading source root {}", root.display()))?;
        let expanded = if root.is_dir() {
            Dotfile::load_or_default(&root)?
                .expanded_source_roots()
                .into_iter()
                .map(|p| root.join(p))
                .collect()
        } else {
            vec![root]
        };
        for root in expanded {
            let root = fs::canonicalize(&root)
                .with_context(|| format!("loading source root {}", root.display()))?;
            let base = if root.is_file() {
                root.parent().context("source file has no parent")?
            } else {
                &root
            };
            let mut inputs = Vec::new();
            if root.is_file() {
                ensure!(
                    root.extension().is_some_and(|ext| ext == "reds"),
                    "source file must have a .reds extension: {}",
                    root.display()
                );
                inputs.push(root.clone());
            } else {
                ensure!(
                    root.is_dir(),
                    "source root must be a file or directory: {}",
                    root.display()
                );
                for entry in ignore::WalkBuilder::new(&root).follow_links(true).build() {
                    let entry =
                        entry.with_context(|| format!("walking source root {}", root.display()))?;
                    if entry.file_type().is_some_and(|typ| typ.is_file())
                        && entry.path().extension().is_some_and(|ext| ext == "reds")
                    {
                        inputs.push(entry.into_path());
                    }
                }
            }
            let loaded = SourceMap::from_files(inputs)
                .with_context(|| format!("reading source root {}", root.display()))?;
            let mut files: Vec<_> = loaded.files().map(|(_, file)| file).collect();
            files.sort_by_key(|file| file.path());
            for file in files {
                let canonical = fs::canonicalize(file.path())?;
                if !seen.insert(canonical) {
                    continue;
                }
                let relative = file.path().strip_prefix(base)?.with_extension("script");
                // The package API requires portable '/' separators, including on Windows.
                let path = relative
                    .components()
                    .map(|c| c.as_os_str().to_str().context("source path must be UTF-8"))
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .join("/");
                let id = sources.push_back(file.path(), file.source());
                selected.push((id, PathBuf::from(path)));
            }
        }
    }
    Ok(selected)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timeout_is_positive_finite_and_representable() {
        for value in ["NaN", "inf", "-inf", "-1", "0", "1e100", "nope", "1e-100"] {
            assert!(parse_timeout(value).is_err(), "{value}");
        }
        assert_eq!(parse_timeout("0.25").unwrap(), Duration::from_millis(250));
    }

    #[test]
    fn help() {
        let help =
            crate::Args::from_args(&["redscript-cli"], &["export-redmod", "--help"]).unwrap_err();
        assert!(help.status.is_ok());
        insta::assert_snapshot!(help.output);
    }
}
