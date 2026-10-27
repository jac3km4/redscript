//! The compiler-agnostic part of the `.reds` to `.script` conversion of a set of files: collecting
//! the sources, converting them one by one, reporting diagnostics and writing the results.
//!
//! The type information comes from the compiler, which is provided by the caller:
//!
//! ```ignore
//! driver::run(&opts, |converter| {
//!     let types = type_check(converter.map(), &converter.files());
//!     converter.convert(Some(TypeCheck { query: &types, summary }))
//! })
//! ```

use std::path::{Path, PathBuf};
use std::{fmt, fs, io};

use hashbrown::HashMap;
use redscript_ast::{Dialect, FileId, SourceMap};
use redscript_formatter::{
    FormatSettings, ParseOptions, parse_document_with_options, print_document,
};

use crate::{ConvertDiagnostic, ConvertOptions, NameArena, SourceFields, TypeQuery};

const FROM: Dialect = Dialect::Community;
const TO: Dialect = Dialect::Redmod;

/// What to convert and where to write it.
#[derive(Debug, Clone, Copy)]
pub struct DriverOptions<'a> {
    /// Files or directories to convert.
    pub src: &'a [PathBuf],
    /// Output directory, files are written next to their sources by default.
    pub output: Option<&'a Path>,
    /// Print the converted code to stdout instead of writing files.
    pub dry_run: bool,
    /// Overwrite output files that already exist.
    pub force: bool,
}

/// What the type checking of the converted files found.
#[derive(Debug, Default)]
pub struct TypeCheckSummary {
    /// `unsupported-api` diagnostics, by file.
    pub diagnostics: HashMap<FileId, Vec<ConvertDiagnostic>>,
    /// Number of other type errors.
    pub untyped: usize,
}

impl TypeCheckSummary {
    /// A summary of the type errors that are not reported individually.
    pub fn summary(&self) -> String {
        let plural = if self.untyped == 1 { "" } else { "s" };
        format!(
            "{} type error{plural}; affected conversions fall back to warnings",
            self.untyped
        )
    }
}

/// Type information about the converted sources.
pub struct TypeCheck<'a> {
    pub query: &'a dyn TypeQuery,
    pub summary: TypeCheckSummary,
}

/// The result of a conversion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Outcome {
    /// Whether some sources could not be parsed.
    pub failed: bool,
}

#[derive(Debug)]
pub enum DriverError {
    Io(io::Error),
    /// The message describes the problem.
    Invalid(String),
}

impl fmt::Display for DriverError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => err.fmt(f),
            Self::Invalid(msg) => f.write_str(msg),
        }
    }
}

impl std::error::Error for DriverError {}

impl From<io::Error> for DriverError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

/// Collects the sources and hands them over to `typecheck`, which type-checks them (if it can)
/// and then calls [`Converter::convert`]. The callback style lets the type information borrow
/// from the source map owned by the driver.
pub fn run<E: From<DriverError>>(
    opts: &DriverOptions<'_>,
    typecheck: impl FnOnce(Converter<'_>) -> Result<Outcome, E>,
) -> Result<Outcome, E> {
    // declared before the source map, generated names live as long as the parsed sources
    let arena = NameArena::new();

    // all the files share a single source map, so that the spans recorded while type-checking
    // the sources refer to the same files as the spans of the converted documents
    let map = SourceMap::new();
    let groups = collect(opts, &map)?;
    typecheck(Converter {
        opts,
        arena: &arena,
        map: &map,
        groups,
    })
}

/// The collected sources, ready to be converted.
pub struct Converter<'a> {
    opts: &'a DriverOptions<'a>,
    arena: &'a NameArena,
    map: &'a SourceMap,
    groups: Vec<(&'a Path, Vec<FileId>)>,
}

impl<'a> Converter<'a> {
    /// The source map holding all the files to convert.
    pub fn map(&self) -> &'a SourceMap {
        self.map
    }

    /// The files to convert, in the order they are converted in.
    pub fn files(&self) -> Vec<FileId> {
        self.groups
            .iter()
            .flat_map(|(_, ids)| ids.iter().copied())
            .collect()
    }

    /// Converts all the files, using `types` when available.
    pub fn convert(self, types: Option<TypeCheck<'_>>) -> Result<Outcome, DriverError> {
        let Self {
            opts,
            arena,
            map,
            groups,
        } = self;
        let settings = FormatSettings {
            dialect: TO,
            ..FormatSettings::default()
        };
        let (query, mut summary) = match types {
            Some(TypeCheck { query, summary }) => (Some(query), summary),
            None => (None, TypeCheckSummary::default()),
        };
        // the fields with reserved names are renamed in every file, which needs the classes of
        // all the files first
        let mut modules = vec![];
        for (_, ids) in &groups {
            for &id in ids {
                let file = map.get(id).ok_or_else(unknown_source)?;
                let (doc, errors) =
                    parse_document_with_options(file.source(), id, ParseOptions::compat(FROM));
                modules.extend(doc.filter(|_| errors.is_empty()).map(|doc| doc.module));
            }
        }
        let fields = SourceFields::collect(&modules);
        drop(modules);
        let convert_opts = ConvertOptions {
            types: query,
            fields: Some(&fields),
        };
        let mut failed = false;

        for (root, ids) in &groups {
            for &id in ids {
                let file = map.get(id).ok_or_else(unknown_source)?;
                // community sources may be written for redscript 0.5.x
                let (doc, errors) =
                    parse_document_with_options(file.source(), id, ParseOptions::compat(FROM));
                let Some(doc) = doc.filter(|_| errors.is_empty()) else {
                    for err in &errors {
                        eprintln!("[ERROR] {}", err.display(map).ok_or_else(unknown_source)?);
                    }
                    failed = true;
                    continue;
                };
                let (doc, mut diagnostics) = crate::convert(doc, &convert_opts, arena);
                if let Some(unsupported) = summary.diagnostics.remove(&id) {
                    diagnostics.extend(unsupported);
                    diagnostics.sort_by_key(|d| (d.span.start, d.span.end));
                }
                for diagnostic in &diagnostics {
                    eprintln!(
                        "{}:{}: {diagnostic}",
                        file.path().display(),
                        file.lookup(diagnostic.span.start)
                    );
                }
                let output = print_document(&doc, &settings).to_string();

                let relative = file.path().strip_prefix(root).unwrap_or(file.path());
                let target = opts
                    .output
                    .map_or_else(|| file.path().to_owned(), |dir| dir.join(relative))
                    .with_extension(TO.extension());

                if opts.dry_run {
                    println!("// {}", target.display());
                    print!("{output}");
                    continue;
                }
                if target == file.path() {
                    return Err(DriverError::Invalid(format!(
                        "refusing to overwrite {}",
                        file.path().display()
                    )));
                }
                if target.exists() && !opts.force {
                    log::warn!(
                        "{} already exists, skipping it (use --force to overwrite)",
                        target.display()
                    );
                    continue;
                }
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent)?;
                }
                fs::write(&target, output)?;
                log::info!("{} -> {}", file.path().display(), target.display());
            }
        }

        if summary.untyped > 0 {
            eprintln!("info[untyped]: {}", summary.summary());
        }
        Ok(Outcome { failed })
    }
}

fn unknown_source() -> DriverError {
    DriverError::Invalid("unknown source".to_owned())
}

/// Loads the files with one of the given extensions found recursively in `dir`, like
/// [`SourceMap::from_paths_recursively`] does for `.reds` files.
fn files_in_dir(dir: &Path, exts: &[&str]) -> io::Result<SourceMap> {
    let paths = ignore::WalkBuilder::new(dir)
        .follow_links(true)
        .build()
        .filter_map(Result::ok)
        .filter(|entry| {
            entry
                .path()
                .extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| exts.contains(&e))
        })
        .map(ignore::DirEntry::into_path);
    SourceMap::from_files(paths)
}

/// Reads the `.reds` files of every source into `map`, grouped by the root they are relative to.
fn collect<'a>(
    opts: &DriverOptions<'a>,
    map: &SourceMap,
) -> Result<Vec<(&'a Path, Vec<FileId>)>, DriverError> {
    let mut groups = vec![];
    for src in opts.src {
        let (root, files) = if src.is_dir() {
            let files = files_in_dir(src, &[FROM.extension(), TO.extension()])?;
            (src.as_path(), files)
        } else {
            let ext = src.extension().and_then(|e| e.to_str());
            if ext == Some(TO.extension()) {
                log::warn!(
                    "skipping {}, converting .script back to .reds is not supported",
                    src.display()
                );
                continue;
            }
            if ext != Some(FROM.extension()) {
                return Err(DriverError::Invalid(format!(
                    "{} is not a .{} file, cannot convert it to .{}",
                    src.display(),
                    FROM.extension(),
                    TO.extension()
                )));
            }
            let root = src.parent().unwrap_or(Path::new(""));
            (root, SourceMap::from_files([src])?)
        };
        let ids = files
            .files()
            .filter(|(_, file)| {
                let skip = file.path().extension().and_then(|e| e.to_str()) == Some(TO.extension());
                if skip {
                    log::warn!(
                        "skipping {}, converting .script back to .reds is not supported",
                        file.path().display()
                    );
                }
                !skip
            })
            .map(|(_, file)| map.push_back(file.path(), file.source()))
            .collect();
        groups.push((root, ids));
    }
    Ok(groups)
}
