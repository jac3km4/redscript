//! Optional, boot-only semantic survey. See ../CORPUS.md.
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::{env, fs, io};

use redscript_community_ast::{SourceMap, Span};
use redscript_compiler_api::SourceMapExt;
use redscript_compiler_frontend::pass::UnusedLocals;
use redscript_compiler_frontend::{CompileErrorReporter, Symbols, TypeInterner, parse_file};
use redscript_redmod_adapter::{Error, parse_file_with_metadata, process_sources};

const USAGE: &str = "Usage: cargo run -p redscript-redmod-adapter --locked --example semantic_corpus -- <corpus-directory> [output-directory]\nDependencies: embedded boot library and default symbols only; no game bundle.";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = env::args_os().skip(1);
    let Some(root) = args.next() else {
        return Err(USAGE.into());
    };
    if root == "--help" || root == "-h" {
        println!("{USAGE}");
        return Ok(());
    }
    let output = args
        .next()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("target/redmod-semantic-corpus"));
    if args.next().is_some() {
        return Err(USAGE.into());
    }
    run(Path::new(&root), &output)?;
    Ok(())
}

/// Do not follow symlinks/junctions or honor ignore files: survey the explicit tree.
fn discover(root: &Path) -> io::Result<Vec<PathBuf>> {
    fn visit(path: &Path, paths: &mut Vec<PathBuf>) -> io::Result<()> {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let kind = entry.file_type()?;
            if kind.is_symlink() {
                continue;
            }
            if kind.is_dir() {
                visit(&entry.path(), paths)?;
            } else if kind.is_file()
                && entry
                    .path()
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("script"))
            {
                paths.push(entry.path());
            }
        }
        Ok(())
    }
    if !fs::symlink_metadata(root)?.is_dir() {
        return Err(io::Error::other(
            "corpus must be a directory, not a symlink",
        ));
    }
    let mut paths = Vec::new();
    visit(root, &mut paths)?;
    paths.sort_by_cached_key(|path| {
        let relative = portable(path.strip_prefix(root).expect("discovered below root"));
        (relative.to_ascii_lowercase(), relative)
    });
    if paths.is_empty() {
        return Err(io::Error::other("corpus contains no .script files"));
    }
    Ok(paths)
}

fn portable(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn tsv(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
        .replace('\n', "\\n")
}

#[derive(Debug)]
struct Record {
    stage: &'static str,
    code: &'static str,
    reason: String,
    level: String,
    path: String,
    line: usize,
    column: usize,
    start: u32,
    end: u32,
    message: String,
}

impl Record {
    fn new(
        sources: &SourceMap,
        stage: &'static str,
        code: &'static str,
        reason: String,
        level: String,
        span: Span,
        message: String,
    ) -> io::Result<Self> {
        let file = sources
            .get(span.file)
            .ok_or_else(|| io::Error::other("diagnostic has an unknown source file"))?;
        if span.start > span.end
            || !file.source().is_char_boundary(span.start as usize)
            || !file.source().is_char_boundary(span.end as usize)
        {
            return Err(io::Error::other(format!(
                "invalid diagnostic span in {}: {}..{}",
                file.path().display(),
                span.start,
                span.end
            )));
        }
        let loc = file.lookup(span.start);
        Ok(Self {
            stage,
            code,
            reason,
            level,
            path: portable(file.path()),
            line: loc.line + 1,
            column: loc.col + 1,
            start: span.start,
            end: span.end,
            message,
        })
    }

    fn adapter(sources: &SourceMap, stage: &'static str, error: &Error) -> io::Result<Self> {
        let reason = match error {
            Error::Parse(_) => "Syntax".into(),
            Error::Capability { kind, .. } => format!("{kind:?}"),
            Error::Semantic { kind, .. } => format!("{kind:?}"),
            Error::Condition { kind, .. } => format!("{kind:?}"),
        };
        Self::new(
            sources,
            stage,
            error.code(),
            reason,
            "Error".into(),
            error.span(),
            error.to_string(),
        )
    }
}

fn run(root: &Path, output: &Path) -> io::Result<()> {
    let paths = discover(root)?;
    let sources = SourceMap::new();
    sources.populate_boot_lib();
    let mut bytes = 0;
    for path in &paths {
        let source = fs::read_to_string(path)
            .map_err(|err| io::Error::other(format!("{}: {err}", path.display())))?;
        bytes += source.len();
        // Relative names keep reports portable; the corpus root is recorded separately.
        sources.push_back(
            path.strip_prefix(root).expect("discovered below root"),
            source,
        );
    }
    let interner = TypeInterner::default();
    let mut reporter = CompileErrorReporter::default();
    let mut dependencies = Vec::new();
    let mut modules = Vec::new();
    let mut records = Vec::new();
    let mut counts = BTreeMap::<&str, usize>::new();
    for stage in [
        "syntax",
        "capability",
        "validation",
        "missing-module",
        "processed",
    ] {
        counts.insert(stage, 0);
    }
    let mut files = String::from("path\tbytes\tstage\tparse_diagnostics\n");
    for (index, (id, file)) in sources.files().enumerate() {
        if index == 0 {
            let module = parse_file(id, file, &mut reporter)
                .ok_or_else(|| io::Error::other("embedded boot library did not parse"))?;
            dependencies.push(module);
            continue;
        }
        let (parsed, errors) = parse_file_with_metadata(file.source(), id);
        let stage = if errors.iter().any(|error| matches!(error, Error::Parse(_))) {
            "syntax"
        } else if errors
            .iter()
            .any(|error| matches!(error, Error::Capability { .. }))
        {
            "capability"
        } else if !errors.is_empty() {
            "validation"
        } else if parsed.is_none() {
            "missing-module"
        } else {
            "processed"
        };
        *counts.get_mut(stage).expect("known stage") += 1;
        writeln!(
            files,
            "{}\t{}\t{stage}\t{}",
            tsv(&portable(file.path())),
            file.source().len(),
            errors.len()
        )
        .unwrap();
        for error in &errors {
            records.push(Record::adapter(&sources, stage, error)?);
        }
        // Match the IDE: retain ownership even for empty modules, and exclude
        // every module with parse-stage diagnostics (including validation errors).
        if errors.is_empty()
            && let Some(parsed) = parsed
        {
            modules.push(parsed);
        }
    }
    if !reporter.reported().is_empty() {
        return Err(io::Error::other(
            "embedded boot library has syntax diagnostics",
        ));
    }
    let (unit, _, errors) = process_sources(
        modules,
        dependencies,
        Symbols::with_default_types(),
        &mut reporter,
        &interner,
    );
    unit.run_diagnostics(&[&UnusedLocals], &mut reporter);
    let adapter_errors = errors.len();
    for error in &errors {
        records.push(Record::adapter(&sources, "adapter-processing", error)?);
    }
    let mut shared_errors = 0;
    let mut shared_warnings = 0;
    let mut shared_pedantic = 0;
    for diagnostic in reporter.reported() {
        use redscript_compiler_frontend::DiagnosticLevel;
        match diagnostic.level() {
            DiagnosticLevel::Error | DiagnosticLevel::ErrorAllowedAtRuntime => shared_errors += 1,
            DiagnosticLevel::Warning => shared_warnings += 1,
            DiagnosticLevel::Pedantic => shared_pedantic += 1,
        }
        records.push(Record::new(
            &sources,
            "shared",
            diagnostic.code(),
            // Shared typed diagnostic codes, not message text, are the grouping key.
            String::new(),
            format!("{:?}", diagnostic.level()),
            diagnostic.span(),
            diagnostic.to_string(),
        )?);
    }
    records.sort_by(|a, b| {
        (
            &a.stage, &a.code, &a.reason, &a.level, &a.path, a.start, a.end, &a.message,
        )
            .cmp(&(
                &b.stage, &b.code, &b.reason, &b.level, &b.path, b.start, b.end, &b.message,
            ))
    });
    let mut summary = format!(
        "# REDmod semantic corpus (boot only)\n\nCorpus: `{}`\n\nDependencies: embedded `boot.reds` + `Symbols::with_default_types()`. No game bundle.\n\nFiles: {}; UTF-8 bytes: {}.\n\n| Parse-stage outcome | Files |\n| --- | ---: |\n",
        portable(root),
        paths.len(),
        bytes
    );
    for (stage, count) in &counts {
        writeln!(summary, "| {stage} | {count} |").unwrap();
    }
    write!(
        summary,
        "\nAdapter processing errors: {adapter_errors}.\n\nShared errors: {shared_errors}; warnings: {shared_warnings}; pedantic: {shared_pedantic}.\n\nAll eligible modules were processed together, with the IDE's unused-local pass.\nCapability counts are first blockers per file; validation may report several errors.\nExcluded declarations and absent engine definitions cause unresolved-symbol cascades.\nReturned IR and diagnostic-free files are NOT semantic passes or runtime guarantees.\n\nSee [grouped diagnostics](aggregate.tsv), [per-file outcomes](files.tsv),\n[all diagnostics](diagnostics.tsv), and [full log](diagnostics.log).\nReduce findings to authored fixtures under `crates/compiler/redmod/adapter/tests/data/`;\nsee the adapter CORPUS.md for reproduction guidance and fixture links.\n"
    )
    .unwrap();

    let (diagnostics, aggregate, log) = reports(&records);
    fs::create_dir_all(output)?;
    for (name, contents) in [
        ("summary.md", summary.as_str()),
        ("files.tsv", files.as_str()),
        ("diagnostics.tsv", diagnostics.as_str()),
        ("aggregate.tsv", aggregate.as_str()),
        ("diagnostics.log", log.as_str()),
    ] {
        fs::write(output.join(name), contents)?;
    }
    println!("{summary}\nReports: {}", output.display());
    Ok(())
}

fn reports(records: &[Record]) -> (String, String, String) {
    let mut diagnostics =
        String::from("stage\tcode\treason\tlevel\tpath\tline\tcolumn\tstart\tend\tmessage\n");
    let mut log = String::new();
    let mut groups = BTreeMap::<_, Vec<_>>::new();
    for record in records {
        let Record {
            stage,
            code,
            reason,
            level,
            path,
            line,
            column,
            start,
            end,
            message,
        } = record;
        writeln!(
            diagnostics,
            "{stage}\t{code}\t{}\t{level}\t{}\t{line}\t{column}\t{start}\t{end}\t{}",
            tsv(reason),
            tsv(path),
            tsv(message)
        )
        .unwrap();
        writeln!(log, "{path}:{line}:{column}: {level}[{code}] ({stage}/{reason}, bytes {start}..{end}): {message}").unwrap();
        groups
            .entry((stage, code, reason, level))
            .or_default()
            .push(record);
    }
    let mut aggregate = String::from(
        "stage\tcode\treason\tlevel\tdiagnostics\tfiles\texample_locations\texample_message\n",
    );
    for ((stage, code, reason, level), group) in groups {
        let files: BTreeSet<_> = group.iter().map(|record| &record.path).collect();
        let examples = group
            .iter()
            .take(3)
            .map(|record| format!("{}:{}:{}", record.path, record.line, record.column))
            .collect::<Vec<_>>()
            .join("; ");
        writeln!(
            aggregate,
            "{stage}\t{code}\t{}\t{level}\t{}\t{}\t{}\t{}",
            tsv(reason),
            group.len(),
            files.len(),
            tsv(&examples),
            tsv(&group[0].message)
        )
        .unwrap();
    }
    (diagnostics, aggregate, log)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discovery_is_case_insensitive_sorted_and_recursive() -> io::Result<()> {
        let root =
            env::temp_dir().join(format!("redmod-semantic-discovery-{}", std::process::id()));
        fs::create_dir(&root)?;
        let result = (|| {
            fs::create_dir(root.join("nested"))?;
            fs::write(root.join(".gitignore"), "*.script\n")?;
            for path in [
                "z.script",
                "A.Script",
                "nested/b.SCRIPT",
                "ignore.reds",
                ".hidden.script",
            ] {
                fs::write(root.join(path), "")?;
            }
            let paths = discover(&root)?;
            let relative: Vec<_> = paths
                .iter()
                .map(|path| portable(path.strip_prefix(&root).unwrap()))
                .collect();
            assert_eq!(
                relative,
                [".hidden.script", "A.Script", "nested/b.SCRIPT", "z.script"]
            );
            assert_eq!(paths, discover(&root)?);
            assert!(discover(&root.join("missing")).is_err());
            assert!(discover(&root.join("z.script")).is_err());
            fs::create_dir(root.join("empty"))?;
            assert!(discover(&root.join("empty")).is_err());
            Ok(())
        })();
        fs::remove_dir_all(&root)?;
        result
    }

    #[test]
    fn survey_filters_invalid_modules_and_resolves_valid_modules_together() -> io::Result<()> {
        let root = env::temp_dir().join(format!("redmod-semantic-survey-{}", std::process::id()));
        fs::create_dir(&root)?;
        let result = (|| {
            for (name, source) in [
                ("a.Script", "function Caller(): Int32 { return Callee(); }"),
                ("b.script", "function Callee(): Int32 { return 1; }"),
                ("empty.script", ""),
                ("syntax.script", "function {"),
                (
                    "capability.script",
                    "function Invalid() { const var x: Int32 = 1; }",
                ),
                (
                    "validation.script",
                    "function Invalid(x: MissingType, x: MissingType) {}",
                ),
            ] {
                fs::write(root.join(name), source)?;
            }
            let output = root.join("reports");
            run(&root, &output)?;
            let files = fs::read_to_string(output.join("files.tsv"))?;
            for (name, stage) in [
                ("a.Script", "processed"),
                ("b.script", "processed"),
                ("empty.script", "processed"),
                ("syntax.script", "syntax"),
                ("capability.script", "capability"),
                ("validation.script", "validation"),
            ] {
                assert!(
                    files.lines().any(|line| {
                        let fields: Vec<_> = line.split('\t').collect();
                        fields[0] == name && fields[2] == stage
                    }),
                    "missing {name}/{stage}: {files}"
                );
            }
            let diagnostics = fs::read_to_string(output.join("diagnostics.tsv"))?;
            assert!(!diagnostics.contains("UNRESOLVED_FN"), "{diagnostics}");
            assert!(!diagnostics.contains("UNRESOLVED_TYPE"), "{diagnostics}");
            Ok(())
        })();
        fs::remove_dir_all(&root)?;
        result
    }

    #[test]
    fn locations_are_one_based_unicode_columns_and_groups_keep_counts() -> io::Result<()> {
        let sources = SourceMap::new();
        let id = sources.push_back("example.Script", "a\néx");
        let record = Record::new(
            &sources,
            "shared",
            "TEST",
            String::new(),
            "Error".into(),
            Span {
                file: id,
                start: 4,
                end: 5,
            },
            "test\nmessage".into(),
        )?;
        assert_eq!((record.line, record.column), (2, 2));
        let (diagnostics, aggregate, log) = reports(&[record]);
        assert!(diagnostics.contains("test\\nmessage"));
        assert!(aggregate.contains("\t1\t1\texample.Script:2:2\t"));
        assert!(log.contains("example.Script:2:2:"));
        Ok(())
    }
}
