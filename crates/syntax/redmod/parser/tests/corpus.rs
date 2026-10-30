//! Opt-in local corpus assessment. Never stores source text in the repository.
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::{fs, io};

use redscript_community_ast::FileId;
use redscript_redmod_parser::Error;

// Do not follow symlinks: avoid cycles and accidentally leaving the corpus root.
fn scripts(root: &Path) -> io::Result<Vec<PathBuf>> {
    let mut pending = vec![root.to_owned()];
    let mut files = Vec::new();
    while let Some(dir) = pending.pop() {
        let mut entries = fs::read_dir(dir)?.collect::<io::Result<Vec<_>>>()?;
        entries.sort_by_key(std::fs::DirEntry::file_name);
        for entry in entries {
            let kind = entry.file_type()?;
            let path = entry.path();
            if kind.is_dir() {
                pending.push(path);
            } else if kind.is_file()
                && path
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("script"))
            {
                files.push(path);
            }
        }
    }
    files.sort_by_cached_key(|path| relative_path(root, path));
    Ok(files)
}

fn relative_path(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap()
        .to_string_lossy()
        .replace('\\', "/")
}

fn cell(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
        .replace('\n', "\\n")
}

#[test]
fn discovery_is_case_insensitive_and_sorted() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../target/redmod-parser-corpus/discovery-test");
    if root.exists() {
        fs::remove_dir_all(&root).unwrap();
    }
    fs::create_dir_all(root.join("nested")).unwrap();
    for name in [
        "z.Script",
        "a.script",
        "nested/B.SCRIPT",
        "ignored.reds",
        "ignored.script.bak",
    ] {
        fs::write(root.join(name), "").unwrap();
    }
    fs::create_dir(root.join("directory.script")).unwrap();
    let discovered = scripts(&root)
        .unwrap()
        .iter()
        .map(|path| relative_path(&root, path))
        .collect::<Vec<_>>();
    fs::remove_dir_all(&root).unwrap();
    assert_eq!(discovered, ["a.script", "nested/B.SCRIPT", "z.Script"]);
}

#[derive(Default)]
struct DiagnosticGroup {
    count: usize,
    files: BTreeSet<String>,
}

#[test]
#[ignore = "requires a local REDmod corpus; set REDMOD_SCRIPTS_DIR (see README)"]
fn local_scripts_corpus() {
    let root = PathBuf::from(
        std::env::var_os("REDMOD_SCRIPTS_DIR")
            .expect("set REDMOD_SCRIPTS_DIR to the installed REDmod scripts directory"),
    );
    assert!(root.is_dir(), "not a directory: {}", root.display());
    let files = scripts(&root).expect("enumerate REDmod corpus");
    assert!(
        !files.is_empty(),
        "no case-insensitive .script files in {}",
        root.display()
    );
    // Fixed workspace target location, independent of the corpus path and cwd.
    let output =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../../target/redmod-parser-corpus");
    fs::create_dir_all(&output).expect("create corpus report directory");
    let mut inventory = String::from("path\tbytes\tstatus\tdiagnostics\n");
    let mut diagnostics = String::from("path\tkind\tstart\tend\tline\tcolumn\tmessage\n");
    let mut groups = BTreeMap::<(String, String), DiagnosticGroup>::new();
    let (mut bytes, mut clean, mut recovered, mut absent) = (0, 0, 0, 0);
    let (mut io_failures, mut panics, mut invalid_spans) = (0, 0, 0);
    let mut diagnostic_count = 0;
    let mut mixed_case_extensions = 0;
    for path in &files {
        let relative = relative_path(&root, path);
        let escaped = cell(&relative);
        mixed_case_extensions += usize::from(path.extension().unwrap() != "script");
        let source = match fs::read_to_string(path) {
            Ok(source) => source,
            Err(error) => {
                io_failures += 1;
                writeln!(inventory, "{escaped}\t0\tread-error\t0").unwrap();
                writeln!(
                    diagnostics,
                    "{escaped}\tio\t\t\t\t\t{}",
                    cell(&error.to_string())
                )
                .unwrap();
                continue;
            }
        };
        bytes += source.len();
        let result = std::panic::catch_unwind(|| {
            let (module, errors) =
                redscript_redmod_parser::parse_module(&source, FileId::from_i32(0));
            (module.is_some(), errors)
        });
        let (has_module, mut errors) = if let Ok(result) = result {
            result
        } else {
            panics += 1;
            writeln!(inventory, "{escaped}\t{}\tpanic\t0", source.len()).unwrap();
            continue;
        };
        let status = match (has_module, errors.is_empty()) {
            (true, true) => {
                clean += 1;
                "clean"
            }
            (true, false) => {
                recovered += 1;
                "recovered"
            }
            (false, _) => {
                absent += 1;
                "no-module"
            }
        };
        writeln!(
            inventory,
            "{escaped}\t{}\t{status}\t{}",
            source.len(),
            errors.len()
        )
        .unwrap();
        diagnostic_count += errors.len();
        errors
            .sort_by_cached_key(|error| (error.span().start, error.span().end, error.to_string()));
        for error in errors {
            let span = error.span();
            let kind = match error {
                Error::Lex(..) => "lex",
                Error::Parse(..) => "parse",
            };
            let message = error.to_string();
            let group = groups
                .entry((kind.to_owned(), message.clone()))
                .or_default();
            group.count += 1;
            group.files.insert(relative.clone());
            let valid = span.file == FileId::from_i32(0)
                && span.start <= span.end
                && source.get(span.start as usize..span.end as usize).is_some();
            let (line, column) = if valid {
                let prefix = &source[..span.start as usize];
                (
                    prefix.bytes().filter(|&b| b == b'\n').count() + 1,
                    prefix.rsplit('\n').next().unwrap().chars().count() + 1,
                )
            } else {
                invalid_spans += 1;
                (0, 0)
            };
            writeln!(
                diagnostics,
                "{escaped}\t{kind}\t{}\t{}\t{line}\t{column}\t{}",
                span.start,
                span.end,
                cell(&message)
            )
            .unwrap();
        }
    }
    let group_count = groups.len();
    let lex_count: usize = groups
        .iter()
        .filter(|((kind, _), _)| kind == "lex")
        .map(|(_, group)| group.count)
        .sum();
    let parse_count = diagnostic_count - lex_count;
    let mut aggregate = String::from("kind\tdiagnostics\tfiles\tmessage\texample_path\n");
    for ((kind, message), group) in groups {
        writeln!(
            aggregate,
            "{kind}\t{}\t{}\t{}\t{}",
            group.count,
            group.files.len(),
            cell(&message),
            cell(group.files.first().unwrap())
        )
        .unwrap();
    }
    let summary = format!(
        "# Local REDmod parser corpus\n\n\
        Files: {}\n\nBytes: {bytes}\n\nNon-lowercase extensions: {mixed_case_extensions}\n\n\
        Clean: {clean}\n\nRecovered with diagnostics: {recovered}\n\nNo module: {absent}\n\n\
        Diagnostics: {diagnostic_count} (lex: {lex_count}, parse: {parse_count}, groups: {group_count})\n\nRead failures: {io_failures}\n\nPanics: {panics}\n\nInvalid spans: {invalid_spans}\n\n\
        See files.tsv, diagnostics.tsv and aggregate.tsv. Paths are relative, spans are byte offsets,\n\
        lines/Unicode-scalar columns are 1-based (0 for invalid spans). TSV cells escape backslashes, tabs and newlines.\n\
        No source snippets are stored. Recovered ASTs are not clean parses; clean parses are not semantic validation.\n",
        files.len()
    );
    for (name, text) in [
        ("summary.md", summary.as_str()),
        ("files.tsv", &inventory),
        ("diagnostics.tsv", &diagnostics),
        ("aggregate.tsv", &aggregate),
    ] {
        fs::write(output.join(name), text).expect("write corpus report");
    }
    println!("{summary}\nReport: {}", output.display());
    assert_eq!(
        (io_failures, panics, invalid_spans),
        (0, 0, 0),
        "corpus infrastructure/robustness failures; see report"
    );
}
