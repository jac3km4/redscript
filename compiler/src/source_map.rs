use std::cmp::Ordering;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fmt;
use std::ops::Range;
use std::path::{Path, PathBuf};

use redscript::ast::Pos;
use redscript::error::Error;
use walkdir::WalkDir;

#[derive(Debug)]
pub struct Files {
    files: Vec<File>,
}

impl Files {
    pub fn new() -> Self {
        Files::default()
    }

    pub fn from_dir(dir: &Path, filter: SourceFilter) -> Result<Self, Error> {
        let iter = WalkDir::new(dir)
            .into_iter()
            .filter_map(Result::ok)
            .map(|entry| entry.into_path())
            .filter(|path| filter.apply(path.strip_prefix(dir).unwrap()));

        Files::from_files(iter)
    }

    pub fn from_files(paths: impl Iterator<Item = PathBuf>) -> Result<Self, Error> {
        let mut files = Files::new();
        for path in paths {
            let sources = std::fs::read_to_string(&path)?;
            files.add(path, sources);
        }
        Ok(files)
    }

    pub fn files(&self) -> impl Iterator<Item = &File> {
        self.files.iter()
    }

    pub fn add(&mut self, path: PathBuf, source: String) {
        let low = self.files.last().map(|f| f.high + 1).unwrap_or(Pos(0));
        let high = low + source.len();
        let mut lines = vec![];
        for (offset, _) in source.match_indices('\n') {
            lines.push(low + offset + 1);
        }
        let file = File {
            path,
            lines: NonEmptyVec(low, lines),
            high,
            source,
        };
        self.files.push(file)
    }

    pub fn lookup(&self, pos: Pos) -> Option<SourceLocation> {
        let index = self
            .files
            .binary_search_by(|file| match file.span() {
                Span { low, .. } if low > pos => Ordering::Greater,
                Span { high, .. } if high < pos => Ordering::Less,
                _ => Ordering::Equal,
            })
            .ok()?;
        let file = self.files.get(index)?;
        let position = file.lookup(pos)?;
        let result = SourceLocation { file, position };
        Some(result)
    }
}

impl fmt::Display for Files {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, file) in self.files.iter().enumerate() {
            if idx == self.files.len() - 1 {
                f.write_fmt(format_args!("{}", file))?;
            } else {
                f.write_fmt(format_args!("{}, ", file))?;
            }
        }
        Ok(())
    }
}

impl Default for Files {
    fn default() -> Self {
        Files { files: vec![] }
    }
}

#[derive(Debug)]
pub struct File {
    path: PathBuf,
    lines: NonEmptyVec<Pos>,
    high: Pos,
    source: String,
}

impl File {
    pub fn span(&self) -> Span {
        let low = self.lines.0;
        Span { low, high: self.high }
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    fn lookup(&self, pos: Pos) -> Option<FilePosition> {
        let res = self.lines.1.binary_search(&pos).map(|p| p + 1);
        let index = res.err().or_else(|| res.ok()).unwrap();
        let (line, low) = if pos < self.lines.0 {
            None?
        } else if index == 0 {
            (0, self.lines.0)
        } else {
            (index, *self.lines.1.get(index - 1)?)
        };
        let line_span = Span { low, high: pos };
        let col = self.source_slice(line_span).chars().count();
        let loc = FilePosition { line, col };
        Some(loc)
    }

    fn enclosing_line(&self, line: usize) -> &str {
        let low = if line == 0 {
            self.lines.0
        } else {
            self.lines.1[line - 1]
        };
        let high = self.lines.1.get(line).cloned().unwrap_or(self.high);
        let span = Span { low, high };
        self.source_slice(span)
    }

    fn source_slice(&self, span: Span) -> &str {
        let range: Range<usize> = span.into();
        &self.source[range]
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.path.display()))
    }
}

#[derive(Debug)]
struct NonEmptyVec<A>(pub A, pub Vec<A>);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    low: Pos,
    high: Pos,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        Range {
            start: span.low.0 as usize,
            end: span.high.0 as usize,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FilePosition {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

#[derive(Debug)]
pub struct SourceLocation<'a> {
    pub file: &'a File,
    pub position: FilePosition,
}

impl<'a> SourceLocation<'a> {
    pub fn enclosing_line(&self) -> &'a str {
        self.file.enclosing_line(self.position.line)
    }
}

impl<'a> fmt::Display for SourceLocation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.file.path.display(), self.position)
    }
}

pub enum SourceFilter {
    None,
    Exclude(HashSet<String>),
}

impl SourceFilter {
    fn apply(&self, rel_path: &Path) -> bool {
        let is_correct_extension = rel_path.extension() == Some(OsStr::new("reds"));
        let is_matching = match self {
            SourceFilter::None => true,
            SourceFilter::Exclude(exclusions) => {
                let without_ext = rel_path.with_extension("");
                let top_level_name = without_ext
                    .components()
                    .next()
                    .and_then(|comp| comp.as_os_str().to_str());
                match top_level_name {
                    Some(name) => !exclusions.contains(name),
                    None => false,
                }
            }
        };

        is_correct_extension && is_matching
    }
}
