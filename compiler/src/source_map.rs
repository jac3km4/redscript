use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::{fmt, io, iter};

use itertools::{Either, Itertools};
use redscript::ast::{Pos, Span};
use walkdir::{DirEntry, WalkDir};

use crate::diagnostics::DisplayFn;

#[derive(Debug, Default)]
pub struct Files {
    entries: Vec<File>,
}

impl Files {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_dirs(paths: impl IntoIterator<Item = impl AsRef<Path>>) -> io::Result<Self> {
        let files = paths.into_iter().flat_map(|path| dir_file_iter(path.as_ref()));
        Self::from_files(files)
    }

    pub fn from_dir(path: impl AsRef<Path>) -> io::Result<Self> {
        Self::from_files(dir_file_iter(path.as_ref()))
    }

    pub fn from_files(paths: impl IntoIterator<Item = PathBuf>) -> io::Result<Self> {
        let mut files = Self::new();
        for path in paths {
            let sources = std::fs::read_to_string(&path)?;
            files.add(path, sources);
        }
        Ok(files)
    }

    pub fn files(&self) -> impl Iterator<Item = &File> {
        self.entries.iter()
    }

    pub fn add(&mut self, path: PathBuf, source: String) {
        let low = self.entries.last().map_or(Pos::ZERO, |f| f.high + 1);
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
        self.entries.push(file);
    }

    pub fn lookup_file(&self, pos: Pos) -> Option<&File> {
        let index = self
            .entries
            .binary_search_by(|file| file.span().compare_pos(pos))
            .ok()?;
        self.entries.get(index)
    }

    pub fn lookup(&self, span: Span) -> Option<SourceLoc<'_>> {
        let file = self.lookup_file(span.low)?;
        let start = file.lookup(span.low)?;
        let end = file.lookup(span.high)?;
        let result = SourceLoc { file, start, end };
        Some(result)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn display<'a>(&'a self, root: &'a Path) -> impl fmt::Display + 'a {
        DisplayFn::new(move |f: &mut fmt::Formatter<'_>| {
            write!(
                f,
                "{}",
                self.entries
                    .iter()
                    .map(|entry| entry.path.strip_prefix(root).unwrap_or(&entry.path).display())
                    .format("\n")
            )
        })
    }
}

fn dir_file_iter(path: &Path) -> impl Iterator<Item = PathBuf> + use<> {
    if path.is_file() {
        Either::Left(iter::once(path.to_path_buf()))
    } else {
        let iter = WalkDir::new(path)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|entry| entry.path().extension() == Some(OsStr::new("reds")))
            .map(DirEntry::into_path);
        Either::Right(iter)
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

    pub fn byte_offset(&self) -> Pos {
        self.lines.0
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn with_source(self, source: String) -> Self {
        Self { source, ..self }
    }

    pub fn lookup(&self, pos: Pos) -> Option<FilePos> {
        let res = self.lines.1.binary_search(&pos).map(|p| p + 1);
        let index = res.unwrap_or_else(|i| i);
        let (line, low) = if pos < self.lines.0 {
            return None;
        } else if index == 0 {
            (0, self.lines.0)
        } else {
            (index, *self.lines.1.get(index - 1)?)
        };
        let line_span = Span { low, high: pos };
        let col = self.source_slice(line_span).chars().count();
        let loc = FilePos { line, col };
        Some(loc)
    }

    pub fn enclosing_line(&self, line: usize) -> &str {
        let low = if line == 0 {
            self.lines.0
        } else {
            self.lines.1[line - 1]
        };
        let high = self.lines.1.get(line).copied().unwrap_or(self.high);
        let span = Span { low, high };
        self.source_slice(span)
    }

    pub fn source_slice(&self, span: Span) -> &str {
        let start = usize::from(span.low) - usize::from(self.byte_offset());
        let end = usize::from(span.high) - usize::from(self.byte_offset());
        &self.source[start..end]
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Default for File {
    fn default() -> Self {
        Self {
            path: PathBuf::new(),
            lines: NonEmptyVec(Pos::ZERO, vec![]),
            high: Pos::ZERO,
            source: String::new(),
        }
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
pub struct FilePos {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for FilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

#[derive(Debug)]
pub struct SourceLoc<'a> {
    pub file: &'a File,
    pub start: FilePos,
    pub end: FilePos,
}

impl<'a> SourceLoc<'a> {
    pub fn enclosing_line(&self) -> &'a str {
        self.file.enclosing_line(self.start.line)
    }
}

impl fmt::Display for SourceLoc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.file.path.display(), self.start)
    }
}
