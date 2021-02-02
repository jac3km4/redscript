use core::fmt;
use std::cmp::Ordering;
use std::ops::{Range, Sub};
use std::path::PathBuf;

use redscript::ast::Pos;

pub struct Files {
    files: Vec<File>,
    sources: String,
}

impl Files {
    pub fn new() -> Files {
        Files {
            files: vec![],
            sources: String::new(),
        }
    }

    pub fn files(&self) -> &[File] {
        &self.files
    }

    pub fn sources(&self) -> &str {
        &self.sources
    }

    pub fn add(&mut self, path: PathBuf, source: &str) {
        let low = self.files.last().map(|f| f.high).unwrap_or(Pos(0));
        let high = low + source.len();
        let mut lines = vec![];
        for (offset, _) in source.match_indices('\n') {
            lines.push(low + offset + 1);
        }
        let file = File {
            path,
            lines: NonEmptyVec(low, lines),
            high,
        };
        self.sources.push_str(source);
        self.files.push(file)
    }

    pub fn enclosing_line(&self, loc: &Location) -> &str {
        loc.file.enclosing_line(loc.position.line, self.sources())
    }

    pub fn lookup(&self, pos: Pos) -> Option<Location> {
        let index = self
            .files
            .binary_search_by(|file| match file.span() {
                Span { low, .. } if low > pos => Ordering::Greater,
                Span { high, .. } if high < pos => Ordering::Less,
                _ => Ordering::Equal,
            })
            .ok()?;
        let file = self.files.get(index)?.clone();
        let line_col = file.lookup(pos, &self.sources)?;
        let result = Location {
            file,
            position: line_col,
        };
        Some(result)
    }
}

pub struct File {
    path: PathBuf,
    lines: NonEmptyVec<Pos>,
    high: Pos,
}

impl File {
    fn span(&self) -> Span {
        let low = self.lines.0;
        Span { low, high: self.high }
    }

    fn lookup(&self, pos: Pos, source: &str) -> Option<FilePosition> {
        let index = self.lines.1.binary_search(&pos).unwrap_or_else(|v| v - 1);
        let (line, low) = if pos < self.lines.0 {
            None?
        } else if index == 0 {
            (0, self.lines.0)
        } else {
            (index + 1, *self.lines.1.get(index)?)
        };
        let line_span = Span { low, high: pos };
        let col = self.source_slice(line_span, source).chars().count();
        let loc = FilePosition { line, col };
        Some(loc)
    }

    fn enclosing_line<'a>(&self, line: usize, source: &'a str) -> &'a str {
        let low = if line == 0 {
            self.lines.0
        } else {
            self.lines.1[line - 1]
        };
        let high = self.lines.1.get(line).cloned().unwrap_or(self.high);
        let span = Span { low, high };
        self.source_slice(span, source)
    }

    fn source_slice<'a>(&self, span: Span, source: &'a str) -> &'a str {
        let range: Range<usize> = (span - self.lines.0).into();
        &source[range]
    }
}

struct NonEmptyVec<A>(pub A, pub Vec<A>);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    low: Pos,
    high: Pos,
}

impl Sub<Pos> for Span {
    type Output = Span;
    fn sub(self, other: Pos) -> Span {
        Span {
            low: self.low - other,
            high: self.high - other,
        }
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        Range {
            start: self.low.0 as usize,
            end: self.high.0 as usize,
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

pub struct Location<'a> {
    pub file: &'a File,
    pub position: FilePosition,
}

impl<'a> fmt::Display for Location<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.file.path.display(), self.position)
    }
}
