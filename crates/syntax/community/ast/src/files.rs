use std::path::{Path, PathBuf};
use std::{fmt, fs, io};

use elsa::FrozenVec;

use crate::Span;
use crate::span::FileId;

#[derive(Default)]
pub struct SourceMap {
    files: StableDeque<File>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_files(it: impl IntoIterator<Item = impl Into<PathBuf>>) -> io::Result<Self> {
        let files = Self::new();
        for path in it {
            let path = path.into();
            let source = fs::read_to_string(&path)?;
            files.push_back(path, source);
        }
        Ok(files)
    }

    #[cfg(feature = "ignore")]
    pub fn from_paths_recursively(
        it: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> io::Result<Self> {
        use std::ffi::OsStr;

        let it = it.into_iter().flat_map(|path| {
            ignore::WalkBuilder::new(path.into())
                .follow_links(true)
                .build()
                .filter_map(Result::ok)
                .filter(|entry| entry.path().extension() == Some(OsStr::new("reds")))
                .map(ignore::DirEntry::into_path)
        });
        Self::from_files(it)
    }

    pub fn push_front(&self, path: impl Into<PathBuf>, source: impl Into<String>) -> FileId {
        FileId(self.files.push_front(File::new(path, source)))
    }

    pub fn push_back(&self, path: impl Into<PathBuf>, source: impl Into<String>) -> FileId {
        FileId(self.files.push_back(File::new(path, source)))
    }

    #[inline]
    pub fn get(&self, id: FileId) -> Option<&File> {
        self.files.get(id.0)
    }

    pub fn files(&self) -> impl Iterator<Item = (FileId, &File)> {
        self.files.iter().map(|(id, file)| (FileId(id), file))
    }

    pub fn len(&self) -> usize {
        self.files.len()
    }

    pub fn is_empty(&self) -> bool {
        self.files.len() == 0
    }

    pub fn display_at<'a>(&'a self, root: &'a Path) -> DisplaySourceMap<'a> {
        DisplaySourceMap { map: self, root }
    }
}

pub struct DisplaySourceMap<'a> {
    map: &'a SourceMap,
    root: &'a Path,
}

impl fmt::Display for DisplaySourceMap<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.map.files().enumerate().try_for_each(|(i, (_, file))| {
            if i > 0 {
                writeln!(f)?;
            }
            let path = file.path.strip_prefix(self.root).unwrap_or(&file.path);
            write!(f, "{}", path.display())
        })
    }
}

#[derive(Debug)]
pub struct File {
    path: PathBuf,
    source: String,
    lines: Vec<u32>,
}

impl File {
    pub fn new(path: impl Into<PathBuf>, source: impl Into<String>) -> Self {
        let mut lines = vec![];
        let source = source.into();
        for (offset, _) in source.match_indices('\n') {
            lines.push(offset as u32 + 1);
        }
        Self {
            path: path.into(),
            source,
            lines,
        }
    }

    #[inline]
    pub fn source(&self) -> &str {
        &self.source
    }

    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn lookup(&self, offset: u32) -> SourceLoc {
        let (line, line_offset) = self.line_and_offset(offset);
        SourceLoc {
            line,
            col: self.source[line_offset as usize..offset as usize]
                .chars()
                .count(),
        }
    }

    pub fn span_contents(&self, span: Span) -> &str {
        &self.source[span.start as usize..span.end as usize]
    }

    pub fn line_contents(&self, line: usize) -> Option<&str> {
        let start = if line == 0 {
            0
        } else {
            self.lines.get(line - 1).copied()?
        };
        let end = self.lines.get(line).copied().unwrap_or_else(|| {
            u32::try_from(self.source.len()).expect("source size should fit in u32")
        });
        Some(&self.source[start as usize..end as usize])
    }

    pub fn line_and_offset(&self, offset: u32) -> (usize, u32) {
        if self.lines.first().is_none_or(|&p| p > offset) {
            (0, 0)
        } else {
            let line = self
                .lines
                .binary_search(&offset)
                .map(|i| i + 1)
                .unwrap_or_else(|i| i);
            (line, self.lines[line - 1])
        }
    }
}

#[cfg(test)]
mod tests {
    use super::File;

    #[test]
    fn empty_source_location() {
        let file = File::new("empty.reds", "");

        assert_eq!(file.line_and_offset(0), (0, 0));
        assert_eq!(file.lookup(0).to_string(), "1:1");
        assert_eq!(file.line_contents(0), Some(""));
    }

    #[test]
    fn single_line_source_locations() {
        let file = File::new("single.reds", "aéz");

        for (offset, col) in [(0, 0), (1, 1), (3, 2), (4, 3)] {
            assert_eq!(file.line_and_offset(offset), (0, 0));
            let loc = file.lookup(offset);
            assert_eq!((loc.line, loc.col), (0, col));
            assert_eq!(loc.to_string(), format!("1:{}", col + 1));
        }
        assert_eq!(file.line_contents(0), Some("aéz"));
    }

    #[test]
    fn multiline_source_locations() {
        let file = File::new("multi.reds", "ab\nc");

        for (offset, expected) in [(0, (0, 0)), (2, (0, 0)), (3, (1, 3)), (4, (1, 3))] {
            assert_eq!(file.line_and_offset(offset), expected);
        }
        assert_eq!(file.lookup(4).to_string(), "2:2");
    }

    #[test]
    fn eof_after_trailing_newline() {
        let file = File::new("trailing.reds", "ab\n");

        assert_eq!(file.line_and_offset(3), (1, 3));
        assert_eq!(file.lookup(3).to_string(), "2:1");
        assert_eq!(file.line_contents(1), Some(""));
    }
}

#[derive(Debug, Clone)]
pub struct SourceLoc {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

struct StableDeque<T> {
    front: FrozenVec<Box<T>>,
    back: FrozenVec<Box<T>>,
}

impl<T> StableDeque<T> {
    pub fn get(&self, index: i32) -> Option<&T> {
        if index < 0 {
            self.front.get((-index - 1) as usize)
        } else {
            self.back.get(index as usize)
        }
    }

    fn push_front(&self, value: T) -> i32 {
        self.front.push(Box::new(value));
        -i32::try_from(self.front.len()).expect("deque size overflows i32")
    }

    fn push_back(&self, value: T) -> i32 {
        self.back.push(Box::new(value));
        i32::try_from(self.back.len()).expect("deque size overflows i32") - 1
    }

    pub fn len(&self) -> usize {
        self.front.len() + self.back.len()
    }

    fn iter(&self) -> impl Iterator<Item = (i32, &T)> {
        self.front
            .iter()
            .enumerate()
            .map(|(i, v)| (-(i as i32) - 1, v))
            .chain(self.back.iter().enumerate().map(|(i, v)| (i as i32, v)))
    }
}

impl<A> Default for StableDeque<A> {
    fn default() -> Self {
        Self {
            front: FrozenVec::default(),
            back: FrozenVec::default(),
        }
    }
}
