use std::{fmt, ops};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
    pub file: FileId,
}

impl Span {
    pub fn new(start: u32, end: u32, file: FileId) -> Self {
        Span { start, end, file }
    }

    pub fn contains(&self, pos: u32) -> bool {
        self.start <= pos && pos < self.end
    }

    pub fn cmp_pos(&self, pos: u32) -> std::cmp::Ordering {
        if self.start > pos {
            std::cmp::Ordering::Greater
        } else if self.end <= pos {
            std::cmp::Ordering::Less
        } else {
            std::cmp::Ordering::Equal
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        assert_eq!(self.file, other.file);
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            file: self.file,
        }
    }
}

impl ops::Add<Span> for Span {
    type Output = Span;

    fn add(self, other: Span) -> Self::Output {
        assert_eq!(self.file, other.file);
        Self {
            start: self.start + other.start,
            end: self.start + other.end,
            file: self.file,
        }
    }
}

impl ops::Add<u8> for Span {
    type Output = Span;

    fn add(self, other: u8) -> Self::Output {
        Self {
            start: self.start + other as u32,
            end: self.end + other as u32,
            file: self.file,
        }
    }
}

#[cfg(feature = "chumsky")]
impl From<(FileId, chumsky::span::SimpleSpan)> for Span {
    #[inline]
    fn from((file, sp): (FileId, chumsky::span::SimpleSpan)) -> Self {
        Span {
            start: sp.start as u32,
            end: sp.end as u32,
            file,
        }
    }
}

impl From<(FileId, std::ops::Range<u32>)> for Span {
    #[inline]
    fn from((file, range): (FileId, std::ops::Range<u32>)) -> Self {
        Span {
            start: range.start,
            end: range.end,
            file,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{} in file {}", self.start, self.end, self.file.0)
    }
}

#[cfg(feature = "chumsky")]
impl chumsky::span::Span for Span {
    type Context = FileId;
    type Offset = u32;

    #[inline]
    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Span {
            start: range.start,
            end: range.end,
            file: context,
        }
    }

    #[inline]
    fn context(&self) -> Self::Context {
        self.file
    }

    #[inline]
    fn start(&self) -> Self::Offset {
        self.start
    }

    #[inline]
    fn end(&self) -> Self::Offset {
        self.end
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub(super) i32);

impl FileId {
    #[cfg(feature = "testing")]
    pub fn from_i32(id: i32) -> Self {
        Self(id)
    }
}
