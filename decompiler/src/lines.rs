use std::cell::Cell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Default, Clone)]
pub struct LineCount(Rc<Cell<u32>>);

impl LineCount {
    pub(crate) fn get_cell(&self) -> &Cell<u32> {
        &self.0
    }

    pub fn wrap_fmt<W: fmt::Write>(self, writer: W) -> LineCounter<W> {
        LineCounter {
            current_line: self.0,
            line_indices: vec![],
            inner: writer,
        }
    }
}

#[derive(Debug)]
pub struct LineCounter<W> {
    current_line: Rc<Cell<u32>>,
    line_indices: Vec<u32>,
    inner: W,
}

impl<W> LineCounter<W> {
    pub fn new(inner: W) -> Self {
        Self {
            current_line: Rc::default(),
            line_indices: vec![],
            inner,
        }
    }

    pub fn into_line_indices(self) -> Vec<u32> {
        self.line_indices
    }
}

impl<W: fmt::Write> fmt::Write for LineCounter<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for _ in s.matches('\n') {
            self.line_indices.push(self.current_line.get());
        }
        self.inner.write_str(s)
    }
}
