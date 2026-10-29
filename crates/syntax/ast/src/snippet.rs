use std::fmt;

use unicode_width::UnicodeWidthStr;

use crate::{File, SourceLoc, Span};

/// A source excerpt whose underline uses terminal display columns, not source offsets.
#[derive(Debug)]
pub struct SourceSnippet<'a> {
    file: &'a File,
    start: SourceLoc,
    end: SourceLoc,
}

impl<'a> SourceSnippet<'a> {
    pub fn new(file: &'a File, span: Span) -> Option<Self> {
        file.source().get(span.start as usize..span.end as usize)?;
        Some(Self {
            file,
            start: file.lookup(span.start),
            end: file.lookup(span.end),
        })
    }

    pub fn start(&self) -> &SourceLoc {
        &self.start
    }
}

impl fmt::Display for SourceSnippet<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = self.file.line_contents(self.start.line).ok_or(fmt::Error)?;
        let line = line.trim_end_matches(['\r', '\n']);
        let gutter = (self.start.line + 1).to_string().len();
        writeln!(f, "{:gutter$} |", "")?;
        writeln!(
            f,
            "{:>gutter$} | {}",
            self.start.line + 1,
            expand_tabs(line)
        )?;
        let pad = display_column(line, self.start.col);
        let length = if self.start.line == self.end.line {
            display_column(line, self.end.col)
                .saturating_sub(pad)
                .max(1)
        } else {
            3
        };
        writeln!(f, "{:gutter$} | {:pad$}{:^>length$}", "", "", "")
    }
}

fn display_column(line: &str, column: usize) -> usize {
    let prefix = line.chars().take(column).collect::<String>();
    expand_tabs(&prefix).width()
}

fn expand_tabs(line: &str) -> String {
    const TAB_WIDTH: usize = 4;
    let mut expanded = String::new();
    let mut column = 0;
    for (index, segment) in line.split('\t').enumerate() {
        if index > 0 {
            let spaces = TAB_WIDTH - column % TAB_WIDTH;
            expanded.extend(std::iter::repeat_n(' ', spaces));
            column += spaces;
        }
        expanded.push_str(segment);
        column += segment.width();
    }
    expanded
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::FileId;

    fn render(source: &str, highlighted: &str) -> String {
        let file = File::new("test.reds", source);
        let start = source.find(highlighted).unwrap() as u32;
        SourceSnippet::new(
            &file,
            Span::new(start, start + highlighted.len() as u32, FileId::from_i32(0)),
        )
        .unwrap()
        .to_string()
    }

    #[test]
    fn expands_tabs_at_tab_stops() {
        assert_eq!(render("\tbad", "bad"), "  |\n1 |     bad\n  |     ^^^\n");
        assert_eq!(render("a\tbad", "bad"), "  |\n1 | a   bad\n  |     ^^^\n");
        assert_eq!(
            render("\t\tbad", "bad"),
            "  |\n1 |         bad\n  |         ^^^\n"
        );
    }

    #[test]
    fn aligns_wide_and_combining_characters() {
        assert_eq!(
            render("界😀e\u{301} bad", "bad"),
            "  |\n1 | 界😀e\u{301} bad\n  |       ^^^\n"
        );
        assert_eq!(
            render("a界e\u{301}", "界e\u{301}"),
            "  |\n1 | a界e\u{301}\n  |  ^^^\n"
        );
    }

    #[test]
    fn marks_zero_width_and_empty_source_spans() {
        assert_eq!(render("", ""), "  |\n1 | \n  | ^\n");
        let file = File::new("test.reds", "a\t");
        let span = Span::new(2, 2, FileId::from_i32(0));
        assert_eq!(
            SourceSnippet::new(&file, span).unwrap().to_string(),
            "  |\n1 | a   \n  |     ^\n"
        );
    }
}
