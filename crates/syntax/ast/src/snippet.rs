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

    fn write_line(
        &self,
        f: &mut fmt::Formatter<'_>,
        number: usize,
        last_line: usize,
        gutter: usize,
    ) -> fmt::Result {
        let line = self.file.line_contents(number).ok_or(fmt::Error)?;
        let line = line.trim_end_matches(['\r', '\n']);
        writeln!(f, "{:>gutter$} | {}", number + 1, expand_tabs(line))?;
        let pad = if number == self.start.line {
            display_column(line, self.start.col)
        } else {
            0
        };
        let end = if number == self.end.line {
            display_column(line, self.end.col)
        } else {
            expand_tabs(line).width()
        };
        let length = end.saturating_sub(pad).max(1);
        let continuation = if number < last_line { " ..." } else { "" };
        writeln!(
            f,
            "{:gutter$} | {:pad$}{:^>length$}{continuation}",
            "", "", ""
        )
    }
}

impl fmt::Display for SourceSnippet<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Spans are half-open: ending at column zero does not include that line.
        let last_line = if self.end.line > self.start.line && self.end.col == 0 {
            self.end.line - 1
        } else {
            self.end.line
        };
        let gutter = (last_line + 1).to_string().len();
        writeln!(f, "{:gutter$} |", "")?;
        self.write_line(f, self.start.line, last_line, gutter)?;
        if last_line > self.start.line {
            if last_line > self.start.line + 1 {
                writeln!(f, "{:gutter$} | ...", "")?;
            }
            self.write_line(f, last_line, last_line, gutter)?;
        }
        Ok(())
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
    use crate::SourceMap;

    fn render(source: &str, highlighted: &str) -> String {
        let files = SourceMap::new();
        let file_id = files.push_back("test.reds", source);
        let start = source.find(highlighted).unwrap() as u32;
        SourceSnippet::new(
            files.get(file_id).unwrap(),
            Span::new(start, start + highlighted.len() as u32, file_id),
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
    fn multiline_spans_show_endpoints_and_ellipsis() {
        assert_eq!(
            render("abc\nmiddle\nxyz", "bc\nmiddle\nxy"),
            "  |\n1 | abc\n  |  ^^ ...\n  | ...\n3 | xyz\n  | ^^\n"
        );
        assert_eq!(
            render("a\nb", "a\nb"),
            "  |\n1 | a\n  | ^ ...\n2 | b\n  | ^\n"
        );
    }

    #[test]
    fn half_open_multiline_end_excludes_next_line() {
        assert_eq!(render("abc\nnext", "bc\n"), "  |\n1 | abc\n  |  ^^\n");
        assert_eq!(
            render("abc\nmiddle\nnext", "bc\nmiddle\n"),
            "  |\n1 | abc\n  |  ^^ ...\n2 | middle\n  | ^^^^^^\n"
        );
    }

    #[test]
    fn multiline_crlf_and_gutters() {
        let source = format!("{}a\r\nb\r\nc", "\n".repeat(8));
        assert_eq!(
            render(&source, "a\r\nb\r\nc"),
            "   |\n 9 | a\n   | ^ ...\n   | ...\n11 | c\n   | ^\n"
        );
    }

    #[test]
    fn marks_zero_width_and_empty_source_spans() {
        assert_eq!(render("", ""), "  |\n1 | \n  | ^\n");
        let files = SourceMap::new();
        let file_id = files.push_back("test.reds", "a\t");
        let span = Span::new(2, 2, file_id);
        assert_eq!(
            SourceSnippet::new(files.get(file_id).unwrap(), span)
                .unwrap()
                .to_string(),
            "  |\n1 | a   \n  |     ^\n"
        );
    }
}
