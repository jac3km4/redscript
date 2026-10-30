//! Diagnostic representation and rendering shared by syntax parsers.

use std::fmt;

use redscript_community_ast::{self as ast, SourceMap, Span};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    Parse(String, Span),
    Lex(String, Span),
}

impl Error {
    pub fn new_parse<T>(error: chumsky::error::Rich<'_, T, Span>) -> Self
    where
        T: fmt::Display,
    {
        let span = *error.span();
        Error::Parse(FormattedError(error).to_string(), span)
    }

    pub fn display<'a>(&'a self, sources: &'a SourceMap) -> Option<impl fmt::Display + use<'a>> {
        let span = self.span();
        let file = sources.get(span.file)?;
        let snippet = ast::SourceSnippet::new(file, span)?;
        Some(ErrorDisplay {
            file,
            snippet,
            err: self,
        })
    }

    pub fn span(&self) -> Span {
        match self {
            Error::Parse(_, span) | Error::Lex(_, span) => *span,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Parse(msg, _) | Error::Lex(msg, _) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for Error {}

pub struct FormattedError<'a, T, S>(pub chumsky::error::Rich<'a, T, S>);

impl<T: fmt::Display, S> fmt::Display for FormattedError<'_, T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.reason())?;
        self.0
            .contexts()
            .try_for_each(|(label, _)| write!(f, " in {label}"))
    }
}

#[derive(Debug)]
struct ErrorDisplay<'a> {
    file: &'a ast::File,
    snippet: ast::SourceSnippet<'a>,
    err: &'a Error,
}

impl fmt::Display for ErrorDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start = self.snippet.start();
        writeln!(f, "error: {}", self.err)?;
        writeln!(
            f,
            " --> {}:{}:{}",
            self.file.path().display(),
            start.line + 1,
            start.col + 1
        )?;

        write!(f, "{}", self.snippet)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn renders_both_error_kinds_and_validates_source_spans() {
        let sources = SourceMap::new();
        let file = sources.push_back("test.script", "\t猫 bad\r\n");
        let span = Span::new(5, 8, file);
        for error in [
            Error::Lex("bad token".into(), span),
            Error::Parse("bad token".into(), span),
        ] {
            assert_eq!(error.span(), span);
            assert_eq!(error.to_string(), "bad token");
            assert_eq!(
                error.display(&sources).unwrap().to_string(),
                "error: bad token\n --> test.script:1:4\n  |\n1 |     猫 bad\n  |        ^^^\n"
            );
        }
        for (start, end) in [(2, 3), (8, 5), (0, 100)] {
            assert!(
                Error::Parse("bad".into(), Span::new(start, end, file))
                    .display(&sources)
                    .is_none()
            );
        }
    }
}
