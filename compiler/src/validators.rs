use std::cell::RefCell;
use std::fmt::Display;
use std::ops::Range;

use redscript::Str;
use strum::{Display, IntoStaticStr};

use crate::lexer::Token;

pub type Span<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;

pub trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

impl<'a> ToRange for Span<'a> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

impl<'a> ToRange for Token<'a> {
    fn to_range(&self) -> Range<usize> {
        match self {
            Token::Trivia(r, _) => r.to_range(),
            Token::Num(r, _) => r.to_range(),
            Token::Str(r, _, _) => r.to_range(),
            Token::StrIs(r, _, _) => r.to_range(),
            Token::StrIe(r, _) => r.to_range(),
            Token::StrIp(r, _) => r.to_range(),
            Token::Null(r) => r.to_range(),
            Token::Bool(r, _) => r.to_range(),
            Token::Op(r, _) => r.to_range(),
            Token::Ctrl(r, _) => r.to_range(),
            Token::Ident(r) => r.to_range(),
            Token::Kw(r, _) => r.to_range(),
        }
    }
}

/// Error containing a text span and an error message to display.
#[derive(Debug)]
pub struct Diagnostic {
    sl: usize,
    sc: usize,
    el: usize,
    ec: usize,
    file: Str,
    text: String,
    severity: Severity,
    code: &'static str,
    msg: String,
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}[{}]: {}", self.severity.as_str(), self.code, self.msg)?;
        writeln!(
            f,
            "--> {}:{}:{} to l{}:{}",
            self.file, self.sl, self.sc, self.el, self.ec
        )?;
        for line in self.text.lines() {
            writeln!(f, "| {}", line)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Display, IntoStaticStr)]
#[strum(serialize_all = "snake_case")]
pub enum Severity {
    Error,
    Warn,
    Info,
    Hint,
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        self.into()
    }
}

/// Carried around in the `LocatedSpan::extra` field in
/// between `nom` parsers.
#[derive(Clone, Debug)]
pub struct State<'a>(pub &'a RefCell<Vec<Diagnostic>>, pub Str);

impl<'a> State<'a> {
    /// Something not allowed by the rules of the language or other authority.
    #[allow(dead_code)]
    fn report_diagnostic(&self, error: Diagnostic) {
        self.0.borrow_mut().push(error);
    }
}

pub trait ReportOrigin {
    fn report(&self, diag: &'static DiagnosticTemplate, msg: String);
}

impl<'a> ReportOrigin for Span<'a> {
    /// Reports the entire fragment as faulty.
    /// Does not terminate the compilation.
    fn report(&self, diag: &'static DiagnosticTemplate, msg: String) {
        let bytes = std::str::from_utf8(&self.get_line_beginning()).unwrap_or_default();
        let line = self.location_line() as usize;
        let sc = self.location_offset();
        let ec = sc + self.fragment().len();
        self.extra.report_diagnostic(Diagnostic {
            sl: line,
            sc,
            el: line,
            ec,
            file: self.extra.1.clone(),
            text: bytes.to_string(),
            severity: diag.0,
            code: diag.1,
            msg,
        })
    }
}

impl<'a> ReportOrigin for Range<&Span<'a>> {
    /// reports the range from the start of the first span to the end of the last span.
    /// The first span must contain the last span.
    /// This is archived, by cloning the first span, before consuming.
    fn report(&self, diag: &'static DiagnosticTemplate, msg: String) {
        let sl = self.start.location_line() as usize;
        let el = self.end.location_line() as usize;
        let bytes = self
            .start
            .get(self.start.location_offset()..self.end.location_offset())
            .unwrap_or_else(|| std::str::from_utf8(&self.start.get_line_beginning()).unwrap_or_default());
        self.start.extra.report_diagnostic(Diagnostic {
            sl,
            sc: self.start.get_column(),
            el,
            ec: self.end.get_column(),
            file: self.start.extra.1.clone(),
            text: bytes.to_string(),
            severity: diag.0,
            code: diag.1,
            msg,
        })
    }
}

/// A diagnostic message in the format `Severity, Code, Message format`.
pub struct DiagnosticTemplate(Severity, &'static str, &'static str);

/// Reports a diagnostic message.
/// Requires the crate level imported, because format macros are defined there.
/// ```ignore
/// use crate::*;
/// ```
#[macro_export]
macro_rules! diag_report {
    ($origin:expr, $name:ident, $($arg:tt)*) =>{
        paste::paste! {
            $origin.report(&$name, [<format_ $name>]!($($arg)*))
        }
    };
}

/// A diagnostic message in the format `Severity, Code, Message format`.
/// The code is a key constructed from the initials and a id.
/// The message format is a `format!` string.
///
/// ## Error Code
/// - The first char represents the severity: [E]rror, [W]arn, [I]nfo, [H]int.
/// - The second char represents the source: [L]lexical, S[Y}ntax, [S]emantic, [T]ype, etc.
/// - The third char represents a category. This can really be whatever. It is used to group similar errors.
///
/// ## Example
/// ```ignore
/// diag!(ERR_INVALID_UTF8, Error, "ELS0001", "Invalid UTF-8 sequence `{}`");
/// ```
macro_rules! diag {
    ($name:ident, $severity:ident, $code:tt, $msg:tt) => {
        paste::paste! {
            #[allow(dead_code)]
            pub const $name: DiagnosticTemplate = DiagnosticTemplate(Severity::$severity, $code, $msg);

            // Formats the arguments with the arguments with the msg of the diagnostic.
            #[allow(dead_code)]
            #[macro_export]
            macro_rules! [<format_ $name>] {
                ($$($$a:tt)*) => {
                    format!($msg, $$($a)*)
                };
            }
        }
    };
}

diag!(ERR_INVALID_UTF8, Error, "ELS0001", "Invalid UTF-8 sequence `{}`");
diag!(ERR_EXPECT_HEX_DIGIT, Error, "ELS0002", "Invalid hex digit `{}`");
diag!(ERR_INVALID_ESCAPE, Error, "ELS0003", "Invalid escape sequence `{}`");
diag!(ERR_PARSE_INT, Error, "ELN0001", "Invalid integer `{}`, {}");
diag!(ERR_PARSE_FLOAT, Error, "ELN0002", "Invalid float `{}`, {}");
