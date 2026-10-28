mod lexer;
pub mod parser;

use std::fmt;

use chumsky::prelude::*;
pub use lexer::Token;
use parser::{Parse, ParserInput};
use redscript_ast::{
    self as ast, Dialect, FileId, SourceExpr, SourceItem, SourceItemDecl, SourceLoc, SourceMap,
    SourceModule, SourceStmt, Span, Spanned,
};

pub type ParseResult<A> = (Option<A>, Vec<Error>);

/// Picks one of two parsers built for different dialects or modes with static dispatch, so that
/// the community parser pays no indirection for the alternatives (unlike `boxed`).
pub(crate) trait Branch: Sized {
    #[inline]
    fn left_branch<R>(self) -> either::Either<Self, R> {
        either::Either::Left(self)
    }

    #[inline]
    fn right_branch<L>(self) -> either::Either<L, Self> {
        either::Either::Right(self)
    }
}

impl<T> Branch for T {}

/// Options controlling how source code is lexed and parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ParseOptions {
    /// The dialect of the source code.
    pub dialect: Dialect,
    /// Accepts the syntax of redscript 0.5.x on top of the community dialect: `///` comments
    /// anywhere, `as` in the postfix chain, right-associative assignments, qualifiers in any
    /// order, keywords used as names and the more permissive number literals of 0.5.x.
    /// It has no effect on the REDmod dialect.
    pub compat: bool,
}

impl ParseOptions {
    /// Options for the given dialect, without the 0.5.x compatibility mode.
    pub const fn new(dialect: Dialect) -> Self {
        Self {
            dialect,
            compat: false,
        }
    }

    /// Options for the given dialect, with the 0.5.x compatibility mode enabled for the
    /// community dialect.
    pub const fn compat(dialect: Dialect) -> Self {
        Self {
            dialect,
            compat: true,
        }
    }

    /// Whether the 0.5.x compatibility mode is in effect.
    pub(crate) fn is_compat(self) -> bool {
        self.compat && self.dialect == Dialect::Community
    }
}

impl From<Dialect> for ParseOptions {
    fn from(dialect: Dialect) -> Self {
        Self::new(dialect)
    }
}

// this can't be written as a generic function due to a GAT bug
macro_rules! parse {
    ($src:expr, $parser:expr, $ctx:expr) => {
        parse!($src, $parser, $ctx, ParseOptions::default())
    };
    ($src:expr, $parser:expr, $ctx:expr, $opts:expr) => {{
        let (toks, mut errs) = lex_with_options($src, $ctx, $opts.into());
        if let Some(toks) = toks {
            let (res, e) = parse($parser, &toks, $ctx);
            errs.extend(e);
            (res, errs)
        } else {
            (None, errs)
        }
    }};
}

pub fn parse_modules<'a>(
    it: impl IntoIterator<Item = (FileId, &'a str)>,
) -> (Vec<SourceModule<'a>>, Vec<Error>) {
    let mut errs = vec![];
    let toks = it
        .into_iter()
        .filter_map(|(file, src)| {
            let (t, e) = lex(src, file);
            errs.extend(e);
            Some((file, t?))
        })
        .collect::<Vec<_>>();

    let parser = parser::module();
    let modules = toks
        .iter()
        .filter_map(|(file, toks)| {
            let (res, e) = parse(parser.clone(), toks, *file);
            errs.extend(e);
            res
        })
        .collect::<Vec<_>>();
    (modules, errs)
}

pub fn parse_module(src: &str, file: FileId) -> ParseResult<SourceModule<'_>> {
    parse!(src, parser::module(), file)
}

pub fn parse_module_with(
    src: &str,
    file: FileId,
    dialect: Dialect,
) -> ParseResult<SourceModule<'_>> {
    parse!(src, parser::module_with(dialect), file, dialect)
}

pub fn parse_module_with_options(
    src: &str,
    file: FileId,
    opts: ParseOptions,
) -> ParseResult<SourceModule<'_>> {
    parse!(src, parser::module_with_options(opts), file, opts)
}

pub fn parse_item_decl(src: &str, file: FileId) -> ParseResult<SourceItemDecl<'_>> {
    parse!(src, parser::item_decl(), file)
}

pub fn parse_item_decl_with(
    src: &str,
    file: FileId,
    dialect: Dialect,
) -> ParseResult<SourceItemDecl<'_>> {
    parse!(src, parser::item_decl_with(dialect), file, dialect)
}

pub fn parse_item_decl_with_options(
    src: &str,
    file: FileId,
    opts: ParseOptions,
) -> ParseResult<SourceItemDecl<'_>> {
    parse!(src, parser::item_decl_with_options(opts), file, opts)
}

pub fn parse_item(src: &str, file: FileId) -> ParseResult<SourceItem<'_>> {
    parse!(src, parser::item(), file)
}

pub fn parse_item_with(src: &str, file: FileId, dialect: Dialect) -> ParseResult<SourceItem<'_>> {
    parse!(src, parser::item_with(dialect), file, dialect)
}

pub fn parse_stmt(src: &str, file: FileId) -> ParseResult<SourceStmt<'_>> {
    parse!(src, parser::stmt(), file)
}

pub fn parse_stmt_with(src: &str, file: FileId, dialect: Dialect) -> ParseResult<SourceStmt<'_>> {
    parse!(src, parser::stmt_with(dialect), file, dialect)
}

pub fn parse_stmt_with_options(
    src: &str,
    file: FileId,
    opts: ParseOptions,
) -> ParseResult<SourceStmt<'_>> {
    parse!(src, parser::stmt_with_options(opts), file, opts)
}

pub fn parse_expr(src: &str, file: FileId) -> ParseResult<SourceExpr<'_>> {
    parse!(src, parser::expr(), file)
}

pub fn parse_expr_with(src: &str, file: FileId, dialect: Dialect) -> ParseResult<SourceExpr<'_>> {
    parse!(src, parser::expr_with(dialect), file, dialect)
}

pub fn parse_expr_with_options(
    src: &str,
    file: FileId,
    opts: ParseOptions,
) -> ParseResult<SourceExpr<'_>> {
    parse!(src, parser::expr_with_options(opts), file, opts)
}

pub fn lex(src: &str, f: FileId) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_with_dialect(src, f, Dialect::Community)
}

pub fn lex_with_dialect(
    src: &str,
    f: FileId,
    dialect: Dialect,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_internal(src, f, false, dialect.into())
}

pub fn lex_with_options(
    src: &str,
    f: FileId,
    opts: ParseOptions,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_internal(src, f, false, opts)
}

pub fn lex_with_lf_and_comments(
    src: &str,
    f: FileId,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_with_lf_and_comments_and_dialect(src, f, Dialect::Community)
}

pub fn lex_with_lf_and_comments_and_dialect(
    src: &str,
    f: FileId,
    dialect: Dialect,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_internal(src, f, true, dialect.into())
}

pub fn lex_with_lf_and_comments_and_options(
    src: &str,
    f: FileId,
    opts: ParseOptions,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_internal(src, f, true, opts)
}

fn lex_internal(
    src: &str,
    file_id: FileId,
    keep_lf_and_comments: bool,
    opts: ParseOptions,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    let (output, errors) = lexer::lex(keep_lf_and_comments, opts.dialect, opts.is_compat())
        .parse(src)
        .into_output_errors();
    let errors = errors
        .into_iter()
        .map(|err| {
            let span = Span::from((file_id, *err.span()));
            Error::Lex(FormattedError(err).to_string(), span)
        })
        .collect();
    let Some(mut tokens) = output else {
        return (None, errors);
    };
    if opts.is_compat() {
        lexer::demote_doc_comments(&mut tokens);
        if !keep_lf_and_comments {
            tokens.retain(|(tok, _)| !tok.is_ws_or_comment());
        }
        lexer::split_generic_ge(&mut tokens);
    }

    let output = tokens
        .into_iter()
        .map(|(tok, span)| {
            let span = Span::from((file_id, span));
            let tok = tok.map_span(span, |s| Span::from((file_id, s)));
            (tok, span)
        })
        .collect();
    (Some(output), errors)
}

pub fn parse<'tok, 'src: 'tok, A>(
    parser: impl Parse<'tok, 'src, A>,
    tokens: &'tok [(Token<'src>, Span)],
    file: FileId,
) -> ParseResult<A> {
    let parser: &dyn Parser<'tok, _, A, extra::Err<_>> = &parser.with_ctx(file);
    let (output, errors) = parser
        .parse(parser_input(tokens, file))
        .into_output_errors();
    let errors = errors.into_iter().map(Error::new_parse).collect();
    (output, errors)
}

fn parser_input<'tok, 'src>(
    tokens: &'tok [(Token<'src>, Span)],
    file: FileId,
) -> ParserInput<'tok, 'src> {
    let max = tokens.last().map(|(_, span)| span.end()).unwrap_or(0);
    tokens.spanned(Span {
        start: max,
        end: max,
        file,
    })
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    Parse(String, Span),
    Lex(String, Span),
}

impl Error {
    fn new_parse<T>(error: chumsky::error::Rich<'_, T, Span>) -> Self
    where
        T: fmt::Display,
    {
        let span = *error.span();
        Error::Parse(FormattedError(error).to_string(), span)
    }

    pub fn display<'a>(&'a self, sources: &'a SourceMap) -> Option<impl fmt::Display + use<'a>> {
        let span = self.span();
        let file = sources.get(span.file)?;
        let start = file.lookup(span.start);
        let end = file.lookup(span.end);
        let line = file.line_contents(start.line)?;
        Some(ErrorDisplay {
            file,
            start,
            end,
            line,
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

struct FormattedError<'a, T, S>(chumsky::error::Rich<'a, T, S>);

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
    start: SourceLoc,
    end: SourceLoc,
    line: &'a str,
    err: &'a Error,
}

impl fmt::Display for ErrorDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "At {}:{}:{}",
            self.file.path().display(),
            self.start.line + 1,
            self.start.col + 1
        )?;

        writeln!(f, "{}", self.line.trim_end())?;

        let pad = self.start.col;
        let underline_len = if self.start.line == self.end.line {
            (self.end.col - self.start.col).max(1)
        } else {
            3
        };
        writeln!(f, "{:>pad$}{:^>underline_len$}", "", "")?;
        writeln!(f, "{}", self.err)
    }
}

#[cfg(test)]
mod compat_tests;
#[cfg(test)]
mod redmod_tests;
