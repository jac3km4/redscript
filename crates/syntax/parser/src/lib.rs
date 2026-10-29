mod lexer;
pub mod parser;

use std::fmt;

use chumsky::prelude::*;
pub use lexer::Token;
use parser::{Parse, ParserInput};
use redscript_ast::{
    self as ast, FileId, SourceExpr, SourceItem, SourceItemDecl, SourceMap, SourceModule,
    SourceStmt, Span, Spanned,
};

pub type ParseResult<A> = (Option<A>, Vec<Error>);

// this can't be written as a generic function due to a GAT bug
macro_rules! parse {
    ($src:expr, $parser:expr, $ctx:expr) => {{
        let (toks, mut errs) = lex($src, $ctx);
        if let Some(toks) = toks {
            let (res, e) = parse_with_eof($parser, &toks, $ctx, $src.len() as u32);
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
            Some((file, t?, src.len() as u32))
        })
        .collect::<Vec<_>>();

    let parser = parser::module();
    let modules = toks
        .iter()
        .filter_map(|(file, toks, eof)| {
            let (res, e) = parse_with_eof(parser.clone(), toks, *file, *eof);
            errs.extend(e);
            res
        })
        .collect::<Vec<_>>();
    (modules, errs)
}

pub fn parse_module(src: &str, file: FileId) -> ParseResult<SourceModule<'_>> {
    parse!(src, parser::module(), file)
}

pub fn parse_item_decl(src: &str, file: FileId) -> ParseResult<SourceItemDecl<'_>> {
    parse!(src, parser::item_decl(), file)
}

pub fn parse_item(src: &str, file: FileId) -> ParseResult<SourceItem<'_>> {
    parse!(src, parser::item(), file)
}

pub fn parse_stmt(src: &str, file: FileId) -> ParseResult<SourceStmt<'_>> {
    parse!(src, parser::stmt(), file)
}

pub fn parse_expr(src: &str, file: FileId) -> ParseResult<SourceExpr<'_>> {
    parse!(src, parser::expr(), file)
}

pub fn lex(src: &str, f: FileId) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_internal(src, f, false)
}

pub fn lex_with_lf_and_comments(
    src: &str,
    f: FileId,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    lex_internal(src, f, true)
}

fn lex_internal(
    src: &str,
    file_id: FileId,
    keep_lf_and_comments: bool,
) -> ParseResult<Vec<Spanned<Token<'_, Span>>>> {
    let (output, errors) = lexer::lex(keep_lf_and_comments)
        .parse(src)
        .into_output_errors();
    let errors = errors
        .into_iter()
        .map(|err| {
            let span = Span::from((file_id, *err.span()));
            Error::Lex(FormattedError(err).to_string(), span)
        })
        .collect();
    let Some(tokens) = output else {
        return (None, errors);
    };

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
    let eof = tokens.last().map(|(_, span)| span.end()).unwrap_or(0);
    parse_with_eof(parser, tokens, file, eof)
}

fn parse_with_eof<'tok, 'src: 'tok, A>(
    parser: impl Parse<'tok, 'src, A>,
    tokens: &'tok [(Token<'src>, Span)],
    file: FileId,
    eof: u32,
) -> ParseResult<A> {
    let parser: &dyn Parser<'tok, _, A, extra::Err<_>> = &parser.with_ctx(file);
    let (output, errors) = parser
        .parse(parser_input(tokens, file, eof))
        .into_output_errors();
    let errors = errors
        .into_iter()
        .map(|error| {
            if matches!(
                error.reason(),
                chumsky::error::RichReason::ExpectedFound { found: None, .. }
            ) {
                Error::Parse(FormattedError(error).to_string(), Span::new(eof, eof, file))
            } else {
                Error::new_parse(error)
            }
        })
        .collect();
    (output, errors)
}

fn parser_input<'tok, 'src>(
    tokens: &'tok [(Token<'src>, Span)],
    file: FileId,
    eof: u32,
) -> ParserInput<'tok, 'src> {
    tokens.spanned(Span {
        start: eof,
        end: eof,
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
mod eof_tests {
    use super::*;

    fn assert_parse_error_at(errors: &[Error], span: Span) {
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, Error::Parse(_, s) if *s == span)),
            "expected parse error at {span:?}, got {errors:?}"
        );
    }

    #[test]
    fn source_entry_points_use_source_eof() {
        let file = FileId::from_i32(7);
        for trailing in [
            " \n\t",
            " // trailing comment",
            " /* trailing comment */ \n",
        ] {
            let src = trailing.to_owned();
            let (_, errors) = parse_expr(&src, file);
            assert_parse_error_at(&errors, Span::new(src.len() as u32, src.len() as u32, file));

            let src = trailing.to_owned();
            let (_, errors) = parse_stmt(&src, file);
            assert_parse_error_at(&errors, Span::new(src.len() as u32, src.len() as u32, file));

            let eof = Span::new(src.len() as u32, src.len() as u32, file);
            assert_parse_error_at(&parse_item(&src, file).1, eof);
            assert_parse_error_at(&parse_item_decl(&src, file).1, eof);
            assert!(parse_module(&src, file).1.is_empty());
            assert!(parse_modules([(file, src.as_str())]).1.is_empty());
        }
    }

    #[test]
    fn incomplete_sources_use_true_eof() {
        let file = FileId::from_i32(12);
        for trailing in [" \n\t", " //comment", " /* comment */ \n"] {
            let src = format!("1 +{trailing}");
            let (_, errors) = parse_expr(&src, file);
            assert_parse_error_at(&errors, Span::new(src.len() as u32, src.len() as u32, file));

            let src = format!("func Test({trailing}");
            let eof = Span::new(src.len() as u32, src.len() as u32, file);
            assert_parse_error_at(&parse_item(&src, file).1, eof);
            assert_parse_error_at(&parse_item_decl(&src, file).1, eof);
            assert_parse_error_at(&parse_module(&src, file).1, eof);
            assert_parse_error_at(&parse_modules([(file, src.as_str())]).1, eof);
        }
    }

    #[test]
    fn custom_errors_and_actual_tokens_keep_their_spans() {
        let file = FileId::from_i32(13);
        let (_, errors) = parse_expr("value. //comment", file);
        assert_parse_error_at(&errors, Span::new(5, 6, file));
        assert!(
            errors
                .iter()
                .all(|error| error.span() != Span::new(16, 16, file))
        );

        let (_, errors) = parse_expr("1 + ) //comment", file);
        assert_parse_error_at(&errors, Span::new(4, 5, file));
    }

    #[test]
    fn trivia_only_expression_uses_source_eof() {
        let file = FileId::from_i32(8);
        for src in ["", " \n\t", "// comment", " /* comment */ \n"] {
            let (_, errors) = parse_expr(src, file);
            assert_parse_error_at(&errors, Span::new(src.len() as u32, src.len() as u32, file));
        }
    }

    #[test]
    fn token_only_parse_keeps_last_token_eof() {
        let file = FileId::from_i32(9);
        let (tokens, errors) = lex("1 + // trailing comment", file);
        assert!(errors.is_empty());
        let tokens = tokens.unwrap();
        let eof_parser = any().repeated().ignore_then(just(Token::Null));
        let (_, errors) = parse(eof_parser, &tokens, file);
        assert_parse_error_at(&errors, Span::new(3, 3, file));

        use chumsky::input::ExactSizeInput;
        // SAFETY: zero is a valid starting token offset for this input.
        let span = unsafe { parser_input(&tokens, file, 22).span_from(0..) };
        assert_eq!(span, Span::new(0, 22, file));
    }

    #[test]
    fn empty_interpolation_errors_use_original_source_positions() {
        let file = FileId::from_i32(10);
        for src in [
            r#"s"\()""#,
            r#"s"prefix \(   ) suffix""#,
            r#"s"é \(/* comment */ )""#,
            r#"s"\(1 + /* comment */ )""#,
        ] {
            let (_, errors) = parse_expr(src, file);
            let close = src.find(')').unwrap() as u32;
            assert_parse_error_at(&errors, Span::new(close, close + 1, file));
        }
    }

    #[test]
    fn empty_group_token_uses_outer_group_eof() {
        let file = FileId::from_i32(11);
        let tokens = [(
            Token::InterpStr(vec![(Token::Group(Box::new([])), Span::new(20, 23, file))].into()),
            Span::new(10, 30, file),
        )];
        let eof_parser = end()
            .map_with(|(), e| e.span())
            .nested_in(select_ref! { Token::Group(tok) = ex => {
                let span: Span = ex.span();
                parser_input(tok, *ex.ctx(), span.end)
            } })
            .nested_in(select_ref! { Token::InterpStr(tok) = ex => {
                let span: Span = ex.span();
                parser_input(tok, *ex.ctx(), span.end - 1)
            } });
        let (span, errors) = parse(eof_parser, &tokens, file);
        assert!(errors.is_empty());
        assert_eq!(span, Some(Span::new(23, 23, file)));
    }
}
