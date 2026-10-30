//! REDmod syntax parsed directly into the native redscript-redmod-ast source tree.
//!
//! Parse results may contain recovered nodes alongside diagnostics. Always check
//! the error list before treating a source file as valid. See the crate README
//! for the supported subset and semantic integration backlog.

mod lexer;
pub mod parser;
use chumsky::prelude::*;
pub use lexer::Token;
use parser::Parse;
pub use redscript_community_parser::Error;
use redscript_community_parser::diagnostic::FormattedError;
pub use redscript_redmod_ast as syntax;
use redscript_redmod_ast::{Declaration, Expr, FileId, Item, Module, Span, Spanned, Statement};

pub type ParseResult<A> = (Option<A>, Vec<Error>);

/// Whether a name is safe to emit as a REDmod target identifier.
///
/// This conservative emission policy accepts ASCII identifiers and excludes
/// reserved spellings, including contextual keywords. It is not the parser's
/// context-dependent identifier grammar.
pub fn is_target_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        && !matches!(
            name,
            "name"
                | "true"
                | "false"
                | "NULL"
                | "this"
                | "super"
                | "function"
                | "event"
                | "class"
                | "struct"
                | "enum"
                | "var"
                | "let"
                | "if"
                | "else"
                | "while"
                | "for"
                | "switch"
                | "case"
                | "default"
                | "break"
                | "continue"
                | "return"
                | "new"
                | "as"
                | "out"
                | "optional"
                | "public"
                | "private"
                | "protected"
                | "static"
                | "const"
                | "final"
                | "abstract"
                | "import"
                | "importonly"
                | "extends"
                | "persistent"
                | "override"
        )
}

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
) -> (Vec<Module<'a>>, Vec<Error>) {
    let mut errs = vec![];
    let modules = it
        .into_iter()
        .filter_map(|(file, src)| {
            let (module, errors) = parse_module(src, file);
            errs.extend(errors);
            module
        })
        .collect();
    (modules, errs)
}

pub fn parse_module(src: &str, file: FileId) -> ParseResult<Module<'_>> {
    parse!(src, parser::module(), file)
}

fn is_path_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

pub fn parse_item_decl(src: &str, file: FileId) -> ParseResult<Declaration<'_>> {
    parse!(src, parser::item_decl(), file)
}

pub fn parse_item(src: &str, file: FileId) -> ParseResult<Item<'_>> {
    parse!(src, parser::item(), file)
}

pub fn parse_stmt(src: &str, file: FileId) -> ParseResult<Statement<'_>> {
    parse!(src, parser::stmt(), file)
}

pub fn parse_expr(src: &str, file: FileId) -> ParseResult<Expr<'_>> {
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
    let docs = parser::DeclarationDocs::from_tokens(tokens);
    let parser: &dyn Parser<'tok, _, A, extra::Err<_>> = &parser.with_ctx(docs);
    let (output, errors) = parser
        .parse(parser::input(tokens, file, eof))
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

#[cfg(test)]
mod tests {
    use super::is_target_identifier;

    #[test]
    fn target_identifiers_accept_ascii_names() {
        for name in [
            "_", "_value2", "Run", "value123", "Override", "null", "Name",
        ] {
            assert!(is_target_identifier(name), "{name:?}");
        }
    }

    #[test]
    fn target_identifiers_reject_malformed_names() {
        for name in [
            "", "1name", "a-b", "a.b", "a b", " value", "café", "猫", "a\n", "a\0",
        ] {
            assert!(!is_target_identifier(name), "{name:?}");
        }
    }

    #[test]
    fn target_identifiers_reject_reserved_names() {
        for name in [
            "name",
            "true",
            "false",
            "NULL",
            "this",
            "super",
            "function",
            "event",
            "class",
            "struct",
            "enum",
            "var",
            "let",
            "if",
            "else",
            "while",
            "for",
            "switch",
            "case",
            "default",
            "break",
            "continue",
            "return",
            "new",
            "as",
            "out",
            "optional",
            "public",
            "private",
            "protected",
            "static",
            "const",
            "final",
            "abstract",
            "import",
            "importonly",
            "extends",
            "persistent",
            "override",
        ] {
            assert!(!is_target_identifier(name), "{name:?}");
        }
    }
}
