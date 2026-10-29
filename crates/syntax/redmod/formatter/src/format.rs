//! Conservative native REDmod indentation, not AST-based pretty-printing.
//!
//! Only leading horizontal whitespace on lines outside multiline tokens changes.
//! Token spelling, comments, line endings, and literal contents are preserved.
//! This deliberately does not implement spacing, wrapping, or code edits.

use redscript_redmod_ast::FileId;
use redscript_redmod_parser::{Error, Token};

/// Indent valid REDmod source with four spaces per brace level.
/// Invalid input is returned as diagnostics rather than formatted recovery nodes.
pub fn format_source(source: &str, file: FileId) -> Result<String, Vec<Error>> {
    let (_, errors) = redscript_redmod_parser::parse_module(source, file);
    if !errors.is_empty() {
        return Err(errors);
    }
    let (tokens, errors) = redscript_redmod_parser::lex_with_lf_and_comments(source, file);
    if !errors.is_empty() {
        return Err(errors);
    }
    let tokens = tokens.unwrap_or_default();
    let mut output = String::with_capacity(source.len());
    let mut offset = 0usize;
    let mut depth = 0usize;
    let mut cursor = 0usize;
    for line in source.split_inclusive('\n') {
        let end = offset + line.len();
        let protected = tokens.get(cursor).is_some_and(|(token, span)| {
            span.start < offset as u32
                && span.end > offset as u32
                && !matches!(token, Token::Whitespace | Token::LineFeed)
        });
        let trimmed = line.trim_start_matches([' ', '\t']);
        let closing = tokens[cursor..]
            .iter()
            .take_while(|(_, span)| (span.start as usize) < end)
            .any(|(token, span)| {
                *token == Token::RBrace
                    && span.start as usize == offset + line.len() - trimmed.len()
            });
        if protected || trimmed.trim_end_matches(['\r', '\n']).is_empty() {
            output.push_str(line);
        } else {
            for _ in 0..depth.saturating_sub(usize::from(closing)) {
                output.push_str("    ");
            }
            output.push_str(trimmed);
        }
        while let Some((token, span)) = tokens.get(cursor) {
            if span.start as usize >= end {
                break;
            }
            if span.end as usize > end {
                break;
            }
            match token {
                Token::LBrace => depth += 1,
                Token::RBrace => depth = depth.saturating_sub(1),
                _ => {}
            }
            cursor += 1;
        }
        offset = end;
    }
    Ok(output)
}
