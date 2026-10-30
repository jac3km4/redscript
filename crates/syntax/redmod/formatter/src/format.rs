//! Conservative native REDmod indentation, not AST-based pretty-printing.
//!
//! Only leading horizontal whitespace on lines outside multiline tokens changes.
//! Token spelling, comments, line endings, and literal contents are preserved.
//! This deliberately does not implement spacing, wrapping, or code edits.

use redscript_redmod_ast::{Block, Declaration, FileId, Item, Span, Spanned, Statement};
use redscript_redmod_parser::{Error, Token};

/// Indent valid REDmod source with four spaces per block or continuation level.
/// Invalid input is returned as diagnostics rather than formatted recovery nodes.
pub fn format_source(source: &str, file: FileId) -> Result<String, Vec<Error>> {
    let (module, errors) = redscript_redmod_parser::parse_module(source, file);
    if !errors.is_empty() {
        return Err(errors);
    }
    let (tokens, errors) = redscript_redmod_parser::lex_with_lf_and_comments(source, file);
    if !errors.is_empty() {
        return Err(errors);
    }
    let tokens = tokens.unwrap_or_default();
    let mut switches = SwitchIndent::default();
    if let Some(module) = &module {
        switches.declarations(&module.items, &tokens);
    }
    switches
        .events
        .sort_unstable_by_key(|&(offset, start)| (offset, !start));
    let mut events = switches.events.into_iter().peekable();
    let mut case_depth = 0usize;
    let mut output = String::with_capacity(source.len());
    let mut offset = 0usize;
    let mut depth = 0usize;
    let mut continuation = 0usize;
    let mut chain_continuation = false;
    let mut cursor = 0usize;
    for line in source.split_inclusive('\n') {
        let end = offset + line.len();
        let protected = tokens.get(cursor).is_some_and(|(token, span)| {
            span.start < offset as u32
                && span.end > offset as u32
                && !matches!(token, Token::Whitespace | Token::LineFeed)
        });
        let trimmed = line.trim_start_matches([' ', '\t']);
        let start = offset + line.len() - trimmed.len();
        while events
            .peek()
            .is_some_and(|&(offset, _)| offset <= start as u32)
        {
            if events.next().unwrap().1 {
                case_depth += 1;
            } else {
                case_depth -= 1;
            }
        }
        let first = tokens[cursor..]
            .iter()
            .take_while(|(_, span)| (span.start as usize) < end)
            .find(|(_, span)| span.start as usize == start)
            .map(|(token, _)| token);
        let closing = matches!(first, Some(Token::RBrace));
        let closing_delimiters = tokens[cursor..]
            .iter()
            .skip_while(|(_, span)| (span.start as usize) < start)
            .take_while(|(token, span)| {
                (span.start as usize) < end
                    && matches!(token, Token::RParen | Token::RBracket | Token::Whitespace)
            })
            .filter(|(token, _)| matches!(token, Token::RParen | Token::RBracket))
            .count();
        let continuation_depth = continuation.saturating_sub(closing_delimiters);
        let chain_depth = usize::from(
            chain_continuation || (matches!(first, Some(Token::Period)) && continuation == 0),
        );
        if protected || trimmed.trim_end_matches(['\r', '\n']).is_empty() {
            output.push_str(line);
        } else {
            let indent = depth.saturating_sub(usize::from(closing))
                + case_depth
                + continuation_depth
                + chain_depth;
            for _ in 0..indent {
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
                Token::LParen | Token::LBracket => continuation += 1,
                Token::RParen | Token::RBracket => continuation = continuation.saturating_sub(1),
                _ => {}
            }
            cursor += 1;
        }
        chain_continuation = chain_depth != 0 && continuation != 0;
        offset = end;
    }
    Ok(output)
}

#[derive(Default)]
struct SwitchIndent {
    events: Vec<(u32, bool)>,
}

impl SwitchIndent {
    fn declarations(
        &mut self,
        declarations: &[Spanned<Declaration<'_>>],
        tokens: &[(Token<'_>, Span)],
    ) {
        for (decl, _) in declarations {
            match &decl.item {
                Item::Aggregate { items, .. } => self.declarations(items, tokens),
                Item::Function {
                    body: Some(body), ..
                } => self.block(body, tokens),
                _ => {}
            }
        }
    }

    fn block(&mut self, block: &Block<'_>, tokens: &[(Token<'_>, Span)]) {
        for (stmt, span) in block {
            match stmt {
                Statement::Switch { labels, .. } => {
                    let switch_tokens = &tokens[tokens
                        .partition_point(|(_, token_span)| token_span.start < span.start)
                        ..tokens.partition_point(|(_, token_span)| token_span.end <= span.end)];
                    let end = switch_tokens
                        .iter()
                        .rev()
                        .find(|(token, token_span)| {
                            *token == Token::RBrace
                                && token_span.start >= span.start
                                && token_span.end <= span.end
                        })
                        .map_or(span.end, |(_, span)| span.start);
                    for (index, (label, span)) in labels.iter().enumerate() {
                        // Start after the label colon, not its expression: a case
                        // value may itself contain a conditional expression.
                        let value_end = label
                            .value
                            .as_ref()
                            .map_or(span.start, |(_, span)| span.end);
                        let label_tokens = &switch_tokens
                            [switch_tokens.partition_point(|(_, span)| span.start < value_end)..];
                        let colon = label_tokens.iter().find(|(token, token_span)| {
                            *token == Token::Colon
                                && token_span.start >= value_end
                                && token_span.end <= span.end
                        });
                        if let Some((_, colon)) = colon {
                            let end = labels.get(index + 1).map_or(end, |(_, span)| span.start);
                            self.events.push((colon.end, true));
                            self.events.push((end, false));
                        }
                        self.block(&label.body, tokens);
                    }
                }
                Statement::If { blocks, else_ } => {
                    for (_, block) in blocks {
                        self.block(block, tokens);
                    }
                    if let Some(block) = else_ {
                        self.block(block, tokens);
                    }
                }
                Statement::Block(block)
                | Statement::While { body: block, .. }
                | Statement::DoWhile { body: block, .. }
                | Statement::For { body: block, .. } => self.block(block, tokens),
                _ => {}
            }
        }
    }
}
