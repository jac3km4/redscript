use std::convert::Infallible;

use hashbrown::HashMap;
use redscript_community_ast::{
    AstNode, AstVisitor, NodeId, SourceAstNode, SourceBlock, SourceStmt, Span, Spanned, WithSpan,
};
use redscript_community_parser::Token;

// The parser requires trivia-free input at every interpolation level. Keep trivia in
// source order so the prefix collector can attach it using the original spans.
pub(crate) fn extract_trivia<'src>(
    tokens: Vec<Spanned<Token<'src>>>,
    trivia: &mut Vec<Spanned<Token<'src>>>,
) -> Vec<Spanned<Token<'src>>> {
    tokens
        .into_iter()
        .filter_map(|(token, span)| {
            let token = match token {
                Token::Whitespace => return None,
                token if token.is_ws_or_comment() => {
                    trivia.push((token, span));
                    return None;
                }
                Token::Group(tokens) => {
                    Token::Group(extract_trivia(tokens.into_vec(), trivia).into())
                }
                Token::InterpStr(tokens) => {
                    Token::InterpStr(extract_trivia(tokens.into_vec(), trivia).into())
                }
                token => token,
            };
            Some((token, span))
        })
        .collect()
}

#[derive(Debug)]
pub enum Prefix<'src> {
    LineBreak,
    LineComment(&'src str),
    BlockComment(&'src str),
}

#[derive(Debug)]
pub(crate) struct PrefixCollector<'ctx, 'src> {
    prefixes: HashMap<NodeId, Vec<Prefix<'src>>>,
    remainder: &'ctx [Spanned<Token<'src>>],
}

impl<'ctx, 'src> PrefixCollector<'ctx, 'src> {
    pub(crate) fn new(tokens: &'ctx [Spanned<Token<'src>>]) -> Self {
        Self {
            prefixes: HashMap::new(),
            remainder: tokens,
        }
    }

    pub(crate) fn into_inner(
        self,
    ) -> (
        HashMap<NodeId, Vec<Prefix<'src>>>,
        &'ctx [Spanned<Token<'src>>],
    ) {
        (self.prefixes, self.remainder)
    }

    fn skip_until(&mut self, span: Span) {
        while let [(Token::LineFeed, s), rest @ ..] = self.remainder {
            if s.start > span.start {
                break;
            }
            self.remainder = rest;
        }
    }
}

impl<'src> AstVisitor<'src, WithSpan> for PrefixCollector<'_, 'src> {
    type Error = Infallible;

    fn visit_node(&mut self, node: SourceAstNode<'_, 'src>) -> Result<(), Self::Error> {
        let mut prefixes = vec![];
        let mut is_lf_seq = false;

        while let [(fst, span), rest @ ..] = self.remainder {
            if span.start > node.span().start {
                break;
            }
            match (fst, rest) {
                // two consecutive line feeds form a line break between statements
                (Token::LineFeed, [(Token::LineFeed, span), rest @ ..])
                    if matches!(node, AstNode::Stmt(_)) && span.start <= node.span().start =>
                {
                    is_lf_seq = true;
                    self.remainder = rest;
                }
                _ => {
                    if is_lf_seq {
                        prefixes.push(Prefix::LineBreak);
                        is_lf_seq = false;
                    }
                    self.remainder = rest;
                }
            }
            match fst {
                Token::LineComment(comment) => prefixes.push(Prefix::LineComment(comment)),
                Token::BlockComment(comment) => prefixes.push(Prefix::BlockComment(comment)),
                _ => {}
            }
        }
        if is_lf_seq {
            prefixes.push(Prefix::LineBreak);
        }
        if !prefixes.is_empty() {
            self.prefixes.insert(node.id(), prefixes);
        }
        Ok(())
    }

    fn post_visit_node(&mut self, node: AstNode<'_, 'src, WithSpan>) -> Result<(), Self::Error> {
        while let [(Token::LineFeed, span), rest @ ..] = self.remainder {
            if span.start >= node.span().end {
                break;
            }
            self.remainder = rest;
        }

        Ok(())
    }

    fn visit_block(&mut self, block: &SourceBlock<'src>) -> Result<(), Self::Error> {
        if let Some(block_span) = block.bounds_span() {
            self.skip_until(block_span);
        };

        block
            .stmts
            .iter()
            .try_for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_default(&mut self, stmts: &[Spanned<SourceStmt<'src>>]) -> Result<(), Self::Error> {
        if let (Some((_, fst)), Some((_, lst))) = (stmts.first(), stmts.last()) {
            self.skip_until(fst.merge(lst));
        };
        stmts.iter().try_for_each(|stmt| self.visit_stmt(stmt))
    }
}
