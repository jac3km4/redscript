use std::fmt;

use hashbrown::HashMap;
use redscript_community_ast::{AstVisitor, FileId, NodeId};
use redscript_community_parser::{ParseResult, Token, lex_with_lf_and_comments, parse, parser};

use crate::layout::{DisplayFn, Indent, ParentOp};
use crate::trivia::{PrefixCollector, extract_trivia};

mod exprs;
mod items;
mod layout;
mod trivia;

pub use crate::trivia::Prefix;

#[derive(Debug, Clone)]
pub struct FormatSettings {
    pub indent: u16,
    pub max_width: u16,
    pub max_chain_fields: u8,
    pub max_chain_calls: u8,
    pub max_chain_operators: u8,
    pub max_chain_total: u8,
    pub trunc_sig_digits: Option<u8>,
}

impl FormatSettings {
    pub const DEFAULT: FormatSettings = FormatSettings {
        indent: 2,
        max_width: 80,
        max_chain_fields: 4,
        max_chain_calls: 2,
        max_chain_operators: 4,
        max_chain_total: 4,
        trunc_sig_digits: None,
    };
}

impl Default for FormatSettings {
    fn default() -> Self {
        Self::DEFAULT
    }
}

pub fn format_document<'a>(
    source: &'a str,
    id: FileId,
    settings: &'a FormatSettings,
) -> ParseResult<impl fmt::Display + use<'a>> {
    let mut errors = vec![];

    let (tokens, e) = lex_with_lf_and_comments(source, id);
    errors.extend(e);
    let Some(tokens) = tokens else {
        return (None, errors);
    };
    let mut ws = vec![];
    let tokens = extract_trivia(tokens, &mut ws);

    let (module, e) = parse(parser::module(), &tokens, id);
    errors.extend(e);
    let Some(module) = module else {
        return (None, errors);
    };

    let mut collector = PrefixCollector::new(&ws);
    match collector.visit_module(&module) {
        Ok(()) => {}
        Err(never) => match never {},
    }
    let (prefixes, remainder) = collector.into_inner();
    let remainder = remainder
        .iter()
        .filter(|(t, _)| !matches!(t, Token::LineFeed))
        .map(|(t, _)| t.clone())
        .collect::<Vec<_>>();

    let display = DisplayFn(move |f: &mut fmt::Formatter<'_>| {
        let ctx = FormatCtx::new(settings, &prefixes);
        writeln!(f, "{}", module.as_fmt(ctx))?;
        remainder.iter().try_for_each(|t| writeln!(f, "{t}"))
    });
    (Some(display), errors)
}

#[derive(Debug, Clone, Copy)]
pub struct FormatCtx<'a> {
    settings: &'a FormatSettings,
    prefixes: &'a HashMap<NodeId, Vec<Prefix<'a>>>,
    depth: usize,
    parent: Option<ParentOp>,
}

impl<'a> FormatCtx<'a> {
    pub fn new(
        settings: &'a FormatSettings,
        prefixes: &'a HashMap<NodeId, Vec<Prefix<'a>>>,
    ) -> Self {
        Self {
            settings,
            prefixes,
            depth: 0,
            parent: None,
        }
    }

    fn indent(&self) -> Indent {
        Indent(self.indent_width())
    }

    fn indented(&self) -> Self {
        Self {
            depth: self.depth + 1,
            ..*self
        }
    }

    fn decrement(&self) -> Self {
        Self {
            depth: self.depth - 1,
            ..*self
        }
    }

    fn indent_width(&self) -> usize {
        self.depth * usize::from(self.settings.indent)
    }

    fn with_parent_op(&self, parent: ParentOp) -> Self {
        Self {
            parent: Some(parent),
            ..*self
        }
    }

    fn without_parent_op(&self) -> Self {
        Self {
            parent: None,
            ..*self
        }
    }

    fn node_prefix(&self, id: NodeId, indent: Option<Indent>) -> impl fmt::Display + use<'_> {
        DisplayFn(move |f: &mut fmt::Formatter<'_>| {
            self.prefixes
                .get(&id)
                .map(Vec::as_slice)
                .unwrap_or_default()
                .iter()
                .try_for_each(|prefix| match (prefix, indent) {
                    (Prefix::LineBreak, _) => writeln!(f),
                    (
                        Prefix::LineComment(comment) | Prefix::BlockComment(comment),
                        Some(indent),
                    ) => {
                        writeln!(f, "{indent}{comment}")
                    }
                    (Prefix::LineComment(comment), None) => {
                        writeln!(f, "{comment}")?;
                        write!(f, "{}", self.indented().indent())
                    }
                    (Prefix::BlockComment(comment), None) => {
                        write!(f, "{comment} ")
                    }
                })
        })
    }
}

pub trait Formattable: Sized {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result;
}

impl<A: Formattable, B: Formattable> Formattable for (A, B) {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        self.0.format(f, ctx)?;
        self.1.format(f, ctx)
    }
}

impl<A: Formattable, B: Formattable, C: Formattable> Formattable for (A, B, C) {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        self.0.format(f, ctx)?;
        self.1.format(f, ctx)?;
        self.2.format(f, ctx)
    }
}

impl<A: Formattable> Formattable for Option<A> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        if let Some(value) = self {
            value.format(f, ctx)
        } else {
            Ok(())
        }
    }
}

struct DisplayProxy<'s, A>(A, FormatCtx<'s>);

impl<A: Formattable> fmt::Display for DisplayProxy<'_, &A> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.format(f, self.1)
    }
}

pub trait SyntaxOps {
    fn as_fmt(&self, ctx: FormatCtx<'_>) -> impl fmt::Display;
}

impl<A> SyntaxOps for A
where
    A: Formattable,
{
    #[inline]
    fn as_fmt(&self, ctx: FormatCtx<'_>) -> impl fmt::Display {
        DisplayProxy(self, ctx)
    }
}

impl Formattable for &str {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<A: Formattable> Formattable for &A {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        (*self).format(f, ctx)
    }
}

#[cfg(test)]
mod operator_tests {
    use hashbrown::HashMap;
    use redscript_community_ast::{BinOp, UnOp};

    use super::{FormatCtx, FormatSettings, SyntaxOps};
    use crate::layout::{ApproxWidth, SepByMultiline};

    #[test]
    fn shared_operator_symbols_match_formatting_and_widths() {
        let prefixes = HashMap::default();
        let ctx = FormatCtx::new(&FormatSettings::DEFAULT, &prefixes);
        for (op, symbol) in [
            (BinOp::AssignAdd, "+="),
            (BinOp::AssignSub, "-="),
            (BinOp::AssignMul, "*="),
            (BinOp::AssignDiv, "/="),
            (BinOp::AssignBitOr, "|="),
            (BinOp::AssignBitAnd, "&="),
            (BinOp::Or, "||"),
            (BinOp::And, "&&"),
            (BinOp::BitOr, "|"),
            (BinOp::BitXor, "^"),
            (BinOp::BitAnd, "&"),
            (BinOp::Eq, "=="),
            (BinOp::Ne, "!="),
            (BinOp::Lt, "<"),
            (BinOp::Le, "<="),
            (BinOp::Gt, ">"),
            (BinOp::Ge, ">="),
            (BinOp::Add, "+"),
            (BinOp::Sub, "-"),
            (BinOp::Mul, "*"),
            (BinOp::Div, "/"),
            (BinOp::Mod, "%"),
        ] {
            assert_eq!(op.as_fmt(ctx).to_string(), symbol);
            assert_eq!(op.approx_width(), symbol.len());
        }
        for (op, symbol) in [(UnOp::BitNot, "~"), (UnOp::Not, "!"), (UnOp::Neg, "-")] {
            assert_eq!(op.as_fmt(ctx).to_string(), symbol);
            assert_eq!(op.approx_width(), symbol.len());
        }
    }

    #[test]
    fn sep_by_multiline_uses_custom_separator_on_single_line() {
        let prefixes = HashMap::default();
        let ctx = FormatCtx::new(&FormatSettings::DEFAULT, &prefixes);
        let items = ["a", "b", "c"];
        let formatted = SepByMultiline::new(items.into_iter(), "; ", ctx).to_string();
        assert_eq!(formatted, "a; b; c");
    }
}
