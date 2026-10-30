//! Dialect-independent parser utilities. Grammar and lexical policy stay with each parser.

use std::borrow::Cow;
use std::iter;
use std::ops::BitOr;

use chumsky::container::Container;
use chumsky::span::Span as _;
use redscript_community_ast::{Assoc, BinOp, Expr, SourceExpr, Span};

#[derive(Debug, Default, Clone, Copy)]
pub struct BitCollection<A> {
    pub value: A,
}

impl<A: Default + Copy + BitOr<Output = A>> Container<A> for BitCollection<A> {
    #[inline]
    fn push(&mut self, item: A) {
        self.value = self.value | item;
    }
}

#[derive(Debug, Default)]
pub struct StrParts<'src> {
    pub str: Cow<'src, str>,
}

impl<'src> Container<Cow<'src, str>> for StrParts<'src> {
    fn push(&mut self, c: Cow<'src, str>) {
        if self.str.is_empty() {
            self.str = c;
        } else {
            self.str = Cow::Owned(std::mem::take(&mut self.str).into_owned() + &c);
        }
    }
}

pub fn climb_prec<'src, I>(
    mut lhs: (SourceExpr<'src>, Span),
    it: &mut iter::Peekable<I>,
    min_prec: u8,
) -> (SourceExpr<'src>, Span)
where
    I: Iterator<Item = (BinOp, (SourceExpr<'src>, Span))>,
{
    while let Some((op, mut rhs)) = it.next_if(|(op, _)| op.precedence() >= min_prec) {
        while let Some((lookahead, _)) = it.peek() {
            if lookahead.precedence() > op.precedence() {
                rhs = climb_prec(rhs, it, op.precedence() + 1);
            } else if lookahead.assoc() == Assoc::Right && lookahead.precedence() == op.precedence()
            {
                rhs = climb_prec(rhs, it, op.precedence());
            } else {
                break;
            }
        }
        let span = lhs.1.union(rhs.1);
        lhs = (
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            span,
        );
    }
    lhs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_parts_preserve_borrowing_until_concatenation() {
        let mut parts = StrParts::default();
        parts.push(Cow::Borrowed(""));
        parts.push(Cow::Borrowed("猫"));
        assert!(matches!(parts.str, Cow::Borrowed("猫")));
        parts.push(Cow::Owned("\n".into()));
        parts.push(Cow::Borrowed("end"));
        assert_eq!(parts.str, "猫\nend");
    }

    #[test]
    fn bit_collection_unions_repeated_flags() {
        use redscript_community_ast::ParamQualifiers as Q;
        let mut flags = BitCollection::<Q>::default();
        assert!(flags.value.is_empty());
        for flag in [Q::OUT, Q::CONST, Q::OUT] {
            flags.push(flag);
        }
        assert_eq!(flags.value, Q::OUT | Q::CONST);
    }
}
