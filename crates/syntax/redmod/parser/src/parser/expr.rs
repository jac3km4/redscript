use std::iter;

use chumsky::prelude::*;
use redscript_redmod_ast::{BinOp, CastStyle, Constant, Expr, SourceExpr, Span, UnOp};

use super::{Parse, extended_ident, ident, type_with_span};
use crate::lexer::Token;

pub fn expr_with_span_rec<'tok, 'src: 'tok>(
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> {
    expr_with_span_impl(expr)
        // handle trailing period explicitly because it's a common error
        .then(just(Token::Period).or_not())
        .validate(|(exp, period), ctx, errs| {
            if period.is_some() {
                errs.emit(Rich::custom(ctx.span(), "unexpected '.'"));
            };
            exp
        })
}

fn expr_with_span_impl<'tok, 'src: 'tok>(
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
) -> impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> {
    let value = select! {
        Token::Null => Expr::Null,
        Token::This => Expr::This,
        Token::Super => Expr::Super,
        Token::True => Expr::Constant(Constant::Bool(true)),
        Token::False => Expr::Constant(Constant::Bool(false)),
        Token::Int(s) => Expr::Constant(Constant::I32(s)),
        Token::Uint(s) => Expr::Constant(Constant::U32(s)),
        Token::Ulong(s) => Expr::Constant(Constant::U64(s)),
        Token::Long(s) => Expr::Constant(Constant::I64(s)),
        Token::Float(s) => Expr::Constant(Constant::F32(s)),
        Token::Double(s) => Expr::Constant(Constant::F64(s)),
        Token::Str(s) => Expr::Constant(Constant::String(s)),
        Token::CName(s) => Expr::Constant(Constant::CName(s)),
        Token::ResRef(s) => Expr::Constant(Constant::Resource(s)),
        Token::TdbId(s) => Expr::Constant(Constant::TweakDbId(s)),
    }
    .labelled("value");

    let unop = select! {
        Token::Minus => UnOp::Neg,
        Token::Not => UnOp::Not,
        Token::BitNot => UnOp::BitNot,
    }
    .labelled("unary operator");

    let assignment = select! {
        Token::Assign => None,
        Token::AssignAdd => Some(BinOp::AssignAdd),
        Token::AssignSub => Some(BinOp::AssignSub),
        Token::AssignMul => Some(BinOp::AssignMul),
        Token::AssignDiv => Some(BinOp::AssignDiv),
        Token::AssignBitOr => Some(BinOp::AssignBitOr),
        Token::AssignBitAnd => Some(BinOp::AssignBitAnd),
    }
    .labelled("assignment operator");

    let binop = select! {
        Token::Plus => BinOp::Add,
        Token::Minus => BinOp::Sub,
        Token::Star => BinOp::Mul,
        Token::Slash => BinOp::Div,
        Token::Percent => BinOp::Mod,
        Token::Eq => BinOp::Eq,
        Token::Ne => BinOp::Ne,
        Token::LAngle => BinOp::Lt,
        Token::Le => BinOp::Le,
        Token::RAngle => BinOp::Gt,
        Token::Ge => BinOp::Ge,
        Token::And => BinOp::And,
        Token::Or => BinOp::Or,
        Token::BitAnd => BinOp::BitAnd,
        Token::BitOr => BinOp::BitOr,
        Token::BitXor => BinOp::BitXor,
    }
    .labelled("binary operator");

    // These words must not fall back to expression identifiers when a statement
    // or allocation is malformed (in particular, community pattern conditions).
    let ident = ident().filter(|name: &&str| {
        !matches!(
            *name,
            "new"
                | "do"
                | "const"
                | "var"
                | "let"
                | "if"
                | "else"
                | "while"
                | "switch"
                | "for"
                | "return"
                | "break"
                | "continue"
                | "function"
                | "event"
                | "class"
                | "struct"
                | "enum"
                | "public"
                | "private"
                | "protected"
                | "override"
        )
    });
    let typ = type_with_span();

    let slot = expr.clone().or_not().map_with(|arg, e| (arg, e.span()));
    let arguments = slot
        .clone()
        .then(
            just(Token::Comma)
                .ignore_then(slot)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(first, rest)| {
            if first.0.is_none() && rest.is_empty() {
                return Vec::new();
            }
            std::iter::once(first)
                .chain(rest)
                .map(|(arg, span)| {
                    arg.unwrap_or_else(|| {
                        (Expr::Omitted, Span::new(span.start, span.start, span.file))
                    })
                })
                .collect::<Vec<_>>()
        })
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .erased();
    // Reject generic-call-shaped suffixes rather than interpreting Foo<T>(x)
    // as chained comparisons. Only the Cast intrinsic accepts type arguments.
    let generic_call_start = typ
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LAngle), just(Token::RAngle))
        .then(just(Token::LParen))
        .erased();

    let new = just(Token::Ident("new"))
        .ignore_then(typ.clone())
        .then(arguments.clone().or_not())
        .validate(|(typ, args), e, errs| {
            if args.is_some() {
                errs.emit(Rich::custom(
                    e.span(),
                    "REDmod allocation does not accept constructor parentheses or arguments; use 'new Type'",
                ));
                Expr::Error
            } else {
                Expr::New {
                    typ: typ.into(),

                }
            }
        })
        .erased();

    let array = expr
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .validate(|els, e, errs| {
            if els.is_empty() {
                errs.emit(Rich::custom(
                    e.span(),
                    "REDmod array literals must not be empty",
                ));
                Expr::Error
            } else {
                Expr::ArrayLit(els.into())
            }
        })
        .erased();

    let cast = just(Token::Ident("Cast"))
        .map_with(|_, e| (Expr::Ident("Cast"), e.span()))
        .then(
            typ.clone()
                .delimited_by(just(Token::LAngle), just(Token::RAngle)),
        )
        .then(
            expr.clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .map(|((_, typ), operand)| Expr::Cast {
            typ: Box::new(typ),
            expr: Box::new(operand),
            style: CastStyle::Intrinsic,
        })
        .erased();

    let parens = expr
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .erased();

    let atom = choice((array, value, new, cast, ident.clone().map(Expr::Ident)))
        .map_with(|ex, e| (ex, e.span()))
        .or(parens)
        .recover_with(via_parser(nested_delimiters(
            Token::LParen,
            Token::RParen,
            [
                (Token::LBracket, Token::RBracket),
                (Token::LBrace, Token::RBrace),
            ],
            |span| (Expr::Error, span),
        )))
        .erased();

    // Array member intrinsics require a type-aware REDmod semantic adapter.
    // A method name alone cannot distinguish an array from a user-defined receiver.
    let member_access = just(Token::Period)
        .ignore_then(extended_ident())
        .map(TopPrecedence::MemberAccess)
        .erased();
    let array_access = expr
        .clone()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map(|args| TopPrecedence::ArrayAccess(args.into()))
        .erased();
    let call = arguments
        .map(|args| TopPrecedence::Call(args.into()))
        .erased();
    let member = atom
        .foldl_with(
            choice((member_access, array_access, call)).repeated(),
            |expr, member, e| {
                let expr = Box::new(expr);
                let res = match member {
                    TopPrecedence::MemberAccess(member) => Expr::Member { expr, member },
                    TopPrecedence::ArrayAccess(index) => Expr::Index { expr, index },
                    TopPrecedence::Call(args) => Expr::Call { expr, args },
                };
                (res, e.span())
            },
        )
        .then_ignore(generic_call_start.not())
        .recover_with(via_parser(nested_delimiters(
            Token::LBracket,
            Token::RBracket,
            [
                (Token::LParen, Token::RParen),
                (Token::LBrace, Token::RBrace),
            ],
            |span| (Expr::Error, span),
        )))
        .erased();

    // Prefer REDmod cast syntax over grouped-identifier calls: (T)(x) is
    // always a cast, even if T could name a value. Casts and unary operators
    // associate right-to-left, below postfix and above binary/dynamic casts.
    // Thus (x)-y casts -y, while (x)+y is grouping (there is no unary '+').
    let signed_min = just(Token::Minus)
        .ignore_then(select! {
            Token::IntMinMagnitude => Expr::Constant(Constant::I32(i32::MIN)),
            Token::LongMinMagnitude => Expr::Constant(Constant::I64(i64::MIN)),
        })
        .map_with(|expr, e| (expr, e.span()));

    let unops = recursive(|unary| {
        let c_cast = typ
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with(|typ, e| (typ, e.span()))
            .then(unary.clone())
            .map_with(|((typ, _prefix_span), operand), e| {
                (
                    Expr::Cast {
                        expr: Box::new(operand),
                        typ: Box::new(typ),
                        style: CastStyle::CStyle,
                    },
                    e.span(),
                )
            });
        let unary_op = unop.then(unary).map_with(|(op, expr), e| {
            (
                Expr::UnOp {
                    op,
                    expr: Box::new(expr),
                },
                e.span(),
            )
        });
        choice((signed_min, c_cast, unary_op, member))
    })
    .erased();

    let as_ = unops
        .foldl_with(
            just(Token::Ident("as")).ignore_then(typ).repeated(),
            |expr, typ, e| {
                let expr = Box::new(expr);
                let typ = Box::new(typ);
                (Expr::DynCast { expr, typ }, e.span())
            },
        )
        .erased();

    let binops = as_
        .clone()
        .then(binop.then(as_).repeated().collect::<Vec<_>>())
        .map(|(lhs, ops)| climb_prec(lhs, &mut ops.into_iter().peekable(), 0))
        .erased();

    let ternary = binops
        .then(
            just(Token::Question)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::Colon))
                .then(expr.clone())
                .or_not(),
        )
        .map_with(|(cond, tern), e| match tern {
            Some((then, els)) => {
                let cond = Box::new(cond);
                let then = Box::new(then);
                let else_ = Box::new(els);
                (Expr::Conditional { cond, then, else_ }, e.span())
            }
            None => cond,
        })
        .erased();

    let assign = ternary
        .clone()
        .then(assignment.then(expr).or_not())
        .map_with(|(lhs, rhs), e| match rhs {
            Some((op, rhs)) => {
                let lhs = Box::new(lhs);
                let rhs = Box::new(rhs);
                let expr = match op {
                    Some(op) => Expr::BinOp { lhs, op, rhs },
                    None => Expr::Assign { lhs, rhs },
                };
                (expr, e.span())
            }
            None => lhs,
        })
        .erased();

    assign.labelled("expression").as_context().erased()
}

#[derive(Debug)]
enum TopPrecedence<'src> {
    Call(Box<[(SourceExpr<'src>, Span)]>),
    ArrayAccess(Box<(SourceExpr<'src>, Span)>),
    MemberAccess(&'src str),
}

fn climb_prec<'src, I>(
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
            } else if lookahead.assoc() == redscript_community_ast::Assoc::Right
                && lookahead.precedence() == op.precedence()
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
