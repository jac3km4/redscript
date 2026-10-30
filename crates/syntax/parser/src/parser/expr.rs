use std::iter;

use chumsky::prelude::*;
use redscript_ast::{
    Assoc, BinOp, Constant, Expr, FunctionBody, Param, ParamQualifiers, SourceBlock, SourceExpr,
    SourceType, Span, StrPart, UnOp,
};

use super::{Parse, extended_ident, ident, type_with_span};
use crate::lexer::Token;
use crate::parser_input;

pub fn expr_with_span_rec<'tok, 'src: 'tok>(
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
) -> impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> {
    expr_with_span_impl(expr, block)
        // handle trailing period explicitly because it's a common error
        .then(just(Token::Period).map_with(|_, e| e.span()).or_not())
        .validate(|(exp, period), _, errs| {
            if let Some(span) = period {
                errs.emit(Rich::custom(span, "unexpected '.'"));
            };
            exp
        })
}

fn expr_with_span_impl<'tok, 'src: 'tok>(
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
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
    .or(select! {
        Token::IntMinMagnitude => "invalid Int32 literal; use the `l` suffix for Int64 or `ul` for Uint64",
        Token::LongMinMagnitude => "invalid Int64 literal; use the `ul` suffix for a larger nonnegative integer",
    }
    .validate(|message, e, emitter| {
        emitter.emit(Rich::custom(e.span(), message));
        Expr::Error
    }))
    .labelled("value");

    let unop = select! {
        Token::Minus => UnOp::Neg,
        Token::Not => UnOp::Not,
        Token::BitNot => UnOp::BitNot,
    }
    .labelled("unary operator");

    let assignment_op = select! {
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

    let interp_str = expr
        .clone()
        .nested_in(select_ref! { Token::Group(tok) = ex => {
            let span: Span = ex.span();
            parser_input(tok, *ex.ctx(), span.end)
        } })
        .map(StrPart::Expr)
        .or(select! { Token::StrFrag(str) => StrPart::Str(str) })
        .repeated()
        .collect::<Vec<_>>()
        .nested_in(select_ref! { Token::InterpStr(tok) = ex => {
            let span: Span = ex.span();
            parser_input(tok, *ex.ctx(), span.end - 1)
        } })
        .map(|parts| Expr::InterpolatedString(parts.into()))
        .erased();

    let ident = ident();
    let typ = type_with_span();

    let arguments = expr
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .erased();
    let type_arguments = typ
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LAngle), just(Token::RAngle))
        .erased();

    let new = just(Token::Ident("new"))
        .ignore_then(typ.clone())
        .then(arguments.clone())
        .map(|(typ, args)| Expr::New {
            typ: typ.into(),
            args: args.into(),
        })
        .erased();

    let array = expr
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map(|els| Expr::ArrayLit(els.into()))
        .erased();

    let lambda = ident
        .clone()
        .then(just(Token::Colon).ignore_then(typ.clone()).or_not())
        .map_with(|(name, typ), e| (Param::new(name, typ, ParamQualifiers::default()), e.span()))
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .then_ignore(just(Token::Arrow))
        .then(
            block
                .clone()
                .map(FunctionBody::Block)
                .or(expr.clone().map(|e| FunctionBody::Inline(Box::new(e)))),
        )
        .map(|(params, body)| Expr::Lambda {
            params: params.into(),
            body,
        })
        .erased();

    let parens = expr
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .erased();

    let atom = choice((
        array,
        value,
        interp_str,
        new,
        ident.clone().map(Expr::Ident),
        lambda,
    ))
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

    let member_access = just(Token::Period)
        .ignore_then(extended_ident())
        .map(TopPrecedence::MemberAccess)
        .erased();
    let array_access = expr
        .clone()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map(|args| TopPrecedence::ArrayAccess(args.into()))
        .erased();
    let call = type_arguments
        .or_not()
        .then(arguments)
        .map(|(targs, args)| TopPrecedence::Call(targs.unwrap_or_default().into(), args.into()))
        .erased();
    let member = atom
        .foldl_with(
            choice((member_access, array_access, call)).repeated(),
            |expr, member, e| {
                let expr = Box::new(expr);
                let res = match member {
                    TopPrecedence::MemberAccess(member) => Expr::Member { expr, member },
                    TopPrecedence::ArrayAccess(index) => Expr::Index { expr, index },
                    TopPrecedence::Call(type_args, args) => Expr::Call {
                        expr,
                        type_args,
                        args,
                    },
                };
                (res, e.span())
            },
        )
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

    let signed_min = just(Token::Minus)
        .ignore_then(select! {
            Token::IntMinMagnitude => Expr::Constant(Constant::I32(i32::MIN)),
            Token::LongMinMagnitude => Expr::Constant(Constant::I64(i64::MIN)),
        })
        .map_with(|expr, e| (expr, e.span()));

    let unops = recursive(|unary| {
        signed_min.or(unop
            .then(unary)
            .map_with(|(op, expr), e| {
                let expr = Box::new(expr);
                (Expr::UnOp { op, expr }, e.span())
            })
            .or(member))
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
        .then(assignment_op.then(expr).or_not())
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

#[derive(Debug)]
enum TopPrecedence<'src> {
    Call(
        Box<[(SourceType<'src>, Span)]>,
        Box<[(SourceExpr<'src>, Span)]>,
    ),
    ArrayAccess(Box<(SourceExpr<'src>, Span)>),
    MemberAccess(&'src str),
}

#[cfg(test)]
mod tests {
    use redscript_ast::{FileId, Type};
    use similar_asserts::assert_eq;

    use super::*;
    use crate::parse_expr;

    #[test]
    fn signed_integer_minima() {
        let file = FileId::from_i32(7);
        for (literal, expected) in [
            ("2147483648", Constant::I32(i32::MIN)),
            ("9223372036854775808l", Constant::I64(i64::MIN)),
        ] {
            for separator in ["", " ", " /* minimum */ "] {
                let source = format!("-{separator}{literal}");
                let (tokens, errors) = crate::lex(&source, file);
                assert!(errors.is_empty(), "{source}: {errors:?}");
                let tokens = tokens.unwrap();
                let (expr, errors) = crate::parse(super::super::expr_with_span(), &tokens, file);
                assert!(errors.is_empty(), "{source}: {errors:?}");
                let (expr, span) = expr.unwrap();
                assert_eq!(expr.unwrapped(), Expr::Constant(expected.clone()));
                assert_eq!(span, Span::new(0, source.len() as u32, file));
            }
        }
        let (_, errors) = crate::parse_stmt("data.sortingPriority = -2147483648;", file);
        assert!(errors.is_empty(), "{errors:?}");
    }

    #[test]
    fn signed_integer_negation_and_subtraction() {
        let file = FileId::from_i32(0);
        for (source, expected) in [
            ("-42", Constant::I32(42)),
            ("-9223372036854775807l", Constant::I64(i64::MAX)),
        ] {
            let (expr, errors) = parse_expr(source, file);
            assert!(errors.is_empty(), "{source}: {errors:?}");
            assert_eq!(
                expr.unwrap().unwrapped(),
                Expr::UnOp {
                    op: UnOp::Neg,
                    expr: Expr::Constant(expected).into(),
                }
            );
        }
        for (source, rhs) in [
            ("x-1", Expr::Constant(Constant::I32(1))),
            ("x--2147483648", Expr::Constant(Constant::I32(i32::MIN))),
            (
                "x - -9223372036854775808l",
                Expr::Constant(Constant::I64(i64::MIN)),
            ),
        ] {
            let (expr, errors) = parse_expr(source, file);
            assert!(errors.is_empty(), "{source}: {errors:?}");
            assert_eq!(
                expr.unwrap().unwrapped(),
                Expr::BinOp {
                    lhs: Expr::Ident("x").into(),
                    op: BinOp::Sub,
                    rhs: rhs.into(),
                }
            );
        }
        for (source, equivalent) in [
            ("-x.Member", "-(x.Member)"),
            ("-x * y", "(-x) * y"),
            ("!-2147483648", "!(-2147483648)"),
            ("- -9223372036854775808l", "-(-9223372036854775808l)"),
        ] {
            let (actual, errors) = parse_expr(source, file);
            assert!(errors.is_empty(), "{source}: {errors:?}");
            let (expected, errors) = parse_expr(equivalent, file);
            assert!(errors.is_empty(), "{equivalent}: {errors:?}");
            assert_eq!(actual.unwrap().unwrapped(), expected.unwrap().unwrapped());
        }
    }

    #[test]
    fn signed_integer_overflow_is_rejected() {
        for source in [
            "2147483648",
            "2147483649",
            "9223372036854775808l",
            "9223372036854775809l",
            "18446744073709551616",
            "-2147483649",
            "-9223372036854775809l",
            "x-2147483648",
            "x-9223372036854775808l",
            "-(2147483648)",
            "~2147483648",
            "-2147483648.Member",
        ] {
            let (_, errors) = parse_expr(source, FileId::from_i32(0));
            assert!(!errors.is_empty(), "{source} must be rejected");
        }
    }

    #[test]
    fn operators() {
        let code = r#"-b + 10 * 23-4 / 20 + 2"#;
        let res = parse_expr(code, FileId::from_i32(0));
        assert_eq!(res.1, []);
        let res = res.0.unwrap().unwrapped();

        assert_eq!(
            res,
            Expr::BinOp {
                lhs: Expr::BinOp {
                    lhs: Expr::BinOp {
                        lhs: Expr::UnOp {
                            op: UnOp::Neg,
                            expr: Expr::Ident("b").into()
                        }
                        .into(),
                        op: BinOp::Add,
                        rhs: Expr::BinOp {
                            lhs: Expr::Constant(Constant::I32(10)).into(),
                            op: BinOp::Mul,
                            rhs: Expr::Constant(Constant::I32(23)).into()
                        }
                        .into()
                    }
                    .into(),
                    op: BinOp::Sub,
                    rhs: Expr::BinOp {
                        lhs: Expr::Constant(Constant::I32(4)).into(),
                        op: BinOp::Div,
                        rhs: Expr::Constant(Constant::I32(20)).into()
                    }
                    .into()
                }
                .into(),
                op: BinOp::Add,
                rhs: Expr::Constant(Constant::I32(2)).into()
            }
        );
    }

    #[test]
    fn assignment_precedence_and_associativity() {
        for op in ["=", "+=", "-=", "*=", "/=", "|=", "&="] {
            for (source, expected) in [
                (
                    format!("x {op} flag ? 1 : 2"),
                    format!("x {op} (flag ? 1 : 2)"),
                ),
                (format!("x {op} y {op} z"), format!("x {op} (y {op} z)")),
                (format!("x {op} y + z * 2"), format!("x {op} (y + (z * 2))")),
                (format!("x = y {op} z"), format!("x = (y {op} z)")),
            ] {
                let (actual, errors) = parse_expr(&source, FileId::from_i32(0));
                assert!(errors.is_empty(), "{source}: {errors:?}");
                let (expected, errors) = parse_expr(&expected, FileId::from_i32(0));
                assert!(errors.is_empty(), "{errors:?}");
                assert_eq!(actual.unwrap().unwrapped(), expected.unwrap().unwrapped());
            }
        }
    }

    #[test]
    fn comparison() {
        let code = r#"5 > 4"#;
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(
            res,
            Expr::BinOp {
                lhs: Expr::Constant(Constant::I32(5)).into(),
                op: BinOp::Gt,
                rhs: Expr::Constant(Constant::I32(4)).into()
            }
        );
    }

    #[test]
    fn nested_ternary() {
        let code = "true ? false ? 1 : 2 : 3";
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();
        assert_eq!(
            res,
            Expr::Conditional {
                cond: Expr::Constant(Constant::Bool(true)).into(),
                then: Expr::Conditional {
                    cond: Expr::Constant(Constant::Bool(false)).into(),
                    then: Expr::Constant(Constant::I32(1)).into(),
                    else_: Expr::Constant(Constant::I32(2)).into(),
                }
                .into(),
                else_: Expr::Constant(Constant::I32(3)).into(),
            }
        );
    }

    #[test]
    fn member_access() {
        let code = r#"obj[obj.index].method()[0].field"#;
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(
            res,
            Expr::Member {
                expr: Expr::Index {
                    expr: Expr::Call {
                        expr: Expr::Member {
                            member: "method",
                            expr: Expr::Index {
                                expr: Expr::Ident("obj").into(),
                                index: Expr::Member {
                                    expr: Expr::Ident("obj").into(),
                                    member: "index",
                                }
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                        type_args: [].into(),
                        args: [].into(),
                    }
                    .into(),
                    index: Expr::Constant(Constant::I32(0)).into(),
                }
                .into(),
                member: "field",
            }
        );
    }

    #[test]
    fn number_literals() {
        let code = r#"[1, 2l, 3u, 4ul, 5., 6.d]"#;
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(
            res,
            Expr::ArrayLit(
                [
                    Expr::Constant(Constant::I32(1)),
                    Expr::Constant(Constant::I64(2)),
                    Expr::Constant(Constant::U32(3)),
                    Expr::Constant(Constant::U64(4)),
                    Expr::Constant(Constant::F32(5.)),
                    Expr::Constant(Constant::F64(6.)),
                ]
                .into()
            )
        );
    }

    #[test]
    fn str_literals() {
        let code = r#"["a", n"b", r"c", t"d"]"#;
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(
            res,
            Expr::ArrayLit(
                [
                    Expr::Constant(Constant::String("a".into())),
                    Expr::Constant(Constant::CName("b".into())),
                    Expr::Constant(Constant::Resource("c".into())),
                    Expr::Constant(Constant::TweakDbId("d".into())),
                ]
                .into()
            )
        );
    }

    #[test]
    fn cast() {
        let code = r#"Cast<Uint8>(1)"#;
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(
            res,
            Expr::Call {
                expr: Expr::Ident("Cast").into(),
                type_args: [Type::plain("Uint8")].into(),
                args: [Expr::Constant(Constant::I32(1))].into(),
            }
        );
    }

    #[test]
    fn str_interp() {
        let code = r#"s"2 + 2 is \(2 + 2)""#;
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(
            res,
            Expr::InterpolatedString(
                [
                    StrPart::Str("2 + 2 is ".into()),
                    StrPart::Expr(Expr::BinOp {
                        lhs: Expr::Constant(Constant::I32(2)).into(),
                        op: BinOp::Add,
                        rhs: Expr::Constant(Constant::I32(2)).into(),
                    }),
                ]
                .into()
            )
        );
    }

    #[test]
    fn escaped_string() {
        let code = r#""te\"\u{0A}st""#;
        let res = parse_expr(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(res, Expr::Constant(Constant::String("te\"\nst".into())));
    }
}
