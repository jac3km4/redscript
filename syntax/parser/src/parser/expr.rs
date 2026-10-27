use std::iter;

use chumsky::prelude::*;
use redscript_ast::{
    Assoc, BinOp, Constant, Dialect, Expr, FunctionBody, Param, ParamQualifiers, SourceBlock,
    SourceExpr, SourceType, Span, StrPart, Type, UnOp,
};

use super::{Parse, ident, member_name, type_with_span};
use crate::lexer::Token;
use crate::{Branch, ParseOptions, parser_input};

pub fn expr_with_span_rec<'tok, 'src: 'tok>(
    opts: ParseOptions,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
) -> impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> {
    expr_with_span_impl(opts, expr, block)
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
    opts: ParseOptions,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
) -> impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> {
    let dialect = opts.dialect;
    let compat = opts.is_compat();
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

    let binop = select! {
        Token::AssignAdd => BinOp::AssignAdd,
        Token::AssignSub => BinOp::AssignSub,
        Token::AssignMul => BinOp::AssignMul,
        Token::AssignDiv => BinOp::AssignDiv,
        Token::AssignBitOr => BinOp::AssignBitOr,
        Token::AssignBitAnd => BinOp::AssignBitAnd,
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

    // 0.5.x compatibility: compound assignments are parsed at the assignment level instead
    let assign_op = select! {
        Token::AssignAdd => BinOp::AssignAdd,
        Token::AssignSub => BinOp::AssignSub,
        Token::AssignMul => BinOp::AssignMul,
        Token::AssignDiv => BinOp::AssignDiv,
        Token::AssignBitOr => BinOp::AssignBitOr,
        Token::AssignBitAnd => BinOp::AssignBitAnd,
    }
    .labelled("binary operator");
    let compat_binop = select! {
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
    let binop = if compat {
        compat_binop.left_branch()
    } else {
        binop.right_branch()
    };

    let interp_str = expr
        .clone()
        .nested_in(select_ref! { Token::Group(tok) = ex => parser_input(tok, *ex.ctx()) })
        .map(StrPart::Expr)
        .or(select! { Token::StrFrag(str) => StrPart::Str(str) })
        .repeated()
        .collect::<Vec<_>>()
        .nested_in(select_ref! { Token::InterpStr(tok) = ex => parser_input(tok, *ex.ctx()) })
        .map(|parts| Expr::InterpolatedString(parts.into()))
        .erased();

    let ident = ident();
    let typ = type_with_span(dialect);

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

    let new = match dialect {
        Dialect::Community => just(Token::Ident("new"))
            .ignore_then(typ.clone())
            .then(arguments.clone())
            .map(|(typ, args)| Expr::New {
                typ: typ.into(),
                args: args.into(),
            })
            .left_branch(),
        // REDmod: `new X` without parentheses
        Dialect::Redmod => just(Token::Ident("new"))
            .ignore_then(typ.clone())
            .then(arguments.clone().or_not())
            .map(|(typ, args)| Expr::New {
                typ: typ.into(),
                args: args.unwrap_or_default().into(),
            })
            .right_branch(),
    };

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

    // 0.5.x compatibility: `default` is a valid variable name, except at the start of a
    // `default:` switch arm
    let ident_atom = if compat {
        ident
            .clone()
            .or(just(Token::Default)
                .then_ignore(just(Token::Colon).not())
                .to("default"))
            .left_branch()
    } else {
        ident.clone().right_branch()
    };

    let atom = choice((
        array,
        value,
        interp_str,
        new,
        ident_atom.map(Expr::Ident),
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
        .ignore_then(member_name(opts))
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
    // 0.5.x compatibility: `as` is a postfix operator, so `x as T.f` is `(x as T).f` and
    // `!x as T` is `!(x as T)`
    let postfix = if compat {
        choice((
            member_access,
            array_access,
            call,
            just(Token::Ident("as"))
                .ignore_then(typ.clone())
                .map(TopPrecedence::As),
        ))
        .left_branch()
    } else {
        choice((member_access, array_access, call)).right_branch()
    };
    let member = atom
        .foldl_with(postfix.repeated(), |expr, member, e| {
            let expr = Box::new(expr);
            let res = match member {
                TopPrecedence::MemberAccess(member) => Expr::Member { expr, member },
                TopPrecedence::ArrayAccess(index) => Expr::Index { expr, index },
                TopPrecedence::Call(type_args, args) => Expr::Call {
                    expr,
                    type_args,
                    args,
                },
                TopPrecedence::As(typ) => Expr::DynCast {
                    expr,
                    typ: Box::new(typ),
                },
            };
            (res, e.span())
        })
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

    let prefix = match dialect {
        Dialect::Community => unop.map(Prefix::UnOp).left_branch(),
        Dialect::Redmod => c_style_cast(dialect)
            .map(Prefix::Cast)
            .or(unop.map(Prefix::UnOp))
            .right_branch(),
    };

    let unops = prefix
        .repeated()
        .foldr_with(member, |prefix, expr, e| {
            let expr = Box::new(expr);
            let res = match prefix {
                Prefix::UnOp(op) => Expr::UnOp { op, expr },
                Prefix::Cast(typ) => Expr::DynCast {
                    expr,
                    typ: Box::new(typ),
                },
            };
            (res, e.span())
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
                .then(expr)
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

    let assign = if compat {
        // 0.5.x compatibility: `=` and the compound assignments share the lowest precedence
        // level and are right-associative, so `a += b ? 1 : 2` is `a += (b ? 1 : 2)` and
        // `a = b = c` is `a = (b = c)`
        recursive(|assign| {
            ternary
                .clone()
                .then(
                    just(Token::Assign)
                        .to(None)
                        .or(assign_op.map(Some))
                        .then(assign)
                        .or_not(),
                )
                .map_with(|(lhs, rhs), e| match rhs {
                    Some((op, rhs)) => {
                        let lhs = Box::new(lhs);
                        let rhs = Box::new(rhs);
                        let res = match op {
                            Some(op) => Expr::BinOp { lhs, op, rhs },
                            None => Expr::Assign { lhs, rhs },
                        };
                        (res, e.span())
                    }
                    None => lhs,
                })
        })
        .left_branch()
    } else {
        ternary
            .clone()
            .then(just(Token::Assign).ignore_then(ternary).or_not())
            .map_with(|(lhs, rhs), e| match rhs {
                Some(rhs) => {
                    let lhs = Box::new(lhs);
                    let rhs = Box::new(rhs);
                    (Expr::Assign { lhs, rhs }, e.span())
                }
                None => lhs,
            })
            .right_branch()
    };

    assign.labelled("expression").as_context().erased()
}

/// REDmod C-style cast prefix `(Type)`, only accepted when it cannot be mistaken for a
/// parenthesized expression:
/// - the type is a plain name starting with an uppercase letter (optionally with type arguments)
/// - the following token can start a unary expression
///
/// This means `(a) + b`, `(a)`, `(a)(b)` and `(a) - b` stay parenthesized expressions, while
/// `(Int32)x`, `(Int32)-x` and `(EInputKey)(n)` are casts. A leading minus is only accepted as
/// the operand when the cast type is a primitive name.
fn c_style_cast<'tok, 'src: 'tok>(
    dialect: Dialect,
) -> impl Parse<'tok, 'src, (SourceType<'src>, Span)> {
    let name = ident().filter(|name| name.starts_with(|c: char| c.is_ascii_uppercase()));
    let args = type_with_span(dialect)
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LAngle), just(Token::RAngle))
        .or_not();
    let operand_start = select! {
        Token::Minus => true,
        Token::Ident(name) if name != "as" => false,
        Token::Null => false,
        Token::This => false,
        Token::Super => false,
        Token::True => false,
        Token::False => false,
        Token::Int(_) => false,
        Token::Uint(_) => false,
        Token::Ulong(_) => false,
        Token::Long(_) => false,
        Token::Float(_) => false,
        Token::Double(_) => false,
        Token::Str(_) => false,
        Token::CName(_) => false,
        Token::ResRef(_) => false,
        Token::TdbId(_) => false,
        Token::LParen => false,
        Token::Not => false,
        Token::BitNot => false,
    };

    name.then(args)
        .map_with(|(name, args), e| {
            let args = args.unwrap_or_default().into();
            (Type::Named { name, args }, e.span())
        })
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .then(operand_start.rewind())
        // `(Upper) - b` is a subtraction unless the cast type is a primitive
        .filter(|((typ, _), minus)| !*minus || is_primitive_cast(typ))
        .map(|(typ, _)| typ)
        .erased()
}

fn is_primitive_cast(typ: &SourceType<'_>) -> bool {
    matches!(
        typ,
        Type::Named { name, args } if args.is_empty()
            && matches!(
                *name,
                "Int8" | "Int16" | "Int32" | "Int64" | "Uint8" | "Uint16" | "Uint32" | "Uint64"
                    | "Float" | "Double" | "Bool" | "String" | "CName" | "Variant"
            )
    )
}

#[derive(Debug)]
enum Prefix<'src> {
    UnOp(UnOp),
    Cast((SourceType<'src>, Span)),
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
    As((SourceType<'src>, Span)),
}

#[cfg(test)]
mod tests {
    use redscript_ast::{FileId, Type};
    use similar_asserts::assert_eq;

    use super::*;
    use crate::parse_expr;

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
