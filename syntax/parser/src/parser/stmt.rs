use std::iter;

use chumsky::prelude::*;
use redscript_ast::{
    ArraySpread, Case, Condition, ConditionalBlock, Dialect, LetCondition, Pattern, SourceBlock,
    SourceExpr, SourcePattern, SourceStmt, SourceType, Span, Stmt,
};

use super::{Parse, ident_with_span, name_with_span, type_with_span};
use crate::lexer::Token;
use crate::{Branch, ParseOptions};

pub fn stmt_rec<'tok, 'src: 'tok>(
    opts: ParseOptions,
    expr: impl Parse<'tok, 'src, (SourceExpr<'src>, Span)> + 'tok,
    stmt: impl Parse<'tok, 'src, SourceStmt<'src>> + 'tok,
    block: impl Parse<'tok, 'src, SourceBlock<'src>> + 'tok,
) -> impl Parse<'tok, 'src, SourceStmt<'src>> {
    let dialect = opts.dialect;
    let typ = type_with_span(dialect);

    let semicolon = just(Token::Semicolon).or_not().validate(|semi, ctx, errs| {
        if semi.is_none() {
            errs.emit(Rich::custom(ctx.span(), "expected ';' after expression"));
        }
    });

    let let_kw = match dialect {
        Dialect::Community => just(Token::Ident("let")).ignored().left_branch(),
        // `var x : T = e;` is equivalent to `let x: T = e;`
        Dialect::Redmod => just(Token::Ident("let"))
            .or(just(Token::Ident("var")))
            .ignored()
            .right_branch(),
    };

    let let_core = let_kw
        .ignore_then(name_with_span(opts))
        .then(just(Token::Colon).ignore_then(typ.clone()).or_not())
        .then(just(Token::Assign).ignore_then(expr.clone()).or_not())
        .map(|((name, typ), value)| {
            let value = value.map(Box::new);
            let typ = typ.map(Box::new);
            Stmt::Let { name, typ, value }
        })
        .erased();

    let let_ = let_core.clone().then_ignore(semicolon.clone()).erased();

    let pattern = recursive(|this| {
        let array = this
            .clone()
            .map(ArrayElemPattern::Elem)
            .or(just(Token::Period)
                .repeated()
                .exactly(2)
                .to(ArrayElemPattern::Spread))
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .try_map(|mut arr, span| {
                let (elems, spread) = match &arr[..] {
                    [ArrayElemPattern::Spread, ..] => (arr.drain(1..), ArraySpread::Start),
                    [.., ArrayElemPattern::Spread] => {
                        (arr.drain(..arr.len() - 1), ArraySpread::End)
                    }
                    _ => (arr.drain(..), ArraySpread::None),
                };
                let elems = elems
                    .map(|elem| match elem {
                        ArrayElemPattern::Spread => {
                            Err(Rich::custom(span, "unexpected spread in array pattern"))
                        }
                        ArrayElemPattern::Elem(pat) => Ok(pat),
                    })
                    .collect::<Result<_, _>>()?;
                Ok(Pattern::Array(spread, (elems, span)))
            })
            .erased();

        let fields = ident_with_span()
            .then(just(Token::Colon).ignore_then(this).or_not())
            .map(|(name, pat)| (name, pat.unwrap_or(Pattern::Name(name))))
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace));

        let binding = ident_with_span()
            .then(fields.or_not())
            .map(|(name, fields)| match fields {
                Some(fields) => Pattern::Aggregate(name, fields.into()),
                None => Pattern::Name(name),
            });
        let atom = choice((array, binding));

        atom.foldl_with(
            just(Token::Ident("as"))
                .ignore_then(typ)
                .map(|(typ, span)| PatternSuffix::As(typ, span))
                .or(just(Token::Question).to(PatternSuffix::Nullable))
                .repeated(),
            |inner, typ, e| match typ {
                PatternSuffix::As(typ, span) => Pattern::As((inner, e.span()).into(), (typ, span)),
                PatternSuffix::Nullable => Pattern::Nullable((inner, e.span()).into()),
            },
        )
    })
    .map_with(|stmt, e| (stmt, e.span()))
    .erased();

    let condition = just(Token::Ident("let"))
        .ignore_then(pattern.clone())
        .map(Condition::Pattern)
        .or(expr.clone().map(Condition::Expr))
        .erased();

    let let_condition = just(Token::Ident("let"))
        .ignore_then(pattern)
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|(pat, expr)| LetCondition::LetPattern(pat, expr))
        .or(expr.clone().map(LetCondition::Expr))
        .erased();

    let case_body = stmt
        .map_with(|stmt, e| (stmt, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .erased();
    let cases = just(Token::Case)
        .ignore_then(condition.clone())
        .then_ignore(just(Token::Colon))
        .then(case_body.clone())
        .map(|(m, body)| Case::new(m, body))
        .repeated()
        .collect::<Vec<_>>()
        .then(
            just(Token::Default)
                .ignore_then(just(Token::Colon).ignore_then(case_body))
                .or_not(),
        )
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .erased();

    let switch = just(Token::Ident("switch"))
        .ignore_then(expr.clone())
        .then(cases)
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|(expr, (cases, default))| Stmt::Switch {
            expr: expr.into(),
            cases: cases.into(),
            default: default.map(Into::into),
        })
        .erased();

    let if_ = just(Token::Ident("if"))
        .ignore_then(let_condition.clone().then(block.clone()))
        .map(|(cond, body)| ConditionalBlock::new(cond, body))
        .erased();
    let else_if = just(Token::Ident("else")).ignore_then(if_.clone());
    let else_ = just(Token::Ident("else")).ignore_then(block.clone());
    let if_stmt = if_
        .then(else_if.repeated().collect::<Vec<_>>())
        .then(else_.or_not())
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|((if_, else_ifs), else_)| Stmt::If {
            blocks: iter::once(if_).chain(else_ifs).collect(),
            else_,
        })
        .erased();

    let while_stmt = just(Token::Ident("while"))
        .ignore_then(let_condition.then(block.clone()))
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|(cond, body)| Stmt::While(ConditionalBlock::new(cond, body).into()))
        .erased();

    let for_in = just(Token::Ident("for"))
        .ignore_then(ident_with_span())
        .then_ignore(just(Token::Ident("in")))
        .then(expr.clone())
        .then(block.clone())
        .then_ignore(just(Token::Semicolon).or_not())
        .map(|((name, iter), body)| {
            let iter = Box::new(iter);
            Stmt::ForIn { name, iter, body }
        })
        .erased();

    let for_stmt = match dialect {
        Dialect::Community => for_in.left_branch(),
        Dialect::Redmod => {
            // C-style `for (init; cond; step) { ... }`
            let init = let_core
                .or(expr.clone().map(|e| Stmt::Expr(e.into())))
                .map_with(|stmt, e| (stmt, e.span()))
                .or_not();
            let c_for = just(Token::Ident("for"))
                .ignore_then(
                    init.then_ignore(just(Token::Semicolon))
                        .then(expr.clone().or_not())
                        .then_ignore(just(Token::Semicolon))
                        .then(expr.clone().or_not())
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .then(block)
                .then_ignore(just(Token::Semicolon).or_not())
                .map(|(((init, cond), step), body)| Stmt::For {
                    init: init.map(Box::new),
                    cond: cond.map(Box::new),
                    step: step.map(Box::new),
                    body,
                })
                .erased();
            for_in.or(c_for).right_branch()
        }
    };

    let return_stmt = just(Token::Ident("return"))
        .ignore_then(expr.clone().or_not())
        .then_ignore(semicolon.clone())
        .map(|e| Stmt::Return(e.map(Box::new)))
        .erased();

    let break_stmt = just(Token::Ident("break"))
        .ignore_then(semicolon.clone())
        .map(|_| Stmt::Break)
        .erased();

    let continue_stmt = just(Token::Ident("continue"))
        .ignore_then(semicolon.clone())
        .map(|_| Stmt::Continue)
        .erased();

    let expr_stmt = expr.then_ignore(semicolon).map(|e| Stmt::Expr(e.into()));

    choice((
        let_,
        switch,
        if_stmt,
        while_stmt,
        for_stmt,
        return_stmt,
        break_stmt,
        continue_stmt,
        expr_stmt,
    ))
    .labelled("statement")
    .as_context()
    .erased()
}

#[derive(Debug, Clone)]
enum PatternSuffix<'src> {
    As(SourceType<'src>, Span),
    Nullable,
}

#[derive(Debug, Clone)]
enum ArrayElemPattern<'src> {
    Spread,
    Elem(SourcePattern<'src>),
}

#[cfg(test)]
mod tests {
    use redscript_ast::{BinOp, Block, Condition, Constant, Expr, FileId, Type};
    use similar_asserts::assert_eq;

    use super::*;
    use crate::{Error, Span, parse_stmt};

    #[test]
    fn if_else_chain() {
        let code = r#"
        if true {
            return 1;
        } else if false {
            return 2;
        } else {
            return 3;
        }
        "#;

        assert_eq!(
            parse_stmt(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Stmt::If {
                blocks: [
                    ConditionalBlock::new(
                        LetCondition::Expr(Expr::Constant(Constant::Bool(true))),
                        Block::single(Stmt::Return(Some(Expr::Constant(Constant::I32(1)).into())))
                    ),
                    ConditionalBlock::new(
                        LetCondition::Expr(Expr::Constant(Constant::Bool(false))),
                        Block::single(Stmt::Return(Some(Expr::Constant(Constant::I32(2)).into())))
                    ),
                ]
                .into(),
                else_: Some(Block::single(Stmt::Return(Some(
                    Expr::Constant(Constant::I32(3)).into()
                )))),
            }
        );
    }

    #[test]
    fn switch() {
        let code = r#"
        switch a {
            case 0:
                break;
            case 1:
                return 0;
            default:
                return 1;
        }
        "#;

        assert_eq!(
            parse_stmt(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Stmt::Switch {
                expr: Expr::Ident("a").into(),
                cases: [
                    Case::new(
                        Condition::Expr(Expr::Constant(Constant::I32(0))),
                        [Stmt::Break]
                    ),
                    Case::new(
                        Condition::Expr(Expr::Constant(Constant::I32(1))),
                        [(Stmt::Return(Some(Expr::Constant(Constant::I32(0)).into())))]
                    ),
                ]
                .into(),
                default: Some([Stmt::Return(Some(Expr::Constant(Constant::I32(1)).into()))].into()),
            }
        );
    }

    #[test]
    fn while_() {
        let code = r#"
        while i > 0 {
            i = i - 1;
        }
        "#;

        assert_eq!(
            parse_stmt(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Stmt::While(
                ConditionalBlock::new(
                    LetCondition::Expr(Expr::BinOp {
                        op: BinOp::Gt,
                        lhs: Box::new(Expr::Ident("i")),
                        rhs: Box::new(Expr::Constant(Constant::I32(0))),
                    }),
                    Block::single(Stmt::Expr(
                        Expr::Assign {
                            lhs: Box::new(Expr::Ident("i")),
                            rhs: Box::new(Expr::BinOp {
                                op: BinOp::Sub,
                                lhs: Box::new(Expr::Ident("i")),
                                rhs: Box::new(Expr::Constant(Constant::I32(1))),
                            }),
                        }
                        .into()
                    ))
                )
                .into()
            )
        );
    }

    #[test]
    fn for_in() {
        let code = r#"
        for i in range {
            print(i);
        }
        "#;

        assert_eq!(
            parse_stmt(code, FileId::from_i32(0)).0.unwrap().unwrapped(),
            Stmt::ForIn {
                name: "i",
                iter: Expr::Ident("range").into(),
                body: Block::single(Stmt::Expr(
                    Expr::Call {
                        expr: Expr::Ident("print").into(),
                        type_args: [].into(),
                        args: [Expr::Ident("i")].into(),
                    }
                    .into()
                )),
            }
        );
    }

    #[test]
    fn stmt_with_comments() {
        let code = r#"
        // a line comment
        /* block comment */
        /* /* */ */
        let a: Int32 = 1;
        "#;
        let res = parse_stmt(code, FileId::from_i32(0)).0.unwrap().unwrapped();

        assert_eq!(
            res,
            Stmt::Let {
                name: "a",
                typ: Some(Type::plain("Int32").into()),
                value: Some(Expr::Constant(Constant::I32(1)).into()),
            }
        );
    }

    #[test]
    fn missing_semicolon() {
        let code = "a";
        let file = FileId::from_i32(0);
        let (stmt, errors) = parse_stmt(code, file);

        assert_eq!(
            errors,
            vec![Error::Parse(
                "expected ';' after expression in statement".into(),
                Span::from((file, 0..1))
            )]
        );
        assert_eq!(
            stmt.expect("should parse").unwrapped(),
            Stmt::Expr(Expr::Ident("a").into())
        );
    }

    #[test]
    fn trailing_comma() {
        let code = "a.";
        let file = FileId::from_i32(0);
        let (stmt, errors) = parse_stmt(code, file);

        assert_eq!(
            errors,
            vec![
                Error::Parse(
                    "unexpected '.' in statement".into(),
                    Span::from((file, 0..2))
                ),
                Error::Parse(
                    "expected ';' after expression in statement".into(),
                    Span::from((file, 0..2))
                )
            ]
        );
        assert_eq!(
            stmt.expect("should parse").unwrapped(),
            Stmt::Expr(Expr::Ident("a").into())
        );
    }
}
