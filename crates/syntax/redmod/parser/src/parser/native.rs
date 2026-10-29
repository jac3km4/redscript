use chumsky::prelude::*;
use redscript_redmod_ast::*;

use super::{Parse, ident_with_span, type_with_span};
use crate::Token;

pub fn statement<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Statement<'src>> {
    recursive(|stmt| {
        let expr = super::expr_with_span();
        let block = super::block_rec(stmt.clone());
        let body = super::recovered_stmt(stmt)
            .map_with(|stmt, e| (stmt, e.span()))
            .repeated()
            .collect::<Vec<_>>();
        let cond = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen));
        let if_block = just(Token::Ident("if"))
            .ignore_then(cond.clone())
            .then(block.clone());
        let if_ = if_block
            .clone()
            .then(
                just(Token::Ident("else"))
                    .ignore_then(if_block)
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                just(Token::Ident("else"))
                    .ignore_then(block.clone())
                    .or_not(),
            )
            .then_ignore(just(Token::Semicolon).or_not())
            .map(|((first, rest), else_)| Statement::If {
                blocks: std::iter::once(first).chain(rest).collect(),
                else_,
            });
        let while_ = just(Token::Ident("while"))
            .ignore_then(cond.clone())
            .then(block.clone())
            .then_ignore(just(Token::Semicolon).or_not())
            .map(|(condition, body)| Statement::While { condition, body });
        let semicolon = just(Token::Semicolon).or_not().validate(|semi, e, errs| {
            if semi.is_none() {
                let span: Span = e.span();
                errs.emit(Rich::custom(
                    Span::new(span.start, span.start, span.file),
                    "expected ';' after statement",
                ));
            }
        });
        let do_ = just(Token::Ident("do"))
            .ignore_then(block.clone())
            .then_ignore(just(Token::Ident("while")))
            .then(cond.clone())
            .then_ignore(semicolon.clone())
            .map(|(body, condition)| Statement::DoWhile { body, condition });
        let label = just(Token::Case)
            .ignore_then(expr.clone())
            .map(Some)
            .or(just(Token::Default).to(None))
            .then_ignore(just(Token::Colon))
            .then(body)
            .map_with(|(value, body), e| (SwitchLabel { value, body }, e.span()));
        let switch = just(Token::Ident("switch"))
            .ignore_then(cond)
            .then(
                label
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .then_ignore(just(Token::Semicolon).or_not())
            .map(|(expr, labels)| Statement::Switch { expr, labels });
        let sequence = expr
            .clone()
            .separated_by(just(Token::Comma))
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|mut exprs, e| {
                if exprs.len() == 1 {
                    exprs.remove(0)
                } else {
                    (Expr::Sequence(exprs.into()), e.span())
                }
            });
        let local = just(Token::Ident("const"))
            .map_with(|_, e| e.span())
            .or_not()
            .then(one_of([Token::Ident("var"), Token::Ident("let")]))
            .then(
                ident_with_span()
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Colon))
            .then(type_with_span())
            .then(just(Token::Assign).ignore_then(expr.clone()).or_not())
            .map(|((((const_, _), names), typ), value)| {
                Statement::Local(Local {
                    const_,
                    names,
                    typ,
                    value,
                })
            });
        let init = local
            .clone()
            .or(sequence.clone().map(|expr| Statement::Expr(Box::new(expr))))
            .map_with(|stmt, e| Box::new((stmt, e.span())))
            .or_not();
        let for_ = just(Token::Ident("for"))
            .ignore_then(
                init.then_ignore(just(Token::Semicolon))
                    .then(sequence.clone().or_not())
                    .then_ignore(just(Token::Semicolon))
                    .then(sequence.clone().or_not())
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then(block.clone())
            .then_ignore(just(Token::Semicolon).or_not())
            .map(|(((init, condition), step), body)| Statement::For {
                init,
                condition,
                step,
                body,
            });
        let return_ = just(Token::Ident("return"))
            .ignore_then(expr.clone().or_not())
            .then_ignore(semicolon.clone())
            .map(|expr| Statement::Return(expr.map(Box::new)));
        choice((
            if_,
            while_,
            do_,
            switch,
            for_,
            local.then_ignore(semicolon.clone()),
            block
                .then_ignore(just(Token::Semicolon).or_not())
                .map(Statement::Block),
            just(Token::Semicolon).to(Statement::Empty),
            return_,
            just(Token::Ident("break"))
                .then_ignore(semicolon.clone())
                .to(Statement::Break),
            just(Token::Ident("continue"))
                .then_ignore(semicolon.clone())
                .to(Statement::Continue),
            sequence
                .then_ignore(semicolon)
                .map(|expr| Statement::Expr(Box::new(expr))),
        ))
        .labelled("statement")
        .as_context()
        .erased()
    })
}

pub fn declaration<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Declaration<'src>> {
    recursive(|decl| {
        let expr = super::expr_with_span();
        let block = super::block_rec(statement());
        let metadata = ident_with_span()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map_with(|(key, value), e| (Metadata { key, value }, e.span()))
            .separated_by(just(Token::Comma))
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket));
        let prefix = metadata
            .repeated()
            .collect::<Vec<_>>()
            .then(
                super::item::annotation(expr.clone())
                    .map_with(|a, e| (a, e.span()))
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(super::item::visibility().or_not())
            .then(
                modifier()
                    .map_with(|m, e| (m, e.span()))
                    .repeated()
                    .collect::<Vec<_>>(),
            );
        let directive = just(Token::Default)
            .to(DirectiveKind::Default)
            .or(just(Token::Ident("hint")).to(DirectiveKind::Hint))
            .then(ident_with_span())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::Semicolon))
            .map(|((kind, target), value)| Item::Directive {
                kind,
                target,
                value,
            });
        let aggregate = one_of([Token::Ident("class"), Token::Ident("struct")])
            .then(ident_with_span())
            .then(
                just(Token::Ident("extends"))
                    .ignore_then(type_with_span())
                    .or_not(),
            )
            .then(
                super::declarations(decl, true)
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .then_ignore(just(Token::Semicolon).or_not())
            .map(|(((kind, name), extends), items)| Item::Aggregate {
                is_struct: kind == Token::Ident("struct"),
                name,
                extends,
                items,
            });
        let variants = ident_with_span()
            .then(
                just(Token::Assign)
                    .ignore_then(
                        just(Token::Minus)
                            .ignore_then(just(Token::IntMinMagnitude))
                            .to(i32::MIN)
                            .or(just(Token::Minus)
                                .or_not()
                                .then(select! { Token::Int(i) => i })
                                .map(
                                    |(minus, value)| {
                                        if minus.is_some() { -value } else { value }
                                    },
                                ))
                            .map_with(|value, e| (value, e.span())),
                    )
                    .or_not(),
            )
            .map_with(|(name, value), e| (EnumVariant { name, value }, e.span()))
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace));
        let enum_ = just(Token::Ident("enum"))
            .ignore_then(ident_with_span())
            .then(just(Token::Colon).ignore_then(type_with_span()).or_not())
            .then(variants)
            .then_ignore(just(Token::Semicolon).or_not())
            .map(|((name, storage), variants)| Item::Enum {
                name,
                variants,
                storage,
            });
        let params = select! {
            Token::Ident("optional") => ParamModifier::Optional,
            Token::Ident("out") => ParamModifier::Out,
            Token::Ident("const") => ParamModifier::Const,
                        Token::Ident("skip") => ParamModifier::Skip,
        }
        .map_with(|modifier, e| (modifier, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .then(
            ident_with_span()
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::Colon))
        .then(type_with_span())
        .map_with(|((modifiers, names), typ), e| {
            (
                ParamGroup {
                    modifiers,
                    names,
                    typ,
                },
                e.span(),
            )
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen));
        let function_kind = just(Token::Ident("function"))
            .ignore_then(ident_with_span())
            .map(FunctionKind::Function)
            .or(just(Token::Ident("event"))
                .ignore_then(ident_with_span())
                .map(FunctionKind::Event))
            .or(just(Token::Ident("operator"))
                .ignore_then(operator().map_with(|op, e| (op, e.span())))
                .map(FunctionKind::Operator))
            .or(just(Token::Ident("implicit"))
                .map_with(|_, e| e.span())
                .or_not()
                .then(just(Token::Ident("cast")).map_with(|_, e| e.span()))
                .map(|(implicit, span)| FunctionKind::Cast { implicit, span }));
        let ret = one_of([Token::Colon, Token::Arrow])
            .ignore_then(
                just(Token::Ident("const"))
                    .map_with(|_, e| e.span())
                    .or_not(),
            )
            .then(type_with_span())
            .or_not();
        let function = function_kind
            .then(params)
            .then(ret)
            .then(block.or_not())
            .then_ignore(just(Token::Semicolon).or_not())
            .map(|(((kind, params), ret), body)| {
                let (const_return, return_type) = ret.map_or((None, None), |(c, t)| (c, Some(t)));
                Item::Function {
                    kind,
                    params,
                    return_type,
                    const_return,
                    body,
                }
            });
        prefix
            .then(choice((
                directive,
                aggregate,
                enum_,
                function,
                super::item::field(expr).map(Item::Field),
            )))
            .map_with(
                |((((metadata, annotations), visibility), modifiers), item), e| Declaration {
                    annotations,
                    visibility,
                    modifiers,
                    metadata: metadata.into_iter().flatten().collect(),
                    docs: e.ctx().at(e.span().start),
                    item,
                },
            )
            .labelled("declaration")
            .erased()
    })
}

fn modifier<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Modifier> {
    select! {
        Token::Ident("abstract") => Modifier::Abstract,
        Token::Ident("const") => Modifier::Const,
        Token::Ident("exec") => Modifier::Exec,
        Token::Ident("final") => Modifier::Final,
        Token::Ident("importonly") => Modifier::ImportOnly,
        Token::Ident("import") => Modifier::Import,
        Token::Ident("persistent") => Modifier::Persistent,
        Token::Ident("quest") => Modifier::Quest,
        Token::Ident("static") => Modifier::Static,
        Token::Ident("editable") => Modifier::Editable,
        Token::Ident("instanceeditable") => Modifier::InstanceEditable,
        Token::Ident("inlined") => Modifier::Inlined,
        Token::Ident("replicated") => Modifier::Replicated,
        Token::Ident("testonly") => Modifier::TestOnly,
        Token::Ident("override") => Modifier::Override,
        Token::Ident("export") => Modifier::Export,
        Token::Ident("virtual") => Modifier::Virtual,
        Token::Ident("constexpr") => Modifier::Constexpr,
        Token::Ident("mutable") => Modifier::Mutable,
    }
}

fn operator<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Operator> {
    select! {
        Token::Plus => Operator::Add, Token::Minus => Operator::Subtract,
        Token::Star => Operator::Multiply, Token::Slash => Operator::Divide, Token::Percent => Operator::Modulo,
        Token::Eq => Operator::Equal, Token::Ne => Operator::NotEqual,
        Token::LAngle => Operator::Less, Token::Le => Operator::LessEqual,
        Token::RAngle => Operator::Greater, Token::Ge => Operator::GreaterEqual,
        Token::And => Operator::And, Token::Or => Operator::Or,
        Token::BitAnd => Operator::BitAnd, Token::BitOr => Operator::BitOr, Token::BitXor => Operator::BitXor,
        Token::Not => Operator::Not, Token::BitNot => Operator::BitNot,
        Token::AssignAdd => Operator::AssignAdd, Token::AssignSub => Operator::AssignSubtract,
        Token::AssignMul => Operator::AssignMultiply, Token::AssignDiv => Operator::AssignDivide,
        Token::AssignBitAnd => Operator::AssignBitAnd, Token::AssignBitOr => Operator::AssignBitOr,
    }.or(just(Token::LBracket).then_ignore(just(Token::RBracket)).to(Operator::Index))
}

pub fn module<'tok, 'src: 'tok>() -> impl Parse<'tok, 'src, Module<'src>> {
    let path =
        select! { Token::Str(std::borrow::Cow::Borrowed(path)) => path }.try_map(|path, span| {
            if path.split('.').all(crate::is_path_identifier) {
                Ok(Path::new(path.split('.').collect::<Vec<_>>()))
            } else {
                Err(Rich::custom(
                    span,
                    "expected a dotted module path without escapes",
                ))
            }
        });
    let using = just(Token::At)
        .ignore_then(just(Token::Ident("using")))
        .ignore_then(path)
        .then_ignore(just(Token::Semicolon).or_not())
        .map_with(|path, e| {
            (
                Declaration {
                    annotations: vec![],
                    visibility: None,
                    modifiers: vec![],
                    metadata: vec![],
                    docs: {
                        let docs: &super::DeclarationDocs<'src> = e.ctx();
                        let span: Span = e.span();
                        docs.at(span.start)
                    },
                    item: Item::Import(path),
                },
                e.span(),
            )
        });
    just(Token::At)
        .ignore_then(just(Token::Ident("module")))
        .ignore_then(path)
        .then_ignore(just(Token::Semicolon).or_not())
        .or_not()
        .then(using.repeated().collect::<Vec<_>>())
        .then(super::declarations(declaration(), false))
        .map(|((path, mut imports), items)| {
            imports.extend(items);
            Module {
                path,
                items: imports,
            }
        })
}
