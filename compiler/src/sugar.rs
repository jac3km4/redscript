use std::mem;

use redscript::ast::{BinOp, Constant, Expr, Literal, Seq, SourceAst};
use redscript::bytecode::Intrinsic;
use redscript::Str;

use crate::codegen::names;
use crate::visit_expr;

#[derive(Debug, Default)]
pub struct Desugar {
    to_prepend: Vec<Expr<SourceAst>>,
    local_count: usize,
}

impl Desugar {
    pub fn run(seq: &mut Seq<SourceAst>) {
        Self::default().apply_seq(seq);
    }

    fn apply(&mut self, expr: &mut Expr<SourceAst>) {
        match expr {
            Expr::ArrayLit(elems, _, span) => {
                let name = self.new_array();
                let allocate = Expr::Call(
                    Expr::Ident(Str::from_static(Intrinsic::ArrayResize.into()), *span).into(),
                    (),
                    [].into(),
                    [
                        Expr::Ident(name.clone(), *span),
                        Expr::Constant(Constant::I32(elems.len() as i32), *span),
                    ]
                    .into(),
                    (),
                    *span,
                );
                self.to_prepend.push(Expr::Declare(name.clone(), None, None, *span));
                self.to_prepend.push(allocate);

                for (i, elem) in elems.iter_mut().enumerate() {
                    self.apply(elem);
                    let array_elem = Expr::ArrayElem(
                        Expr::Ident(name.clone(), *span).into(),
                        Expr::Constant(Constant::I32(i as i32), *span).into(),
                        (),
                        *span,
                    );
                    self.to_prepend
                        .push(Expr::Assign(array_elem.into(), mem::take(elem).into(), (), *span));
                }
                *expr = Expr::Ident(name, *span);
            }
            Expr::ForIn(loc, it, body, span) => {
                self.apply_seq(body);
                self.apply(it);

                let counter = self.new_counter();
                let array = self.new_array();

                let get_element = Expr::ArrayElem(
                    Expr::Ident(array.clone(), *span).into(),
                    Expr::Ident(counter.clone(), *span).into(),
                    (),
                    *span,
                );
                let loop_body = vec![
                    Expr::Assign(Expr::Ident(loc.clone(), *span).into(), get_element.into(), (), *span),
                    Expr::Seq(mem::take(body)),
                    Expr::BinOp(
                        Expr::Ident(counter.clone(), *span).into(),
                        Expr::Constant(Constant::I32(1), *span).into(),
                        BinOp::AssignAdd,
                        *span,
                    ),
                ];
                let array_size = Expr::Call(
                    Expr::Ident(Str::from_static(Intrinsic::ArraySize.into()), *span).into(),
                    (),
                    [].into(),
                    [Expr::Ident(array.clone(), *span)].into(),
                    (),
                    *span,
                );
                let condition = Expr::BinOp(
                    Expr::Ident(counter.clone(), *span).into(),
                    array_size.into(),
                    BinOp::Less,
                    *span,
                );
                let replacement = vec![
                    Expr::Declare(
                        counter,
                        None,
                        Some(Expr::Constant(Constant::I32(0), *span).into()),
                        *span,
                    ),
                    Expr::Declare(array, None, Some(mem::take(it)), *span),
                    Expr::Declare(loc.clone(), None, None, *span),
                    Expr::While(condition.into(), Seq::new(loop_body), *span),
                ];

                *expr = Expr::Seq(Seq::new(replacement));
            }
            Expr::InterpolatedString(prefix, parts, span) => {
                let concat = parts.iter_mut().fold(
                    Expr::Constant(Constant::String(Literal::String, prefix.clone()), *span),
                    |acc, (el, postfix)| {
                        let span = el.span();
                        let to_string = Expr::Ident(Str::from_static(Intrinsic::ToString.into()), span);
                        let el_as_str = Expr::Call(to_string.into(), (), [].into(), [mem::take(el)].into(), (), span);
                        let to_add = if postfix.is_empty() {
                            el_as_str
                        } else {
                            let postfix = Expr::Constant(Constant::String(Literal::String, postfix.clone()), span);
                            Expr::BinOp(el_as_str.into(), postfix.into(), BinOp::Add, span)
                        };
                        Expr::BinOp(acc.into(), to_add.into(), BinOp::Add, span)
                    },
                );
                *expr = concat;
            }
            Expr::While(cond, body, _) => {
                self.apply(cond);
                self.apply_seq(body);
            }
            Expr::Switch(scrutinee, cases, default, _, _) => {
                self.apply(scrutinee);
                for case in cases {
                    self.apply_seq(&mut case.body);
                }
                if let Some(default) = default {
                    self.apply_seq(default);
                }
            }
            Expr::If(cond, if_, else_, _) => {
                self.apply(cond);
                self.apply_seq(if_);
                if let Some(else_) = else_ {
                    self.apply_seq(else_);
                }
            }
            Expr::Seq(seq) => {
                self.apply_seq(seq);
            }
            _ => visit_expr!(self, apply, expr, mut),
        }
    }

    fn apply_seq(&mut self, seq: &mut Seq<SourceAst>) {
        for expr in &mut seq.exprs {
            self.apply(expr);
            if !self.to_prepend.is_empty() {
                let mut local_seq = mem::take(&mut self.to_prepend);
                local_seq.push(mem::take(expr));
                *expr = Expr::Seq(Seq::new(local_seq));
            }
        }
    }

    fn new_counter(&mut self) -> Str {
        let name = names::local(format_args!("counter${}", self.local_count));
        self.local_count += 1;
        name
    }

    fn new_array(&mut self) -> Str {
        let name = names::local(format_args!("array${}", self.local_count));
        self.local_count += 1;
        name
    }
}
