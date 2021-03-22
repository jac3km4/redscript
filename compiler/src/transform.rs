use std::fmt::Debug;

use redscript::ast::{BinOp, Constant, Expr, NameKind, Pos, Seq, SwitchCase, Target, UnOp};
use redscript::error::Error;

pub trait ExprTransformer<N: NameKind>
where
    N: NameKind,
    N::Reference: Debug,
    N::Callable: Debug,
    N::Local: Debug,
    N::Function: Debug,
    N::Member: Debug,
    N::Type: Debug,
{
    fn on_ident(&mut self, reference: N::Reference, pos: Pos) -> Result<Expr<N>, Error> {
        Ok(Expr::Ident(reference, pos))
    }

    fn on_constant(&mut self, constant: Constant, pos: Pos) -> Result<Expr<N>, Error> {
        Ok(Expr::Constant(constant, pos))
    }

    fn on_array_lit(&mut self, exprs: Vec<Expr<N>>, type_: Option<N::Type>, pos: Pos) -> Result<Expr<N>, Error> {
        let mut processed = Vec::with_capacity(exprs.len());
        for expr in exprs {
            processed.push(self.on_expr(expr)?);
        }
        Ok(Expr::ArrayLit(processed, type_, pos))
    }

    fn on_declare(
        &mut self,
        local: N::Local,
        type_: Option<N::Type>,
        init: Option<Expr<N>>,
        pos: Pos,
    ) -> Result<Expr<N>, Error> {
        let init = init
            .into_iter()
            .fold(Ok(None), |_, expr| self.on_expr(expr).map(Some))?;
        Ok(Expr::Declare(local, type_, init.map(Box::new), pos))
    }

    fn on_cast(&mut self, type_: N::Type, expr: Expr<N>, pos: Pos) -> Result<Expr<N>, Error> {
        Ok(Expr::Cast(type_, Box::new(self.on_expr(expr)?), pos))
    }

    fn on_assign(&mut self, lhs: Expr<N>, rhs: Expr<N>, pos: Pos) -> Result<Expr<N>, Error> {
        let lhs = Box::new(self.on_expr(lhs)?);
        let rhs = Box::new(self.on_expr(rhs)?);
        Ok(Expr::Assign(lhs, rhs, pos))
    }

    fn on_call(&mut self, callable: N::Callable, args: Vec<Expr<N>>, pos: Pos) -> Result<Expr<N>, Error> {
        let mut processed = Vec::with_capacity(args.len());
        for arg in args {
            processed.push(self.on_expr(arg)?)
        }
        Ok(Expr::Call(callable, processed, pos))
    }

    fn on_method_call(
        &mut self,
        context: Expr<N>,
        name: N::Function,
        args: Vec<Expr<N>>,
        pos: Pos,
    ) -> Result<Expr<N>, Error> {
        let context = self.on_expr(context)?;
        let mut processed = Vec::with_capacity(args.len());
        for arg in args {
            processed.push(self.on_expr(arg)?)
        }
        Ok(Expr::MethodCall(Box::new(context), name, processed, pos))
    }

    fn on_member(&mut self, context: Expr<N>, name: N::Member, pos: Pos) -> Result<Expr<N>, Error> {
        let context = self.on_expr(context)?;
        Ok(Expr::Member(Box::new(context), name, pos))
    }

    fn on_array_elem(&mut self, expr: Expr<N>, index: Expr<N>, pos: Pos) -> Result<Expr<N>, Error> {
        let expr = self.on_expr(expr)?;
        let index = self.on_expr(index)?;
        Ok(Expr::ArrayElem(Box::new(expr), Box::new(index), pos))
    }

    fn on_new(&mut self, name: N::Type, args: Vec<Expr<N>>, pos: Pos) -> Result<Expr<N>, Error> {
        let mut processed = Vec::with_capacity(args.len());
        for arg in args {
            processed.push(self.on_expr(arg)?)
        }
        Ok(Expr::New(name, processed, pos))
    }

    fn on_return(&mut self, expr: Option<Expr<N>>, pos: Pos) -> Result<Expr<N>, Error> {
        let expr = expr
            .into_iter()
            .fold(Ok(None), |_, expr| self.on_expr(expr).map(Some))?;
        Ok(Expr::Return(expr.map(Box::new), pos))
    }

    fn on_switch(
        &mut self,
        matched: Expr<N>,
        cases: Vec<SwitchCase<N>>,
        default: Option<Seq<N>>,
    ) -> Result<Expr<N>, Error> {
        let matched = self.on_expr(matched)?;
        let mut processed = Vec::with_capacity(cases.len());
        for case in cases {
            let matcher = self.on_expr(case.matcher)?;
            let body = self.on_seq(case.body)?;
            processed.push(SwitchCase { matcher, body });
        }
        let default = default
            .into_iter()
            .fold(Ok(None), |_, expr| self.on_seq(expr).map(Some))?;
        Ok(Expr::Switch(Box::new(matched), processed, default))
    }

    fn on_goto(&mut self, target: Target, pos: Pos) -> Result<Expr<N>, Error> {
        Ok(Expr::Goto(target, pos))
    }

    fn on_if(&mut self, cond: Expr<N>, if_: Seq<N>, else_: Option<Seq<N>>, pos: Pos) -> Result<Expr<N>, Error> {
        let cond = self.on_expr(cond)?;
        let if_ = self.on_seq(if_)?;
        let else_ = else_
            .into_iter()
            .fold(Ok(None), |_, expr| self.on_seq(expr).map(Some))?;
        Ok(Expr::If(Box::new(cond), if_, else_, pos))
    }

    fn on_conditional(&mut self, cond: Expr<N>, true_: Expr<N>, false_: Expr<N>, pos: Pos) -> Result<Expr<N>, Error> {
        let cond = self.on_expr(cond)?;
        let true_ = self.on_expr(true_)?;
        let false_ = self.on_expr(false_)?;
        Ok(Expr::Conditional(
            Box::new(cond),
            Box::new(true_),
            Box::new(false_),
            pos,
        ))
    }

    fn on_while(&mut self, cond: Expr<N>, body: Seq<N>, pos: Pos) -> Result<Expr<N>, Error> {
        let cond = self.on_expr(cond)?;
        let body = self.on_seq(body)?;
        Ok(Expr::While(Box::new(cond), body, pos))
    }

    fn on_binop(&mut self, lhs: Expr<N>, rhs: Expr<N>, op: BinOp, pos: Pos) -> Result<Expr<N>, Error> {
        let lhs = self.on_expr(lhs)?;
        let rhs = self.on_expr(rhs)?;
        Ok(Expr::BinOp(Box::new(lhs), Box::new(rhs), op, pos))
    }

    fn on_unop(&mut self, expr: Expr<N>, op: UnOp, pos: Pos) -> Result<Expr<N>, Error> {
        let expr = self.on_expr(expr)?;
        Ok(Expr::UnOp(Box::new(expr), op, pos))
    }

    fn on_this(&mut self, pos: Pos) -> Result<Expr<N>, Error> {
        Ok(Expr::This(pos))
    }

    fn on_super(&mut self, pos: Pos) -> Result<Expr<N>, Error> {
        Ok(Expr::Super(pos))
    }

    fn on_break(&mut self, pos: Pos) -> Result<Expr<N>, Error> {
        Ok(Expr::Break(pos))
    }

    fn on_null(&mut self) -> Result<Expr<N>, Error> {
        Ok(Expr::Null)
    }

    fn on_expr(&mut self, expr: Expr<N>) -> Result<Expr<N>, Error> {
        match expr {
            Expr::Ident(reference, pos) => self.on_ident(reference, pos),
            Expr::Constant(constant, pos) => self.on_constant(constant, pos),
            Expr::ArrayLit(exprs, type_, pos) => self.on_array_lit(exprs, type_, pos),
            Expr::Declare(local, type_, init, pos) => self.on_declare(local, type_, init.map(|e| *e), pos),
            Expr::Cast(type_, expr, pos) => self.on_cast(type_, *expr, pos),
            Expr::Assign(lhs, rhs, pos) => self.on_assign(*lhs, *rhs, pos),
            Expr::Call(callable, args, pos) => self.on_call(callable, args, pos),
            Expr::MethodCall(context, name, args, pos) => self.on_method_call(*context, name, args, pos),
            Expr::Member(context, name, pos) => self.on_member(*context, name, pos),
            Expr::ArrayElem(expr, index, pos) => self.on_array_elem(*expr, *index, pos),
            Expr::New(name, args, pos) => self.on_new(name, args, pos),
            Expr::Return(expr, pos) => self.on_return(expr.map(|e| *e), pos),
            Expr::Seq(seq) => self.on_seq(seq).map(Expr::Seq),
            Expr::Switch(matched, cases, default) => self.on_switch(*matched, cases, default),
            Expr::Goto(target, pos) => self.on_goto(target, pos),
            Expr::If(cond, if_, else_, pos) => self.on_if(*cond, if_, else_, pos),
            Expr::Conditional(cond, true_, false_, pos) => self.on_conditional(*cond, *true_, *false_, pos),
            Expr::While(cond, body, pos) => self.on_while(*cond, body, pos),
            Expr::BinOp(lhs, rhs, op, pos) => self.on_binop(*lhs, *rhs, op, pos),
            Expr::UnOp(expr, op, pos) => self.on_unop(*expr, op, pos),
            Expr::This(pos) => self.on_this(pos),
            Expr::Super(pos) => self.on_super(pos),
            Expr::Break(pos) => self.on_break(pos),
            Expr::Null => self.on_null(),
        }
    }

    fn on_seq(&mut self, seq: Seq<N>) -> Result<Seq<N>, Error> {
        let mut processed = Vec::with_capacity(seq.exprs.len());
        for expr in seq.exprs {
            processed.push(self.on_expr(expr)?)
        }
        Ok(Seq::new(processed))
    }
}
