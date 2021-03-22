use redscript::ast::{Expr, Pos, Seq};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Definition, Local, LocalFlags};
use redscript::error::Error;

use crate::scope::Scope;
use crate::transform::ExprTransformer;
use crate::typechecker::{Callable, IntrinsicOp, Typed};
use crate::{Reference, TypeId};

pub struct Desugar<'a> {
    pool: &'a mut ConstantPool,
    scope: &'a mut Scope,
    prefix_exprs: Vec<Expr<Typed>>,
}

impl<'a> ExprTransformer<Typed> for Desugar<'a> {
    fn on_array_lit(&mut self, exprs: Vec<Expr<Typed>>, type_: Option<TypeId>, pos: Pos) -> Result<Expr<Typed>, Error> {
        let type_ = TypeId::Array(Box::new(type_.unwrap()));
        let local = self.fresh_local(&type_)?;
        let reference = Expr::Ident(Reference::Local(local), pos);

        self.add_prefix(Expr::Declare(local, Some(type_), None, pos));
        for expr in exprs {
            self.add_prefix(Expr::Call(
                Callable::Intrinsic(IntrinsicOp::ArrayPush, TypeId::Void),
                vec![Expr::Ident(Reference::Local(local), pos), expr],
                pos,
            ))
        }
        Ok(reference)
    }

    fn on_seq(&mut self, seq: Seq<Typed>) -> Result<Seq<Typed>, Error> {
        let mut processed = Vec::with_capacity(seq.exprs.len());
        for expr in seq.exprs {
            let done = self.on_expr(expr)?;
            if !self.prefix_exprs.is_empty() {
                processed.append(&mut self.prefix_exprs);
            }
            processed.push(done);
        }
        Ok(Seq::new(processed))
    }
}

impl<'a> Desugar<'a> {
    pub fn new(pool: &'a mut ConstantPool, scope: &'a mut Scope) -> Self {
        Desugar {
            pool,
            scope,
            prefix_exprs: vec![],
        }
    }

    fn add_prefix(&mut self, expr: Expr<Typed>) {
        self.prefix_exprs.push(expr)
    }

    fn fresh_local(&mut self, type_: &TypeId) -> Result<PoolIndex<Local>, Error> {
        let type_idx = self.scope.get_type_index(&type_, self.pool)?;
        let local = Local::new(type_idx, LocalFlags::new());
        let def = Definition::local(PoolIndex::UNDEFINED, self.scope.function.unwrap(), local);
        let idx = self.pool.push_definition(def);
        Ok(idx.cast())
    }
}
