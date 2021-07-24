use std::rc::Rc;
use std::vec;

use redscript::ast::{BinOp, Constant, Expr, Ident, Pos, Seq, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::IntrinsicOp;
use redscript::definition::{Definition, Local, LocalFlags};
use redscript::error::Error;

use crate::scope::Scope;
use crate::transform::ExprTransformer;
use crate::typechecker::{type_of, Callable, TypedAst};
use crate::{FunctionSignature, FunctionSignatureBuilder, Reference, TypeId, Value};

pub struct Desugar<'a> {
    pool: &'a mut ConstantPool,
    scope: &'a mut Scope,
    name_count: usize,
    prefix_exprs: Vec<Expr<TypedAst>>,
    locals: Vec<PoolIndex<Local>>,
}

impl<'a> Desugar<'a> {
    pub fn new(scope: &'a mut Scope, pool: &'a mut ConstantPool) -> Self {
        Desugar {
            pool,
            scope,
            prefix_exprs: vec![],
            locals: vec![],
            name_count: 0,
        }
    }

    pub fn locals(self) -> Vec<PoolIndex<Local>> {
        self.locals
    }

    fn add_prefix(&mut self, expr: Expr<TypedAst>) {
        self.prefix_exprs.push(expr)
    }

    fn get_function(&self, signature: FunctionSignature, pos: Pos) -> Result<Callable, Error> {
        let fun_idx = self
            .scope
            .resolve_function(Ident::new(signature.name().to_owned()), pos)?
            .by_id(&signature, self.pool)
            .ok_or_else(|| Error::function_not_found(signature.as_ref(), pos))?;

        Ok(Callable::Function(fun_idx))
    }

    fn fresh_local(&mut self, type_: &TypeId) -> Result<Reference, Error> {
        let fun_idx = self.scope.function.unwrap();
        let name_idx = self.pool.names.add(Rc::new(format!("synthetic${}", self.name_count)));
        let type_idx = self.scope.get_type_index(type_, self.pool)?;
        let local = Local::new(type_idx, LocalFlags::new());
        let def = Definition::local(name_idx, fun_idx, local);
        let idx = self.pool.add_definition(def);
        self.locals.push(idx);
        self.name_count += 1;
        Ok(Reference::Value(Value::Local(idx)))
    }
}

impl<'a> ExprTransformer<TypedAst> for Desugar<'a> {
    fn on_array_lit(
        &mut self,
        exprs: Vec<Expr<TypedAst>>,
        type_: Option<TypeId>,
        pos: Pos,
    ) -> Result<Expr<TypedAst>, Error> {
        let type_ = TypeId::Array(Box::new(type_.unwrap()));
        let local = self.fresh_local(&type_)?;

        for expr in exprs {
            let callable = Callable::Intrinsic(IntrinsicOp::ArrayPush, TypeId::Void);
            let expr = self.on_expr(expr)?;
            self.add_prefix(Expr::Call(callable, vec![Expr::Ident(local.clone(), pos), expr], pos))
        }
        Ok(Expr::Ident(local, pos))
    }

    fn on_for_in(
        &mut self,
        name: PoolIndex<Local>,
        array: Expr<TypedAst>,
        seq: Seq<TypedAst>,
        pos: Pos,
    ) -> Result<Expr<TypedAst>, Error> {
        let mut seq = self.on_seq(seq)?;

        let array = self.on_expr(array)?;
        let arr_type = type_of(&array, self.scope, self.pool)?;
        let arr_local = self.fresh_local(&arr_type)?;

        let counter_type = self.scope.resolve_type(&TypeName::INT32, self.pool, pos)?;
        let counter_local = self.fresh_local(&counter_type)?;

        self.add_prefix(Expr::Assign(
            Box::new(Expr::Ident(arr_local.clone(), pos)),
            Box::new(array),
            pos,
        ));
        self.add_prefix(Expr::Assign(
            Box::new(Expr::Ident(counter_local.clone(), pos)),
            Box::new(Expr::Constant(Constant::I32(0), pos)),
            pos,
        ));

        let array_size = Callable::Intrinsic(IntrinsicOp::ArraySize, counter_type);
        let assign_add = FunctionSignatureBuilder::new(BinOp::AssignAdd.to_string())
            .parameter(&TypeName::INT32, true)
            .parameter(&TypeName::INT32, false)
            .return_type(&TypeName::INT32);
        let assign_add = self.get_function(assign_add, pos)?;

        let less_than = FunctionSignatureBuilder::new(BinOp::Less.to_string())
            .parameter(&TypeName::INT32, false)
            .parameter(&TypeName::INT32, false)
            .return_type(&TypeName::BOOL);
        let less_than = self.get_function(less_than, pos)?;

        let condition = Expr::Call(
            less_than,
            vec![
                Expr::Ident(counter_local.clone(), pos),
                Expr::Call(array_size, vec![Expr::Ident(arr_local.clone(), pos)], pos),
            ],
            pos,
        );
        let assign_iter_value = Expr::Assign(
            Box::new(Expr::Ident(Reference::Value(Value::Local(name)), pos)),
            Box::new(Expr::ArrayElem(
                Box::new(Expr::Ident(arr_local, pos)),
                Box::new(Expr::Ident(counter_local.clone(), pos)),
                pos,
            )),
            pos,
        );
        let increment_counter = Expr::Call(
            assign_add,
            vec![Expr::Ident(counter_local, pos), Expr::Constant(Constant::I32(1), pos)],
            pos,
        );

        let mut body = vec![assign_iter_value];
        body.append(&mut seq.exprs);
        body.push(increment_counter);

        Ok(Expr::While(Box::new(condition), Seq::new(body), pos))
    }

    fn on_seq(&mut self, seq: Seq<TypedAst>) -> Result<Seq<TypedAst>, Error> {
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
