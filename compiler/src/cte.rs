use std::collections::HashSet;

use enum_as_inner::EnumAsInner;
use redscript::ast::{BinOp, Constant, Expr, Literal, SourceAst, UnOp};

use crate::error::Error;
use crate::symbol::ModulePath;

#[derive(Debug, EnumAsInner)]
pub enum Value {
    Bool(bool),
}

pub struct Context {
    modules: HashSet<ModulePath>,
}

impl Context {
    pub fn new(modules: HashSet<ModulePath>) -> Self {
        Self { modules }
    }

    pub fn eval(&self, expr: &Expr<SourceAst>) -> Result<Value, Error> {
        match expr {
            Expr::Constant(constant, span) => match constant {
                Constant::Bool(val) => Ok(Value::Bool(*val)),
                _ => Err(Error::CteError("unsupported constant", *span)),
            },
            Expr::Call(ident, _, args, span) => match (ident.as_ref(), &args[..]) {
                ("ModuleExists", &[Expr::Constant(Constant::String(Literal::String, ref str), _)]) => {
                    Ok(Value::Bool(self.does_module_exist(str.as_ref())))
                }
                _ => Err(Error::CteError("unsupported funtion", *span)),
            },
            Expr::Conditional(cond, true_, false_, _) => match self.eval(&*cond)? {
                Value::Bool(true) => self.eval(&*true_),
                Value::Bool(false) => self.eval(&*false_),
            },
            Expr::BinOp(lhs, rhs, op, span) => match (op, self.eval(&*lhs)?, self.eval(&*rhs)?) {
                (BinOp::LogicAnd, Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x && y)),
                (BinOp::LogicOr, Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x || y)),
                _ => Err(Error::CteError("unsupported operation", *span)),
            },
            Expr::UnOp(expr, op, span) => match (op, self.eval(&*expr)?) {
                (UnOp::LogicNot, Value::Bool(res)) => Ok(Value::Bool(!res)),
                _ => Err(Error::CteError("unsupported operation", *span)),
            },
            _ => Err(Error::CteError("unsupported expression", expr.span())),
        }
    }

    fn does_module_exist(&self, name: &str) -> bool {
        self.modules.contains(&ModulePath::parse(name))
    }
}
