use std::io;
use std::ops::Deref;

use redscript::ast::{Expr, Ident, LiteralType, Seq, SwitchCase, Target, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{CodeCursor, Instr, Offset, Position};
use redscript::error::Error;

pub mod print;

pub struct Decompiler<'a> {
    code: &'a mut CodeCursor<'a>,
    pool: &'a ConstantPool,
}

impl<'a> Decompiler<'a> {
    pub fn new(code: &'a mut CodeCursor<'a>, pool: &'a ConstantPool) -> Decompiler<'a> {
        Decompiler { code, pool }
    }

    pub fn decompile(&mut self) -> Result<Seq, Error> {
        self.consume_path(Position::MAX)
    }

    fn definition_ident<A>(&self, index: PoolIndex<A>) -> Result<Ident, Error> {
        Ok(Ident(self.pool.definition_name(index)?))
    }

    fn consume_n(&mut self, n: usize) -> Result<Vec<Expr>, Error> {
        let mut body = Vec::new();
        for _ in 0..n {
            body.push(self.consume()?)
        }
        Ok(body)
    }

    fn consume_path(&mut self, target: Position) -> Result<Seq, Error> {
        let mut body = Vec::new();
        loop {
            if self.code.pos() >= target
                || matches!(body.last(), Some(Expr::Goto(_)))
                || matches!(body.last(), Some(Expr::Return(_)))
            {
                break;
            }
            match self.consume() {
                Ok(expr) => body.push(expr),
                Err(Error::IOError(err)) if err.kind() == io::ErrorKind::UnexpectedEof => break,
                Err(err) => Err(err)?,
            }
        }
        Ok(Seq::new(body))
    }

    fn consume_call(&mut self, name: &str, param_count: usize) -> Result<Expr, Error> {
        let params = self.consume_n(param_count)?;
        Ok(Expr::Call(Ident::new(name.to_owned()), params))
    }

    fn consume_params(&mut self) -> Result<Vec<Expr>, Error> {
        let mut params = Vec::new();
        loop {
            while matches!(self.code.peek(), Some(Instr::Skip(_))) || matches!(self.code.peek(), Some(Instr::Nop)) {
                self.code.pop()?;
            }
            if matches!(self.code.peek(), Some(Instr::ParamEnd)) {
                break;
            }
            params.push(self.consume()?);
        }
        self.code.pop()?;
        Ok(params)
    }

    fn consume_conditional_jump(&mut self, position: Position, offset: Offset) -> Result<Expr, Error> {
        let condition = self.consume()?;
        let target = offset.absolute(position);
        let mut body = self.consume_path(target)?;
        self.code.goto(target)?;

        let result = if let Some(_) = resolve_jump(&mut body, Some(position)) {
            Expr::While(Box::new(condition), body)
        } else if let Some(jump) = resolve_jump(&mut body, None) {
            let else_case = self.consume_path(Position::new(jump.position))?;
            Expr::If(Box::new(condition), body, Some(else_case))
        } else {
            Expr::If(Box::new(condition), body, None)
        };
        Ok(result)
    }

    fn consume_switch(&mut self) -> Result<Expr, Error> {
        let subject = self.consume()?;

        let mut labels = Vec::new();
        while let Some(Instr::SwitchLabel(exit_offset, start_offset)) = self.code.peek() {
            let position = self.code.pos();
            labels.push((position, start_offset.absolute(position)));
            self.code.seek(exit_offset.into())?;
        }
        if let Some(Instr::SwitchDefault) = self.code.peek() {
            labels.push((self.code.pos(), self.code.pos()));
        };
        labels.sort_by_key(|(_, start)| *start);

        let mut default = None;
        let mut cases = Vec::new();
        for (label, start_position) in labels {
            self.code.goto(label.into())?;

            match self.code.pop()? {
                Instr::SwitchLabel(exit_offset, _) => {
                    let exit = exit_offset.absolute(label);
                    let matched = self.consume()?;

                    self.code.goto(start_position)?;
                    let mut body = self.consume_path(exit)?;
                    if let Some(Expr::Goto(_)) = body.exprs.last() {
                        body.exprs.pop();
                        body.exprs.push(Expr::Break);
                    }
                    cases.push(SwitchCase(matched, body));
                }
                Instr::SwitchDefault => default = Some(Seq::new(vec![self.consume()?])),
                _ => Err(Error::DecompileError("Unexpected switch label instruction".to_owned()))?,
            }
        }

        Ok(Expr::Switch(Box::new(subject), cases, default))
    }

    fn consume(&mut self) -> Result<Expr, Error> {
        self.consume_with(None)
    }

    fn consume_with(&mut self, context: Option<Expr>) -> Result<Expr, Error> {
        let position = self.code.pos();
        let res = match self.code.pop()? {
            Instr::Nop => Expr::EMPTY,
            Instr::Null => Expr::Null,
            Instr::I32One => Expr::IntLit(1),
            Instr::I32Zero => Expr::IntLit(0),
            Instr::I8Const(val) => Expr::IntLit(val.into()),
            Instr::I16Const(val) => Expr::IntLit(val.into()),
            Instr::I32Const(val) => Expr::IntLit(val.into()),
            Instr::I64Const(val) => Expr::IntLit(val.into()),
            Instr::U8Const(val) => Expr::UintLit(val.into()),
            Instr::U16Const(val) => Expr::UintLit(val.into()),
            Instr::U32Const(val) => Expr::UintLit(val.into()),
            Instr::U64Const(val) => Expr::UintLit(val),
            Instr::F32Const(val) => Expr::FloatLit(val.into()),
            Instr::F64Const(val) => Expr::FloatLit(val),
            Instr::NameConst(idx) => Expr::StringLit(LiteralType::Name, self.pool.names.get(idx)?.deref().clone()),
            Instr::EnumConst(enum_, member) => {
                let enum_ident = self.definition_ident(enum_)?;
                let member_ident = self.definition_ident(member)?;
                Expr::Member(Box::new(Expr::Ident(enum_ident)), member_ident)
            }
            Instr::StringConst(str) => Expr::StringLit(LiteralType::String, String::from_utf8(str).unwrap()),
            Instr::TweakDbIdConst(idx) => {
                Expr::StringLit(LiteralType::TweakDbId, self.pool.tweakdb_ids.get(idx)?.deref().clone())
            }
            Instr::ResourceConst(idx) => {
                Expr::StringLit(LiteralType::Resource, self.pool.resources.get(idx)?.deref().clone())
            }
            Instr::TrueConst => Expr::True,
            Instr::FalseConst => Expr::False,
            Instr::Breakpoint(_, _, _, _, _, _) => Err(Error::DecompileError("Unexpected Breakpoint".to_owned()))?,
            Instr::Assign => {
                let lhs = self.consume()?;
                let rhs = self.consume()?;
                Expr::Assign(Box::new(lhs), Box::new(rhs))
            }
            Instr::Target => Err(Error::DecompileError("Unexpected Target".to_owned()))?,
            Instr::Local(idx) => Expr::Ident(self.definition_ident(idx)?),
            Instr::Param(idx) => Expr::Ident(self.definition_ident(idx)?),
            Instr::ObjectField(idx) => {
                let field = self.definition_ident(idx)?;
                if let Some(object) = context {
                    Expr::Member(Box::new(object), field)
                } else {
                    Expr::Member(Box::new(Expr::This), field)
                }
            }
            Instr::ExternalVar => Err(Error::DecompileError("Unexpected ExternalVar".to_owned()))?,
            Instr::Switch(_, _) => self.consume_switch()?,
            Instr::SwitchLabel(_, _) => Err(Error::DecompileError("Unexpected SwitchLabel".to_owned()))?,
            Instr::SwitchDefault => Err(Error::DecompileError("Unexpected SwitchDefault".to_owned()))?,
            Instr::Jump(Offset { value: 3 }) => Expr::EMPTY,
            Instr::Jump(offset) => Expr::Goto(Target::new(offset.absolute(position).value)),
            Instr::JumpIfFalse(offset) => {
                assert!(offset.value >= 0, "negative offset is not supported for JumpIfFalse");
                self.consume_conditional_jump(position, offset)?
            }
            Instr::Skip(offset) => Expr::Goto(Target::new(offset.absolute(position).value)),
            Instr::Conditional(_, _) => {
                let expr = self.consume()?;
                let true_case = self.consume()?;
                let false_case = self.consume()?;
                Expr::Conditional(Box::new(expr), Box::new(true_case), Box::new(false_case))
            }
            Instr::Construct(n, class) => {
                let params = self.consume_n(n.into())?;
                Expr::New(Ident(self.pool.definition_name(class)?), params)
            }
            Instr::InvokeStatic(_, _, idx) => {
                let def = self.pool.definition(idx)?;
                let name = Ident(self.pool.names.get(def.name)?);
                let params = self.consume_params()?;
                if let Some(ctx) = context {
                    Expr::MethodCall(Box::new(ctx), name, params)
                } else if self.pool.function(idx)?.flags.is_static() {
                    if def.parent.is_undefined() {
                        Expr::Call(name, params)
                    } else {
                        let class_name = Ident(self.pool.definition_name(def.parent)?);
                        Expr::MethodCall(Box::new(Expr::Ident(class_name)), name, params)
                    }
                } else {
                    Expr::MethodCall(Box::new(Expr::This), name, params)
                }
                // if let AnyDefinition::Function(ref fun) = def.value {
                // assert_eq!(fun.parameters.len(), params.len(), "Invalid number of parameters {:?}", params);
                // }
            }
            Instr::InvokeVirtual(_, _, idx) => {
                let name = Ident(self.pool.names.get(idx)?);
                let params = self.consume_params()?;
                if let Some(ctx) = context {
                    Expr::MethodCall(Box::new(ctx), name, params)
                } else {
                    Expr::MethodCall(Box::new(Expr::This), name, params)
                }
            }
            Instr::ParamEnd => Err(Error::DecompileError("Unexpected ParamEnd".to_owned()))?,
            Instr::Return => Expr::Return(self.consume().ok().map(|e| Box::new(e))),
            Instr::StructField(idx) => {
                let target = self.consume()?;
                let field = self.definition_ident(idx)?;
                Expr::Member(Box::new(target), field)
            }
            Instr::Context(_) => {
                let expr = self.consume()?;
                self.consume_with(Some(expr))?
            }
            Instr::Equals(_) => self.consume_call("Equals", 2)?,
            Instr::NotEquals(_) => self.consume_call("NotEquals", 2)?,
            Instr::New(class) => Expr::New(Ident(self.pool.definition_name(class)?), vec![]),
            Instr::Delete => self.consume_call("Delete", 1)?,
            Instr::This => Expr::This,
            Instr::StartProfiling(_, _) => Err(Error::DecompileError("Unexpected StartProfiling".to_owned()))?,
            Instr::ArrayClear(_) => self.consume_call("ArrayClear", 1)?,
            Instr::ArraySize(_) => self.consume_call("ArraySize", 1)?,
            Instr::ArrayResize(_) => self.consume_call("ArrayResize", 2)?,
            Instr::ArrayFindFirst(_) => self.consume_call("ArrayFindFirst", 2)?,
            Instr::ArrayFindFirstFast(_) => self.consume_call("ArrayFindFirst", 2)?,
            Instr::ArrayFindLast(_) => self.consume_call("ArrayFindLast", 2)?,
            Instr::ArrayFindLastFast(_) => self.consume_call("ArrayFindLast", 2)?,
            Instr::ArrayContains(_) => self.consume_call("ArrayContains", 2)?,
            Instr::ArrayContainsFast(_) => self.consume_call("ArrayContains", 2)?,
            Instr::ArrayCount(_) => self.consume_call("ArrayCount", 2)?,
            Instr::ArrayCountFast(_) => self.consume_call("ArrayCount", 2)?,
            Instr::ArrayPush(_) => self.consume_call("ArrayPush", 2)?,
            Instr::ArrayPop(_) => self.consume_call("ArrayPop", 1)?,
            Instr::ArrayInsert(_) => self.consume_call("ArrayInsert", 3)?,
            Instr::ArrayRemove(_) => self.consume_call("ArrayRemove", 2)?,
            Instr::ArrayRemoveFast(_) => self.consume_call("ArrayRemove", 2)?,
            Instr::ArrayGrow(_) => self.consume_call("ArrayGrow", 2)?,
            Instr::ArrayErase(_) => self.consume_call("ArrayErase", 2)?,
            Instr::ArrayEraseFast(_) => self.consume_call("ArrayErase", 2)?,
            Instr::ArrayLast(_) => self.consume_call("ArrayLast", 1)?,
            Instr::ArrayElement(_) => {
                let arr = self.consume()?;
                let idx = self.consume()?;
                Expr::ArrayElem(Box::new(arr), Box::new(idx))
            }
            Instr::StaticArraySize(_) => self.consume_call("StaticArraySize", 1)?,
            Instr::StaticArrayFindFirst(_) => self.consume_call("StaticArrayFindFirst", 2)?,
            Instr::StaticArrayFindFirstFast(_) => self.consume_call("StaticArrayFindFirstFast", 2)?,
            Instr::StaticArrayFindLast(_) => self.consume_call("StaticArrayFindLast", 2)?,
            Instr::StaticArrayFindLastFast(_) => self.consume_call("StaticArrayFindLastFast", 2)?,
            Instr::StaticArrayContains(_) => self.consume_call("StaticArrayContains", 2)?,
            Instr::StaticArrayContainsFast(_) => self.consume_call("StaticArrayContainsFast", 2)?,
            Instr::StaticArrayCount(_) => self.consume_call("StaticArrayCount", 2)?,
            Instr::StaticArrayCountFast(_) => self.consume_call("StaticArrayCountFast", 2)?,
            Instr::StaticArrayLast(_) => self.consume_call("StaticArrayLast", 1)?,
            Instr::StaticArrayElement(_) => {
                let arr = self.consume()?;
                let idx = self.consume()?;
                Expr::ArrayElem(Box::new(arr), Box::new(idx))
            }
            Instr::RefToBool => self.consume_call("RefToBool", 1)?,
            Instr::WeakRefToBool => self.consume_call("WeakRefToBool", 1)?,
            Instr::EnumToI32(_, _) => self.consume_call("EnumInt", 1)?,
            Instr::I32ToEnum(_, _) => self.consume_call("IntEnum", 1)?,
            Instr::DynamicCast(type_, _) => {
                let name = self.pool.definition_name(type_)?;
                let type_name = TypeName {
                    name: name.deref().to_owned(),
                    arguments: vec![],
                };
                let expr = self.consume()?;
                Expr::Cast(type_name, Box::new(expr))
            }
            Instr::ToString(_) => self.consume_call("ToString", 1)?,
            Instr::ToVariant(_) => self.consume_call("ToVariant", 1)?,
            Instr::FromVariant(_) => self.consume_call("FromVariant", 1)?,
            Instr::VariantIsValid => self.consume_call("IsValid", 1)?,
            Instr::VariantIsRef => self.consume_call("IsRef", 1)?,
            Instr::VariantIsArray => self.consume_call("IsArray", 1)?,
            Instr::VatiantToCName => self.consume_call("ToCName", 1)?,
            Instr::VariantToString => self.consume_call("ToString", 1)?,
            Instr::WeakRefToRef => self.consume_call("WeakRefToRef", 1)?,
            Instr::RefToWeakRef => self.consume_call("RefToWeakRef", 1)?,
            Instr::WeakRefNull => Expr::Null,
            Instr::AsRef(_) => self.consume_call("AsRef", 1)?,
            Instr::Deref(_) => self.consume_call("Deref", 1)?,
        };
        Ok(res)
    }
}

fn resolve_jump(seq: &mut Seq, target: Option<Position>) -> Option<&mut Target> {
    seq.exprs.iter_mut().rev().find_map(|expr| match expr {
        Expr::Goto(goto) if !goto.resolved && target.map(|target| goto.position == target.value).unwrap_or(true) => {
            goto.resolved = true;
            Some(goto)
        }
        Expr::If(_, if_, None) => resolve_jump(if_, target),
        Expr::If(_, if_, Some(else_)) => resolve_jump(if_, target).or(resolve_jump(else_, target)),
        _ => None,
    })
}
