use std::collections::HashMap;
use std::io;
use std::rc::Rc;

use redscript::ast::{Constant, Expr, Ident, Literal, Pos, Seq, SourceAst, SwitchCase, Target, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{CodeCursor, Instr, IntrinsicOp, Location, Offset};
use redscript::definition::Function;
use redscript::error::Error;

pub mod files;
pub mod print;

pub struct Decompiler<'a> {
    code: CodeCursor<'a, Offset>,
    pool: &'a ConstantPool,
    base_method: Option<PoolIndex<Function>>,
}

impl<'a> Decompiler<'a> {
    pub fn new(
        code: CodeCursor<'a, Offset>,
        base_method: Option<PoolIndex<Function>>,
        pool: &'a ConstantPool,
    ) -> Decompiler<'a> {
        Decompiler {
            code,
            pool,
            base_method,
        }
    }

    pub fn decompiled(function: &Function, pool: &'a ConstantPool) -> Result<Seq<SourceAst>, Error> {
        let mut locals = HashMap::new();
        for local_index in &function.locals {
            let local = pool.local(*local_index)?;
            let name = Ident::Owned(pool.definition_name(*local_index)?);
            let type_name = pool.definition_name(local.type_)?;
            let type_ = TypeName::from_repr(type_name.as_ref());
            locals.insert(name, type_);
        }

        let mut decompiler = Decompiler::new(function.code.cursor(), function.base_method, pool);
        let body = decompiler.decompile()?;
        merge_declarations(locals, body)
    }

    pub fn decompile(&mut self) -> Result<Seq<SourceAst>, Error> {
        self.consume_path(Location::MAX)
    }

    fn definition_ident<A>(&self, index: PoolIndex<A>) -> Result<Ident, Error> {
        Ok(Ident::Owned(self.pool.definition_name(index)?))
    }

    fn consume_n(&mut self, n: usize) -> Result<Vec<Expr<SourceAst>>, Error> {
        let mut body = Vec::new();
        for _ in 0..n {
            body.push(self.consume()?)
        }
        Ok(body)
    }

    fn consume_path(&mut self, target: Location) -> Result<Seq<SourceAst>, Error> {
        let mut body = Vec::new();
        loop {
            if self.code.pos() >= target
                || matches!(body.last(), Some(Expr::Goto(_, _)))
                || matches!(body.last(), Some(Expr::Return(_, _)))
            {
                break;
            }
            match self.consume() {
                Ok(expr) => body.push(expr),
                Err(Error::IoError(err)) if err.kind() == io::ErrorKind::UnexpectedEof => break,
                Err(err) => return Err(err),
            }
        }
        Ok(Seq::new(body))
    }

    fn consume_intrisnic(&mut self, op: IntrinsicOp) -> Result<Expr<SourceAst>, Error> {
        let params = self.consume_n(op.arg_count() as usize)?;
        Ok(Expr::Call(Ident::Static(op.into()), params, Pos::ZERO))
    }

    fn consume_call(&mut self, name: &'static str, param_count: usize) -> Result<Expr<SourceAst>, Error> {
        let params = self.consume_n(param_count)?;
        Ok(Expr::Call(Ident::Static(name), params, Pos::ZERO))
    }

    fn consume_params(&mut self) -> Result<Vec<Expr<SourceAst>>, Error> {
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

    fn consume_conditional_jump(&mut self, position: Location, offset: Offset) -> Result<Expr<SourceAst>, Error> {
        let condition = self.consume()?;
        let target = offset.absolute(position);
        let mut body = self.consume_path(target)?;
        self.code.set_pos(target)?;

        let result = if resolve_jump(&mut body, Some(position)).is_some() {
            Expr::While(Box::new(condition), body, Pos::ZERO)
        } else if let Some(jump) = resolve_jump(&mut body, None) {
            let else_case = self.consume_path(Location::new(jump.position))?;
            Expr::If(Box::new(condition), body, Some(else_case), Pos::ZERO)
        } else {
            Expr::If(Box::new(condition), body, None, Pos::ZERO)
        };
        Ok(result)
    }

    fn consume_switch(&mut self) -> Result<Expr<SourceAst>, Error> {
        let subject = self.consume()?;

        let mut labels = Vec::new();
        while let Some(Instr::SwitchLabel(exit_offset, start_offset)) = self.code.peek() {
            let position = self.code.pos();
            labels.push((position, start_offset.absolute(position)));
            self.code.seek(exit_offset)?;
        }
        if let Some(Instr::SwitchDefault) = self.code.peek() {
            labels.push((self.code.pos(), self.code.pos()));
        };
        labels.sort_by_key(|(_, start)| *start);

        let mut default = None;
        let mut cases = Vec::new();
        for (label, start_position) in labels {
            self.code.set_pos(label)?;

            match self.code.pop()? {
                Instr::SwitchLabel(exit_offset, _) => {
                    let exit = exit_offset.absolute(label);
                    let matcher = self.consume()?;

                    self.code.set_pos(start_position)?;
                    let mut body = self.consume_path(exit)?;
                    if let Some(Expr::Goto(_, _)) = body.exprs.last() {
                        body.exprs.pop();
                        body.exprs.push(Expr::Break(Pos::ZERO));
                    }
                    cases.push(SwitchCase { matcher, body });
                }
                Instr::SwitchDefault => default = Some(Seq::new(vec![self.consume()?])),
                _ => return Err(Error::DecompileError("Unexpected switch label instruction".to_owned())),
            }
        }

        Ok(Expr::Switch(Box::new(subject), cases, default))
    }

    fn consume(&mut self) -> Result<Expr<SourceAst>, Error> {
        self.consume_with(None)
    }

    fn consume_with(&mut self, context: Option<Expr<SourceAst>>) -> Result<Expr<SourceAst>, Error> {
        let position = self.code.pos();
        let res = match self.code.pop()? {
            Instr::Nop => Expr::EMPTY,
            Instr::Null => Expr::Null,
            Instr::I32One => Expr::Constant(Constant::Int(1), Pos::ZERO),
            Instr::I32Zero => Expr::Constant(Constant::Int(0), Pos::ZERO),
            Instr::I8Const(val) => Expr::Constant(Constant::Int(val.into()), Pos::ZERO),
            Instr::I16Const(val) => Expr::Constant(Constant::Int(val.into()), Pos::ZERO),
            Instr::I32Const(val) => Expr::Constant(Constant::Int(val.into()), Pos::ZERO),
            Instr::I64Const(val) => Expr::Constant(Constant::Int(val), Pos::ZERO),
            Instr::U8Const(val) => Expr::Constant(Constant::Uint(val.into()), Pos::ZERO),
            Instr::U16Const(val) => Expr::Constant(Constant::Uint(val.into()), Pos::ZERO),
            Instr::U32Const(val) => Expr::Constant(Constant::Uint(val.into()), Pos::ZERO),
            Instr::U64Const(val) => Expr::Constant(Constant::Uint(val), Pos::ZERO),
            Instr::F32Const(val) => Expr::Constant(Constant::Float(val.into()), Pos::ZERO),
            Instr::F64Const(val) => Expr::Constant(Constant::Float(val), Pos::ZERO),
            Instr::StringConst(str) => Expr::Constant(Constant::String(Literal::String, Rc::new(str)), Pos::ZERO),
            Instr::NameConst(idx) => {
                let str = self.pool.names.get(idx)?.to_string();
                Expr::Constant(Constant::String(Literal::Name, Rc::new(str)), Pos::ZERO)
            }
            Instr::TweakDbIdConst(idx) => {
                let str = self.pool.tweakdb_ids.get(idx)?.to_string();
                Expr::Constant(Constant::String(Literal::TweakDbId, Rc::new(str)), Pos::ZERO)
            }
            Instr::ResourceConst(idx) => {
                let str = self.pool.resources.get(idx)?.to_string();
                Expr::Constant(Constant::String(Literal::Resource, Rc::new(str)), Pos::ZERO)
            }
            Instr::TrueConst => Expr::Constant(Constant::Bool(true), Pos::ZERO),
            Instr::FalseConst => Expr::Constant(Constant::Bool(false), Pos::ZERO),
            Instr::EnumConst(enum_, member) => {
                let enum_ident = self.definition_ident(enum_)?;
                let member_ident = self.definition_ident(member)?;
                let expr = Box::new(Expr::Ident(enum_ident, Pos::ZERO));
                if member_ident.as_ref().is_empty() {
                    let value = self.pool.enum_value(member)?;
                    let constant = Expr::Constant(Constant::Int(value), Pos::ZERO);
                    Expr::Call(Ident::Static(IntrinsicOp::IntEnum.into()), vec![constant], Pos::ZERO)
                } else {
                    Expr::Member(expr, member_ident, Pos::ZERO)
                }
            }
            Instr::Breakpoint(_, _, _, _, _, _) => {
                return Err(Error::DecompileError("Unexpected Breakpoint".to_owned()))
            }
            Instr::Assign => {
                let lhs = self.consume()?;
                let rhs = self.consume()?;
                Expr::Assign(Box::new(lhs), Box::new(rhs), Pos::ZERO)
            }
            Instr::Target(_) => return Err(Error::DecompileError("Unexpected Target".to_owned())),
            Instr::Local(idx) => Expr::Ident(self.definition_ident(idx)?, Pos::ZERO),
            Instr::Param(idx) => Expr::Ident(self.definition_ident(idx)?, Pos::ZERO),
            Instr::ObjectField(idx) => {
                let field = self.definition_ident(idx)?;
                if let Some(object) = context {
                    Expr::Member(Box::new(object), field, Pos::ZERO)
                } else {
                    Expr::Member(Box::new(Expr::This(Pos::ZERO)), field, Pos::ZERO)
                }
            }
            Instr::ExternalVar => return Err(Error::DecompileError("Unexpected ExternalVar".to_owned())),
            Instr::Switch(_, _) => self.consume_switch()?,
            Instr::SwitchLabel(_, _) => return Err(Error::DecompileError("Unexpected SwitchLabel".to_owned())),
            Instr::SwitchDefault => return Err(Error::DecompileError("Unexpected SwitchDefault".to_owned())),
            Instr::Jump(Offset { value: 3 }) => Expr::EMPTY,
            Instr::Jump(offset) => Expr::Goto(Target::new(offset.absolute(position).value), Pos::ZERO),
            Instr::JumpIfFalse(offset) => {
                assert!(offset.value >= 0, "negative offset is not supported for JumpIfFalse");
                self.consume_conditional_jump(position, offset)?
            }
            Instr::Skip(offset) => Expr::Goto(Target::new(offset.absolute(position).value), Pos::ZERO),
            Instr::Conditional(_, _) => {
                let expr = self.consume()?;
                let true_case = self.consume()?;
                let false_case = self.consume()?;
                Expr::Conditional(Box::new(expr), Box::new(true_case), Box::new(false_case), Pos::ZERO)
            }
            Instr::Construct(n, class) => {
                let params = self.consume_n(n.into())?;
                Expr::New(
                    TypeName::basic_owned(self.pool.definition_name(class)?),
                    params,
                    Pos::ZERO,
                )
            }
            Instr::InvokeStatic(_, _, idx) => {
                let def = self.pool.definition(idx)?;
                let name = Ident::Owned(self.pool.names.get(def.name)?);
                let params = self.consume_params()?;
                if let Some(ctx) = context {
                    Expr::MethodCall(Box::new(ctx), name, params, Pos::ZERO)
                } else if self.pool.function(idx)?.flags.is_static() {
                    if def.parent.is_undefined() {
                        Expr::Call(name, params, Pos::ZERO)
                    } else {
                        let class_name = Ident::Owned(self.pool.definition_name(def.parent)?);
                        let expr = Box::new(Expr::Ident(class_name, Pos::ZERO));
                        Expr::MethodCall(expr, name, params, Pos::ZERO)
                    }
                } else {
                    let ctx = if self.base_method == Some(idx) {
                        Expr::Super(Pos::ZERO)
                    } else {
                        Expr::This(Pos::ZERO)
                    };
                    Expr::MethodCall(Box::new(ctx), name, params, Pos::ZERO)
                }
                // if let AnyDefinition::Function(ref fun) = def.value {
                // assert_eq!(fun.parameters.len(), params.len(), "Invalid number of parameters {:?}", params);
                // }
            }
            Instr::InvokeVirtual(_, _, idx) => {
                let name = Ident::Owned(self.pool.names.get(idx)?);
                let params = self.consume_params()?;
                if let Some(ctx) = context {
                    Expr::MethodCall(Box::new(ctx), name, params, Pos::ZERO)
                } else {
                    Expr::MethodCall(Box::new(Expr::This(Pos::ZERO)), name, params, Pos::ZERO)
                }
            }
            Instr::ParamEnd => return Err(Error::DecompileError("Unexpected ParamEnd".to_owned())),
            Instr::Return => Expr::Return(self.consume().ok().map(Box::new), Pos::ZERO),
            Instr::StructField(idx) => {
                let target = self.consume()?;
                let field = self.definition_ident(idx)?;
                Expr::Member(Box::new(target), field, Pos::ZERO)
            }
            Instr::Context(_) => {
                let expr = self.consume()?;
                self.consume_with(Some(expr))?
            }
            Instr::Equals(_) => self.consume_intrisnic(IntrinsicOp::Equals)?,
            Instr::NotEquals(_) => self.consume_intrisnic(IntrinsicOp::NotEquals)?,
            Instr::New(class) => Expr::New(
                TypeName::basic_owned(self.pool.definition_name(class)?),
                vec![],
                Pos::ZERO,
            ),
            Instr::Delete => self.consume_call("Delete", 1)?,
            Instr::This => Expr::This(Pos::ZERO),
            Instr::StartProfiling(_, _) => return Err(Error::DecompileError("Unexpected StartProfiling".to_owned())),
            Instr::ArrayClear(_) => self.consume_intrisnic(IntrinsicOp::ArrayClear)?,
            Instr::ArraySize(_) => self.consume_intrisnic(IntrinsicOp::ArraySize)?,
            Instr::ArrayResize(_) => self.consume_intrisnic(IntrinsicOp::ArrayResize)?,
            Instr::ArrayFindFirst(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindFirst)?,
            Instr::ArrayFindFirstFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindFirst)?,
            Instr::ArrayFindLast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindLast)?,
            Instr::ArrayFindLastFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindLast)?,
            Instr::ArrayContains(_) => self.consume_intrisnic(IntrinsicOp::ArrayContains)?,
            Instr::ArrayContainsFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayContains)?,
            Instr::ArrayCount(_) => self.consume_intrisnic(IntrinsicOp::ArrayCount)?,
            Instr::ArrayCountFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayCount)?,
            Instr::ArrayPush(_) => self.consume_intrisnic(IntrinsicOp::ArrayPush)?,
            Instr::ArrayPop(_) => self.consume_intrisnic(IntrinsicOp::ArrayPop)?,
            Instr::ArrayInsert(_) => self.consume_intrisnic(IntrinsicOp::ArrayInsert)?,
            Instr::ArrayRemove(_) => self.consume_intrisnic(IntrinsicOp::ArrayRemove)?,
            Instr::ArrayRemoveFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayRemove)?,
            Instr::ArrayGrow(_) => self.consume_intrisnic(IntrinsicOp::ArrayGrow)?,
            Instr::ArrayErase(_) => self.consume_intrisnic(IntrinsicOp::ArrayErase)?,
            Instr::ArrayEraseFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayErase)?,
            Instr::ArrayLast(_) => self.consume_intrisnic(IntrinsicOp::ArrayLast)?,
            Instr::ArrayElement(_) => {
                let arr = self.consume()?;
                let idx = self.consume()?;
                Expr::ArrayElem(Box::new(arr), Box::new(idx), Pos::ZERO)
            }
            Instr::StaticArraySize(_) => self.consume_intrisnic(IntrinsicOp::ArraySize)?,
            Instr::StaticArrayFindFirst(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindFirst)?,
            Instr::StaticArrayFindFirstFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindFirst)?,
            Instr::StaticArrayFindLast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindLast)?,
            Instr::StaticArrayFindLastFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindLast)?,
            Instr::StaticArrayContains(_) => self.consume_intrisnic(IntrinsicOp::ArrayContains)?,
            Instr::StaticArrayContainsFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayContains)?,
            Instr::StaticArrayCount(_) => self.consume_intrisnic(IntrinsicOp::ArrayCount)?,
            Instr::StaticArrayCountFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayCount)?,
            Instr::StaticArrayLast(_) => self.consume_intrisnic(IntrinsicOp::ArrayLast)?,
            Instr::StaticArrayElement(_) => {
                let arr = self.consume()?;
                let idx = self.consume()?;
                Expr::ArrayElem(Box::new(arr), Box::new(idx), Pos::ZERO)
            }
            Instr::RefToBool => self.consume_intrisnic(IntrinsicOp::IsDefined)?,
            Instr::WeakRefToBool => self.consume_intrisnic(IntrinsicOp::IsDefined)?,
            Instr::EnumToI32(_, _) => self.consume_intrisnic(IntrinsicOp::EnumInt)?,
            Instr::I32ToEnum(_, _) => self.consume_intrisnic(IntrinsicOp::IntEnum)?,
            Instr::DynamicCast(type_, _) => {
                let name = self.pool.definition_name(type_)?;
                let type_name = TypeName {
                    name: Ident::Owned(name),
                    arguments: vec![],
                };
                let expr = self.consume()?;
                Expr::Cast(type_name, Box::new(expr), Pos::ZERO)
            }
            Instr::ToString(_) => self.consume_intrisnic(IntrinsicOp::ToString)?,
            Instr::ToVariant(_) => self.consume_intrisnic(IntrinsicOp::ToVariant)?,
            Instr::FromVariant(_) => self.consume_intrisnic(IntrinsicOp::FromVariant)?,
            Instr::VariantIsValid => self.consume_call("VariantIsValid", 1)?,
            Instr::VariantIsRef => self.consume_call("VariantIsRef", 1)?,
            Instr::VariantIsArray => self.consume_call("VariantIsArray", 1)?,
            Instr::VatiantToCName => self.consume_call("VatiantToCName", 1)?,
            Instr::VariantToString => self.consume_intrisnic(IntrinsicOp::ToString)?,
            Instr::WeakRefToRef => self.consume_intrisnic(IntrinsicOp::WeakRefToRef)?,
            Instr::RefToWeakRef => self.consume_intrisnic(IntrinsicOp::RefToWeakRef)?,
            Instr::WeakRefNull => Expr::Null,
            Instr::AsRef(_) => self.consume_intrisnic(IntrinsicOp::AsRef)?,
            Instr::Deref(_) => self.consume_intrisnic(IntrinsicOp::Deref)?,
        };
        Ok(res)
    }
}

fn merge_declarations(mut locals: HashMap<Ident, TypeName>, seq: Seq<SourceAst>) -> Result<Seq<SourceAst>, Error> {
    let mut body = Vec::with_capacity(seq.exprs.len());
    let mut it = seq.exprs.into_iter();

    let stmt = loop {
        match it.next() {
            Some(Expr::Assign(ident, val, _)) => {
                if let Expr::Ident(name, _) = ident.as_ref() {
                    if let Some(ty) = locals.remove(name) {
                        body.push(Expr::Declare(name.clone(), Some(ty), Some(val), Pos::ZERO));
                    } else {
                        body.push(Expr::Assign(ident, val, Pos::ZERO));
                    }
                } else {
                    body.push(Expr::Assign(ident, val, Pos::ZERO));
                }
            }
            other => break other,
        }
    };

    for (name, ty) in locals {
        body.push(Expr::Declare(name.clone(), Some(ty), None, Pos::ZERO));
    }

    body.extend(stmt);
    body.extend(it);
    Ok(Seq::new(body))
}

fn resolve_jump(seq: &mut Seq<SourceAst>, target: Option<Location>) -> Option<&mut Target> {
    seq.exprs.iter_mut().rev().find_map(|expr| match expr {
        Expr::Goto(goto, _) if !goto.resolved && target.map(|target| goto.position == target.value).unwrap_or(true) => {
            goto.resolved = true;
            Some(goto)
        }
        Expr::If(_, if_, None, _) => resolve_jump(if_, target),
        Expr::If(_, if_, Some(else_), _) => resolve_jump(if_, target).or_else(move || resolve_jump(else_, target)),
        _ => None,
    })
}
