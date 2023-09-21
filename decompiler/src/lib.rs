use std::collections::BTreeMap;

use error::Error;
use redscript::ast::{Constant, Expr, Ident, Literal, Seq, SourceAst, Span, SwitchCase, Target, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{CodeCursor, CursorError, Instr, IntrinsicOp, Location, Offset};
use redscript::definition::Function;

pub mod error;
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
        let mut locals = BTreeMap::new();
        for local_index in &function.locals {
            let local = pool.local(*local_index)?;
            let name = Ident::from_heap(pool.def_name(*local_index)?);
            let type_ = TypeName::from_repr(&pool.def_name(local.type_)?);
            locals.insert(name, type_);
        }

        let mut decompiler = Decompiler::new(function.code.cursor(), function.base_method, pool);
        let body = decompiler.decompile()?;
        merge_declarations(locals, body)
    }

    pub fn decompile(&mut self) -> Result<Seq<SourceAst>, Error> {
        self.consume_path(Location::MAX, None)
    }

    fn definition_ident<A>(&self, index: PoolIndex<A>) -> Result<Ident, Error> {
        Ok(Ident::from_heap(self.pool.def_name(index)?))
    }

    fn consume_n(&mut self, n: usize) -> Result<Vec<Expr<SourceAst>>, Error> {
        let mut body = Vec::new();
        for _ in 0..n {
            body.push(self.consume()?);
        }
        Ok(body)
    }

    fn consume_path(&mut self, target: Location, block: Option<BlockContext>) -> Result<Seq<SourceAst>, Error> {
        let mut body = Vec::new();
        loop {
            if self.code.pos() >= target
                || matches!(body.last(), Some(Expr::Goto(_, _)))
                || matches!(body.last(), Some(Expr::Break(_)))
                || matches!(body.last(), Some(Expr::Return(_, _)))
            {
                break;
            }
            match self.consume_with(None, block) {
                Ok(expr) => body.push(expr),
                Err(Error::CursorError(CursorError::EndOfCode)) => break,
                Err(err) => return Err(err),
            }
        }
        Ok(Seq::new(body))
    }

    fn consume_intrisnic_typed(&mut self, op: IntrinsicOp, type_args: Vec<TypeName>) -> Result<Expr<SourceAst>, Error> {
        let params = self.consume_n(op.arg_count() as usize)?;
        Ok(Expr::Call(
            Ident::from_static(op.into()),
            type_args.into_boxed_slice(),
            params.into_boxed_slice(),
            Span::ZERO,
        ))
    }

    fn consume_intrisnic(&mut self, op: IntrinsicOp) -> Result<Expr<SourceAst>, Error> {
        self.consume_intrisnic_typed(op, vec![])
    }

    fn consume_call(&mut self, name: &'static str, param_count: usize) -> Result<Expr<SourceAst>, Error> {
        let params = self.consume_n(param_count)?;
        Ok(Expr::Call(
            Ident::from_static(name),
            [].into(),
            params.into_boxed_slice(),
            Span::ZERO,
        ))
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

    fn consume_conditional_jump(
        &mut self,
        position: Location,
        offset: Offset,
        block: Option<BlockContext>,
    ) -> Result<Expr<SourceAst>, Error> {
        let condition = self.consume()?;
        let target = offset.absolute(position);

        let is_loop = self
            .code
            .range(position, target)?
            .any(|(loc, i)| matches!(i, Instr::Jump(offset) if offset.absolute(loc) == position));

        let result = if is_loop {
            let block = BlockContext::new_loop(position, target);
            let body = self.consume_path(target, Some(block))?;
            Expr::While(condition.into(), body, Span::ZERO)
        } else {
            let mut body = self.consume_path(target, block)?;
            self.code.seek_abs(target)?;
            if let Some(jump) = resolve_jump(&mut body) {
                let else_case = self.consume_path(jump.position, block)?;
                Expr::If(condition.into(), body, Some(else_case), Span::ZERO)
            } else {
                Expr::If(condition.into(), body, None, Span::ZERO)
            }
        };

        Ok(result)
    }

    fn consume_switch(&mut self, start: Location) -> Result<Expr<SourceAst>, Error> {
        let subject = self.consume()?;

        self.code.seek_abs(start)?;
        let mut labels = Vec::new();
        while let Some(Instr::SwitchLabel(exit_offset, start_offset)) = self.code.peek() {
            let position = self.code.pos();
            labels.push((position, start_offset.absolute(position)));
            self.code.seek_rel(exit_offset)?;
        }
        if self.code.peek() == Some(Instr::SwitchDefault) {
            labels.push((self.code.pos(), self.code.pos()));
        };
        labels.sort_by_key(|(_, start)| *start);

        let mut default = None;
        let mut cases = Vec::new();
        let mut block = None;
        for (label, start_position) in labels {
            self.code.seek_abs(label)?;

            match self.code.pop()? {
                Instr::SwitchLabel(exit_offset, _) => {
                    let exit = exit_offset.absolute(label);
                    let matcher = self.consume()?;

                    if block.is_none() && exit > start_position {
                        if let Some(epilogue) = self.code.range(start_position, exit)?.find_map(|(loc, i)| match i {
                            Instr::Jump(offset) if offset.absolute(loc) > exit => Some(offset.absolute(loc)),
                            _ => None,
                        }) {
                            block = Some(BlockContext::new_switch(epilogue));
                        }
                    }

                    self.code.seek_abs(start_position)?;
                    let mut body = self.consume_path(exit, block)?;

                    if let Some(Expr::Goto(_, _)) = body.exprs.last() {
                        body.exprs.pop();
                        body.exprs.push(Expr::Break(Span::ZERO));
                    }
                    cases.push(SwitchCase { matcher, body });
                }
                Instr::SwitchDefault => {
                    if let Some(BlockContext::Switch { epilogue }) = block {
                        default = Some(self.consume_path(epilogue, block)?);
                    } else {
                        default = Some(Seq::new(vec![self.consume_with(None, block)?]));
                    }
                }
                _ => return Err(Error::DecompileError("Unexpected switch label instruction")),
            }
        }

        Ok(Expr::Switch(Box::new(subject), cases, default, Span::ZERO))
    }

    fn consume(&mut self) -> Result<Expr<SourceAst>, Error> {
        self.consume_with(None, None)
    }

    fn consume_with(
        &mut self,
        context: Option<Expr<SourceAst>>,
        block: Option<BlockContext>,
    ) -> Result<Expr<SourceAst>, Error> {
        let position = self.code.pos();
        let res = match self.code.pop()? {
            Instr::Null | Instr::WeakRefNull => Expr::Null(Span::ZERO),
            Instr::I32One => Expr::Constant(Constant::I32(1), Span::ZERO),
            Instr::I32Zero => Expr::Constant(Constant::I32(0), Span::ZERO),
            Instr::I8Const(val) => Expr::Constant(Constant::I32(val.into()), Span::ZERO),
            Instr::I16Const(val) => Expr::Constant(Constant::I32(val.into()), Span::ZERO),
            Instr::I32Const(val) => Expr::Constant(Constant::I32(val), Span::ZERO),
            Instr::I64Const(val) => Expr::Constant(Constant::I64(val), Span::ZERO),
            Instr::U8Const(val) => Expr::Constant(Constant::U32(val.into()), Span::ZERO),
            Instr::U16Const(val) => Expr::Constant(Constant::U32(val.into()), Span::ZERO),
            Instr::U32Const(val) => Expr::Constant(Constant::U32(val), Span::ZERO),
            Instr::U64Const(val) => Expr::Constant(Constant::U64(val), Span::ZERO),
            Instr::F32Const(val) => Expr::Constant(Constant::F32(val), Span::ZERO),
            Instr::F64Const(val) => Expr::Constant(Constant::F64(val), Span::ZERO),
            Instr::StringConst(idx) => {
                let str = self.pool.strings.get(idx)?;
                Expr::Constant(Constant::String(Literal::String, str), Span::ZERO)
            }
            Instr::NameConst(idx) => {
                let str = self.pool.names.get(idx)?;
                Expr::Constant(Constant::String(Literal::Name, str), Span::ZERO)
            }
            Instr::TweakDbIdConst(idx) => {
                let str = self.pool.tweakdb_ids.get(idx)?;
                Expr::Constant(Constant::String(Literal::TweakDbId, str), Span::ZERO)
            }
            Instr::ResourceConst(idx) => {
                let str = self.pool.resources.get(idx)?;
                Expr::Constant(Constant::String(Literal::Resource, str), Span::ZERO)
            }
            Instr::TrueConst => Expr::Constant(Constant::Bool(true), Span::ZERO),
            Instr::FalseConst => Expr::Constant(Constant::Bool(false), Span::ZERO),
            Instr::EnumConst(enum_, member) => {
                let enum_ident = self.definition_ident(enum_)?;
                let member_ident = self.definition_ident(member)?;
                let expr = Box::new(Expr::Ident(enum_ident, Span::ZERO));
                Expr::Member(expr, member_ident, Span::ZERO)
            }
            Instr::Assign => {
                let lhs = self.consume()?;
                let rhs = self.consume()?;
                Expr::Assign(Box::new(lhs), Box::new(rhs), Span::ZERO)
            }
            Instr::Target(_) => return Err(Error::DecompileError("Unexpected Target")),
            Instr::Local(idx) => Expr::Ident(self.definition_ident(idx)?, Span::ZERO),
            Instr::Param(idx) => Expr::Ident(self.definition_ident(idx)?, Span::ZERO),
            Instr::ObjectField(idx) => {
                let field = self.definition_ident(idx)?;
                if let Some(object) = context {
                    Expr::Member(Box::new(object), field, Span::ZERO)
                } else {
                    Expr::Member(Box::new(Expr::This(Span::ZERO)), field, Span::ZERO)
                }
            }
            Instr::ExternalVar => return Err(Error::DecompileError("Unexpected ExternalVar")),
            Instr::Switch(_, start) => self.consume_switch(start.absolute(position))?,
            Instr::SwitchLabel(_, _) => return Err(Error::DecompileError("Unexpected SwitchLabel")),
            Instr::SwitchDefault => return Err(Error::DecompileError("Unexpected SwitchDefault")),
            Instr::JumpIfFalse(offset) => {
                assert!(offset.value >= 0, "negative offset is not supported for JumpIfFalse");
                self.consume_conditional_jump(position, offset, block)?
            }
            Instr::Skip(offset) => Expr::Goto(Target::new(offset.absolute(position)), Span::ZERO),
            Instr::Conditional(_, _) => {
                let expr = self.consume()?;
                let true_case = self.consume()?;
                let false_case = self.consume()?;
                Expr::Conditional(Box::new(expr), Box::new(true_case), Box::new(false_case), Span::ZERO)
            }
            Instr::Construct(n, class) => {
                let params = self.consume_n(n.into())?;
                Expr::New(
                    TypeName::basic_owned(self.pool.def_name(class)?),
                    params.into_boxed_slice(),
                    Span::ZERO,
                )
            }
            Instr::InvokeStatic(_, _, idx, _) => {
                let def = self.pool.definition(idx)?;
                let fun = self.pool.function(idx)?;
                let name = Ident::from_heap(self.pool.names.get(def.name)?);
                let params = self.consume_params()?;
                if let Some(ctx) = context {
                    Expr::MethodCall(Box::new(ctx), name, params, Span::ZERO)
                } else if fun.flags.is_static() {
                    if def.parent.is_undefined() {
                        if name.as_ref().starts_with("Cast;") {
                            let ret_type = fun
                                .return_type
                                .ok_or(Error::DecompileError("Cast without return type"))?;
                            let type_name = TypeName::from_repr(&self.pool.def_name(ret_type)?);
                            Expr::Call(name, [type_name].into(), params.into_boxed_slice(), Span::ZERO)
                        } else {
                            Expr::Call(name, [].into(), params.into_boxed_slice(), Span::ZERO)
                        }
                    } else {
                        let class_name = Ident::from_heap(self.pool.def_name(def.parent)?);
                        let expr = Box::new(Expr::Ident(class_name, Span::ZERO));
                        Expr::MethodCall(expr, name, params, Span::ZERO)
                    }
                } else {
                    let ctx = if self.base_method == Some(idx) {
                        Expr::Super(Span::ZERO)
                    } else {
                        Expr::This(Span::ZERO)
                    };
                    Expr::MethodCall(Box::new(ctx), name, params, Span::ZERO)
                }
                // if let AnyDefinition::Function(ref fun) = def.value {
                // assert_eq!(fun.parameters.len(), params.len(), "Invalid number of parameters {:?}", params);
                // }
            }
            Instr::InvokeVirtual(_, _, idx, _) => {
                let name = Ident::from_heap(self.pool.names.get(idx)?);
                let params = self.consume_params()?;
                if let Some(ctx) = context {
                    Expr::MethodCall(Box::new(ctx), name, params, Span::ZERO)
                } else {
                    Expr::MethodCall(Box::new(Expr::This(Span::ZERO)), name, params, Span::ZERO)
                }
            }
            Instr::ParamEnd => return Err(Error::DecompileError("Unexpected ParamEnd")),
            Instr::Return => {
                if self.code.peek() == Some(Instr::Nop) {
                    self.code.pop()?;
                    Expr::Return(None, Span::ZERO)
                } else {
                    Expr::Return(Some(Box::new(self.consume()?)), Span::ZERO)
                }
            }
            Instr::StructField(idx) => {
                let target = self.consume()?;
                let field = self.definition_ident(idx)?;
                Expr::Member(Box::new(target), field, Span::ZERO)
            }
            Instr::Context(_) => {
                let expr = self.consume()?;
                self.consume_with(Some(expr), None)?
            }
            Instr::Equals(_) | Instr::RefStringEqualsString(_) | Instr::StringEqualsRefString(_) => {
                self.consume_intrisnic(IntrinsicOp::Equals)?
            }
            Instr::NotEquals(_) | Instr::RefStringNotEqualsString(_) | Instr::StringNotEqualsRefString(_) => {
                self.consume_intrisnic(IntrinsicOp::NotEquals)?
            }
            Instr::New(class) => Expr::New(TypeName::basic_owned(self.pool.def_name(class)?), [].into(), Span::ZERO),
            Instr::Delete => self.consume_call("Delete", 1)?,
            Instr::This => Expr::This(Span::ZERO),
            Instr::ArrayClear(_) => self.consume_intrisnic(IntrinsicOp::ArrayClear)?,
            Instr::ArraySize(_) | Instr::StaticArraySize(_) => self.consume_intrisnic(IntrinsicOp::ArraySize)?,
            Instr::ArrayResize(_) => self.consume_intrisnic(IntrinsicOp::ArrayResize)?,
            Instr::ArrayFindFirst(_)
            | Instr::ArrayFindFirstFast(_)
            | Instr::StaticArrayFindFirst(_)
            | Instr::StaticArrayFindFirstFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindFirst)?,
            Instr::ArrayFindLast(_)
            | Instr::ArrayFindLastFast(_)
            | Instr::StaticArrayFindLast(_)
            | Instr::StaticArrayFindLastFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayFindLast)?,
            Instr::ArrayContains(_)
            | Instr::ArrayContainsFast(_)
            | Instr::StaticArrayContains(_)
            | Instr::StaticArrayContainsFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayContains)?,
            Instr::ArrayCount(_)
            | Instr::ArrayCountFast(_)
            | Instr::StaticArrayCount(_)
            | Instr::StaticArrayCountFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayCount)?,
            Instr::ArrayPush(_) => self.consume_intrisnic(IntrinsicOp::ArrayPush)?,
            Instr::ArrayPop(_) => self.consume_intrisnic(IntrinsicOp::ArrayPop)?,
            Instr::ArrayInsert(_) => self.consume_intrisnic(IntrinsicOp::ArrayInsert)?,
            Instr::ArrayRemove(_) | Instr::ArrayRemoveFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayRemove)?,
            Instr::ArrayGrow(_) => self.consume_intrisnic(IntrinsicOp::ArrayGrow)?,
            Instr::ArrayErase(_) | Instr::ArrayEraseFast(_) => self.consume_intrisnic(IntrinsicOp::ArrayErase)?,
            Instr::ArrayLast(_) | Instr::StaticArrayLast(_) => self.consume_intrisnic(IntrinsicOp::ArrayLast)?,
            Instr::ArraySort(_) => self.consume_intrisnic(IntrinsicOp::ArraySort)?,
            Instr::ArraySortByPredicate(_) => self.consume_intrisnic(IntrinsicOp::ArraySortByPredicate)?,
            Instr::ArrayElement(_) | Instr::StaticArrayElement(_) => {
                let arr = self.consume()?;
                let idx = self.consume()?;
                Expr::ArrayElem(Box::new(arr), Box::new(idx), Span::ZERO)
            }
            Instr::RefToBool | Instr::WeakRefToBool | Instr::VariantIsDefined => {
                self.consume_intrisnic(IntrinsicOp::IsDefined)?
            }
            Instr::EnumToI32(_, _) => self.consume_intrisnic(IntrinsicOp::EnumInt)?,
            Instr::I32ToEnum(typ, _) => {
                let type_name = TypeName::from_repr(&self.pool.def_name(typ)?);
                self.consume_intrisnic_typed(IntrinsicOp::IntEnum, vec![type_name])?
            }
            Instr::DynamicCast(typ, _) => {
                let type_name = TypeName::from_repr(&self.pool.def_name(typ)?);
                let expr = self.consume()?;
                Expr::Cast(type_name, Box::new(expr), Span::ZERO)
            }
            Instr::ToString(_) | Instr::VariantToString => self.consume_intrisnic(IntrinsicOp::ToString)?,
            Instr::ToVariant(_) => self.consume_intrisnic(IntrinsicOp::ToVariant)?,
            Instr::FromVariant(typ) => {
                let type_name = TypeName::from_repr(&self.pool.def_name(typ)?);
                self.consume_intrisnic_typed(IntrinsicOp::FromVariant, vec![type_name])?
            }
            Instr::VariantIsRef => self.consume_intrisnic(IntrinsicOp::VariantIsRef)?,
            Instr::VariantIsArray => self.consume_intrisnic(IntrinsicOp::VariantIsArray)?,
            Instr::VariantTypeName => self.consume_intrisnic(IntrinsicOp::VariantTypeName)?,
            Instr::WeakRefToRef => self.consume_intrisnic(IntrinsicOp::WeakRefToRef)?,
            Instr::RefToWeakRef => self.consume_intrisnic(IntrinsicOp::RefToWeakRef)?,
            Instr::AsRef(_) => self.consume_intrisnic(IntrinsicOp::AsRef)?,
            Instr::Deref(_) => self.consume_intrisnic(IntrinsicOp::Deref)?,
            // jump to next instruction does nothing
            Instr::Jump(Offset { value: 3 }) | Instr::Nop | Instr::Breakpoint(_) | Instr::StartProfiling(_) => {
                Expr::EMPTY
            }
            Instr::Jump(offset) => {
                let jump_loc = offset.absolute(position);
                match block {
                    Some(BlockContext::Loop { epilogue, .. }) if jump_loc == epilogue => {
                        // we're jumping out of the loop
                        Expr::Break(Span::ZERO)
                    }
                    Some(BlockContext::Loop { prologue, epilogue })
                        if jump_loc == prologue && self.code.pos() == epilogue =>
                    {
                        // we're jumping back to the beginning of the loop
                        // while being at the tail of it - no control flow required
                        Expr::EMPTY
                    }
                    Some(BlockContext::Switch { epilogue }) if jump_loc == epilogue => {
                        // we're jumping out of the switch
                        Expr::Break(Span::ZERO)
                    }
                    _ => {
                        // unknown control flow construct
                        Expr::Goto(Target::new(offset.absolute(position)), Span::ZERO)
                    }
                }
            }
        };
        Ok(res)
    }
}

fn merge_declarations(mut locals: BTreeMap<Ident, TypeName>, seq: Seq<SourceAst>) -> Result<Seq<SourceAst>, Error> {
    let mut body = Vec::with_capacity(seq.exprs.len() + locals.len());
    let mut init = Vec::new();
    let mut it = seq.exprs.into_iter();

    let stmt = loop {
        match it.next() {
            Some(Expr::Assign(ident, val, _)) => {
                if let Expr::Ident(name, _) = ident.as_ref() {
                    if let Some(ty) = locals.remove(name) {
                        init.push(Expr::Declare(name.clone(), Some(ty.into()), Some(val), Span::ZERO));
                    } else {
                        init.push(Expr::Assign(ident, val, Span::ZERO));
                    }
                } else {
                    init.push(Expr::Assign(ident, val, Span::ZERO));
                }
            }
            other => break other,
        }
    };

    for (name, ty) in locals {
        body.push(Expr::Declare(name.clone(), Some(ty.into()), None, Span::ZERO));
    }

    body.extend(init);
    body.extend(stmt);
    body.extend(it);
    Ok(Seq::new(body))
}

fn resolve_jump(seq: &mut Seq<SourceAst>) -> Option<Target> {
    seq.exprs.iter_mut().rev().find_map(|expr| match expr {
        Expr::Goto(target, _) => {
            let res = *target;
            *expr = Expr::EMPTY;
            Some(res)
        }
        Expr::If(_, if_, None, _) => resolve_jump(if_),
        Expr::If(_, if_, Some(else_), _) => resolve_jump(if_).or_else(move || resolve_jump(else_)),
        _ => None,
    })
}

#[derive(Debug, Clone, Copy)]
enum BlockContext {
    Loop { prologue: Location, epilogue: Location },
    Switch { epilogue: Location },
}

impl BlockContext {
    fn new_loop(prologue: Location, epilogue: Location) -> Self {
        Self::Loop { prologue, epilogue }
    }

    fn new_switch(epilogue: Location) -> Self {
        Self::Switch { epilogue }
    }
}
