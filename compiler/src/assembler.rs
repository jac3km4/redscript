use std::iter;
use std::ops::Deref;
use std::str::FromStr;

use redscript::ast::{Constant, Expr, Ident, LiteralType, Pos, Seq};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Instr, Offset};
use redscript::definition::{Definition, Local, LocalFlags, ParameterFlags, Type};
use redscript::error::Error;
use strum::{Display, EnumString};

use crate::scope::{Conversion, FunctionMatch, FunctionName, Scope};
use crate::{Reference, TypeId};

pub struct Assembler {
    pub code: im::Vector<Instr>,
    pub locals: im::Vector<PoolIndex<Local>>,
    position: u16,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            code: im::Vector::new(),
            locals: im::Vector::new(),
            position: 0,
        }
    }

    fn offset(&self) -> Offset {
        Offset::new(self.position as i16)
    }

    fn prefix(mut self, instr: Instr) -> Self {
        self.position += instr.size();
        self.code.push_front(shifted(instr));
        self
    }

    fn emit(&mut self, instr: Instr) {
        self.position += instr.size();
        self.code.push_back(shifted(instr));
    }

    fn append(&mut self, code: Assembler) {
        self.position += code.position;
        self.code.append(code.code);
        self.locals.append(code.locals);
    }

    fn compile(
        &mut self,
        expr: &Expr,
        expected: Option<&TypeId>,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        match expr {
            Expr::Ident(name, pos) => {
                match scope.resolve_reference(name.clone(), *pos)? {
                    Reference::Local(idx) => self.emit(Instr::Local(idx)),
                    Reference::Parameter(idx) => self.emit(Instr::Param(idx)),
                    _ => panic!("Shouldn't get here"),
                };
            }
            Expr::Constant(cons, _) => match cons {
                Constant::String(LiteralType::String, lit) => {
                    self.emit(Instr::StringConst(lit.as_bytes().to_vec()));
                }
                Constant::String(LiteralType::Name, lit) => {
                    let idx = pool.names.add(lit.clone());
                    self.emit(Instr::NameConst(idx));
                }
                Constant::String(LiteralType::Resource, lit) => {
                    let idx = pool.resources.add(lit.clone());
                    self.emit(Instr::ResourceConst(idx));
                }
                Constant::String(LiteralType::TweakDbId, lit) => {
                    let idx = pool.tweakdb_ids.add(lit.clone());
                    self.emit(Instr::TweakDbIdConst(idx));
                }
                Constant::Float(val) => {
                    self.emit(Instr::F32Const(*val as f32));
                }
                Constant::Int(val) => {
                    self.emit(Instr::I32Const(*val as i32));
                }
                Constant::Uint(val) => {
                    self.emit(Instr::U32Const(*val as u32));
                }
                Constant::Bool(true) => {
                    self.emit(Instr::TrueConst);
                }
                Constant::Bool(false) => {
                    self.emit(Instr::FalseConst);
                }
            },
            Expr::Cast(type_name, expr, pos) => {
                let type_ = scope.resolve_type(type_name, pool, *pos)?;
                self.emit(Instr::DynamicCast(scope.get_type_index(&type_, pool)?, 0));
                self.compile(expr, None, pool, scope)?;
            }
            Expr::Declare(name, type_, init, pos) => {
                let name_idx = pool.names.add(name.0.deref().to_owned());
                let (type_, conversion) = match (type_, init) {
                    (None, None) => {
                        return Err(Error::CompileError(
                            "Type or initializer require on let binding".to_owned(),
                            *pos,
                        ))
                    }
                    (None, Some(val)) => (scope.infer_type(val, None, pool)?, Conversion::Identity),
                    (Some(type_name), None) => (scope.resolve_type(type_name, pool, *pos)?, Conversion::Identity),
                    (Some(type_name), Some(val)) => {
                        let type_ = scope.resolve_type(type_name, pool, *pos)?;
                        let val_type = scope.infer_type(val, Some(&type_), pool)?;
                        let conv = scope.convert_type(&val_type, &type_, pool, *pos)?;
                        (type_, conv)
                    }
                };
                let local = Local::new(scope.get_type_index(&type_, pool)?, LocalFlags::new());
                let idx = pool
                    .push_definition(Definition::local(name_idx, scope.function.unwrap().cast(), local))
                    .cast();
                self.locals.push_back(idx);
                scope.push_local(name.clone(), idx);
                if let Some(val) = init {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(idx));
                    self.compile_conversion(conversion);
                    self.compile(val, Some(&type_), pool, scope)?;
                }
            }
            Expr::Assign(lhs, rhs, pos) => {
                let lhs_type = scope.infer_type(lhs, None, pool)?;
                let rhs_type = scope.infer_type(rhs, Some(&lhs_type), pool)?;
                let conv = scope.convert_type(&rhs_type, &lhs_type, pool, *pos)?;
                self.emit(Instr::Assign);
                self.compile(lhs, None, pool, scope)?;
                self.compile_conversion(conv);
                self.compile(rhs, Some(&lhs_type), pool, scope)?;
            }
            Expr::ArrayElem(expr, idx, pos) => {
                match scope.infer_type(expr, None, pool)? {
                    type_ @ TypeId::Array(_) => {
                        self.emit(Instr::ArrayElement(scope.get_type_index(&type_, pool)?));
                    }
                    type_ @ TypeId::StaticArray(_, _) => {
                        self.emit(Instr::StaticArrayElement(scope.get_type_index(&type_, pool)?));
                    }
                    other => {
                        let error = format!("Array access not allowed on {}", other.pretty(pool)?);
                        return Err(Error::CompileError(error, *pos));
                    }
                }
                self.compile(expr, None, pool, scope)?;
                self.compile(idx, None, pool, scope)?;
            }
            Expr::New(name, args, pos) => match scope.resolve_reference(name.clone(), *pos)? {
                Reference::Class(idx) => {
                    let cls = pool.class(idx)?;
                    if cls.flags.is_struct() {
                        if cls.fields.len() != args.len() {
                            let err = format!("Expected {} parameters for {}", cls.fields.len(), name);
                            return Err(Error::CompileError(err, *pos));
                        }
                        let fields = cls.fields.clone();
                        self.emit(Instr::Construct(args.len() as u8, idx));
                        for (arg, field_idx) in args.iter().zip(fields) {
                            let field = pool.field(field_idx)?;
                            let field_type = scope.resolve_type_from_pool(field.type_, pool, *pos)?;
                            let arg_type = scope.infer_type(arg, Some(&field_type), pool)?;
                            let conv = scope.convert_type(&arg_type, &field_type, pool, *pos)?;
                            self.compile_conversion(conv);
                            self.compile(arg, Some(&field_type), pool, scope)?;
                        }
                    } else if args.is_empty() {
                        self.emit(Instr::New(idx));
                    } else {
                        let err = format!("Expected 0 parameters for {}", name);
                        return Err(Error::CompileError(err, *pos));
                    }
                }
                _ => return Err(Error::CompileError(format!("Cannot construct {}", name), *pos)),
            },
            Expr::Return(Some(expr), pos) => {
                let fun = pool.function(scope.function.unwrap())?;
                if let Some(ret_type) = fun.return_type {
                    let expected = scope.resolve_type_from_pool(ret_type, pool, *pos)?;
                    let type_ = scope.infer_type(expr, Some(&expected), pool)?;
                    let conv = scope.convert_type(&type_, &expected, pool, *pos)?;
                    self.emit(Instr::Return);
                    self.compile_conversion(conv);
                    self.compile(expr, Some(&expected), pool, scope)?;
                } else {
                    return Err(Error::CompileError("Function should return nothing".to_string(), *pos));
                }
            }
            Expr::Return(None, pos) => {
                let fun = pool.function(scope.function.unwrap())?;
                if fun.return_type.is_none() {
                    self.emit(Instr::Return);
                } else {
                    return Err(Error::CompileError("Function should return a value".to_string(), *pos));
                }
            }
            Expr::Seq(seq) => {
                for expr in &seq.exprs {
                    self.compile(expr, None, pool, scope)?;
                }
            }
            Expr::Switch(expr, cases, default) => {
                let type_ = scope.infer_type(expr, None, pool)?;
                let matched = Assembler::from_expr(expr, None, pool, scope)?;
                self.emit(Instr::Switch(scope.get_type_index(&type_, pool)?, matched.offset()));
                self.append(matched);
                for case in cases {
                    let matcher = Assembler::from_expr(&case.0, None, pool, &mut scope.clone())?;
                    let body = Assembler::from_seq(&case.1, pool, &mut scope.clone())?;
                    self.emit(Instr::SwitchLabel(matcher.offset() + body.offset(), matcher.offset()));
                    self.append(matcher);
                    self.append(body);
                }
                if let Some(body) = default {
                    self.emit(Instr::SwitchDefault);
                    self.append(Assembler::from_seq(body, pool, &mut scope.clone())?);
                }
            }
            Expr::If(cond, if_, else_) => {
                let type_ = scope.infer_type(cond, None, pool)?;
                let cond_code = Assembler::from_expr(cond, None, pool, &mut scope.clone())?;
                let cond_code = match type_ {
                    TypeId::Ref(_) => cond_code.prefix(Instr::RefToBool),
                    TypeId::WeakRef(_) => cond_code.prefix(Instr::WeakRefToBool),
                    _ => cond_code,
                };
                let mut if_branch = Assembler::from_seq(if_, pool, &mut scope.clone())?;
                let else_branch = if let Some(else_code) = else_ {
                    let else_branch = Assembler::from_seq(else_code, pool, &mut scope.clone())?;
                    if_branch.emit(Instr::Jump(else_branch.offset()));
                    else_branch
                } else {
                    Assembler::new()
                };
                self.emit(Instr::JumpIfFalse(cond_code.offset() + if_branch.offset()));
                self.append(cond_code);
                self.append(if_branch);
                self.append(else_branch);
            }
            Expr::Conditional(cond, true_, false_, _) => {
                let cond_code = Assembler::from_expr(cond, None, pool, scope)?;
                let true_code = Assembler::from_expr(true_, expected, pool, scope)?;
                let false_code = Assembler::from_expr(false_, expected, pool, scope)?;
                self.emit(Instr::Conditional(
                    cond_code.offset() + true_code.offset(),
                    cond_code.offset() + true_code.offset() + false_code.offset(),
                ));
                self.append(cond_code);
                self.append(true_code);
                self.append(false_code);
            }
            Expr::While(cond, body) => {
                let cond_code = Assembler::from_expr(cond, None, pool, &mut scope.clone())?;
                let mut body_code = Assembler::from_seq(body, pool, &mut scope.clone())?;
                body_code.emit(Instr::Jump(-(cond_code.offset() + body_code.offset())));

                self.emit(Instr::JumpIfFalse(cond_code.offset() + body_code.offset()));
                self.append(cond_code);
                self.append(body_code);
            }
            Expr::Member(expr, ident, pos) => match scope.infer_type(expr, None, pool)?.unwrapped() {
                TypeId::Class(class) => {
                    let object = Assembler::from_expr(expr, None, pool, scope)?;
                    let field = scope.resolve_field(ident.clone(), *class, pool, *pos)?;
                    let inner = Assembler::from_instr(Instr::ObjectField(field));
                    self.emit(Instr::Context(object.offset() + inner.offset()));
                    self.append(object);
                    self.append(inner)
                }
                TypeId::Struct(class) => {
                    let field = scope.resolve_field(ident.clone(), *class, pool, *pos)?;
                    self.emit(Instr::StructField(field));
                    self.compile(expr, None, pool, scope)?;
                }
                TypeId::Enum(enum_) => {
                    let member_idx = scope.resolve_enum_member(ident.clone(), *enum_, pool, *pos)?;
                    self.emit(Instr::EnumConst(*enum_, member_idx));
                }
                t => {
                    let err = format!("Can't access a member of {}", t.pretty(pool)?);
                    return Err(Error::CompileError(err, *pos));
                }
            },
            Expr::Call(ident, args, pos) => {
                if let Ok(intrinsic) = IntrinsicOp::from_str(&ident.0) {
                    self.compile_intrinsic(intrinsic, args, pool, scope, *pos)?;
                } else {
                    let match_ = scope.resolve_function(
                        FunctionName::global(ident.clone()),
                        args.iter(),
                        expected,
                        pool,
                        *pos,
                    )?;
                    self.compile_call(match_, args.iter(), pool, scope, false, *pos)?;
                }
            }
            Expr::MethodCall(expr, ident, args, pos) => {
                let type_ = scope.infer_type(expr, None, pool)?;
                if let Some(Reference::Class(class)) = Self::get_static_reference(expr, scope) {
                    let match_ = scope.resolve_method(ident.clone(), class, args, expected, pool, *pos)?;
                    let fun = pool.function(match_.index)?;
                    if fun.flags.is_static() {
                        self.compile_call(match_, args.iter(), pool, scope, true, *pos)?
                    } else {
                        return Err(Error::CompileError(format!("Method {} is not static", ident), *pos));
                    }
                } else if let TypeId::Class(class) = type_.unwrapped() {
                    let object = if let TypeId::WeakRef(_) = type_ {
                        Assembler::from_expr(expr, None, pool, scope)?.prefix(Instr::WeakRefToRef)
                    } else {
                        Assembler::from_expr(expr, None, pool, scope)?
                    };
                    let force_static_call = matches!(expr.as_ref(), Expr::Super(_));
                    let fun = scope.resolve_method(ident.clone(), *class, args, expected, pool, *pos)?;
                    let mut inner = Assembler::new();
                    inner.compile_call(fun, args.iter(), pool, scope, force_static_call, *pos)?;
                    self.emit(Instr::Context(object.offset() + inner.offset()));
                    self.append(object);
                    self.append(inner);
                } else {
                    let err = format!("Can't call methods on {}", type_.pretty(pool)?);
                    return Err(Error::CompileError(err, *pos));
                }
            }
            Expr::BinOp(lhs, rhs, op, pos) => {
                let ident = Ident::new(op.name());
                let args = iter::once(lhs.as_ref()).chain(iter::once(rhs.as_ref()));
                let fun = scope.resolve_function(FunctionName::global(ident), args.clone(), expected, pool, *pos)?;
                self.compile_call(fun, args, pool, scope, false, *pos)?;
            }
            Expr::UnOp(expr, op, pos) => {
                let ident = Ident::new(op.name());
                let args = iter::once(expr.as_ref());
                let fun = scope.resolve_function(FunctionName::global(ident), args.clone(), expected, pool, *pos)?;
                self.compile_call(fun, args, pool, scope, false, *pos)?;
            }
            Expr::Null => {
                self.emit(Instr::Null);
            }
            Expr::This(_) | Expr::Super(_) => {
                self.emit(Instr::This);
            }
            Expr::Goto(_, _) | Expr::Break => panic!("Not supported yet"),
        };
        Ok(())
    }

    fn compile_call<'a>(
        &mut self,
        function: FunctionMatch,
        args: impl Iterator<Item = &'a Expr>,
        pool: &mut ConstantPool,
        scope: &mut Scope,
        force_static: bool,
        pos: Pos,
    ) -> Result<(), Error> {
        let fun = pool.function(function.index)?;
        let flags = fun.flags;
        let params: Vec<(ParameterFlags, PoolIndex<Type>)> = fun
            .parameters
            .iter()
            .map(|idx| {
                let param = pool.parameter(*idx).unwrap();
                (param.flags, param.type_)
            })
            .collect();
        let name_idx = pool.definition(function.index)?.name;

        let mut args_code = Assembler::new();
        for ((arg, conversion), (flags, type_idx)) in args.zip(function.conversions).zip(params) {
            let mut arg_code = Assembler::new();
            arg_code.compile_conversion(conversion);
            let expected_type = scope.resolve_type_from_pool(type_idx, pool, pos)?;
            arg_code.compile(arg, Some(&expected_type), pool, scope)?;
            if flags.is_short_circuit() {
                args_code.emit(Instr::Skip(arg_code.offset()));
            }
            args_code.append(arg_code);
        }
        for _ in 0..function.unspecified_args {
            args_code.emit(Instr::Nop);
        }
        args_code.emit(Instr::ParamEnd);
        if !force_static && !flags.is_final() && !flags.is_static() && !flags.is_native() {
            self.emit(Instr::InvokeVirtual(args_code.offset(), 0, name_idx));
        } else {
            self.emit(Instr::InvokeStatic(args_code.offset(), 0, function.index));
        }
        self.append(args_code);
        Ok(())
    }

    fn compile_conversion(&mut self, conv: Conversion) {
        match conv {
            Conversion::Identity => {}
            Conversion::RefToWeakRef => self.emit(Instr::RefToWeakRef),
            Conversion::WeakRefToRef => self.emit(Instr::WeakRefToRef),
        }
    }

    fn compile_intrinsic(
        &mut self,
        intrinsic: IntrinsicOp,
        args: &[Expr],
        pool: &mut ConstantPool,
        scope: &mut Scope,
        pos: Pos,
    ) -> Result<(), Error> {
        if args.len() != intrinsic.arg_count().into() {
            let err = format!("Invalid number of arguments for {}", intrinsic);
            return Err(Error::CompileError(err, pos));
        }
        let type_ = scope.infer_type(&args[0], None, pool)?;
        let type_idx = scope.get_type_index(&type_, pool)?;
        match (intrinsic, type_) {
            (IntrinsicOp::Equals, _) => {
                self.emit(Instr::Equals(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::NotEquals, _) => {
                self.emit(Instr::NotEquals(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayClear, TypeId::Array(_)) => {
                self.emit(Instr::ArrayClear(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::ArraySize, TypeId::Array(_)) => {
                self.emit(Instr::ArraySize(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::ArrayResize, TypeId::Array(_)) => {
                self.emit(Instr::ArrayResize(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(_)) => {
                self.emit(Instr::ArrayFindFirst(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayFindLast, TypeId::Array(_)) => {
                self.emit(Instr::ArrayFindLast(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayContains, TypeId::Array(_)) => {
                self.emit(Instr::ArrayContains(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayCount, TypeId::Array(_)) => {
                self.emit(Instr::ArrayCount(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayPush, TypeId::Array(_)) => {
                self.emit(Instr::ArrayPush(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayPop, TypeId::Array(_)) => {
                self.emit(Instr::ArrayPop(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::ArrayInsert, TypeId::Array(_)) => {
                self.emit(Instr::ArrayInsert(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)?;
                self.compile(&args[2], None, pool, scope)
            }
            (IntrinsicOp::ArrayRemove, TypeId::Array(_)) => {
                self.emit(Instr::ArrayRemove(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayGrow, TypeId::Array(_)) => {
                self.emit(Instr::ArrayGrow(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayErase, TypeId::Array(_)) => {
                self.emit(Instr::ArrayErase(type_idx));
                self.compile(&args[0], None, pool, scope)?;
                self.compile(&args[1], None, pool, scope)
            }
            (IntrinsicOp::ArrayLast, TypeId::Array(_)) => {
                self.emit(Instr::ArrayLast(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::ToString, _) => {
                self.emit(Instr::ToString(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::EnumInt, _) => {
                self.emit(Instr::EnumToI32(type_idx, 4));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::IntEnum, _) => {
                self.emit(Instr::I32ToEnum(type_idx, 4));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::ToVariant, _) => {
                self.emit(Instr::ToVariant(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::FromVariant, _) => {
                self.emit(Instr::ToVariant(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::AsRef, TypeId::ScriptRef(_)) => {
                self.emit(Instr::AsRef(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (IntrinsicOp::Deref, _) => {
                self.emit(Instr::Deref(type_idx));
                self.compile(&args[0], None, pool, scope)
            }
            (_, type_) => {
                let err = format!("Invalid intrinsic {} call: {:?}", intrinsic, type_);
                Err(Error::CompileError(err, pos))
            }
        }
    }

    fn from_instr(instr: Instr) -> Assembler {
        let mut code = Assembler::new();
        code.emit(instr);
        code
    }

    fn from_expr(
        expr: &Expr,
        expected: Option<&TypeId>,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        code.compile(expr, expected, pool, scope)?;
        Ok(code)
    }

    pub fn from_seq(seq: &Seq, pool: &mut ConstantPool, scope: &mut Scope) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        for expr in &seq.exprs {
            code.compile(expr, None, pool, scope)?;
        }
        Ok(code)
    }

    fn get_static_reference(expr: &Expr, scope: &Scope) -> Option<Reference> {
        if let Expr::Ident(ident, _) = expr.deref() {
            match scope.resolve_reference(ident.clone(), Pos::ZERO).ok()? {
                r @ Reference::Class(_) => Some(r),
                r @ Reference::Enum(_) => Some(r),
                _ => None,
            }
        } else {
            None
        }
    }
}

fn shifted(instr: Instr) -> Instr {
    let size = instr.size() as i16;
    match instr {
        Instr::InvokeStatic(offset, line, idx) => Instr::InvokeStatic(Offset::new(offset.value + size), line, idx),
        Instr::InvokeVirtual(offset, line, idx) => Instr::InvokeVirtual(Offset::new(offset.value + size), line, idx),
        Instr::Switch(idx, offset) => Instr::Switch(idx, Offset::new(offset.value + size)),
        Instr::SwitchLabel(start, exit) => {
            Instr::SwitchLabel(Offset::new(start.value + size), Offset::new(exit.value + size))
        }
        Instr::Skip(offset) => Instr::Skip(Offset::new(offset.value + size)),
        Instr::Conditional(true_, false_) => {
            Instr::Conditional(Offset::new(true_.value + size), Offset::new(false_.value + size))
        }
        Instr::Context(offset) => Instr::Context(Offset::new(offset.value + size)),
        Instr::Jump(offset) if offset.value > 0 => Instr::Jump(Offset::new(offset.value + size)),
        Instr::JumpIfFalse(offset) if offset.value > 0 => Instr::JumpIfFalse(Offset::new(offset.value + size)),
        Instr::Jump(offset) if offset.value < 0 => Instr::Jump(Offset::new(offset.value - size)),
        Instr::JumpIfFalse(offset) if offset.value < 0 => Instr::JumpIfFalse(Offset::new(offset.value - size)),
        other => other,
    }
}

#[derive(Debug, Clone, Copy, EnumString, Display)]
pub enum IntrinsicOp {
    Equals,
    NotEquals,
    ArrayClear,
    ArraySize,
    ArrayResize,
    ArrayFindFirst,
    ArrayFindLast,
    ArrayContains,
    ArrayCount,
    ArrayPush,
    ArrayPop,
    ArrayInsert,
    ArrayRemove,
    ArrayGrow,
    ArrayErase,
    ArrayLast,
    ToString,
    EnumInt,
    IntEnum,
    ToVariant,
    FromVariant,
    AsRef,
    Deref,
}

impl IntrinsicOp {
    pub fn arg_count(&self) -> u8 {
        match self {
            IntrinsicOp::Equals => 2,
            IntrinsicOp::NotEquals => 2,
            IntrinsicOp::ArrayClear => 1,
            IntrinsicOp::ArraySize => 1,
            IntrinsicOp::ArrayResize => 2,
            IntrinsicOp::ArrayFindFirst => 2,
            IntrinsicOp::ArrayFindLast => 2,
            IntrinsicOp::ArrayContains => 2,
            IntrinsicOp::ArrayCount => 2,
            IntrinsicOp::ArrayPush => 2,
            IntrinsicOp::ArrayPop => 1,
            IntrinsicOp::ArrayInsert => 3,
            IntrinsicOp::ArrayRemove => 2,
            IntrinsicOp::ArrayGrow => 2,
            IntrinsicOp::ArrayErase => 2,
            IntrinsicOp::ArrayLast => 1,
            IntrinsicOp::ToString => 1,
            IntrinsicOp::EnumInt => 1,
            IntrinsicOp::IntEnum => 1,
            IntrinsicOp::ToVariant => 1,
            IntrinsicOp::FromVariant => 1,
            IntrinsicOp::AsRef => 1,
            IntrinsicOp::Deref => 1,
        }
    }
}
