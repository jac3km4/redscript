use std::iter;
use std::ops::Deref;
use std::str::FromStr;

use redscript::ast::{Expr, Ident, LiteralType, Seq};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Instr, Offset};
use redscript::definition::{Definition, Local, LocalFlags, ParameterFlags};
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

    fn compile(&mut self, expr: &Expr, pool: &mut ConstantPool, scope: &mut Scope) -> Result<(), Error> {
        match expr {
            Expr::Ident(name) => {
                match scope.resolve_reference(name.clone())? {
                    Reference::Local(idx) => self.emit(Instr::Local(idx)),
                    Reference::Parameter(idx) => self.emit(Instr::Param(idx)),
                    _ => panic!("Shouldn't get here"),
                };
            }
            Expr::StringLit(LiteralType::String, lit) => {
                self.emit(Instr::StringConst(lit.as_bytes().to_vec()));
            }
            Expr::StringLit(LiteralType::Name, lit) => {
                let idx = pool.names.add(lit.clone());
                self.emit(Instr::NameConst(idx));
            }
            Expr::StringLit(LiteralType::Resource, lit) => {
                let idx = pool.resources.add(lit.clone());
                self.emit(Instr::ResourceConst(idx));
            }
            Expr::StringLit(LiteralType::TweakDbId, lit) => {
                let idx = pool.tweakdb_ids.add(lit.clone());
                self.emit(Instr::TweakDbIdConst(idx));
            }
            Expr::FloatLit(val) => {
                self.emit(Instr::F64Const(*val));
            }
            Expr::IntLit(val) => {
                self.emit(Instr::I32Const(*val as i32));
            }
            Expr::UintLit(val) => {
                self.emit(Instr::U32Const(*val as u32));
            }
            Expr::Cast(type_, expr) => {
                let type_idx = scope.resolve_type_index(Ident::new(type_.repr()))?;
                self.emit(Instr::DynamicCast(type_idx, 0));
                self.compile(expr, pool, scope)?;
            }
            Expr::Declare(name, type_, init) => {
                let name_idx = pool.names.add(name.0.deref().to_owned());
                let (type_, conversion) = match (type_, init) {
                    (None, None) => Err(Error::CompileError(
                        "Type or initializer require on let binding".to_owned(),
                    ))?,
                    (None, Some(val)) => (scope.infer_type(val, pool)?, Conversion::Identity),
                    (Some(type_name), None) => (
                        scope.resolve_type(Ident::new(type_name.repr()), pool)?,
                        Conversion::Identity,
                    ),
                    (Some(type_name), Some(val)) => {
                        let val_type = scope.infer_type(val, pool)?;
                        let type_ = scope.resolve_type(Ident::new(type_name.repr()), pool)?;
                        let conv = scope.convert_type(&val_type, &type_, pool)?;
                        (type_, conv)
                    }
                };
                let local = Local::new(type_.index().unwrap(), LocalFlags::new());
                let idx = pool.push_definition(Definition::local(name_idx, scope.function.unwrap().cast(), local));
                self.locals.push_back(idx.cast());
                scope.push_local(name.clone(), idx.cast());
                if let Some(val) = init {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(idx.cast()));
                    self.compile_conversion(conversion);
                    self.compile(val, pool, scope)?;
                }
            }
            Expr::Assign(lhs, rhs) => {
                let conv = scope.convert_expr(&rhs, &lhs, pool)?;
                self.emit(Instr::Assign);
                self.compile(lhs, pool, scope)?;
                self.compile_conversion(conv);
                self.compile(rhs, pool, scope)?;
            }
            Expr::ArrayElem(expr, idx) => {
                match scope.infer_type(expr, pool)? {
                    TypeId::Array(type_, _) => {
                        self.emit(Instr::ArrayElement(type_));
                    }
                    TypeId::StaticArray(type_, _, _) => {
                        self.emit(Instr::StaticArrayElement(type_));
                    }
                    other => {
                        let error = format!("Array access not allowed on {}", other.pretty(pool)?);
                        Err(Error::CompileError(error))?
                    }
                }
                self.compile(expr, pool, scope)?;
                self.compile(idx, pool, scope)?;
            }
            Expr::New(name, args) => match scope.resolve_reference(name.clone())? {
                Reference::Class(idx) => {
                    let cls = pool.class(idx)?;
                    if cls.flags.is_struct() {
                        if cls.fields.len() != args.len() {
                            let err = format!("Expected {} parameters for {}", cls.fields.len(), name);
                            Err(Error::CompileError(err))?
                        }
                        self.emit(Instr::Construct(args.len() as u8, idx));
                        for arg in args {
                            self.compile(arg, pool, scope)?;
                        }
                    } else if args.is_empty() {
                        self.emit(Instr::New(idx));
                    } else {
                        let err = format!("Expected 0 parameters for {}", name);
                        Err(Error::CompileError(err))?
                    }
                }
                _ => Err(Error::CompileError(format!("Cannot construct {}", name)))?,
            },
            Expr::Return(Some(expr)) => {
                let fun = pool.function(scope.function.unwrap())?;
                if let Some(ret_type) = fun.return_type {
                    let type_ = scope.infer_type(expr, pool)?;
                    let expected = scope.resolve_type_by_index(ret_type, pool)?;
                    let conv = scope.convert_type(&type_, &expected, pool)?;
                    self.emit(Instr::Return);
                    self.compile_conversion(conv);
                    self.compile(expr, pool, scope)?;
                } else {
                    Err(Error::CompileError(format!("Function should return nothing")))?
                }
            }
            Expr::Return(None) => {
                self.emit(Instr::Return);
            }
            Expr::Seq(seq) => {
                for expr in &seq.exprs {
                    self.compile(expr, pool, scope)?;
                }
            }
            Expr::Switch(expr, cases, default) => {
                let type_ = scope.infer_type(expr, pool)?.index().unwrap();
                let matched = Assembler::from_expr(expr, pool, scope)?;
                self.emit(Instr::Switch(type_, matched.offset()));
                self.append(matched);
                for case in cases {
                    let matcher = Assembler::from_expr(&case.0, pool, &mut scope.clone())?;
                    let body = Assembler::from_seq(&case.1, pool, &mut scope.clone())?;
                    self.emit(Instr::SwitchLabel(matcher.offset(), matcher.offset() + body.offset()));
                    self.append(matcher);
                    self.append(body);
                }
                if let Some(body) = default {
                    self.emit(Instr::SwitchDefault);
                    self.append(Assembler::from_seq(body, pool, &mut scope.clone())?);
                }
            }
            Expr::If(cond, if_, else_) => {
                let type_ = scope.infer_type(cond, pool)?;
                let cond_code = Assembler::from_expr(cond, pool, &mut scope.clone())?;
                let cond_code = match type_ {
                    TypeId::Ref(_, _) => cond_code.prefix(Instr::RefToBool),
                    TypeId::WeakRef(_, _) => cond_code.prefix(Instr::WeakRefToBool),
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
            Expr::Conditional(cond, true_, false_) => {
                let cond_code = Assembler::from_expr(cond, pool, scope)?;
                let true_code = Assembler::from_expr(true_, pool, scope)?;
                self.emit(Instr::Conditional(
                    cond_code.offset(),
                    cond_code.offset() + true_code.offset(),
                ));
                self.append(cond_code);
                self.append(true_code);
                self.compile(false_, pool, scope)?
            }
            Expr::While(cond, body) => {
                let cond_code = Assembler::from_expr(cond, pool, &mut scope.clone())?;
                let mut body_code = Assembler::from_seq(body, pool, &mut scope.clone())?;
                body_code.emit(Instr::Jump(-(cond_code.offset() + body_code.offset())));

                self.emit(Instr::JumpIfFalse(cond_code.offset() + body_code.offset()));
                self.append(cond_code);
                self.append(body_code);
            }
            Expr::Member(expr, ident) => match scope.infer_type(expr, pool)?.unwrapped() {
                TypeId::Class(_, class) => {
                    let object = Assembler::from_expr(expr, pool, scope)?;
                    let field = scope.resolve_field(ident.clone(), *class, pool)?;
                    let inner = Assembler::from_instr(Instr::ObjectField(field));
                    self.emit(Instr::Context(object.offset() + inner.offset()));
                    self.append(object);
                    self.append(inner)
                }
                TypeId::Struct(_, class) => {
                    let field = scope.resolve_field(ident.clone(), *class, pool)?;
                    self.emit(Instr::StructField(field));
                    self.compile(expr, pool, scope)?;
                }
                TypeId::Enum(_, enum_) => {
                    let member_idx = scope.resolve_enum_member(ident.clone(), *enum_, pool)?;
                    self.emit(Instr::EnumConst(*enum_, member_idx));
                }
                t => {
                    let err = format!("Can't access a member of {}", t.pretty(pool)?);
                    Err(Error::CompileError(err))?
                }
            },
            Expr::Call(ident, args) => {
                if let Ok(intrinsic) = IntrinsicOp::from_str(&ident.0) {
                    self.compile_intrinsic(intrinsic, args, pool, scope)?;
                } else {
                    let match_ = scope.resolve_function(FunctionName::global(ident.clone()), args.iter(), pool)?;
                    self.compile_call(match_, args.iter(), pool, scope)?;
                }
            }
            Expr::MethodCall(expr, ident, args) => {
                let type_ = scope.infer_type(expr, pool)?;
                if let Some(Reference::Class(class)) = Self::get_static_reference(expr, scope) {
                    let match_ = scope.resolve_method(ident.clone(), class, args, pool)?;
                    let fun = pool.function(match_.index)?;
                    if fun.flags.is_static() {
                        self.compile_call(match_, args.iter(), pool, scope)?
                    } else {
                        Err(Error::CompileError(format!("Method {} is not static", ident)))?
                    }
                } else if let TypeId::Class(_, class) = type_.unwrapped() {
                    let object = if let TypeId::WeakRef(_, _) = type_ {
                        Assembler::from_expr(expr, pool, scope)?.prefix(Instr::WeakRefToRef)
                    } else {
                        Assembler::from_expr(expr, pool, scope)?
                    };
                    let fun = scope.resolve_method(ident.clone(), *class, args, pool)?;
                    let mut inner = Assembler::new();
                    inner.compile_call(fun, args.iter(), pool, scope)?;
                    self.emit(Instr::Context(object.offset() + inner.offset()));
                    self.append(object);
                    self.append(inner);
                } else {
                    let err = format!("Can't call methods on {}", type_.pretty(pool)?);
                    Err(Error::CompileError(err))?
                }
            }
            Expr::BinOp(lhs, rhs, op) => {
                let ident = Ident::new(op.name());
                let args = iter::once(lhs.as_ref()).chain(iter::once(rhs.as_ref()));
                let fun = scope.resolve_function(FunctionName::global(ident), args.clone(), pool)?;
                self.compile_call(fun, args, pool, scope)?;
            }
            Expr::UnOp(expr, op) => {
                let ident = Ident::new(op.name());
                let args = iter::once(expr.as_ref());
                let fun = scope.resolve_function(FunctionName::global(ident), args.clone(), pool)?;
                self.compile_call(fun, args, pool, scope)?;
            }
            Expr::True => {
                self.emit(Instr::TrueConst);
            }
            Expr::False => {
                self.emit(Instr::FalseConst);
            }
            Expr::Null => {
                self.emit(Instr::Null);
            }
            Expr::This => {
                self.emit(Instr::This);
            }
            Expr::Goto(_) | Expr::Break => panic!("Not supported yet"),
        };
        Ok(())
    }

    fn compile_call<'a>(
        &mut self,
        function: FunctionMatch,
        args: impl Iterator<Item = &'a Expr>,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let fun = pool.function(function.index)?;
        let flags = fun.flags;
        let params: Vec<ParameterFlags> = fun
            .parameters
            .iter()
            .map(|idx| pool.parameter(*idx).unwrap().flags)
            .collect();
        let name_idx = pool.definition(function.index)?.name;

        let mut args_code = Assembler::new();
        for ((arg, conversion), flags) in args.zip(function.conversions).zip(params) {
            let mut arg_code = Assembler::new();
            arg_code.compile_conversion(conversion);
            arg_code.compile(arg, pool, scope)?;
            if flags.is_short_circuit() {
                args_code.emit(Instr::Skip(arg_code.offset()));
            }
            args_code.append(arg_code);
        }
        for _ in 0..function.unspecified_args {
            args_code.emit(Instr::Nop);
        }
        args_code.emit(Instr::ParamEnd);
        if !flags.is_final() && !flags.is_static() {
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
    ) -> Result<(), Error> {
        if args.len() != intrinsic.arg_count().into() {
            let err = format!("Invalid number of arguments for {}", intrinsic);
            Err(Error::CompileError(err))?
        }
        let type_ = scope.infer_type(&args[0], pool)?;
        match (intrinsic, type_) {
            (IntrinsicOp::Equals, type_) => {
                self.emit(Instr::Equals(type_.index().unwrap()));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::NotEquals, type_) => {
                self.emit(Instr::NotEquals(type_.index().unwrap()));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayClear, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayClear(type_));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::ArraySize, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArraySize(type_));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::ArrayResize, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayResize(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayFindFirst(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayFindLast, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayFindLast(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayContains, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayContains(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayCount, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayCount(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayPush, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayPush(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayPop, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayPop(type_));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::ArrayInsert, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayInsert(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)?;
                self.compile(&args[2], pool, scope)
            }
            (IntrinsicOp::ArrayRemove, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayRemove(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayGrow, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayGrow(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayErase, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayErase(type_));
                self.compile(&args[0], pool, scope)?;
                self.compile(&args[1], pool, scope)
            }
            (IntrinsicOp::ArrayLast, TypeId::Array(type_, _)) => {
                self.emit(Instr::ArrayLast(type_));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::ToString, any) => {
                self.emit(Instr::ToString(any.index().unwrap()));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::EnumInt, any) => {
                self.emit(Instr::EnumToI32(any.index().unwrap(), 4));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::IntEnum, any) => {
                self.emit(Instr::I32ToEnum(any.index().unwrap(), 4));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::ToVariant, any) => {
                self.emit(Instr::ToVariant(any.index().unwrap()));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::FromVariant, any) => {
                self.emit(Instr::ToVariant(any.index().unwrap()));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::AsRef, any) => {
                self.emit(Instr::AsRef(any.index().unwrap()));
                self.compile(&args[0], pool, scope)
            }
            (IntrinsicOp::Deref, any) => {
                self.emit(Instr::Deref(any.index().unwrap()));
                self.compile(&args[0], pool, scope)
            }
            _ => {
                let err = format!("Invalid intrinsic {} call", intrinsic);
                Err(Error::CompileError(err))
            }
        }
    }

    fn from_instr(instr: Instr) -> Assembler {
        let mut code = Assembler::new();
        code.emit(instr);
        code
    }

    fn from_expr(expr: &Expr, pool: &mut ConstantPool, scope: &mut Scope) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        code.compile(expr, pool, scope)?;
        Ok(code)
    }

    pub fn from_seq(seq: &Seq, pool: &mut ConstantPool, scope: &mut Scope) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        for expr in &seq.exprs {
            code.compile(expr, pool, scope)?;
        }
        Ok(code)
    }

    fn get_static_reference(expr: &Expr, scope: &Scope) -> Option<Reference> {
        if let Expr::Ident(ident) = expr.deref() {
            match scope.resolve_reference(ident.clone()).ok()? {
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
