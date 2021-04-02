use std::ops::Deref;

use redscript::ast::{Constant, Expr, Literal, Seq};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Instr, Offset};
use redscript::definition::{Function, ParameterFlags};
use redscript::error::Error;

use crate::scope::Scope;
use crate::typechecker::{type_of, Callable, IntrinsicOp, Member, TypedAst};
use crate::{Reference, TypeId};

pub struct Assembler {
    pub code: im::Vector<Instr>,
    position: u16,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            code: im::Vector::new(),
            position: 0,
        }
    }

    fn offset(&self) -> Offset {
        Offset::new(self.position as i16)
    }

    fn emit(&mut self, instr: Instr) {
        self.position += instr.size();
        self.code.push_back(shifted(instr));
    }

    fn append(&mut self, code: Assembler) {
        self.position += code.position;
        self.code.append(code.code);
    }

    fn compile(&mut self, expr: Expr<TypedAst>, scope: &mut Scope, pool: &mut ConstantPool) -> Result<(), Error> {
        match expr {
            Expr::Ident(reference, pos) => {
                match reference {
                    Reference::Local(idx) => self.emit(Instr::Local(idx)),
                    Reference::Parameter(idx) => self.emit(Instr::Param(idx)),
                    _ => return Err(Error::CompileError("Expected a value".to_owned(), pos)),
                };
            }
            Expr::Constant(cons, _) => match cons {
                Constant::String(Literal::String, lit) => {
                    self.emit(Instr::StringConst(lit.deref().to_owned()));
                }
                Constant::String(Literal::Name, lit) => {
                    let idx = pool.names.add(lit);
                    self.emit(Instr::NameConst(idx));
                }
                Constant::String(Literal::Resource, lit) => {
                    let idx = pool.resources.add(lit);
                    self.emit(Instr::ResourceConst(idx));
                }
                Constant::String(Literal::TweakDbId, lit) => {
                    let idx = pool.tweakdb_ids.add(lit);
                    self.emit(Instr::TweakDbIdConst(idx));
                }
                Constant::Float(val) => {
                    self.emit(Instr::F32Const(val as f32));
                }
                Constant::Int(val) => {
                    self.emit(Instr::I32Const(val as i32));
                }
                Constant::Uint(val) => {
                    self.emit(Instr::U32Const(val as u32));
                }
                Constant::Bool(true) => {
                    self.emit(Instr::TrueConst);
                }
                Constant::Bool(false) => {
                    self.emit(Instr::FalseConst);
                }
            },
            Expr::Cast(type_, expr, pos) => {
                if let TypeId::Class(class) = type_ {
                    self.emit(Instr::DynamicCast(class, 0));
                    self.compile(*expr, scope, pool)?;
                } else {
                    return Err(Error::invalid_op(type_.pretty(pool)?, "Casting", pos));
                }
            }
            Expr::Declare(local, _, init, _) => {
                if let Some(val) = init {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(local));
                    self.compile(*val, scope, pool)?;
                }
            }
            Expr::Assign(lhs, rhs, _) => {
                self.emit(Instr::Assign);
                self.compile(*lhs, scope, pool)?;
                self.compile(*rhs, scope, pool)?;
            }
            Expr::ArrayElem(expr, idx, pos) => {
                match type_of(&expr, scope, pool)? {
                    type_ @ TypeId::Array(_) => {
                        self.emit(Instr::ArrayElement(scope.get_type_index(&type_, pool)?));
                    }
                    type_ @ TypeId::StaticArray(_, _) => {
                        self.emit(Instr::StaticArrayElement(scope.get_type_index(&type_, pool)?));
                    }
                    other => return Err(Error::invalid_op(other.pretty(pool)?, "Indexing", pos)),
                }
                self.compile(*expr, scope, pool)?;
                self.compile(*idx, scope, pool)?;
            }
            Expr::New(type_, args, pos) => match type_ {
                TypeId::Class(idx) => self.emit(Instr::New(idx)),
                TypeId::Struct(idx) => {
                    self.emit(Instr::Construct(args.len() as u8, idx));
                    for arg in args {
                        self.compile(arg, scope, pool)?;
                    }
                }
                _ => return Err(Error::invalid_op(type_.pretty(pool)?, "Constructing", pos)),
            },
            Expr::Return(Some(expr), _) => {
                self.emit(Instr::Return);
                self.compile(*expr, scope, pool)?;
            }
            Expr::Return(None, _) => {
                self.emit(Instr::Return);
            }
            Expr::Seq(seq) => {
                for expr in seq.exprs {
                    self.compile(expr, scope, pool)?;
                }
            }
            Expr::Switch(expr, cases, default) => {
                let type_ = type_of(&expr, scope, pool)?;
                let matched = Assembler::from_expr(*expr, scope, pool)?;
                self.emit(Instr::Switch(scope.get_type_index(&type_, pool)?, matched.offset()));
                self.append(matched);
                for case in cases {
                    let matcher = Assembler::from_expr(case.matcher, scope, pool)?;
                    let body = Assembler::from_seq(case.body, scope, pool)?;
                    self.emit(Instr::SwitchLabel(matcher.offset() + body.offset(), matcher.offset()));
                    self.append(matcher);
                    self.append(body);
                }
                if let Some(body) = default {
                    self.emit(Instr::SwitchDefault);
                    self.append(Assembler::from_seq(body, scope, pool)?);
                }
            }
            Expr::If(cond, if_, else_, _) => {
                let mut cond_code = Assembler::new();
                cond_code.compile(*cond, scope, pool)?;

                let mut if_branch = Assembler::from_seq(if_, scope, pool)?;
                let else_branch = if let Some(else_code) = else_ {
                    let else_branch = Assembler::from_seq(else_code, scope, pool)?;
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
                let mut cond_code = Assembler::new();
                cond_code.compile(*cond, scope, pool)?;

                let true_code = Assembler::from_expr(*true_, scope, pool)?;
                let false_code = Assembler::from_expr(*false_, scope, pool)?;
                self.emit(Instr::Conditional(
                    cond_code.offset() + true_code.offset(),
                    cond_code.offset() + true_code.offset() + false_code.offset(),
                ));
                self.append(cond_code);
                self.append(true_code);
                self.append(false_code);
            }
            Expr::While(cond, body, _) => {
                let mut cond_code = Assembler::new();
                cond_code.compile(*cond, scope, pool)?;

                let mut body_code = Assembler::from_seq(body, scope, pool)?;
                body_code.emit(Instr::Jump(-(cond_code.offset() + body_code.offset())));

                self.emit(Instr::JumpIfFalse(cond_code.offset() + body_code.offset()));
                self.append(cond_code);
                self.append(body_code);
            }
            Expr::Member(expr, member, _) => match member {
                Member::ClassField(field) => {
                    let object = Assembler::from_expr(*expr, scope, pool)?;
                    let inner = Assembler::from_instr(Instr::ObjectField(field));
                    self.emit(Instr::Context(object.offset() + inner.offset()));
                    self.append(object);
                    self.append(inner)
                }
                Member::StructField(field) => {
                    self.emit(Instr::StructField(field));
                    self.compile(*expr, scope, pool)?;
                }
                Member::EnumMember(enum_, member) => {
                    self.emit(Instr::EnumConst(enum_, member));
                }
            },
            Expr::Call(callable, args, _) => match callable {
                Callable::Function(fun) => {
                    self.compile_call(fun, args, scope, pool, false)?;
                }
                Callable::Intrinsic(op, type_) => {
                    self.compile_intrinsic(op, args, &type_, scope, pool)?;
                }
            },
            Expr::MethodCall(expr, fun_idx, args, _) => match *expr {
                Expr::Ident(Reference::Class(_), pos) => {
                    let fun = pool.function(fun_idx)?;
                    if fun.flags.is_static() {
                        self.compile_call(fun_idx, args, scope, pool, true)?
                    } else {
                        let name = pool.definition_name(fun_idx)?;
                        return Err(Error::CompileError(format!("Method {} is not static", name), pos));
                    }
                }
                other => {
                    let force_static_call = matches!(&other, Expr::Super(_));
                    let object = Assembler::from_expr(other, scope, pool)?;
                    let mut inner = Assembler::new();
                    inner.compile_call(fun_idx, args, scope, pool, force_static_call)?;
                    self.emit(Instr::Context(object.offset() + inner.offset()));
                    self.append(object);
                    self.append(inner);
                }
            },
            Expr::ArrayLit(_, _, pos) => {
                return Err(Error::CompileError("ArrayLit not supported here".to_owned(), pos))
            }
            Expr::ForIn(_, _, _, pos) => return Err(Error::CompileError("ForIn not supported here".to_owned(), pos)),
            Expr::BinOp(_, _, _, pos) => return Err(Error::CompileError("BinOp not supported here".to_owned(), pos)),
            Expr::UnOp(_, _, pos) => return Err(Error::CompileError("UnOp not supported here".to_owned(), pos)),
            Expr::Null => {
                self.emit(Instr::Null);
            }
            Expr::This(_) | Expr::Super(_) => {
                self.emit(Instr::This);
            }
            Expr::Goto(_, pos) | Expr::Break(pos) => {
                return Err(Error::CompileError("Goto is not supported yet".to_owned(), pos))
            }
        };
        Ok(())
    }

    fn compile_call(
        &mut self,
        function_idx: PoolIndex<Function>,
        args: Vec<Expr<TypedAst>>,
        scope: &mut Scope,
        pool: &mut ConstantPool,
        force_static: bool,
    ) -> Result<(), Error> {
        let fun = pool.function(function_idx)?;
        let flags = fun.flags;
        let get_param_flags: Result<Vec<ParameterFlags>, Error> = fun
            .parameters
            .iter()
            .map(|idx| pool.parameter(*idx).map(|param| param.flags))
            .collect();
        let param_flags = get_param_flags?;
        let args_len = args.len();

        let mut args_code = Assembler::new();
        for (arg, flags) in args.into_iter().zip(param_flags.iter()) {
            let mut arg_code = Assembler::new();
            arg_code.compile(arg, scope, pool)?;
            if flags.is_short_circuit() {
                args_code.emit(Instr::Skip(arg_code.offset()));
            }
            args_code.append(arg_code);
        }
        for _ in 0..param_flags.len() - args_len {
            args_code.emit(Instr::Nop);
        }
        args_code.emit(Instr::ParamEnd);
        if !force_static && !flags.is_final() && !flags.is_static() && !flags.is_native() {
            let name_idx = pool.definition(function_idx)?.name;
            self.emit(Instr::InvokeVirtual(args_code.offset(), 0, name_idx));
        } else {
            self.emit(Instr::InvokeStatic(args_code.offset(), 0, function_idx));
        }
        self.append(args_code);
        Ok(())
    }

    fn compile_intrinsic(
        &mut self,
        intrinsic: IntrinsicOp,
        args: Vec<Expr<TypedAst>>,
        return_type: &TypeId,
        scope: &mut Scope,
        pool: &mut ConstantPool,
    ) -> Result<(), Error> {
        let type_ = type_of(&args[0], scope, pool)?;
        let type_idx = scope.get_type_index(&type_, pool)?;
        match intrinsic {
            IntrinsicOp::Equals => {
                self.emit(Instr::Equals(type_idx));
            }
            IntrinsicOp::NotEquals => {
                self.emit(Instr::NotEquals(type_idx));
            }
            IntrinsicOp::ArrayClear => {
                self.emit(Instr::ArrayClear(type_idx));
            }
            IntrinsicOp::ArraySize => {
                self.emit(Instr::ArraySize(type_idx));
            }
            IntrinsicOp::ArrayResize => {
                self.emit(Instr::ArrayResize(type_idx));
            }
            IntrinsicOp::ArrayFindFirst => {
                self.emit(Instr::ArrayFindFirst(type_idx));
            }
            IntrinsicOp::ArrayFindLast => {
                self.emit(Instr::ArrayFindLast(type_idx));
            }
            IntrinsicOp::ArrayContains => {
                self.emit(Instr::ArrayContains(type_idx));
            }
            IntrinsicOp::ArrayCount => {
                self.emit(Instr::ArrayCount(type_idx));
            }
            IntrinsicOp::ArrayPush => {
                self.emit(Instr::ArrayPush(type_idx));
            }
            IntrinsicOp::ArrayPop => {
                self.emit(Instr::ArrayPop(type_idx));
            }
            IntrinsicOp::ArrayInsert => {
                self.emit(Instr::ArrayInsert(type_idx));
            }
            IntrinsicOp::ArrayRemove => {
                self.emit(Instr::ArrayRemove(type_idx));
            }
            IntrinsicOp::ArrayGrow => {
                self.emit(Instr::ArrayGrow(type_idx));
            }
            IntrinsicOp::ArrayErase => {
                self.emit(Instr::ArrayErase(type_idx));
            }
            IntrinsicOp::ArrayLast => {
                self.emit(Instr::ArrayLast(type_idx));
            }
            IntrinsicOp::ToString => {
                self.emit(Instr::ToString(type_idx));
            }
            IntrinsicOp::EnumInt => {
                self.emit(Instr::EnumToI32(type_idx, 4));
            }
            IntrinsicOp::IntEnum => {
                let type_idx = scope.get_type_index(return_type, pool)?;
                self.emit(Instr::I32ToEnum(type_idx, 4));
            }
            IntrinsicOp::ToVariant => {
                self.emit(Instr::ToVariant(type_idx));
            }
            IntrinsicOp::FromVariant => {
                let type_idx = scope.get_type_index(return_type, pool)?;
                self.emit(Instr::FromVariant(type_idx));
            }
            IntrinsicOp::AsRef => {
                self.emit(Instr::AsRef(type_idx));
            }
            IntrinsicOp::Deref => {
                self.emit(Instr::Deref(type_idx));
            }
            IntrinsicOp::RefToWeakRef => {
                self.emit(Instr::RefToWeakRef);
            }
            IntrinsicOp::WeakRefToRef => {
                self.emit(Instr::WeakRefToRef);
            }
        };
        for arg in args {
            self.compile(arg, scope, pool)?;
        }
        Ok(())
    }

    fn from_instr(instr: Instr) -> Assembler {
        let mut code = Assembler::new();
        code.emit(instr);
        code
    }

    fn from_expr(expr: Expr<TypedAst>, scope: &mut Scope, pool: &mut ConstantPool) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        code.compile(expr, scope, pool)?;
        Ok(code)
    }

    pub fn from_seq(seq: Seq<TypedAst>, scope: &mut Scope, pool: &mut ConstantPool) -> Result<Assembler, Error> {
        let mut code = Assembler::new();
        for expr in seq.exprs {
            code.compile(expr, scope, pool)?;
        }
        Ok(code)
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
