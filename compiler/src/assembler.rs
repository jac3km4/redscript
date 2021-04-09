use std::ops::Deref;

use redscript::ast::{Constant, Expr, Literal, Seq};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr, Label, Location, Offset};
use redscript::definition::{Function, ParameterFlags};
use redscript::error::Error;

use crate::scope::Scope;
use crate::typechecker::{type_of, Callable, IntrinsicOp, Member, TypedAst};
use crate::{Reference, TypeId};

pub struct Assembler {
    instructions: Vec<Instr<Label>>,
    labels: usize,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            instructions: Vec::new(),
            labels: 0,
        }
    }

    fn emit(&mut self, instr: Instr<Label>) {
        self.instructions.push(instr);
    }

    fn emit_label(&mut self, label: Label) {
        self.instructions.push(Instr::Target(label))
    }

    fn new_label(&mut self) -> Label {
        let label = Label { index: self.labels };
        self.labels += 1;
        label
    }

    fn assemble(
        &mut self,
        expr: Expr<TypedAst>,
        scope: &mut Scope,
        pool: &mut ConstantPool,
        exit: Option<Label>,
    ) -> Result<(), Error> {
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
                    self.assemble(*expr, scope, pool, None)?;
                } else {
                    return Err(Error::invalid_op(type_.pretty(pool)?, "Cast", pos));
                }
            }
            Expr::Declare(local, _, init, _) => {
                if let Some(val) = init {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(local));
                    self.assemble(*val, scope, pool, None)?;
                }
            }
            Expr::Assign(lhs, rhs, _) => {
                self.emit(Instr::Assign);
                self.assemble(*lhs, scope, pool, None)?;
                self.assemble(*rhs, scope, pool, None)?;
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
                self.assemble(*expr, scope, pool, None)?;
                self.assemble(*idx, scope, pool, None)?;
            }
            Expr::New(type_, args, pos) => match type_ {
                TypeId::Class(idx) => self.emit(Instr::New(idx)),
                TypeId::Struct(idx) => {
                    self.emit(Instr::Construct(args.len() as u8, idx));
                    for arg in args {
                        self.assemble(arg, scope, pool, None)?;
                    }
                }
                _ => return Err(Error::invalid_op(type_.pretty(pool)?, "Constructing", pos)),
            },
            Expr::Return(Some(expr), _) => {
                self.emit(Instr::Return);
                self.assemble(*expr, scope, pool, None)?;
            }
            Expr::Return(None, _) => {
                self.emit(Instr::Return);
            }
            Expr::Seq(seq) => {
                self.assemble_seq(seq, scope, pool, exit)?;
            }
            Expr::Switch(expr, cases, default) => {
                let type_ = type_of(&expr, scope, pool)?;
                let first_case_label = self.new_label();
                let mut next_case_label = self.new_label();
                let exit_label = self.new_label();
                self.emit(Instr::Switch(scope.get_type_index(&type_, pool)?, first_case_label));
                self.assemble(*expr, scope, pool, None)?;
                self.emit_label(first_case_label);

                let mut case_iter = cases.into_iter().peekable();
                while case_iter.peek().is_some() {
                    let body_label = self.new_label();

                    while let Some(case) = case_iter.next() {
                        self.emit_label(next_case_label);
                        next_case_label = self.new_label();
                        self.emit(Instr::SwitchLabel(next_case_label, body_label));
                        self.assemble(case.matcher, scope, pool, None)?;

                        if !case.body.exprs.iter().all(|expr| expr.is_empty()) {
                            self.emit_label(body_label);
                            self.assemble_seq(case.body, scope, pool, Some(exit_label))?;
                            break;
                        }
                    }
                }
                self.emit_label(next_case_label);

                if let Some(body) = default {
                    self.emit(Instr::SwitchDefault);
                    self.assemble_seq(body, scope, pool, Some(exit_label))?;
                }
                self.emit_label(exit_label);
            }
            Expr::If(condition, if_, else_, _) => {
                let else_label = self.new_label();
                let exit_label = self.new_label();
                self.emit(Instr::JumpIfFalse(else_label));
                self.assemble(*condition, scope, pool, None)?;
                self.assemble_seq(if_, scope, pool, None)?;
                if let Some(else_code) = else_ {
                    self.emit(Instr::Jump(exit_label));
                    self.emit_label(else_label);
                    self.assemble_seq(else_code, scope, pool, None)?;
                } else {
                    self.emit_label(else_label);
                }
            }
            Expr::Conditional(cond, true_, false_, _) => {
                let false_label = self.new_label();
                let exit_label = self.new_label();
                self.emit(Instr::Conditional(false_label, exit_label));
                self.assemble(*cond, scope, pool, None)?;
                self.assemble(*true_, scope, pool, None)?;
                self.emit_label(false_label);
                self.assemble(*false_, scope, pool, None)?;
                self.emit_label(exit_label);
            }
            Expr::While(cond, body, _) => {
                let exit_label = self.new_label();
                let loop_label = self.new_label();
                self.emit_label(loop_label);
                self.emit(Instr::JumpIfFalse(exit_label));
                self.assemble(*cond, scope, pool, None)?;
                self.assemble_seq(body, scope, pool, Some(exit_label))?;
                self.emit(Instr::Jump(loop_label));
                self.emit_label(exit_label);
            }
            Expr::Member(expr, member, _) => match member {
                Member::ClassField(field) => {
                    let exit_label = self.new_label();
                    self.emit(Instr::Context(exit_label));
                    self.assemble(*expr, scope, pool, None)?;
                    self.emit(Instr::ObjectField(field));
                    self.emit_label(exit_label);
                }
                Member::StructField(field) => {
                    self.emit(Instr::StructField(field));
                    self.assemble(*expr, scope, pool, None)?;
                }
                Member::EnumMember(enum_, member) => {
                    self.emit(Instr::EnumConst(enum_, member));
                }
            },
            Expr::Call(callable, args, _) => match callable {
                Callable::Function(fun) => {
                    self.assemble_call(fun, args, scope, pool, false)?;
                }
                Callable::Intrinsic(op, type_) => {
                    self.assemble_intrinsic(op, args, &type_, scope, pool)?;
                }
            },
            Expr::MethodCall(expr, fun_idx, args, _) => match *expr {
                Expr::Ident(Reference::Class(_), pos) => {
                    let fun = pool.function(fun_idx)?;
                    if fun.flags.is_static() {
                        self.assemble_call(fun_idx, args, scope, pool, true)?
                    } else {
                        let name = pool.definition_name(fun_idx)?;
                        return Err(Error::CompileError(format!("Method {} is not static", name), pos));
                    }
                }
                expr => {
                    let force_static_call = matches!(&expr, Expr::Super(_));
                    let exit_label = self.new_label();
                    self.emit(Instr::Context(exit_label));
                    self.assemble(expr, scope, pool, None)?;
                    self.assemble_call(fun_idx, args, scope, pool, force_static_call)?;
                    self.emit_label(exit_label);
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
            Expr::Break(_) if exit.is_some() => {
                self.emit(Instr::Jump(exit.unwrap()));
            }
            Expr::Break(pos) => return Err(Error::CompileError("Break can't be used here".to_owned(), pos)),
            Expr::Goto(_, pos) => return Err(Error::CompileError("Goto is not supported".to_owned(), pos)),
        };
        Ok(())
    }

    fn assemble_seq(
        &mut self,
        seq: Seq<TypedAst>,
        scope: &mut Scope,
        pool: &mut ConstantPool,
        exit: Option<Label>,
    ) -> Result<(), Error> {
        for expr in seq.exprs {
            self.assemble(expr, scope, pool, exit)?;
        }
        Ok(())
    }

    fn assemble_call(
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
        let exit_label = self.new_label();

        if !force_static && !flags.is_final() && !flags.is_static() && !flags.is_native() {
            let name_idx = pool.definition(function_idx)?.name;
            self.emit(Instr::InvokeVirtual(exit_label, 0, name_idx));
        } else {
            self.emit(Instr::InvokeStatic(exit_label, 0, function_idx));
        }
        for (arg, flags) in args.into_iter().zip(param_flags.iter()) {
            if flags.is_short_circuit() {
                let skip_label = self.new_label();
                self.emit(Instr::Skip(skip_label));
                self.assemble(arg, scope, pool, None)?;
                self.emit_label(skip_label);
            } else {
                self.assemble(arg, scope, pool, None)?;
            }
        }
        for _ in 0..param_flags.len() - args_len {
            self.emit(Instr::Nop);
        }
        self.emit(Instr::ParamEnd);
        self.emit_label(exit_label);
        Ok(())
    }

    fn assemble_intrinsic(
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
            IntrinsicOp::ToBool => match type_ {
                TypeId::Ref(_) => self.emit(Instr::RefToBool),
                TypeId::WeakRef(_) => self.emit(Instr::WeakRefToBool),
                _ => panic!("Invalid ToBool parameter"),
            },
        };
        for arg in args {
            self.assemble(arg, scope, pool, None)?;
        }
        Ok(())
    }

    fn into_code(self) -> Code<Offset> {
        let mut locations = Vec::with_capacity(self.labels);
        locations.resize(self.labels, Location::new(0));

        let code = Code(self.instructions);
        for (loc, instr) in code.cursor() {
            if let Instr::Target(label) = instr {
                locations[label.index] = loc;
            }
        }

        let mut resolved = Vec::with_capacity(code.0.len());
        for (loc, instr) in code.cursor().filter(|(_, instr)| !matches!(instr, Instr::Target(_))) {
            resolved.push(instr.resolve_labels(loc, &locations));
        }
        Code(resolved)
    }

    pub fn from_body(seq: Seq<TypedAst>, scope: &mut Scope, pool: &mut ConstantPool) -> Result<Code<Offset>, Error> {
        let mut assembler = Assembler::new();
        assembler.assemble_seq(seq, scope, pool, None)?;
        assembler.emit(Instr::Nop);
        Ok(assembler.into_code())
    }
}
