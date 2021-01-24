use std::iter;
use std::ops::Deref;

use redscript::ast::{Expr, Ident, Seq};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Instr, Offset};
use redscript::definition::{Class, Local, LocalFlags};
use redscript::definition::{Definition, Function};
use redscript::error::Error;

use crate::scope::{FunctionId, Scope};
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

    fn emit(&mut self, instr: Instr) {
        self.position += 1 + instr.size();
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
                match scope.resolve(name.clone())? {
                    Reference::Local(idx) => self.emit(Instr::Local(idx)),
                    Reference::Parameter(idx) => self.emit(Instr::Param(idx)),
                    _ => panic!("Shouldn't get here"),
                };
            }
            Expr::StringLit(lit) => {
                self.emit(Instr::StringConst(lit.as_bytes().to_vec()));
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
                let type_idx = scope.resolve_type_name(Ident::new(type_.repr()))?;
                self.emit(Instr::DynamicCast(type_idx, 0));
                self.compile(expr, pool, scope)?;
            }
            Expr::Declare(type_, name, expr) => {
                let name_idx = pool.names.add(name.0.deref().to_owned());
                let type_ = scope.resolve_type_name(Ident::new(type_.repr()))?;
                let local = Local::new(type_, LocalFlags::new());
                let idx = pool.push_definition(Definition::local(name_idx, scope.function.unwrap().cast(), local));
                self.locals.push_back(idx.cast());
                scope.push_reference(name.clone(), Reference::Local(idx.cast()));
                if let Some(val) = expr {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(idx.cast()));
                    self.compile(val, pool, scope)?;
                }
            }
            Expr::Assign(lhs, rhs) => {
                self.emit(Instr::Assign);
                self.compile(lhs, pool, scope)?;
                self.compile(rhs, pool, scope)?;
            }
            Expr::ArrayElem(expr, idx) => {
                match scope.infer_type(expr, pool)? {
                    TypeId::Array(_, member) => {
                        self.emit(Instr::ArrayElement(member.index().unwrap()));
                    }
                    TypeId::StaticArray(_, member, _) => {
                        self.emit(Instr::StaticArrayElement(member.index().unwrap()));
                    }
                    other => {
                        let error = format!("Array access not allowed on {:?}", other);
                        Err(Error::CompileError(error))?
                    }
                }
                self.compile(expr, pool, scope)?;
                self.compile(idx, pool, scope)?;
            }
            Expr::New(name, args) => match scope.resolve(name.clone())? {
                Reference::Class(idx) => {
                    let cls = pool.class(idx)?;
                    if cls.flags.is_struct() {
                        if cls.fields.len() != args.len() {
                            let err = format!("Expected {} parameters for {}", cls.fields.len(), name.0);
                            Err(Error::CompileError(err))?
                        }
                        self.emit(Instr::Construct(args.len() as u8, idx));
                        for arg in args {
                            self.compile(arg, pool, scope)?;
                        }
                    } else if args.is_empty() {
                        self.emit(Instr::New(idx));
                    } else {
                        let err = format!("Expected 0 parameters for {}", name.0);
                        Err(Error::CompileError(err))?
                    }
                }
                _ => Err(Error::CompileError(format!("Cannot construct {}", name.0)))?,
            },
            Expr::Return(Some(expr)) => {
                self.emit(Instr::Return);
                self.compile(expr, pool, scope)?;
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
                let cond_code = Assembler::from_expr(cond, pool, &mut scope.clone())?;
                let if_code = Assembler::from_seq(if_, pool, &mut scope.clone())?;
                self.emit(Instr::JumpIfFalse(cond_code.offset() + if_code.offset()));
                self.append(cond_code);
                self.append(if_code);
                if let Some(else_code) = else_ {
                    self.append(Assembler::from_seq(else_code, pool, &mut scope.clone())?);
                }
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
            Expr::Member(expr, member) => {
                self.compile_member_expr(expr, member, pool, scope)?;
            }
            Expr::Call(ident, args) => {
                let fun_id = FunctionId::by_name_and_args(ident, args, pool, scope)?;
                let fun = scope.resolve_function(fun_id)?;
                self.compile_call(fun, args.iter(), pool, scope)?;
            }
            Expr::BinOp(lhs, rhs, op) => {
                let fun_id = FunctionId::by_binop(lhs, rhs, *op, pool, scope)?;
                let fun = scope.resolve_function(fun_id)?;
                let params = iter::once(lhs.as_ref()).chain(iter::once(rhs.as_ref()));
                self.compile_call(fun, params, pool, scope)?;
            }
            Expr::UnOp(expr, op) => {
                let fun_id = FunctionId::by_unop(expr, *op, pool, scope)?;
                let fun = scope.resolve_function(fun_id)?;
                self.compile_call(fun, iter::once(expr.as_ref()), pool, scope)?;
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

    fn compile_call<'a, I: Iterator<Item = &'a Expr>>(
        &mut self,
        fun_idx: PoolIndex<Function>,
        params: I,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let flags = pool.function(fun_idx)?.flags;
        let name_idx = pool.definition(fun_idx)?.name;
        let mut args_code = Assembler::new();
        for arg in params {
            args_code.compile(arg, pool, scope)?;
        }
        args_code.emit(Instr::ParamEnd);
        if !flags.is_final() && !flags.is_static() {
            self.emit(Instr::InvokeVirtual(args_code.offset(), 0, name_idx));
        } else {
            self.emit(Instr::InvokeStatic(args_code.offset(), 0, fun_idx));
        }
        self.append(args_code);
        Ok(())
    }

    fn compile_member_expr(
        &mut self,
        expr: &Expr,
        member: &Expr,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        match Self::get_static_reference(expr, scope) {
            None => self.compile_instance_access(expr, member, pool, scope),
            Some(Reference::Class(class)) => self.compile_static_access(class, member, pool, scope),
            Some(Reference::Enum(enum_)) => match member.deref() {
                Expr::Ident(ident) => {
                    let member_idx = scope.resolve_enum_member(ident.clone(), enum_, pool)?;
                    self.emit(Instr::EnumConst(enum_, member_idx));
                    Ok(())
                }
                _ => Err(Error::CompileError("Unknown operation on enum".to_owned())),
            },
            _ => panic!("Shouldn't get here"),
        }
    }

    fn compile_static_access(
        &mut self,
        class: PoolIndex<Class>,
        member: &Expr,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        match member.deref() {
            Expr::Call(ident, args) => {
                let fun_id = FunctionId::by_name_and_args(ident, args, pool, scope)?;
                let fun_idx = scope.resolve_method(fun_id, class, pool)?;
                let fun = pool.function(fun_idx)?;
                if fun.flags.is_static() {
                    self.compile_call(fun_idx, args.iter(), pool, scope)
                } else {
                    Err(Error::CompileError(format!("{} is not static", ident.0)))
                }
            }
            _ => Err(Error::CompileError("Can't access fields statically".to_owned())),
        }
    }

    fn compile_instance_access(
        &mut self,
        expr: &Expr,
        member: &Expr,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        match scope.infer_type(expr, pool)?.unwrapped() {
            TypeId::Class(_, class) => {
                let code = match member.deref() {
                    Expr::Ident(ident) => {
                        let field = scope.resolve_field(ident.clone(), *class, pool)?;
                        let mut code = Assembler::new();
                        code.emit(Instr::ObjectField(field));
                        code
                    }
                    Expr::Call(ident, args) => {
                        let fun_id = FunctionId::by_name_and_args(ident, args, pool, scope)?;
                        let fun = scope.resolve_method(fun_id, *class, pool)?;
                        let mut code = Assembler::new();
                        code.compile_call(fun, args.iter(), pool, scope)?;
                        code
                    }
                    _ => {
                        let error = format!("Invalid class instance operation: {:?}", member);
                        Err(Error::CompileError(error))?
                    }
                };
                let object = Assembler::from_expr(expr, pool, scope)?;
                self.emit(Instr::Context(object.offset() + code.offset()));
                self.append(object);
                self.append(code)
            }
            TypeId::Struct(_, class) => {
                if let Expr::Ident(ident) = member.deref() {
                    let field = scope.resolve_field(ident.clone(), *class, pool)?;
                    self.emit(Instr::StructField(field));
                    self.compile(expr, pool, scope)?;
                } else {
                    Err(Error::CompileError(format!("Only field access allowed on structs")))?
                }
            }
            other => Err(Error::CompileError(format!("Can't access members of {:?}", other)))?,
        };
        Ok(())
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
            match scope.resolve(ident.clone()).ok()? {
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
    let size = 1 + instr.size() as i16;
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
