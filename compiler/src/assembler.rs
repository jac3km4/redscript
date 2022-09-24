use itertools::Itertools;
use redscript::ast::{Constant, Expr, Ident, Literal, Seq, Span, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr, IntrinsicOp, Label, Location, Offset};
use redscript::definition::{Function, Local};

use crate::error::{Cause, Error, ResultSpan};
use crate::scope::{Reference, Scope, TypeId, Value};
use crate::symbol::Symbol;
use crate::typechecker::{type_of, Callable, Member, TypedAst};

pub struct Assembler {
    instructions: Vec<Instr<Label>>,
    labels: usize,
}

impl Assembler {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: 0,
        }
    }

    #[inline]
    fn emit(&mut self, instr: Instr<Label>) {
        self.instructions.push(instr);
    }

    #[inline]
    fn emit_label(&mut self, label: Label) {
        self.instructions.push(Instr::Target(label));
    }

    #[inline]
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
            Expr::Ident(reference, span) => {
                match reference {
                    Reference::Value(Value::Local(idx)) => self.emit(Instr::Local(idx)),
                    Reference::Value(Value::Parameter(idx)) => self.emit(Instr::Param(idx)),
                    _ => return Err(Cause::UnexpectedToken("symbol").with_span(span)),
                };
            }
            Expr::Constant(cons, _) => match cons {
                Constant::String(Literal::String, lit) => {
                    let idx = pool.strings.add(lit);
                    self.emit(Instr::StringConst(idx));
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
                Constant::F32(val) => {
                    self.emit(Instr::F32Const(val));
                }
                Constant::F64(val) => {
                    self.emit(Instr::F64Const(val));
                }
                Constant::I32(val) => {
                    self.emit(Instr::I32Const(val));
                }
                Constant::I64(val) => {
                    self.emit(Instr::I64Const(val));
                }
                Constant::U32(val) => {
                    self.emit(Instr::U32Const(val));
                }
                Constant::U64(val) => {
                    self.emit(Instr::U64Const(val));
                }
                Constant::Bool(true) => {
                    self.emit(Instr::TrueConst);
                }
                Constant::Bool(false) => {
                    self.emit(Instr::FalseConst);
                }
            },
            Expr::Cast(type_, expr, span) => {
                if let TypeId::Class(class) = type_ {
                    self.emit(Instr::DynamicCast(class, 0));
                    self.assemble(*expr, scope, pool, None)?;
                } else {
                    return Err(Cause::UnsupportedOperation("casting", type_.pretty(pool)?).with_span(span));
                }
            }
            Expr::Declare(local, typ, init, span) => {
                if let Some(val) = init {
                    self.emit(Instr::Assign);
                    self.emit(Instr::Local(local));
                    self.assemble(*val, scope, pool, None)?;
                } else {
                    let typ = typ.expect("Local without type");
                    self.emit_initializer(local, *typ, scope, pool).with_span(span)?;
                }
            }
            Expr::Assign(lhs, rhs, _) => {
                self.emit(Instr::Assign);
                self.assemble(*lhs, scope, pool, None)?;
                self.assemble(*rhs, scope, pool, None)?;
            }
            Expr::ArrayElem(expr, idx, span) => {
                match type_of(&expr, scope, pool)? {
                    type_ @ TypeId::Array(_) => {
                        let type_idx = scope.get_type_index(&type_, pool).with_span(span)?;
                        self.emit(Instr::ArrayElement(type_idx));
                    }
                    type_ @ TypeId::StaticArray(_, _) => {
                        let type_idx = scope.get_type_index(&type_, pool).with_span(span)?;
                        self.emit(Instr::StaticArrayElement(type_idx));
                    }
                    other => return Err(Cause::UnsupportedOperation("indexing", other.pretty(pool)?).with_span(span)),
                }
                self.assemble(*expr, scope, pool, None)?;
                self.assemble(*idx, scope, pool, None)?;
            }
            Expr::New(type_, args, span) => match type_ {
                TypeId::Class(idx) => self.emit(Instr::New(idx)),
                TypeId::Struct(idx) => {
                    self.emit(Instr::Construct(args.len() as u8, idx));
                    for arg in args.into_vec() {
                        self.assemble(arg, scope, pool, None)?;
                    }
                }
                _ => return Err(Cause::UnsupportedOperation("constructing", type_.pretty(pool)?).with_span(span)),
            },
            Expr::Return(Some(expr), _) => {
                self.emit(Instr::Return);
                self.assemble(*expr, scope, pool, None)?;
            }
            Expr::Return(None, _) => {
                self.emit(Instr::Return);
                self.emit(Instr::Nop);
            }
            Expr::Seq(seq) => {
                self.assemble_seq(seq, scope, pool, exit)?;
            }
            Expr::Switch(expr, cases, default, span) => {
                let type_ = type_of(&expr, scope, pool)?;
                let first_case_label = self.new_label();
                let mut next_case_label = self.new_label();
                let exit_label = self.new_label();
                let type_idx = scope.get_type_index(&type_, pool).with_span(span)?;
                self.emit(Instr::Switch(type_idx, first_case_label));
                self.assemble(*expr, scope, pool, None)?;
                self.emit_label(first_case_label);

                let mut case_iter = cases.into_iter().peekable();
                while case_iter.peek().is_some() {
                    let body_label = self.new_label();

                    for case in &mut case_iter {
                        self.emit_label(next_case_label);
                        next_case_label = self.new_label();
                        self.emit(Instr::SwitchLabel(next_case_label, body_label));
                        self.assemble(case.matcher, scope, pool, None)?;

                        if !case.body.exprs.iter().all(Expr::is_empty) {
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
                self.emit(Instr::JumpIfFalse(else_label));
                self.assemble(*condition, scope, pool, None)?;
                self.assemble_seq(if_, scope, pool, exit)?;
                if let Some(else_code) = else_ {
                    let exit_label = self.new_label();
                    self.emit(Instr::Jump(exit_label));
                    self.emit_label(else_label);
                    self.assemble_seq(else_code, scope, pool, exit)?;
                    self.emit_label(exit_label);
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
            Expr::Call(callable, _, args, span) => match callable {
                Callable::Function(fun) => {
                    self.assemble_call(fun, args.into_vec(), scope, pool, false)?;
                }
                Callable::Intrinsic(op, type_) => {
                    self.assemble_intrinsic(op, args.into_vec(), &type_, scope, pool, span)?;
                }
            },
            Expr::MethodCall(expr, fun_idx, args, span) => {
                let fun = pool.function(fun_idx)?;
                match *expr {
                    Expr::Ident(Reference::Symbol(Symbol::Class(_, _) | Symbol::Struct(_, _)), span) => {
                        if fun.flags.is_static() {
                            self.assemble_call(fun_idx, args, scope, pool, true)?;
                        } else {
                            return Err(
                                Cause::InvalidNonStaticMethodCall(Ident::from_heap(pool.def_name(fun_idx)?))
                                    .with_span(span),
                            );
                        }
                    }
                    _ if fun.flags.is_static() => {
                        return Err(
                            Cause::InvalidStaticMethodCall(Ident::from_heap(pool.def_name(fun_idx)?)).with_span(span),
                        );
                    }
                    expr => {
                        let force_static_call = matches!(&expr, Expr::Super(_));
                        let exit_label = self.new_label();
                        self.emit(Instr::Context(exit_label));
                        self.assemble(expr, scope, pool, None)?;
                        self.assemble_call(fun_idx, args, scope, pool, force_static_call)?;
                        self.emit_label(exit_label);
                    }
                }
            }

            Expr::Null(_) => {
                self.emit(Instr::Null);
            }
            Expr::This(_) | Expr::Super(_) => {
                self.emit(Instr::This);
            }
            Expr::Break(_) if exit.is_some() => {
                self.emit(Instr::Jump(exit.unwrap()));
            }
            Expr::ArrayLit(_, _, span) => return Err(Cause::UnsupportedFeature("ArrayLit").with_span(span)),
            Expr::InterpolatedString(_, _, span) => {
                return Err(Cause::UnsupportedFeature("InterpolatedString").with_span(span))
            }
            Expr::ForIn(_, _, _, span) => return Err(Cause::UnsupportedFeature("For-in").with_span(span)),
            Expr::BinOp(_, _, _, span) => return Err(Cause::UnsupportedFeature("BinOp").with_span(span)),
            Expr::UnOp(_, _, span) => return Err(Cause::UnsupportedFeature("UnOp").with_span(span)),
            Expr::Break(span) => return Err(Cause::UnsupportedFeature("Break").with_span(span)),
            Expr::Goto(_, span) => return Err(Cause::UnsupportedFeature("Goto").with_span(span)),
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

    fn emit_initializer(
        &mut self,
        local: PoolIndex<Local>,
        typ: TypeId,
        scope: &mut Scope,
        pool: &mut ConstantPool,
    ) -> Result<(), Cause> {
        let mut emit_assignment = |instr: Instr<Label>| {
            self.emit(Instr::Assign);
            self.emit(Instr::Local(local));
            self.emit(instr);
        };

        match typ {
            TypeId::Prim(typ_idx) => match Ident::from_heap(pool.def_name(typ_idx)?) {
                tp if tp == TypeName::BOOL.name() => emit_assignment(Instr::FalseConst),
                tp if tp == TypeName::INT8.name() => emit_assignment(Instr::I8Const(0)),
                tp if tp == TypeName::INT16.name() => emit_assignment(Instr::I16Const(0)),
                tp if tp == TypeName::INT32.name() => emit_assignment(Instr::I32Zero),
                tp if tp == TypeName::INT64.name() => emit_assignment(Instr::I64Const(0)),
                tp if tp == TypeName::UINT8.name() => emit_assignment(Instr::U8Const(0)),
                tp if tp == TypeName::UINT16.name() => emit_assignment(Instr::U16Const(0)),
                tp if tp == TypeName::UINT32.name() => emit_assignment(Instr::U32Const(0)),
                tp if tp == TypeName::UINT64.name() => emit_assignment(Instr::U64Const(0)),
                tp if tp == TypeName::FLOAT.name() => emit_assignment(Instr::F32Const(0.0)),
                tp if tp == TypeName::DOUBLE.name() => emit_assignment(Instr::F64Const(0.0)),
                tp if tp == TypeName::STRING.name() => {
                    let empty = pool.strings.add("".into());
                    emit_assignment(Instr::StringConst(empty));
                }
                tp if tp == TypeName::CNAME.name() => emit_assignment(Instr::NameConst(PoolIndex::UNDEFINED)),
                tp if tp == TypeName::TWEAKDB_ID.name() => emit_assignment(Instr::TweakDbIdConst(PoolIndex::UNDEFINED)),
                tp if tp == TypeName::RESOURCE.name() => emit_assignment(Instr::ResourceConst(PoolIndex::UNDEFINED)),
                _ => {}
            },
            TypeId::Struct(struct_) => {
                emit_assignment(Instr::Construct(0, struct_));
            }
            TypeId::Enum(enum_idx) => {
                let enum_ = pool.enum_(enum_idx)?;
                if let Some(member) = enum_.members.first() {
                    emit_assignment(Instr::EnumConst(enum_idx, *member));
                }
            }
            TypeId::Ref(_) => {
                emit_assignment(Instr::Null);
            }
            TypeId::WeakRef(_) => {
                emit_assignment(Instr::WeakRefNull);
            }
            TypeId::Array(_) => {
                self.emit(Instr::ArrayClear(scope.get_type_index(&typ, pool)?));
                self.emit(Instr::Local(local));
            }
            _ => {}
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
        let fun_flags = fun.flags;
        let param_flags: Vec<_> = fun
            .parameters
            .iter()
            .map(|idx| pool.parameter(*idx).map(|param| param.flags))
            .try_collect()?;
        let args_len = args.len();
        let exit_label = self.new_label();
        let mut invoke_flags = 0u16;
        for (n, arg) in args.iter().enumerate() {
            let is_rvalue_ref = Self::is_rvalue_ref(arg, scope, pool).unwrap_or(false);
            if is_rvalue_ref {
                invoke_flags |= 1 << n;
            }
        }

        if !force_static && !fun_flags.is_final() && !fun_flags.is_static() && !fun_flags.is_native() {
            let name_idx = pool.definition(function_idx)?.name;
            self.emit(Instr::InvokeVirtual(exit_label, 0, name_idx, invoke_flags));
        } else {
            self.emit(Instr::InvokeStatic(exit_label, 0, function_idx, invoke_flags));
        }
        for (arg, flags) in args.into_iter().zip(&param_flags) {
            if flags.is_short_circuit() {
                let skip_label = self.new_label();
                self.emit(Instr::Skip(skip_label));
                self.assemble(arg, scope, pool, None)?;
                self.emit_label(skip_label);
            } else {
                self.assemble(arg, scope, pool, None)?;
            }
        }
        if param_flags.len() < args_len {
            return Err(Error::CompileError(
                Cause::UnsupportedFeature("cannot emit function call (probably invalid signature)"),
                Span::ZERO,
            ));
        }
        for _ in 0..param_flags.len() - args_len {
            self.emit(Instr::Nop);
        }
        self.emit(Instr::ParamEnd);
        self.emit_label(exit_label);
        Ok(())
    }

    fn is_rvalue_ref(expr: &Expr<TypedAst>, scope: &mut Scope, pool: &mut ConstantPool) -> Option<bool> {
        let typ = type_of(expr, scope, pool).ok()?;
        match typ {
            TypeId::ScriptRef(_) => match expr {
                Expr::Call(Callable::Intrinsic(IntrinsicOp::AsRef, _), _, args, _) => match args.get(0) {
                    Some(expr) => Some(Self::is_rvalue(expr)),
                    _ => Some(true),
                },
                _ => Some(true),
            },
            _ => None,
        }
    }

    fn is_rvalue(expr: &Expr<TypedAst>) -> bool {
        match expr {
            Expr::Constant(_, _) | Expr::Ident(_, _) | Expr::This(_) | Expr::Super(_) => false,
            Expr::Member(inner, _, _) | Expr::ArrayElem(inner, _, _) => Self::is_rvalue(inner),
            _ => true,
        }
    }

    fn assemble_intrinsic(
        &mut self,
        intrinsic: IntrinsicOp,
        args: Vec<Expr<TypedAst>>,
        return_type: &TypeId,
        scope: &mut Scope,
        pool: &mut ConstantPool,
        span: Span,
    ) -> Result<(), Error> {
        let mut get_arg_type =
            |i| type_of(&args[i], scope, pool).and_then(|typ| scope.get_type_index(&typ, pool).with_span(span));

        match intrinsic {
            IntrinsicOp::Equals => {
                // TODO: eventually enforce type compatibility (https://github.com/jac3km4/redscript/issues/69)
                self.emit(Instr::Equals(get_arg_type(0)?));
            }
            IntrinsicOp::NotEquals => {
                // TODO: eventually enforce type compatibility (https://github.com/jac3km4/redscript/issues/69)
                self.emit(Instr::NotEquals(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayClear => {
                self.emit(Instr::ArrayClear(get_arg_type(0)?));
            }
            IntrinsicOp::ArraySize => {
                self.emit(Instr::ArraySize(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayResize => {
                self.emit(Instr::ArrayResize(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayFindFirst => {
                self.emit(Instr::ArrayFindFirst(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayFindLast => {
                self.emit(Instr::ArrayFindLast(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayContains => {
                self.emit(Instr::ArrayContains(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayCount => {
                self.emit(Instr::ArrayCount(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayPush => {
                self.emit(Instr::ArrayPush(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayPop => {
                self.emit(Instr::ArrayPop(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayInsert => {
                self.emit(Instr::ArrayInsert(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayRemove => {
                self.emit(Instr::ArrayRemove(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayGrow => {
                self.emit(Instr::ArrayGrow(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayErase => {
                self.emit(Instr::ArrayErase(get_arg_type(0)?));
            }
            IntrinsicOp::ArrayLast => {
                self.emit(Instr::ArrayLast(get_arg_type(0)?));
            }
            IntrinsicOp::ToString => match type_of(&args[0], scope, pool)? {
                TypeId::Variant => self.emit(Instr::VariantToString),
                any => {
                    let type_idx = scope.get_type_index(&any, pool).with_span(span)?;
                    self.emit(Instr::ToString(type_idx));
                }
            },
            IntrinsicOp::EnumInt => {
                self.emit(Instr::EnumToI32(get_arg_type(0)?, 4));
            }
            IntrinsicOp::IntEnum => {
                let type_idx = scope.get_type_index(return_type, pool).with_span(span)?;
                self.emit(Instr::I32ToEnum(type_idx, 4));
            }
            IntrinsicOp::ToVariant => {
                self.emit(Instr::ToVariant(get_arg_type(0)?));
            }
            IntrinsicOp::FromVariant => {
                let type_idx = scope.get_type_index(return_type, pool).with_span(span)?;
                self.emit(Instr::FromVariant(type_idx));
            }
            IntrinsicOp::VariantIsRef => {
                self.emit(Instr::VariantIsRef);
            }
            IntrinsicOp::VariantIsArray => {
                self.emit(Instr::VariantIsArray);
            }
            IntrinsicOp::VariantTypeName => {
                self.emit(Instr::VariantTypeName);
            }
            IntrinsicOp::AsRef => {
                self.emit(Instr::AsRef(get_arg_type(0)?));
            }
            IntrinsicOp::Deref => {
                self.emit(Instr::Deref(get_arg_type(0)?));
            }
            IntrinsicOp::RefToWeakRef => {
                self.emit(Instr::RefToWeakRef);
            }
            IntrinsicOp::WeakRefToRef => {
                self.emit(Instr::WeakRefToRef);
            }
            IntrinsicOp::IsDefined => match type_of(&args[0], scope, pool)? {
                TypeId::Ref(_) | TypeId::Null => self.emit(Instr::RefToBool),
                TypeId::WeakRef(_) => self.emit(Instr::WeakRefToBool),
                TypeId::Variant => self.emit(Instr::VariantIsDefined),
                _ => panic!("Invalid IsDefined parameter"),
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
        for (loc, instr) in code.iter() {
            if let Instr::Target(label) = instr {
                locations[label.index] = loc;
            }
        }

        let mut resolved = Vec::with_capacity(code.0.len());
        for (loc, instr) in code.iter().filter(|(_, instr)| !matches!(instr, Instr::Target(_))) {
            resolved.push(instr.resolve_labels(loc, &locations));
        }
        Code(resolved)
    }

    pub fn from_body(seq: Seq<TypedAst>, scope: &mut Scope, pool: &mut ConstantPool) -> Result<Code<Offset>, Error> {
        let mut assembler = Self::new();
        assembler.assemble_seq(seq, scope, pool, None)?;
        assembler.emit(Instr::Nop);
        Ok(assembler.into_code())
    }
}
