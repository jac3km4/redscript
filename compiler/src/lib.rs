use std::iter;
use std::ops::Deref;
use std::rc::Rc;

use im::hashmap::Entry;
use redscript::ast::{BinOp, Expr, Ident, Seq, TypeName, UnOp};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr, Offset};
use redscript::definition::{
    Class, ClassFlags, Enum, FieldFlags, FunctionFlags, Local, LocalFlags, Parameter, ParameterFlags, SourceReference,
    Visibility,
};
use redscript::definition::{Definition, DefinitionValue, Field, Function, Type};
use redscript::error::Error;

use crate::parser::{ClassSource, Declaration, FunctionSource, MemberSource, Qualifier, SourceEntry};

pub mod parser;

pub struct Compiler<'a> {
    pool: &'a mut ConstantPool,
    backlog: Vec<(PoolIndex<Class>, PoolIndex<Function>, Seq)>,
    scope: Scope,
}

impl<'a> Compiler<'a> {
    pub fn new(pool: &'a mut ConstantPool) -> Compiler<'a> {
        let scope = Scope::new(pool);
        let backlog = Vec::new();
        Compiler { pool, scope, backlog }
    }

    pub fn compile(&mut self, sources: Vec<SourceEntry>) -> Result<(), Error> {
        for entry in &sources {
            if let SourceEntry::Class(class) = entry {
                self.stub_type(class)?;
            }
        }
        for entry in sources {
            match entry {
                SourceEntry::Class(class) => {
                    self.define_class(class)?;
                }
                SourceEntry::Function(fun) => {
                    self.define_function(fun, PoolIndex::UNDEFINED)?;
                }
            }
        }
        for (this, fun_idx, seq) in self.backlog.drain(..) {
            Self::compile_function(fun_idx, &seq, self.pool, self.scope.with_context(this, fun_idx))?;
        }
        Ok(())
    }

    fn stub_type(&mut self, class: &ClassSource) -> Result<(), Error> {
        let name = self.pool.names.add(class.name.clone());
        let idx = self.pool.push_definition(Definition::type_(name, Type::Class));
        self.scope.types.insert(Ident(self.pool.names.get(name)?), idx.cast());
        Ok(())
    }

    fn define_type(&mut self, type_name: &TypeName) -> Result<PoolIndex<Type>, Error> {
        let type_ = match type_name.name.as_str() {
            "ref" => Type::Ref(self.define_type(&type_name.arguments[0])?),
            "wref" => Type::WeakRef(self.define_type(&type_name.arguments[0])?),
            "array" => Type::Array(self.define_type(&type_name.arguments[0])?),
            _ => Type::Class,
        };

        match self.scope.types.entry(Ident::new(type_name.repr())) {
            Entry::Occupied(entry) => Ok(entry.get().clone()),
            Entry::Vacant(slot) => {
                let name = self.pool.names.add(type_name.repr());
                let idx = self.pool.push_definition(Definition::type_(name, type_));
                Ok(slot.insert(idx.cast()).clone())
            }
        }
    }

    fn define_class(&mut self, source: ClassSource) -> Result<(), Error> {
        let name = self.pool.names.add(source.name);
        let idx = self.pool.reserve().cast();

        let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);
        let flags = ClassFlags::new();
        let mut functions = vec![];
        let mut fields = vec![];

        for member in source.members {
            match member {
                MemberSource::Function(fun) => {
                    functions.push(self.define_function(fun, idx)?);
                }
                MemberSource::Field(field) => {
                    fields.push(self.define_field(field, idx)?);
                }
            }
        }

        let class = Class {
            visibility,
            flags,
            base: PoolIndex::UNDEFINED,
            functions,
            fields,
            overrides: vec![],
        };
        self.pool.put_definition(idx.cast(), Definition::class(name, class));
        self.scope
            .names
            .insert(Ident(self.pool.names.get(name)?), Reference::Class(idx));

        Ok(())
    }

    fn define_function(
        &mut self,
        source: FunctionSource,
        parent: PoolIndex<Class>,
    ) -> Result<PoolIndex<Function>, Error> {
        let idx = self.pool.reserve().cast();
        let name = self.pool.names.add(FunctionId::from_source(&source)?.0);
        let decl = source.declaration;
        let visibility = decl.qualifiers.visibility().unwrap_or(Visibility::Private);
        let return_type = if decl.type_.name == "void" {
            None
        } else {
            Some(self.define_type(&decl.type_)?)
        };
        let flags = FunctionFlags::new()
            .with_is_static(decl.qualifiers.contain(Qualifier::Static))
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_final(decl.qualifiers.contain(Qualifier::Final));
        let mut parameters = Vec::new();

        for decl in &source.parameters {
            let type_ = self.define_type(&decl.type_)?;
            let flags = ParameterFlags::new();
            let param = Parameter { type_, flags };
            let name = self.pool.names.add(decl.name.clone());
            let idx = self.pool.push_definition(Definition::param(name, idx.cast(), param));
            parameters.push(idx.cast());
        }

        let source_ref = SourceReference {
            file: PoolIndex::UNDEFINED,
            line: 0,
        };

        let function = Function {
            visibility,
            flags,
            source: Some(source_ref),
            return_type,
            unk1: false,
            base_method: None,
            parameters,
            locals: vec![],
            operator: None,
            cast: 0,
            code: Code::EMPTY,
        };
        let definition = Definition::function(name, parent.cast(), function);
        self.pool.put_definition(idx.cast(), definition);
        if let Some(seq) = source.body {
            self.backlog.push((parent, idx, seq))
        }
        Ok(idx)
    }

    fn define_field(&mut self, field: Declaration, parent: PoolIndex<Class>) -> Result<PoolIndex<Field>, Error> {
        let name = self.pool.names.add(field.name);
        let visibility = field.qualifiers.visibility().unwrap_or(Visibility::Private);
        let type_ = self.define_type(&field.type_)?;
        let flags = FieldFlags::new();
        let field = Field {
            visibility,
            type_,
            flags,
            hint: None,
            attributes: vec![],
            defaults: vec![],
        };
        let definition = Definition::field(name, parent.cast(), field);
        let idx = self.pool.push_definition(definition).cast();
        Ok(idx)
    }

    fn compile_function(
        fun_idx: PoolIndex<Function>,
        seq: &Seq,
        pool: &mut ConstantPool,
        mut scope: Scope,
    ) -> Result<(), Error> {
        for param in &pool.function(fun_idx)?.parameters {
            let ident = Ident(pool.definition_name(*param)?);
            scope.names.insert(ident, Reference::Parameter(*param));
        }

        let assembler = Assembler::from_seq(&seq, pool, &mut scope)?;
        let function = pool.function_mut(fun_idx)?;
        function.code = Code(assembler.code.into_iter().collect());
        function.locals = assembler.locals.into_iter().collect();
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Reference {
    Local(PoolIndex<Local>),
    Parameter(PoolIndex<Parameter>),
    Field(PoolIndex<Field>),
    Class(PoolIndex<Class>),
    Enum(PoolIndex<Enum>),
    Function(PoolIndex<Function>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeId {
    Prim(PoolIndex<Type>),
    Class(PoolIndex<Type>, PoolIndex<Class>),
    Struct(PoolIndex<Type>, PoolIndex<Class>),
    Ref(PoolIndex<Type>, Box<TypeId>),
    WeakRef(PoolIndex<Type>, Box<TypeId>),
    Array(PoolIndex<Type>, Box<TypeId>),
    StaticArray(PoolIndex<Type>, Box<TypeId>, u32),
    ScriptRef(PoolIndex<Type>, Box<TypeId>),
    Null,
    Void,
}

impl TypeId {
    fn mangled(&self, pool: &ConstantPool) -> Result<Rc<String>, Error> {
        let res = match self {
            TypeId::Prim(idx) => pool.definition_name(*idx)?,
            TypeId::Class(idx, _) => pool.definition_name(*idx)?,
            TypeId::Struct(idx, _) => pool.definition_name(*idx)?,
            TypeId::Ref(_, inner) => inner.mangled(pool)?,
            TypeId::WeakRef(_, inner) => inner.mangled(pool)?,
            TypeId::Array(_, inner) => Rc::new(format!("array<{}>", inner.mangled(pool)?)),
            TypeId::StaticArray(_, inner, _) => Rc::new(format!("array<{}>", inner.mangled(pool)?)),
            TypeId::ScriptRef(_, inner) => inner.mangled(pool)?,
            TypeId::Null => panic!(),
            TypeId::Void => panic!(),
        };
        Ok(res)
    }

    fn index(&self) -> Option<PoolIndex<Type>> {
        match self {
            TypeId::Prim(idx) => Some(*idx),
            TypeId::Class(idx, _) => Some(*idx),
            TypeId::Struct(idx, _) => Some(*idx),
            TypeId::Ref(idx, _) => Some(*idx),
            TypeId::WeakRef(idx, _) => Some(*idx),
            TypeId::Array(idx, _) => Some(*idx),
            TypeId::StaticArray(idx, _, _) => Some(*idx),
            TypeId::ScriptRef(idx, _) => Some(*idx),
            TypeId::Null => None,
            TypeId::Void => None,
        }
    }
}

pub struct Assembler {
    code: im::Vector<Instr>,
    locals: im::Vector<PoolIndex<Local>>,
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
                self.emit(Instr::I64Const(*val));
            }
            Expr::UintLit(val) => {
                self.emit(Instr::U64Const(*val));
            }
            Expr::Declare(type_, name, _) => {
                let name_idx = pool.names.add(name.0.deref().to_owned());
                let type_ = scope.resolve_type_name(&type_.repr())?;
                let local = Local::new(type_, LocalFlags::new());
                let idx = pool.push_definition(Definition::local(name_idx, scope.function.unwrap().cast(), local));
                self.locals.push_back(idx.cast());
                scope.push_reference(name.clone(), Reference::Local(idx.cast()));
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
                let fun_id = FunctionId::by_name_and_args(ident.clone(), args, pool, scope)?;
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
        function: PoolIndex<Function>,
        params: I,
        pool: &mut ConstantPool,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let mut args_code = Assembler::new();
        for arg in params {
            args_code.compile(arg, pool, scope)?;
        }
        args_code.emit(Instr::ParamEnd);
        self.emit(Instr::InvokeStatic(args_code.offset(), 0, function));
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
                let fun_id = FunctionId::by_name_and_args(ident.clone(), args, pool, scope)?;
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
        match scope.infer_type(expr, pool)? {
            TypeId::Class(_, class) => {
                let code = match member.deref() {
                    Expr::Ident(ident) => {
                        let field = scope.resolve_field(ident.clone(), class, pool)?;
                        let mut code = Assembler::new();
                        code.emit(Instr::ObjectField(field));
                        code
                    }
                    Expr::Call(ident, args) => {
                        let fun_id = FunctionId::by_name_and_args(ident.clone(), args, pool, scope)?;
                        let fun = scope.resolve_method(fun_id, class, pool)?;
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
                    let field = scope.resolve_field(ident.clone(), class, pool)?;
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

    fn from_seq(seq: &Seq, pool: &mut ConstantPool, scope: &mut Scope) -> Result<Assembler, Error> {
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

#[derive(Debug, Clone)]
pub struct Scope {
    names: im::HashMap<Ident, Reference>,
    types: im::HashMap<Ident, PoolIndex<Type>>,
    this: Option<PoolIndex<Class>>,
    function: Option<PoolIndex<Function>>,
}

impl Scope {
    fn new(pool: &ConstantPool) -> Self {
        let names = pool
            .roots()
            .filter_map(|(idx, def)| {
                let id = Ident(pool.definition_name(idx).unwrap());
                match def.value {
                    DefinitionValue::Class(_) => Some((id, Reference::Class(idx.cast()))),
                    DefinitionValue::Enum(_) => Some((id, Reference::Enum(idx.cast()))),
                    DefinitionValue::Function(_) => Some((id, Reference::Function(idx.cast()))),
                    _ => None,
                }
            })
            .collect();

        let types = pool
            .roots()
            .filter_map(|(idx, def)| match def.value {
                DefinitionValue::Type(_) => {
                    let id = pool.definition_name(idx).unwrap();
                    Some((Ident(id), idx.cast()))
                }
                _ => None,
            })
            .collect();

        Scope {
            names,
            types,
            this: None,
            function: None,
        }
    }

    fn with_context(&self, this: PoolIndex<Class>, function: PoolIndex<Function>) -> Self {
        Scope {
            this: Some(this),
            function: Some(function),
            ..self.clone()
        }
    }

    fn push_reference(&mut self, name: Ident, reference: Reference) {
        self.names.insert(name, reference);
    }

    fn resolve_method(
        &self,
        fun_id: FunctionId,
        cls_idx: PoolIndex<Class>,
        pool: &ConstantPool,
    ) -> Result<PoolIndex<Function>, Error> {
        let cls = pool.class(cls_idx)?;
        for fun in &cls.functions {
            if pool.definition_name(*fun)?.as_str() == fun_id.0 {
                return Ok(*fun);
            }
        }
        let err = format!("Method {} not found on {}", fun_id.0, pool.definition_name(cls_idx)?);
        Err(Error::CompileError(err))
    }

    fn resolve_field(
        &self,
        ident: Ident,
        class_idx: PoolIndex<Class>,
        pool: &ConstantPool,
    ) -> Result<PoolIndex<Field>, Error> {
        let class = pool.class(class_idx)?;
        for field in &class.fields {
            if pool.definition_name(*field)? == ident.0 {
                return Ok(*field);
            }
        }
        let err = format!("Field {} not found on {}", ident.0, pool.definition_name(class_idx)?);
        Err(Error::CompileError(err))
    }

    fn resolve_enum_member(
        &self,
        ident: Ident,
        enum_idx: PoolIndex<Enum>,
        pool: &ConstantPool,
    ) -> Result<PoolIndex<i64>, Error> {
        let enum_ = pool.enum_(enum_idx)?;
        for field in &enum_.members {
            if pool.definition_name(*field)? == ident.0 {
                return Ok(*field);
            }
        }
        let err = format!("Member {} not found on {}", ident.0, pool.definition_name(enum_idx)?);
        Err(Error::CompileError(err))
    }

    fn resolve_function(&self, function_id: FunctionId) -> Result<PoolIndex<Function>, Error> {
        let ident = Ident::new(function_id.0);
        let reference = self
            .names
            .get(&ident)
            .ok_or(Error::CompileError(format!("Function {} not found", ident.0)))?;
        if let Reference::Function(idx) = reference {
            Ok(*idx)
        } else {
            Err(Error::CompileError(format!("{} is not a function", ident.0)))
        }
    }

    fn resolve(&self, name: Ident) -> Result<Reference, Error> {
        self.names
            .get(&name)
            .cloned()
            .ok_or(Error::CompileError(format!("Unresolved name {}", name.0)))
    }

    fn resolve_type_name(&self, name: &str) -> Result<PoolIndex<Type>, Error> {
        self.types
            .get(&Ident::new(name.to_owned()))
            .cloned()
            .ok_or(Error::CompileError(format!("Unresolved type {}", name)))
    }

    fn resolve_type(&self, index: PoolIndex<Type>, pool: &ConstantPool) -> Result<TypeId, Error> {
        let result = match pool.type_(index)? {
            Type::Prim => TypeId::Prim(index),
            Type::Class => {
                let ident = Ident(pool.definition_name(index)?);
                if let Some(Reference::Class(cls)) = self.names.get(&ident) {
                    TypeId::Class(index, *cls)
                } else {
                    Err(Error::CompileError(format!("Class {} not found", ident.0)))?
                }
            }
            Type::Ref(type_) => {
                let inner = self.resolve_type(*type_, pool)?;
                TypeId::Ref(index, Box::new(inner))
            }
            Type::WeakRef(type_) => {
                let inner = self.resolve_type(*type_, pool)?;
                TypeId::WeakRef(index, Box::new(inner))
            }
            Type::Array(type_) => {
                let inner = self.resolve_type(*type_, pool)?;
                TypeId::Array(index, Box::new(inner))
            }
            Type::StaticArray(type_, size) => {
                let inner = self.resolve_type(*type_, pool)?;
                TypeId::StaticArray(index, Box::new(inner), *size)
            }
            Type::ScriptRef(type_) => {
                let inner = self.resolve_type(*type_, pool)?;
                TypeId::ScriptRef(index, Box::new(inner))
            }
        };
        Ok(result)
    }

    fn infer_type(&self, expr: &Expr, pool: &ConstantPool) -> Result<TypeId, Error> {
        let res = match expr {
            Expr::Ident(name) => match self.resolve(name.clone())? {
                Reference::Local(idx) => self.resolve_type(pool.local(idx)?.type_, pool)?,
                Reference::Parameter(idx) => self.resolve_type(pool.parameter(idx)?.type_, pool)?,
                Reference::Field(idx) => self.resolve_type(pool.field(idx)?.type_, pool)?,
                Reference::Class(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(self.resolve_type_name(&name)?, pool)?
                }
                Reference::Enum(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(self.resolve_type_name(&name)?, pool)?
                }
                Reference::Function(_) => Err(Error::CompileError("Functions can't be used as values".to_owned()))?,
            },
            Expr::Call(name, _) => {
                if let Reference::Function(idx) = self.resolve(name.clone())? {
                    match pool.function(idx)?.return_type {
                        Some(type_) => self.resolve_type(type_, pool)?,
                        None => TypeId::Void,
                    }
                } else {
                    Err(Error::CompileError(format!("{} can't be invoked", name.0)))?
                }
            }
            Expr::ArrayElem(expr, _) => match self.infer_type(expr, pool)? {
                TypeId::Array(_, inner) => *inner,
                TypeId::StaticArray(_, inner, _) => *inner,
                _ => Err(Error::CompileError("Can't be accessed as an array".to_owned()))?,
            },
            Expr::New(name, _) => {
                if let Reference::Class(cls) = self.resolve(name.clone())? {
                    let name = pool.definition_name(cls)?;
                    self.resolve_type(self.resolve_type_name(&name)?, pool)?
                } else {
                    Err(Error::CompileError(format!("{} can't be constructed", name.0)))?
                }
            }
            Expr::Member(expr, member) => {
                let class = match self.infer_type(expr, pool)? {
                    TypeId::Class(_, class) => class,
                    TypeId::Struct(_, class) => class,
                    _ => Err(Error::CompileError(format!("Can't access a member of {:?}", expr)))?,
                };
                match member.deref() {
                    Expr::Ident(ident) => {
                        let field = self.resolve_field(ident.clone(), class, pool)?;
                        self.resolve_type(pool.field(field)?.type_, pool)?
                    }
                    Expr::Call(ident, args) => {
                        let function_id = FunctionId::by_name_and_args(ident.clone(), args, pool, self)?;
                        let method = self.resolve_method(function_id, class, pool)?;
                        match pool.function(method)?.return_type {
                            None => TypeId::Void,
                            Some(return_type) => self.resolve_type(return_type, pool)?,
                        }
                    }
                    _ => {
                        let error = format!("Invalid member operation: {:?}", member);
                        Err(Error::CompileError(error))?
                    }
                }
            }
            Expr::Conditional(_, lhs, rhs) => {
                let lt = self.infer_type(lhs, pool)?;
                let rt = self.infer_type(rhs, pool)?;
                if lt != rt {
                    let error = format!("Incompatible types: {:?} and {:?}", lt, rt);
                    Err(Error::CompileError(error))?
                }
                lt
            }
            Expr::BinOp(lhs, _, _) => self.infer_type(lhs, pool)?,
            Expr::UnOp(expr, _) => self.infer_type(expr, pool)?,
            Expr::StringLit(_) => self.resolve_type(self.resolve_type_name("String")?, pool)?,
            Expr::FloatLit(_) => self.resolve_type(self.resolve_type_name("Float")?, pool)?,
            Expr::IntLit(_) => self.resolve_type(self.resolve_type_name("Int64")?, pool)?,
            Expr::UintLit(_) => self.resolve_type(self.resolve_type_name("Uint64")?, pool)?,
            Expr::True => self.resolve_type(self.resolve_type_name("Bool")?, pool)?,
            Expr::False => self.resolve_type(self.resolve_type_name("Bool")?, pool)?,
            Expr::Null => TypeId::Null,
            Expr::This => match self.this {
                Some(cls) => {
                    let name = pool.definition_name(cls)?;
                    self.resolve_type(self.resolve_type_name(&name)?, pool)?
                }
                None => Err(Error::CompileError("No 'this' available".to_owned()))?,
            },
            Expr::While(_, _) => TypeId::Void,
            Expr::Goto(_) => TypeId::Void,
            Expr::If(_, _, _) => TypeId::Void,
            Expr::Break => TypeId::Void,
            Expr::Return(_) => TypeId::Void,
            Expr::Seq(_) => TypeId::Void,
            Expr::Switch(_, _, _) => TypeId::Void,
            Expr::Declare(_, _, _) => TypeId::Void,
            Expr::Assign(_, _) => TypeId::Void,
        };
        Ok(res)
    }
}

struct FunctionId(String);

impl FunctionId {
    fn from_source(source: &FunctionSource) -> Result<Self, Error> {
        let mut signature = format!("{};", source.declaration.name);
        for arg in &source.parameters {
            signature.push_str(arg.type_.mangled().as_str());
        }
        Ok(FunctionId(signature))
    }

    fn by_name_and_args(name: Ident, args: &[Expr], pool: &ConstantPool, scope: &Scope) -> Result<Self, Error> {
        let mut signature = format!("{};", name.0);
        for arg in args {
            let type_ = scope.infer_type(arg, pool)?;
            signature.push_str(type_.mangled(pool)?.as_str());
        }
        Ok(FunctionId(signature))
    }

    fn by_binop(lhs: &Expr, rhs: &Expr, op: BinOp, pool: &ConstantPool, scope: &Scope) -> Result<Self, Error> {
        let lhs_type = scope.infer_type(lhs, pool)?.mangled(pool)?;
        let rhs_type = scope.infer_type(rhs, pool)?.mangled(pool)?;
        let type_sig = match op {
            BinOp::LogicOr
            | BinOp::LogicAnd
            | BinOp::Eq
            | BinOp::Neq
            | BinOp::Less
            | BinOp::LessOrEqual
            | BinOp::Greater
            | BinOp::GreaterOrEqual => format!("{}{};Bool", lhs_type, rhs_type),
            BinOp::AssignAdd | BinOp::AssignDiv | BinOp::AssignMul | BinOp::AssignSub => {
                format!("Out{}{};{}", lhs_type, rhs_type, lhs_type)
            }
            _ => format!("{}{};{}", lhs_type, rhs_type, lhs_type),
        };
        let signature = format!("{};{}", binop_name(op), type_sig);
        Ok(FunctionId(signature))
    }

    fn by_unop(expr: &Expr, op: UnOp, pool: &ConstantPool, scope: &Scope) -> Result<Self, Error> {
        let type_ = scope.infer_type(expr, pool)?.mangled(pool)?;
        let type_sig = match op {
            UnOp::LogicNot => format!("{};Bool", type_),
            UnOp::BitNot | UnOp::Neg => format!("{};{}", type_, type_),
        };
        let signature = format!("{};{}", unop_name(op), type_sig);
        Ok(FunctionId(signature))
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

fn binop_name(op: BinOp) -> &'static str {
    match op {
        BinOp::AssignAdd => "OperatorAssignAdd",
        BinOp::AssignSub => "OperatorAssignSubtract",
        BinOp::AssignMul => "OperatorAssignMultiply",
        BinOp::AssignDiv => "OperatorAssignMultiply",
        BinOp::LogicOr => "OperatorLogicOr",
        BinOp::LogicAnd => "OperatorLogicAnd",
        BinOp::Or => "OperatorOr",
        BinOp::Xor => "OperatorXor",
        BinOp::And => "OperatorAnd",
        BinOp::Eq => "OperatorEqual",
        BinOp::Neq => "OperatorNotEqual",
        BinOp::Less => "OperatorLess",
        BinOp::LessOrEqual => "OperatorLessEqual",
        BinOp::Greater => "OperatorGreater",
        BinOp::GreaterOrEqual => "OperatorGreaterEqual",
        BinOp::Add => "OperatorAdd",
        BinOp::Sub => "OperatorSubtract",
        BinOp::Mul => "OperatorMultiply",
        BinOp::Div => "OperatorDivide",
        BinOp::Mod => "OperatorModulo",
    }
}

fn unop_name(op: UnOp) -> &'static str {
    match op {
        UnOp::BitNot => "OperatorBitNot",
        UnOp::LogicNot => "OperatorLogicNot",
        UnOp::Neg => "OperatorNeg",
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use redscript::bundle::ScriptBundle;
    use redscript::error::Error;

    use crate::{parser, Compiler};

    const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

    #[test]
    fn compile_simple_class() -> Result<(), Error> {
        let sources = parser::parse(
            "public class A {
                private const Int32 m_field;

                public void DoStuff(Bool fieldOrNot) {
                    return fieldOrNot ? this.m_field : A.Ten();
                }

                public static Int32 Ten() {
                  return 10;
                }
             }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool);
        compiler.compile(sources)
    }
}
