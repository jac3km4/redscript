use redscript::ast::{BinOp, Expr, Ident, LiteralType, UnOp};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Class, Enum};
use redscript::definition::{DefinitionValue, Field, Function, Type};
use redscript::error::Error;

use crate::{parser::FunctionSource, Reference, TypeId};

#[derive(Debug, Clone)]
pub struct Scope {
    pub names: im::HashMap<Ident, Reference>,
    pub types: im::HashMap<Ident, PoolIndex<Type>>,
    pub this: Option<PoolIndex<Class>>,
    pub function: Option<PoolIndex<Function>>,
}

impl Scope {
    pub fn new(pool: &ConstantPool) -> Self {
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

    pub fn with_context(&self, this: PoolIndex<Class>, function: PoolIndex<Function>) -> Self {
        Scope {
            this: Some(this),
            function: Some(function),
            ..self.clone()
        }
    }

    pub fn push_reference(&mut self, name: Ident, reference: Reference) {
        self.names.insert(name, reference);
    }

    pub fn resolve_method(
        &self,
        fun_id: FunctionId,
        class_idx: PoolIndex<Class>,
        pool: &ConstantPool,
    ) -> Result<PoolIndex<Function>, Error> {
        let class = pool.class(class_idx)?;
        let mangled = fun_id.mangled();
        for fun in &class.functions {
            let fun_name = pool.definition_name(*fun)?;
            if fun_name.as_str() == fun_id.0 || fun_name.as_str() == mangled {
                return Ok(*fun);
            }
        }
        let err = format!("Method {} not found on {}", fun_id.0, pool.definition_name(class_idx)?);
        if class.base != PoolIndex::UNDEFINED {
            self.resolve_method(fun_id, class.base, pool)
                .map_err(|_| Error::CompileError(err))
        } else {
            Err(Error::CompileError(err))
        }
    }

    pub fn resolve_field(
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
        if class.base != PoolIndex::UNDEFINED {
            self.resolve_field(ident, class.base, pool)
                .map_err(|_| Error::CompileError(err))
        } else {
            Err(Error::CompileError(err))
        }
    }

    pub fn resolve_enum_member(
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

    pub fn resolve_function(&self, function_id: FunctionId) -> Result<PoolIndex<Function>, Error> {
        let reference = self
            .names
            .get(&Ident::new(function_id.mangled()))
            .or(self.names.get(&Ident::new(function_id.0.to_owned())))
            .ok_or(Error::CompileError(format!("Function {} not found", function_id.0)))?;
        if let Reference::Function(idx) = reference {
            Ok(*idx)
        } else {
            Err(Error::CompileError(format!("{} is not a function", function_id.0)))
        }
    }

    pub fn resolve(&self, name: Ident) -> Result<Reference, Error> {
        self.names
            .get(&name)
            .cloned()
            .ok_or(Error::CompileError(format!("Unresolved name {}", name.0)))
    }

    pub fn resolve_type_name(&self, name: Ident) -> Result<PoolIndex<Type>, Error> {
        self.types
            .get(&name)
            .cloned()
            .ok_or(Error::CompileError(format!("Unresolved type {}", name.0)))
    }

    fn resolve_type(&self, index: PoolIndex<Type>, pool: &ConstantPool) -> Result<TypeId, Error> {
        let result = match pool.type_(index)? {
            Type::Prim => TypeId::Prim(index),
            Type::Class => {
                let ident = Ident(pool.definition_name(index)?);
                if let Some(Reference::Class(class_idx)) = self.names.get(&ident) {
                    if pool.class(*class_idx)?.flags.is_struct() {
                        TypeId::Struct(index, *class_idx)
                    } else {
                        TypeId::Class(index, *class_idx)
                    }
                } else if let Some(Reference::Enum(enum_idx)) = self.names.get(&ident) {
                    TypeId::Enum(index, *enum_idx)
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

    pub fn infer_type(&self, expr: &Expr, pool: &ConstantPool) -> Result<TypeId, Error> {
        let res = match expr {
            Expr::Ident(name) => match self.resolve(name.clone())? {
                Reference::Local(idx) => self.resolve_type(pool.local(idx)?.type_, pool)?,
                Reference::Parameter(idx) => self.resolve_type(pool.parameter(idx)?.type_, pool)?,
                Reference::Field(idx) => self.resolve_type(pool.field(idx)?.type_, pool)?,
                Reference::Class(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(self.resolve_type_name(Ident(name))?, pool)?
                }
                Reference::Enum(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(self.resolve_type_name(Ident(name))?, pool)?
                }
                Reference::Function(_) => Err(Error::CompileError("Functions can't be used as values".to_owned()))?,
            },
            Expr::Cast(type_, _) => self.resolve_type(self.resolve_type_name(Ident::new(type_.repr()))?, pool)?,
            Expr::Call(name, args) => {
                let idx = self.resolve_function(FunctionId::by_name_and_args(name, args, pool, self)?)?;
                match pool.function(idx)?.return_type {
                    Some(type_) => self.resolve_type(type_, pool)?,
                    None => TypeId::Void,
                }
            }
            Expr::MethodCall(expr, ident, args) => {
                let class = match self.infer_type(expr, pool)?.unwrapped() {
                    TypeId::Class(_, class) => *class,
                    TypeId::Struct(_, class) => *class,
                    _ => Err(Error::CompileError(format!("{:?} doesn't have methods", expr)))?,
                };
                let function_id = FunctionId::by_name_and_args(ident, args, pool, self)?;
                let method = self.resolve_method(function_id, class, pool)?;
                match pool.function(method)?.return_type {
                    None => TypeId::Void,
                    Some(return_type) => self.resolve_type(return_type, pool)?,
                }
            }
            Expr::ArrayElem(expr, _) => match self.infer_type(expr, pool)? {
                TypeId::Array(_, inner) => *inner,
                TypeId::StaticArray(_, inner, _) => *inner,
                _ => Err(Error::CompileError(format!("{:?} can't be indexed", expr)))?,
            },
            Expr::New(name, _) => {
                if let Reference::Class(cls) = self.resolve(name.clone())? {
                    let name = pool.definition_name(cls)?;
                    self.resolve_type(self.resolve_type_name(Ident(name))?, pool)?
                } else {
                    Err(Error::CompileError(format!("{} can't be constructed", name.0)))?
                }
            }
            Expr::Member(expr, ident) => {
                let class = match self.infer_type(expr, pool)?.unwrapped() {
                    TypeId::Class(_, class) => *class,
                    TypeId::Struct(_, class) => *class,
                    t @ TypeId::Enum(_, _) => return Ok(t.clone()),
                    _ => Err(Error::CompileError(format!("Can't access a member of {:?}", expr)))?,
                };
                let field = self.resolve_field(ident.clone(), class, pool)?;
                self.resolve_type(pool.field(field)?.type_, pool)?
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
            Expr::StringLit(LiteralType::String, _) => {
                self.resolve_type(self.resolve_type_name(Ident::new("String".to_owned()))?, pool)?
            }
            Expr::StringLit(LiteralType::Name, _) => {
                self.resolve_type(self.resolve_type_name(Ident::new("CName".to_owned()))?, pool)?
            }
            Expr::StringLit(LiteralType::Resource, _) => {
                self.resolve_type(self.resolve_type_name(Ident::new("ResRef".to_owned()))?, pool)?
            }
            Expr::StringLit(LiteralType::TweakDbId, _) => {
                self.resolve_type(self.resolve_type_name(Ident::new("TweakDBID".to_owned()))?, pool)?
            }
            Expr::FloatLit(_) => self.resolve_type(self.resolve_type_name(Ident::new("Float".to_owned()))?, pool)?,
            Expr::IntLit(_) => self.resolve_type(self.resolve_type_name(Ident::new("Int32".to_owned()))?, pool)?,
            Expr::UintLit(_) => self.resolve_type(self.resolve_type_name(Ident::new("Uint32".to_owned()))?, pool)?,
            Expr::True => self.resolve_type(self.resolve_type_name(Ident::new("Bool".to_owned()))?, pool)?,
            Expr::False => self.resolve_type(self.resolve_type_name(Ident::new("Bool".to_owned()))?, pool)?,
            Expr::Null => TypeId::Null,
            Expr::This => match self.this {
                Some(cls) => {
                    let name = pool.definition_name(cls)?;
                    self.resolve_type(self.resolve_type_name(Ident(name))?, pool)?
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

pub struct FunctionId<'a>(pub &'a str, String);

impl<'a> FunctionId<'a> {
    pub fn mangled(&self) -> String {
        format!("{};{}", self.0, self.1)
    }

    pub fn from_source(source: &'a FunctionSource) -> Result<Self, Error> {
        let mut signature = String::new();
        for arg in &source.parameters {
            signature.push_str(arg.type_.mangled().as_str());
        }
        Ok(FunctionId(&source.declaration.name, signature))
    }

    pub fn by_name_and_args(name: &'a Ident, args: &[Expr], pool: &ConstantPool, scope: &Scope) -> Result<Self, Error> {
        let mut signature = String::new();
        for arg in args {
            let type_ = scope.infer_type(arg, pool)?;
            signature.push_str(type_.mangled(pool)?.as_str());
        }
        Ok(FunctionId(&name.0, signature))
    }

    pub fn by_binop(lhs: &Expr, rhs: &Expr, op: BinOp, pool: &ConstantPool, scope: &Scope) -> Result<Self, Error> {
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
        Ok(FunctionId(binop_name(op), type_sig))
    }

    pub fn by_unop(expr: &Expr, op: UnOp, pool: &ConstantPool, scope: &Scope) -> Result<Self, Error> {
        let type_ = scope.infer_type(expr, pool)?.mangled(pool)?;
        let type_sig = match op {
            UnOp::LogicNot => format!("{};Bool", type_),
            UnOp::BitNot | UnOp::Neg => format!("{};{}", type_, type_),
        };
        Ok(FunctionId(unop_name(op), type_sig))
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
