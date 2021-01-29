use std::iter;
use std::str::FromStr;

use redscript::ast::{Expr, Ident, LiteralType};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Class, DefinitionValue, Enum, Field, Function, Local, Type};
use redscript::error::Error;

use crate::assembler::IntrinsicOp;
use crate::parser::FunctionSource;
use crate::{Reference, TypeId};

#[derive(Debug, Clone)]
pub struct FunctionOverloads(Vec<PoolIndex<Function>>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionName {
    namespace: Option<PoolIndex<Class>>,
    name: Ident,
}

impl FunctionName {
    pub fn global(name: Ident) -> FunctionName {
        FunctionName { namespace: None, name }
    }

    pub fn instance(class: PoolIndex<Class>, name: Ident) -> FunctionName {
        FunctionName {
            namespace: Some(class),
            name,
        }
    }

    pub fn pretty(&self, pool: &ConstantPool) -> String {
        self.namespace
            .and_then(|c| pool.definition_name(c).ok())
            .map(|n| format!("{}::{}", n, self.name))
            .unwrap_or(self.name.to_string())
    }
}

pub struct FunctionMatch {
    pub index: PoolIndex<Function>,
    pub conversions: Vec<Conversion>,
    pub unspecified_args: u8,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub functions: im::HashMap<FunctionName, FunctionOverloads>,
    pub references: im::HashMap<Ident, Reference>,
    pub types: im::HashMap<Ident, PoolIndex<Type>>,
    pub this: Option<PoolIndex<Class>>,
    pub function: Option<PoolIndex<Function>>,
}

impl Scope {
    pub fn new(pool: &ConstantPool) -> Result<Self, Error> {
        let names = pool
            .roots()
            .filter_map(|(idx, def)| {
                let id = Ident(pool.definition_name(idx).unwrap());
                match def.value {
                    DefinitionValue::Class(_) => Some((id, Reference::Class(idx.cast()))),
                    DefinitionValue::Enum(_) => Some((id, Reference::Enum(idx.cast()))),
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

        let mut result = Scope {
            functions: im::HashMap::new(),
            references: names,
            types,
            this: None,
            function: None,
        };

        for (idx, def) in pool.definitions() {
            if let DefinitionValue::Function(_) = def.value {
                let mangled_name = pool.definition_name(idx)?;
                let ident = Ident::new(mangled_name.split(";").next().unwrap().to_owned());
                let name = if def.parent != PoolIndex::UNDEFINED {
                    FunctionName::instance(def.parent.cast(), ident)
                } else {
                    FunctionName::global(ident)
                };
                result.push_function(name, idx.cast())
            }
        }

        Ok(result)
    }

    pub fn with_context(&self, this: PoolIndex<Class>, function: PoolIndex<Function>) -> Self {
        Scope {
            this: Some(this),
            function: Some(function),
            ..self.clone()
        }
    }

    pub fn push_local(&mut self, name: Ident, local: PoolIndex<Local>) {
        self.references.insert(name, Reference::Local(local));
    }

    pub fn push_function(&mut self, name: FunctionName, index: PoolIndex<Function>) {
        self.functions
            .entry(name)
            .and_modify(|overloads: &mut FunctionOverloads| overloads.0.push(index))
            .or_insert_with(|| FunctionOverloads(vec![index]));
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
        let err = format!("Field {} not found on {}", ident, pool.definition_name(class_idx)?);
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
        let err = format!("Member {} not found on {}", ident, pool.definition_name(enum_idx)?);
        Err(Error::CompileError(err))
    }

    pub fn resolve_function<'a>(
        &self,
        name: FunctionName,
        args: impl Iterator<Item = &'a Expr>,
        pool: &ConstantPool,
    ) -> Result<FunctionMatch, Error> {
        let overloads = self
            .functions
            .get(&name)
            .ok_or_else(|| Error::CompileError(format!("Function {} not found", name.pretty(pool))))?;

        let mut types = Vec::new();
        for arg in args {
            types.push(self.infer_type(arg, pool)?);
        }

        for fun_idx in overloads.0.iter() {
            let fun = pool.function(*fun_idx)?;
            let mut conversions = Vec::new();
            let mut undefined_args = 0;
            for i in 0..fun.parameters.len() {
                let param = pool.parameter(fun.parameters[i])?;
                let param_type = self.resolve_type_by_index(param.type_, pool)?;
                if let Some(arg_type) = types.get(i) {
                    if let Some(conv) = self.find_conversion(&arg_type, &param_type, pool)? {
                        conversions.push(conv);
                    } else {
                        break;
                    }
                } else if param.flags.is_optional() {
                    undefined_args += 1;
                } else {
                    break;
                }
            }

            if conversions.len() + undefined_args == fun.parameters.len() {
                let match_ = FunctionMatch {
                    index: *fun_idx,
                    conversions,
                    unspecified_args: undefined_args as u8,
                };
                return Ok(match_);
            }
        }
        Err(Error::FunctionResolutionError(format!(
            "Arguments passed to {} do not match any of the overloads",
            name.pretty(pool)
        )))
    }

    pub fn resolve_method(
        &self,
        name: Ident,
        class_idx: PoolIndex<Class>,
        args: &[Expr],
        pool: &ConstantPool,
    ) -> Result<FunctionMatch, Error> {
        match self.resolve_function(FunctionName::instance(class_idx, name.clone()), args.iter(), pool) {
            Ok(res) => Ok(res),
            Err(err) => {
                let class = pool.class(class_idx)?;
                if class.base != PoolIndex::UNDEFINED {
                    self.resolve_method(name, class.base, args, pool)
                        .map_err(|base_err| match base_err {
                            err @ Error::FunctionResolutionError(_) => err,
                            _ => err,
                        })
                } else {
                    Err(err)
                }
            }
        }
    }

    pub fn resolve_reference(&self, name: Ident) -> Result<Reference, Error> {
        self.references
            .get(&name)
            .cloned()
            .ok_or(Error::CompileError(format!("Unresolved reference {}", name)))
    }

    pub fn resolve_type_index(&self, name: Ident) -> Result<PoolIndex<Type>, Error> {
        self.types
            .get(&name)
            .cloned()
            .ok_or(Error::CompileError(format!("Unresolved type {}", name)))
    }

    pub fn resolve_type(&self, name: Ident, pool: &ConstantPool) -> Result<TypeId, Error> {
        let index = self.resolve_type_index(name)?;
        self.resolve_type_by_index(index, pool)
    }

    pub fn resolve_type_by_index(&self, index: PoolIndex<Type>, pool: &ConstantPool) -> Result<TypeId, Error> {
        let result = match pool.type_(index)? {
            Type::Prim => TypeId::Prim(index),
            Type::Class => {
                let ident = Ident(pool.definition_name(index)?);
                if let Some(Reference::Class(class_idx)) = self.references.get(&ident) {
                    if pool.class(*class_idx)?.flags.is_struct() {
                        TypeId::Struct(index, *class_idx)
                    } else {
                        TypeId::Class(Some(index), *class_idx)
                    }
                } else if let Some(Reference::Enum(enum_idx)) = self.references.get(&ident) {
                    TypeId::Enum(index, *enum_idx)
                } else {
                    Err(Error::CompileError(format!("Class {} not found", ident)))?
                }
            }
            Type::Ref(type_) => {
                let inner = self.resolve_type_by_index(*type_, pool)?;
                TypeId::Ref(index, Box::new(inner))
            }
            Type::WeakRef(type_) => {
                let inner = self.resolve_type_by_index(*type_, pool)?;
                TypeId::WeakRef(index, Box::new(inner))
            }
            Type::Array(type_) => {
                let inner = self.resolve_type_by_index(*type_, pool)?;
                TypeId::Array(index, Box::new(inner))
            }
            Type::StaticArray(type_, size) => {
                let inner = self.resolve_type_by_index(*type_, pool)?;
                TypeId::StaticArray(index, Box::new(inner), *size)
            }
            Type::ScriptRef(type_) => {
                let inner = self.resolve_type_by_index(*type_, pool)?;
                TypeId::ScriptRef(index, Box::new(inner))
            }
        };
        Ok(result)
    }

    pub fn convert_expr(&self, from: &Expr, to: &Expr, pool: &ConstantPool) -> Result<Conversion, Error> {
        let from = self.infer_type(from, pool)?;
        let to = self.infer_type(to, pool)?;
        self.convert_type(&from, &to, pool)
    }

    pub fn convert_type(&self, from: &TypeId, to: &TypeId, pool: &ConstantPool) -> Result<Conversion, Error> {
        self.find_conversion(&from, &to, pool)?
            .ok_or(Error::CompileError(format!(
                "Can't coerce {} to {}",
                from.pretty(pool)?,
                to.pretty(pool)?
            )))
    }

    fn find_conversion(&self, from: &TypeId, to: &TypeId, pool: &ConstantPool) -> Result<Option<Conversion>, Error> {
        let result = if from == to {
            Some(Conversion::Identity)
        } else {
            match (from, to) {
                (TypeId::Null, TypeId::Ref(_, _)) => Some(Conversion::Identity),
                (TypeId::Null, TypeId::WeakRef(_, _)) => Some(Conversion::Identity),
                (TypeId::Class(_, from), TypeId::Class(_, _)) => {
                    let class = pool.class(*from)?;
                    if class.base != PoolIndex::UNDEFINED {
                        let base_name = Ident(pool.definition_name(class.base)?);
                        let base_type = self.resolve_type(base_name, pool).ok().and_then(|t| t.index());
                        self.find_conversion(&TypeId::Class(base_type, class.base), to, pool)?
                    } else {
                        None
                    }
                }
                (from @ TypeId::Class(_, _), TypeId::Ref(_, to)) => self
                    .find_conversion(from, to, pool)?
                    .filter(|conv| *conv == Conversion::Identity),
                (TypeId::Ref(_, from), TypeId::Ref(_, to)) => self
                    .find_conversion(from, to, pool)?
                    .filter(|conv| *conv == Conversion::Identity),
                (TypeId::WeakRef(_, from), TypeId::WeakRef(_, to)) => self
                    .find_conversion(from, to, pool)?
                    .filter(|conv| *conv == Conversion::Identity),
                (TypeId::WeakRef(_, from), TypeId::Ref(_, to))
                    if self.find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
                {
                    Some(Conversion::WeakRefToRef)
                }
                (TypeId::Ref(_, from), TypeId::WeakRef(_, to))
                    if self.find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
                {
                    Some(Conversion::RefToWeakRef)
                }
                _ => None,
            }
        };
        Ok(result)
    }

    pub fn infer_type(&self, expr: &Expr, pool: &ConstantPool) -> Result<TypeId, Error> {
        let res = match expr {
            Expr::Ident(name) => match self.resolve_reference(name.clone())? {
                Reference::Local(idx) => self.resolve_type_by_index(pool.local(idx)?.type_, pool)?,
                Reference::Parameter(idx) => self.resolve_type_by_index(pool.parameter(idx)?.type_, pool)?,
                Reference::Field(idx) => self.resolve_type_by_index(pool.field(idx)?.type_, pool)?,
                Reference::Class(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(Ident(name), pool)?
                }
                Reference::Enum(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(Ident(name), pool)?
                }
            },
            Expr::Cast(type_, _) => self.resolve_type(Ident::new(type_.repr()), pool)?,
            Expr::Call(ident, args) => {
                if let Ok(intrinsic) = IntrinsicOp::from_str(&ident.0) {
                    self.infer_intrinsic_type(intrinsic, args, pool)?
                } else {
                    let name = FunctionName::global(ident.clone());
                    let match_ = self.resolve_function(name, args.iter(), pool)?;
                    match pool.function(match_.index)?.return_type {
                        Some(type_) => self.resolve_type_by_index(type_, pool)?,
                        None => TypeId::Void,
                    }
                }
            }
            Expr::MethodCall(expr, ident, args) => {
                let class = match self.infer_type(expr, pool)?.unwrapped() {
                    TypeId::Class(_, class) => *class,
                    TypeId::Struct(_, class) => *class,
                    _ => Err(Error::CompileError(format!("{:?} doesn't have methods", expr)))?,
                };
                let match_ = self.resolve_method(ident.clone(), class, args, pool)?;
                match pool.function(match_.index)?.return_type {
                    None => TypeId::Void,
                    Some(return_type) => self.resolve_type_by_index(return_type, pool)?,
                }
            }
            Expr::ArrayElem(expr, _) => match self.infer_type(expr, pool)? {
                TypeId::Array(_, inner) => *inner,
                TypeId::StaticArray(_, inner, _) => *inner,
                type_ => Err(Error::CompileError(format!("{} can't be indexed", type_.pretty(pool)?)))?,
            },
            Expr::New(name, _) => {
                if let Reference::Class(cls) = self.resolve_reference(name.clone())? {
                    let name = pool.definition_name(cls)?;
                    self.resolve_type(Ident(name), pool)?
                } else {
                    Err(Error::CompileError(format!("{} can't be constructed", name)))?
                }
            }
            Expr::Member(expr, ident) => {
                let class = match self.infer_type(expr, pool)?.unwrapped() {
                    TypeId::Class(_, class) => *class,
                    TypeId::Struct(_, class) => *class,
                    t @ TypeId::Enum(_, _) => return Ok(t.clone()),
                    t => {
                        let err = format!("Can't access a member of {}", t.pretty(pool)?);
                        Err(Error::CompileError(err))?
                    }
                };
                let field = self.resolve_field(ident.clone(), class, pool)?;
                self.resolve_type_by_index(pool.field(field)?.type_, pool)?
            }
            Expr::Conditional(_, lhs, rhs) => {
                let lt = self.infer_type(lhs, pool)?;
                let rt = self.infer_type(rhs, pool)?;
                if lt != rt {
                    let error = format!("Incompatible types: {} and {}", lt.pretty(pool)?, rt.pretty(pool)?);
                    Err(Error::CompileError(error))?
                }
                lt
            }
            Expr::BinOp(lhs, rhs, op) => {
                let ident = Ident::new(op.name());
                let args = iter::once(lhs.as_ref()).chain(iter::once(rhs.as_ref()));
                let match_ = self.resolve_function(FunctionName::global(ident), args.clone(), pool)?;
                match pool.function(match_.index)?.return_type {
                    Some(type_) => self.resolve_type_by_index(type_, pool)?,
                    None => TypeId::Void,
                }
            }
            Expr::UnOp(expr, op) => {
                let ident = Ident::new(op.name());
                let args = iter::once(expr.as_ref());
                let match_ = self.resolve_function(FunctionName::global(ident), args.clone(), pool)?;
                match pool.function(match_.index)?.return_type {
                    Some(type_) => self.resolve_type_by_index(type_, pool)?,
                    None => TypeId::Void,
                }
            }
            Expr::StringLit(LiteralType::String, _) => self.resolve_type(Ident::new("String".to_owned()), pool)?,
            Expr::StringLit(LiteralType::Name, _) => self.resolve_type(Ident::new("CName".to_owned()), pool)?,
            Expr::StringLit(LiteralType::Resource, _) => self.resolve_type(Ident::new("ResRef".to_owned()), pool)?,
            Expr::StringLit(LiteralType::TweakDbId, _) => {
                self.resolve_type(Ident::new("TweakDBID".to_owned()), pool)?
            }
            Expr::FloatLit(_) => self.resolve_type(Ident::new("Float".to_owned()), pool)?,
            Expr::IntLit(_) => self.resolve_type(Ident::new("Int32".to_owned()), pool)?,
            Expr::UintLit(_) => self.resolve_type(Ident::new("Uint32".to_owned()), pool)?,
            Expr::True => self.resolve_type(Ident::new("Bool".to_owned()), pool)?,
            Expr::False => self.resolve_type(Ident::new("Bool".to_owned()), pool)?,
            Expr::Null => TypeId::Null,
            Expr::This => match self.this {
                Some(cls) => {
                    let name = pool.definition_name(cls)?;
                    self.resolve_type(Ident(name), pool)?
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

    fn infer_intrinsic_type(
        &self,
        intrinsic: IntrinsicOp,
        args: &[Expr],
        pool: &ConstantPool,
    ) -> Result<TypeId, Error> {
        if args.len() != intrinsic.arg_count().into() {
            let err = format!("Invalid number of arguments for {}", intrinsic);
            Err(Error::CompileError(err))?
        }
        let type_ = self.infer_type(&args[0], pool)?;
        let result = match (intrinsic, type_) {
            (IntrinsicOp::Equals, _) => self.resolve_type(Ident::new("Bool".to_owned()), pool)?,
            (IntrinsicOp::NotEquals, _) => self.resolve_type(Ident::new("Bool".to_owned()), pool)?,
            (IntrinsicOp::ArrayClear, _) => TypeId::Void,
            (IntrinsicOp::ArraySize, _) => self.resolve_type(Ident::new("Int32".to_owned()), pool)?,
            (IntrinsicOp::ArrayResize, _) => TypeId::Void,
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(_, member)) => *member,
            (IntrinsicOp::ArrayFindLast, TypeId::Array(_, member)) => *member,
            (IntrinsicOp::ArrayContains, _) => self.resolve_type(Ident::new("Bool".to_owned()), pool)?,
            (IntrinsicOp::ArrayPush, _) => TypeId::Void,
            (IntrinsicOp::ArrayPop, TypeId::Array(_, member)) => *member,
            (IntrinsicOp::ArrayInsert, _) => TypeId::Void,
            (IntrinsicOp::ArrayRemove, _) => TypeId::Void,
            (IntrinsicOp::ArrayGrow, _) => TypeId::Void,
            (IntrinsicOp::ArrayErase, _) => TypeId::Void,
            (IntrinsicOp::ArrayLast, TypeId::Array(_, member)) => *member,
            (IntrinsicOp::ToString, _) => self.resolve_type(Ident::new("String".to_owned()), pool)?,
            (IntrinsicOp::EnumInt, _) => self.resolve_type(Ident::new("Int32".to_owned()), pool)?,
            (IntrinsicOp::IntEnum, _) => panic!(),
            (IntrinsicOp::ToVariant, _) => self.resolve_type(Ident::new("Variant".to_owned()), pool)?,
            (IntrinsicOp::FromVariant, _) => panic!(),
            (IntrinsicOp::AsRef, _) => panic!(),
            (IntrinsicOp::Deref, _) => panic!(),
            _ => {
                let err = format!("Invalid intrinsic {} call", intrinsic);
                Err(Error::CompileError(err))?
            }
        };
        Ok(result)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Conversion {
    Identity,
    RefToWeakRef,
    WeakRefToRef,
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
}
