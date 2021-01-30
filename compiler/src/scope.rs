use std::str::FromStr;
use std::{iter, ops::Deref};

use redscript::ast::{Expr, Ident, LiteralType, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Class, Definition, DefinitionValue, Enum, Field, Function, Local, Type};
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

#[derive(Debug)]
pub struct FunctionMatch {
    pub index: PoolIndex<Function>,
    pub conversions: Vec<Conversion>,
    pub unspecified_args: usize,
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
        args: impl Iterator<Item = &'a Expr> + Clone,
        expected: Option<&TypeId>,
        pool: &ConstantPool,
    ) -> Result<FunctionMatch, Error> {
        let overloads = self
            .functions
            .get(&name)
            .ok_or_else(|| Error::CompileError(format!("Function {} not found", name.pretty(pool))))?;
        let mut errors = Vec::new();

        for fun_idx in overloads.0.iter() {
            match self.resolve_function_overload(*fun_idx, args.clone(), expected, pool) {
                Ok(res) => return Ok(res),
                Err(Error::FunctionResolutionError(msg)) => errors.push(msg),
                Err(other) => Err(other)?,
            }
        }
        Err(Error::FunctionResolutionError(format!(
            "Arguments passed to {} do not match any of the overloads:\n{}",
            name.pretty(pool),
            errors.join("\n")
        )))
    }

    fn resolve_function_overload<'a>(
        &self,
        fun_idx: PoolIndex<Function>,
        args: impl Iterator<Item = &'a Expr>,
        expected: Option<&TypeId>,
        pool: &ConstantPool,
    ) -> Result<FunctionMatch, Error> {
        let fun = pool.function(fun_idx)?;

        if let Some(expected) = expected {
            let return_type_idx = fun
                .return_type
                .ok_or(Error::CompileError("Void value cannot be used".to_owned()))?;
            let return_type = self.resolve_type_from_pool(return_type_idx, pool)?;
            if self.find_conversion(&return_type, &expected, pool)?.is_none() {
                Err(Error::FunctionResolutionError(format!(
                    "Return type {} does not match expected {}",
                    return_type.pretty(pool)?,
                    expected.pretty(pool)?
                )))?;
            }
        }

        let mut conversions = Vec::new();
        for (idx, arg) in args.enumerate() {
            let param_idx = fun.parameters.get(idx).ok_or_else(|| {
                Error::FunctionResolutionError(format!("Too many arguments, expected {}", fun.parameters.len()))
            })?;
            let param = pool.parameter(*param_idx)?;
            let param_type = self.resolve_type_from_pool(param.type_, pool)?;
            let arg_type = self.infer_type(arg, Some(&param_type), pool)?;
            if let Some(conv) = self.find_conversion(&arg_type, &param_type, pool)? {
                conversions.push(conv);
            } else {
                let param_name = pool.definition_name(*param_idx)?;
                let expected = param_type.pretty(pool)?;
                let given = arg_type.pretty(pool)?;
                Err(Error::FunctionResolutionError(format!(
                    "Parameter {}: {} does not match provided {}",
                    param_name, expected, given
                )))?;
            }
        }

        let opt_param_count = fun
            .parameters
            .iter()
            .filter_map(|idx| pool.parameter(*idx).ok())
            .filter(|param| param.flags.is_optional())
            .count();

        let min_params = fun.parameters.len() - opt_param_count;
        if conversions.len() >= min_params {
            let unspecified_args = fun.parameters.len() - conversions.len();
            let match_ = FunctionMatch {
                index: fun_idx,
                conversions,
                unspecified_args,
            };
            Ok(match_)
        } else {
            Err(Error::FunctionResolutionError(format!(
                "Expected {}-{} parameters, given {}",
                min_params,
                fun.parameters.len(),
                conversions.len()
            )))
        }
    }

    pub fn resolve_method(
        &self,
        name: Ident,
        class_idx: PoolIndex<Class>,
        args: &[Expr],
        expected: Option<&TypeId>,
        pool: &ConstantPool,
    ) -> Result<FunctionMatch, Error> {
        let fun_name = FunctionName::instance(class_idx, name.clone());
        match self.resolve_function(fun_name, args.iter(), expected, pool) {
            Ok(res) => Ok(res),
            Err(err) => {
                let class = pool.class(class_idx)?;
                if class.base != PoolIndex::UNDEFINED {
                    self.resolve_method(name, class.base, args, expected, pool)
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

    pub fn get_type_index(&mut self, type_: &TypeId, pool: &mut ConstantPool) -> Result<PoolIndex<Type>, Error> {
        let name = type_.repr(pool)?;
        if let Some(t) = self.types.get(&name) {
            Ok(*t)
        } else {
            let name_idx = pool.names.add(name.0.deref().clone());
            let value = match type_ {
                TypeId::Prim(_) => Type::Prim,
                TypeId::Class(_) | TypeId::Struct(_) | TypeId::Enum(_) => Type::Class,
                TypeId::Ref(inner) => Type::Ref(self.get_type_index(inner, pool)?),
                TypeId::WeakRef(inner) => Type::WeakRef(self.get_type_index(inner, pool)?),
                TypeId::Array(inner) => Type::Array(self.get_type_index(inner, pool)?),
                TypeId::StaticArray(inner, size) => Type::StaticArray(self.get_type_index(inner, pool)?, *size),
                TypeId::ScriptRef(inner) => Type::ScriptRef(self.get_type_index(inner, pool)?),
                TypeId::Null | TypeId::Void => panic!(),
            };
            let idx = pool.push_definition(Definition::type_(name_idx, value)).cast();
            self.types.insert(name, idx);
            Ok(idx)
        }
    }

    pub fn resolve_type(&self, name: &TypeName, pool: &ConstantPool) -> Result<TypeId, Error> {
        let result = if let Some(res) = self.types.get(&Ident::new(name.repr())) {
            self.resolve_type_from_pool(*res, pool)?
        } else {
            match (name.name.as_str(), name.arguments.as_slice()) {
                ("ref", [nested]) => TypeId::Ref(Box::new(self.resolve_type(nested, pool)?)),
                ("wref", [nested]) => TypeId::WeakRef(Box::new(self.resolve_type(nested, pool)?)),
                ("script_ref", [nested]) => TypeId::ScriptRef(Box::new(self.resolve_type(nested, pool)?)),
                ("array", [nested]) => TypeId::Array(Box::new(self.resolve_type(nested, pool)?)),
                _ => Err(Error::CompileError(format!("Unresolved type {}", name)))?,
            }
        };
        Ok(result)
    }

    pub fn resolve_type_from_pool(&self, index: PoolIndex<Type>, pool: &ConstantPool) -> Result<TypeId, Error> {
        let result = match pool.type_(index)? {
            Type::Prim => TypeId::Prim(index),
            Type::Class => {
                let ident = Ident(pool.definition_name(index)?);
                if let Some(Reference::Class(class_idx)) = self.references.get(&ident) {
                    if pool.class(*class_idx)?.flags.is_struct() {
                        TypeId::Struct(*class_idx)
                    } else {
                        TypeId::Class(*class_idx)
                    }
                } else if let Some(Reference::Enum(enum_idx)) = self.references.get(&ident) {
                    TypeId::Enum(*enum_idx)
                } else {
                    Err(Error::CompileError(format!("Class {} not found", ident)))?
                }
            }
            Type::Ref(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool)?;
                TypeId::Ref(Box::new(inner))
            }
            Type::WeakRef(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool)?;
                TypeId::WeakRef(Box::new(inner))
            }
            Type::Array(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool)?;
                TypeId::Array(Box::new(inner))
            }
            Type::StaticArray(type_, size) => {
                let inner = self.resolve_type_from_pool(*type_, pool)?;
                TypeId::StaticArray(Box::new(inner), *size)
            }
            Type::ScriptRef(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool)?;
                TypeId::ScriptRef(Box::new(inner))
            }
        };
        Ok(result)
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
                (TypeId::Null, TypeId::Ref(_)) => Some(Conversion::Identity),
                (TypeId::Null, TypeId::WeakRef(_)) => Some(Conversion::RefToWeakRef),
                (TypeId::Class(from), TypeId::Class(_)) => {
                    let class = pool.class(*from)?;
                    if class.base != PoolIndex::UNDEFINED {
                        self.find_conversion(&TypeId::Class(class.base), to, pool)?
                    } else {
                        None
                    }
                }
                (from @ TypeId::Class(_), TypeId::Ref(to)) => self
                    .find_conversion(from, to, pool)?
                    .filter(|conv| *conv == Conversion::Identity),
                (TypeId::Ref(from), TypeId::Ref(to)) => self
                    .find_conversion(from, to, pool)?
                    .filter(|conv| *conv == Conversion::Identity),
                (TypeId::WeakRef(from), TypeId::WeakRef(to)) => self
                    .find_conversion(from, to, pool)?
                    .filter(|conv| *conv == Conversion::Identity),
                (TypeId::WeakRef(from), TypeId::Ref(to))
                    if self.find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
                {
                    Some(Conversion::WeakRefToRef)
                }
                (TypeId::Ref(from), TypeId::WeakRef(to))
                    if self.find_conversion(from, to, pool)? == Some(Conversion::Identity) =>
                {
                    Some(Conversion::RefToWeakRef)
                }
                _ => None,
            }
        };
        Ok(result)
    }

    pub fn infer_type(&self, expr: &Expr, expected: Option<&TypeId>, pool: &ConstantPool) -> Result<TypeId, Error> {
        let res = match expr {
            Expr::Ident(name) => match self.resolve_reference(name.clone())? {
                Reference::Local(idx) => self.resolve_type_from_pool(pool.local(idx)?.type_, pool)?,
                Reference::Parameter(idx) => self.resolve_type_from_pool(pool.parameter(idx)?.type_, pool)?,
                Reference::Field(idx) => self.resolve_type_from_pool(pool.field(idx)?.type_, pool)?,
                Reference::Class(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(&TypeName::basic(name.deref().clone()), pool)?
                }
                Reference::Enum(idx) => {
                    let name = pool.definition_name(idx)?;
                    self.resolve_type(&TypeName::basic(name.deref().clone()), pool)?
                }
            },
            Expr::Cast(type_name, expr) => {
                let type_ = self.resolve_type(type_name, pool)?;
                match self.infer_type(&expr, None, pool)? {
                    TypeId::Ref(_) => TypeId::Ref(Box::new(type_)),
                    TypeId::WeakRef(_) => TypeId::WeakRef(Box::new(type_)),
                    TypeId::ScriptRef(_) => TypeId::ScriptRef(Box::new(type_)),
                    _ => type_,
                }
            }
            Expr::Call(ident, args) => {
                if let Ok(intrinsic) = IntrinsicOp::from_str(&ident.0) {
                    self.infer_intrinsic_type(intrinsic, args, expected, pool)?
                } else {
                    let name = FunctionName::global(ident.clone());
                    let match_ = self.resolve_function(name, args.iter(), expected, pool)?;
                    match pool.function(match_.index)?.return_type {
                        Some(type_) => self.resolve_type_from_pool(type_, pool)?,
                        None => TypeId::Void,
                    }
                }
            }
            Expr::MethodCall(expr, ident, args) => {
                let class = match self.infer_type(expr, None, pool)?.unwrapped() {
                    TypeId::Class(class) => *class,
                    TypeId::Struct(class) => *class,
                    _ => Err(Error::CompileError(format!("{:?} doesn't have methods", expr)))?,
                };
                let match_ = self.resolve_method(ident.clone(), class, args, expected, pool)?;
                match pool.function(match_.index)?.return_type {
                    None => TypeId::Void,
                    Some(return_type) => self.resolve_type_from_pool(return_type, pool)?,
                }
            }
            Expr::ArrayElem(expr, _) => match self.infer_type(expr, None, pool)? {
                TypeId::Array(inner) => *inner,
                TypeId::StaticArray(inner, _) => *inner,
                type_ => Err(Error::CompileError(format!("{} can't be indexed", type_.pretty(pool)?)))?,
            },
            Expr::New(name, _) => {
                if let Reference::Class(class_idx) = self.resolve_reference(name.clone())? {
                    let name = pool.definition_name(class_idx)?;
                    let type_ = self.resolve_type(&TypeName::basic(name.deref().clone()), pool)?;
                    if pool.class(class_idx)?.flags.is_struct() {
                        type_
                    } else {
                        TypeId::Ref(Box::new(type_))
                    }
                } else {
                    Err(Error::CompileError(format!("{} can't be constructed", name)))?
                }
            }
            Expr::Member(expr, ident) => {
                let class = match self.infer_type(expr, None, pool)?.unwrapped() {
                    TypeId::Class(class) => *class,
                    TypeId::Struct(class) => *class,
                    t @ TypeId::Enum(_) => return Ok(t.clone()),
                    t => {
                        let err = format!("Can't access a member of {}", t.pretty(pool)?);
                        Err(Error::CompileError(err))?
                    }
                };
                let field = self.resolve_field(ident.clone(), class, pool)?;
                self.resolve_type_from_pool(pool.field(field)?.type_, pool)?
            }
            Expr::Conditional(_, lhs, rhs) => {
                let lt = self.infer_type(lhs, expected, pool)?;
                let rt = self.infer_type(rhs, expected, pool)?;
                if lt != rt {
                    let error = format!("Incompatible types: {} and {}", lt.pretty(pool)?, rt.pretty(pool)?);
                    Err(Error::CompileError(error))?
                }
                lt
            }
            Expr::BinOp(lhs, rhs, op) => {
                let ident = Ident::new(op.name());
                let args = iter::once(lhs.as_ref()).chain(iter::once(rhs.as_ref()));
                let match_ = self.resolve_function(FunctionName::global(ident), args.clone(), expected, pool)?;
                match pool.function(match_.index)?.return_type {
                    Some(type_) => self.resolve_type_from_pool(type_, pool)?,
                    None => TypeId::Void,
                }
            }
            Expr::UnOp(expr, op) => {
                let ident = Ident::new(op.name());
                let args = iter::once(expr.as_ref());
                let match_ = self.resolve_function(FunctionName::global(ident), args.clone(), expected, pool)?;
                match pool.function(match_.index)?.return_type {
                    Some(type_) => self.resolve_type_from_pool(type_, pool)?,
                    None => TypeId::Void,
                }
            }
            Expr::StringLit(LiteralType::String, _) => {
                self.resolve_type(&TypeName::basic("String".to_owned()), pool)?
            }
            Expr::StringLit(LiteralType::Name, _) => self.resolve_type(&TypeName::basic("CName".to_owned()), pool)?,
            Expr::StringLit(LiteralType::Resource, _) => {
                self.resolve_type(&TypeName::basic("ResRef".to_owned()), pool)?
            }
            Expr::StringLit(LiteralType::TweakDbId, _) => {
                self.resolve_type(&TypeName::basic("TweakDBID".to_owned()), pool)?
            }
            Expr::FloatLit(_) => self.resolve_type(&TypeName::basic("Float".to_owned()), pool)?,
            Expr::IntLit(_) => self.resolve_type(&TypeName::basic("Int32".to_owned()), pool)?,
            Expr::UintLit(_) => self.resolve_type(&TypeName::basic("Uint32".to_owned()), pool)?,
            Expr::True => self.resolve_type(&TypeName::basic("Bool".to_owned()), pool)?,
            Expr::False => self.resolve_type(&TypeName::basic("Bool".to_owned()), pool)?,
            Expr::Null => TypeId::Null,
            Expr::This => match self.this {
                Some(cls) => {
                    let name = pool.definition_name(cls)?;
                    self.resolve_type(&TypeName::basic(name.deref().to_owned()), pool)?
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
        expected: Option<&TypeId>,
        pool: &ConstantPool,
    ) -> Result<TypeId, Error> {
        if args.len() != intrinsic.arg_count().into() {
            let err = format!("Invalid number of arguments for {}", intrinsic);
            Err(Error::CompileError(err))?
        }
        let type_ = self.infer_type(&args[0], None, pool)?;
        let result = match (intrinsic, type_) {
            (IntrinsicOp::Equals, _) => self.resolve_type(&TypeName::basic("Bool".to_owned()), pool)?,
            (IntrinsicOp::NotEquals, _) => self.resolve_type(&TypeName::basic("Bool".to_owned()), pool)?,
            (IntrinsicOp::ArrayClear, _) => TypeId::Void,
            (IntrinsicOp::ArraySize, _) => self.resolve_type(&TypeName::basic("Int32".to_owned()), pool)?,
            (IntrinsicOp::ArrayResize, _) => TypeId::Void,
            (IntrinsicOp::ArrayFindFirst, TypeId::Array(member)) => *member,
            (IntrinsicOp::ArrayFindLast, TypeId::Array(member)) => *member,
            (IntrinsicOp::ArrayContains, _) => self.resolve_type(&TypeName::basic("Bool".to_owned()), pool)?,
            (IntrinsicOp::ArrayPush, _) => TypeId::Void,
            (IntrinsicOp::ArrayPop, TypeId::Array(member)) => *member,
            (IntrinsicOp::ArrayInsert, _) => TypeId::Void,
            (IntrinsicOp::ArrayRemove, _) => TypeId::Void,
            (IntrinsicOp::ArrayGrow, _) => TypeId::Void,
            (IntrinsicOp::ArrayErase, _) => TypeId::Void,
            (IntrinsicOp::ArrayLast, TypeId::Array(member)) => *member,
            (IntrinsicOp::ToString, _) => self.resolve_type(&TypeName::basic("String".to_owned()), pool)?,
            (IntrinsicOp::EnumInt, _) => self.resolve_type(&TypeName::basic("Int32".to_owned()), pool)?,
            (IntrinsicOp::IntEnum, _) if expected.is_some() => expected.unwrap().clone(),
            (IntrinsicOp::ToVariant, _) => self.resolve_type(&TypeName::basic("Variant".to_owned()), pool)?,
            (IntrinsicOp::FromVariant, _) if expected.is_some() => expected.unwrap().clone(),
            (IntrinsicOp::AsRef, type_) => TypeId::ScriptRef(Box::new(type_)),
            (IntrinsicOp::Deref, TypeId::ScriptRef(inner)) => *inner,
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
