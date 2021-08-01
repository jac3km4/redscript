use hamt_sync::Map;
use redscript::ast::{Expr, Ident, Pos, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{AnyDefinition, Class, Definition, Enum, Field, Function, Local, Parameter, Type};
use redscript::error::Error;

use crate::symbol::{FunctionSignature, Symbol};
use crate::typechecker::TypedAst;

#[derive(Debug, Clone)]
pub struct Scope {
    symbols: Map<Ident, Symbol>,
    references: Map<Ident, Value>,
    types: Map<Ident, PoolIndex<Type>>,

    pub this: Option<PoolIndex<Class>>,
    pub function: Option<PoolIndex<Function>>,
}

impl Scope {
    pub fn new(pool: &ConstantPool) -> Result<Self, Error> {
        let mut types = Map::new();
        for (idx, def) in pool.roots() {
            if let AnyDefinition::Type(_) = def.value {
                let ident = Ident::Owned(pool.definition_name(idx)?);
                types = types.insert(ident, idx.cast());
            }
        }

        let result = Scope {
            symbols: Map::new(),
            references: Map::new(),
            types,
            this: None,
            function: None,
        };

        Ok(result)
    }

    pub fn with_context(&self, this: Option<PoolIndex<Class>>, function: PoolIndex<Function>) -> Self {
        Scope {
            this,
            function: Some(function),
            ..self.clone()
        }
    }

    pub fn add_local(&mut self, name: Ident, local: PoolIndex<Local>) {
        self.references = self.references.insert(name, Value::Local(local));
    }

    pub fn add_parameter(&mut self, name: Ident, param: PoolIndex<Parameter>) {
        self.references = self.references.insert(name, Value::Parameter(param));
    }

    pub fn add_type(&mut self, name: Ident, typ: PoolIndex<Type>) {
        self.types = self.types.insert(name, typ);
    }

    pub fn add_symbol(&mut self, name: Ident, symbol: Symbol) {
        match symbol {
            Symbol::Functions(funs) => match self.symbols.find(&name) {
                Some(Symbol::Functions(existing)) => {
                    let mut combined = existing.clone();
                    combined.extend(funs);
                    self.symbols = self.symbols.insert(name, Symbol::Functions(combined));
                }
                _ => {
                    self.symbols = self.symbols.insert(name, Symbol::Functions(funs));
                }
            },
            _ => {
                self.symbols = self.symbols.insert(name, symbol);
            }
        }
    }

    pub fn resolve_function(&self, name: Ident, pos: Pos) -> Result<FunctionCandidates, Error> {
        if let Some(Symbol::Functions(functions)) = self.symbols.find(&name) {
            Ok(FunctionCandidates {
                functions: functions.iter().map(|(idx, _)| idx).copied().collect(),
            })
        } else {
            Err(Error::function_not_found(name, pos))
        }
    }

    pub fn resolve_field(
        &self,
        ident: Ident,
        class_idx: PoolIndex<Class>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<PoolIndex<Field>, Error> {
        let class = pool.class(class_idx)?;
        for field in &class.fields {
            if pool.definition_name(*field)?.as_ref() == ident.as_ref() {
                return Ok(*field);
            }
        }
        if class.base != PoolIndex::UNDEFINED {
            self.resolve_field(ident.clone(), class.base, pool, pos)
                .map_err(|_| Error::member_not_found(ident, pool.definition_name(class_idx).unwrap(), pos))
        } else {
            Err(Error::member_not_found(ident, pool.definition_name(class_idx)?, pos))
        }
    }

    pub fn resolve_enum_member(
        &self,
        ident: Ident,
        enum_idx: PoolIndex<Enum>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<PoolIndex<i64>, Error> {
        let enum_ = pool.enum_(enum_idx)?;
        for field in &enum_.members {
            if pool.definition_name(*field)?.as_ref() == ident.as_ref() {
                return Ok(*field);
            }
        }
        Err(Error::member_not_found(ident, pool.definition_name(enum_idx)?, pos))
    }

    pub fn resolve_method(
        &self,
        ident: Ident,
        class_idx: PoolIndex<Class>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<FunctionCandidates, Error> {
        let mut current_idx = class_idx;
        let mut functions = vec![];

        while current_idx != PoolIndex::UNDEFINED {
            let class = pool.class(current_idx)?;
            for fun in &class.functions {
                if pool.definition_name(*fun)?.split(';').next().unwrap() == ident.as_ref() {
                    functions.push(*fun);
                }
            }
            current_idx = class.base;
        }
        if functions.is_empty() {
            Err(Error::member_not_found(ident, pool.definition_name(class_idx)?, pos))
        } else {
            Ok(FunctionCandidates { functions })
        }
    }

    pub fn resolve_value(&self, name: Ident, pos: Pos) -> Result<Value, Error> {
        self.references
            .find(&name)
            .cloned()
            .ok_or_else(|| Error::unresolved_reference(name, pos))
    }

    pub fn resolve_symbol(&self, name: Ident, pos: Pos) -> Result<Symbol, Error> {
        self.symbols
            .find(&name)
            .cloned()
            .ok_or_else(|| Error::unresolved_reference(name, pos))
    }

    pub fn resolve_reference(&self, name: Ident, pos: Pos) -> Result<Reference, Error> {
        self.resolve_value(name.clone(), pos)
            .map(Reference::Value)
            .or_else(|_| self.resolve_symbol(name, pos).map(Reference::Symbol))
    }

    pub fn get_type_index(&mut self, type_: &TypeId, pool: &mut ConstantPool) -> Result<PoolIndex<Type>, Error> {
        let name = type_.repr(pool)?;
        if let Some(type_idx) = self.types.find(&name) {
            Ok(*type_idx)
        } else {
            let name_idx = pool.names.add(name.to_owned());
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
            let type_idx = pool.add_definition(Definition::type_(name_idx, value));
            self.add_type(name, type_idx);
            Ok(type_idx)
        }
    }

    pub fn resolve_type(&self, name: &TypeName, pool: &ConstantPool, pos: Pos) -> Result<TypeId, Error> {
        let result = if let Some(res) = self.types.find(&name.repr()) {
            self.resolve_type_from_pool(*res, pool, pos)?
        } else {
            match (name.name.as_ref(), name.arguments.as_slice()) {
                ("ref", [nested]) => TypeId::Ref(Box::new(self.resolve_type(nested, pool, pos)?)),
                ("wref", [nested]) => TypeId::WeakRef(Box::new(self.resolve_type(nested, pool, pos)?)),
                ("script_ref", [nested]) => TypeId::ScriptRef(Box::new(self.resolve_type(nested, pool, pos)?)),
                ("array", [nested]) => TypeId::Array(Box::new(self.resolve_type(nested, pool, pos)?)),
                _ => match self.symbols.find(&name.repr()) {
                    Some(Symbol::Class(idx, _)) => TypeId::Class(*idx),
                    Some(Symbol::Struct(idx, _)) => TypeId::Struct(*idx),
                    Some(Symbol::Enum(idx)) => TypeId::Enum(*idx),
                    _ => return Err(Error::unresolved_type(name, pos)),
                },
            }
        };
        Ok(result)
    }

    pub fn resolve_type_from_pool(
        &self,
        index: PoolIndex<Type>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<TypeId, Error> {
        let result = match pool.type_(index)? {
            Type::Prim => TypeId::Prim(index),
            Type::Class => {
                let name = pool.definition_name(index)?;
                let ident = Ident::new(name.split('.').last().unwrap().to_owned());
                match self.symbols.find(&ident) {
                    Some(Symbol::Class(class_idx, _)) => TypeId::Class(*class_idx),
                    Some(Symbol::Struct(struct_idx, _)) => TypeId::Struct(*struct_idx),
                    Some(Symbol::Enum(enum_idx)) => TypeId::Enum(*enum_idx),
                    _ => return Err(Error::unresolved_type(ident, pos)),
                }
            }
            Type::Ref(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::Ref(Box::new(inner))
            }
            Type::WeakRef(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::WeakRef(Box::new(inner))
            }
            Type::Array(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::Array(Box::new(inner))
            }
            Type::StaticArray(type_, size) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::StaticArray(Box::new(inner), *size)
            }
            Type::ScriptRef(type_) => {
                let inner = self.resolve_type_from_pool(*type_, pool, pos)?;
                TypeId::ScriptRef(Box::new(inner))
            }
        };
        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Local(PoolIndex<Local>),
    Parameter(PoolIndex<Parameter>),
}

#[derive(Debug, Clone)]
pub enum Reference {
    Value(Value),
    Symbol(Symbol),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeId {
    Prim(PoolIndex<Type>),
    Class(PoolIndex<Class>),
    Struct(PoolIndex<Class>),
    Enum(PoolIndex<Enum>),
    Ref(Box<TypeId>),
    WeakRef(Box<TypeId>),
    Array(Box<TypeId>),
    StaticArray(Box<TypeId>, u32),
    ScriptRef(Box<TypeId>),
    Null,
    Void,
}

impl TypeId {
    pub fn unwrapped(&self) -> &TypeId {
        match self {
            TypeId::Ref(inner) => inner.unwrapped(),
            TypeId::WeakRef(inner) => inner.unwrapped(),
            TypeId::ScriptRef(inner) => inner.unwrapped(),
            other => other,
        }
    }

    fn repr(&self, pool: &ConstantPool) -> Result<Ident, Error> {
        match self {
            TypeId::Prim(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Class(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Struct(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Enum(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Ref(idx) => Ok(Ident::new(format!("ref:{}", idx.repr(pool)?))),
            TypeId::WeakRef(idx) => Ok(Ident::new(format!("wref:{}", idx.repr(pool)?))),
            TypeId::Array(idx) => Ok(Ident::new(format!("array:{}", idx.repr(pool)?))),
            TypeId::StaticArray(idx, size) => Ok(Ident::new(format!("{}[{}]", idx.repr(pool)?, size))),
            TypeId::ScriptRef(idx) => Ok(Ident::new(format!("script_ref:{}", idx.repr(pool)?))),
            TypeId::Null => Err(Error::PoolError("Null type".to_owned())),
            TypeId::Void => Err(Error::PoolError("Void type".to_owned())),
        }
    }

    pub fn pretty(&self, pool: &ConstantPool) -> Result<Ident, Error> {
        match self {
            TypeId::Prim(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Class(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Struct(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Enum(idx) => Ok(Ident::Owned(pool.definition_name(*idx)?)),
            TypeId::Ref(idx) => Ok(Ident::new(format!("ref<{}>", idx.pretty(pool)?))),
            TypeId::WeakRef(idx) => Ok(Ident::new(format!("wref<{}>", idx.pretty(pool)?))),
            TypeId::Array(idx) => Ok(Ident::new(format!("array<{}>", idx.pretty(pool)?))),
            TypeId::StaticArray(idx, size) => Ok(Ident::new(format!("array<{}, {}>", idx.pretty(pool)?, size))),
            TypeId::ScriptRef(idx) => Ok(Ident::new(format!("script_ref<{}>", idx.pretty(pool)?))),
            TypeId::Null => Ok(Ident::Static("Null")),
            TypeId::Void => Ok(Ident::Static("Void")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCandidates {
    pub functions: Vec<PoolIndex<Function>>,
}

impl FunctionCandidates {
    pub fn by_id(&self, fun_sig: &FunctionSignature, pool: &ConstantPool) -> Option<PoolIndex<Function>> {
        self.functions.iter().copied().find_map(|idx| {
            pool.definition_name(idx)
                .ok()
                .filter(|name| name.as_ref() == fun_sig.as_ref())
                .map(|_| idx)
        })
    }
}

#[derive(Debug)]
pub struct FunctionMatch {
    pub index: PoolIndex<Function>,
    pub args: Vec<Expr<TypedAst>>,
}
