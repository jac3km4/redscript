use redscript::ast::{Expr, Ident, Pos, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{Class, Definition, DefinitionValue, Enum, Field, Function, Local, Type};
use redscript::error::Error;

use crate::typechecker::TypedAst;
use crate::{FunctionId, Reference, TypeId};

#[derive(Debug, Clone)]
pub struct FunctionCandidates {
    pub functions: Vec<PoolIndex<Function>>,
}

impl FunctionCandidates {
    fn append(&mut self, other: &FunctionCandidates) {
        self.functions.extend(other.functions.iter());
    }

    pub fn by_id(&self, fun_id: &FunctionId, pool: &ConstantPool) -> Option<PoolIndex<Function>> {
        self.functions.iter().copied().find_map(|idx| {
            pool.definition_name(idx)
                .ok()
                .filter(|name| name.as_ref() == fun_id.as_ref())
                .map(|_| idx)
        })
    }
}

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
            .unwrap_or_else(|| self.name.to_string())
    }
}

#[derive(Debug)]
pub struct FunctionMatch {
    pub index: PoolIndex<Function>,
    pub args: Vec<Expr<TypedAst>>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub functions: im::HashMap<FunctionName, FunctionCandidates>,
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
                let ident = Ident::Owned(pool.definition_name(idx).ok()?);
                match def.value {
                    DefinitionValue::Class(ref class) if class.flags.is_struct() => {
                        Some((ident, Reference::Struct(idx.cast())))
                    }
                    DefinitionValue::Class(_) => Some((ident, Reference::Class(idx.cast()))),
                    DefinitionValue::Enum(_) => Some((ident, Reference::Enum(idx.cast()))),
                    _ => None,
                }
            })
            .collect();

        let types = pool
            .roots()
            .filter_map(|(idx, def)| match def.value {
                DefinitionValue::Type(_) => {
                    let ident = Ident::Owned(pool.definition_name(idx).ok()?);
                    Some((ident, idx.cast()))
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
                let ident = Ident::new(mangled_name.split(';').next().unwrap().to_owned());
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

    pub fn with_context(&self, this: Option<PoolIndex<Class>>, function: PoolIndex<Function>) -> Self {
        Scope {
            this,
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
            .and_modify(|overloads: &mut FunctionCandidates| overloads.functions.push(index))
            .or_insert_with(|| FunctionCandidates { functions: vec![index] });
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
        let err = format!("Field {} not found on {}", ident, pool.definition_name(class_idx)?);
        if class.base != PoolIndex::UNDEFINED {
            self.resolve_field(ident, class.base, pool, pos)
                .map_err(|_| Error::CompileError(err, pos))
        } else {
            Err(Error::CompileError(err, pos))
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
        let err = format!("Member {} not found on {}", ident, pool.definition_name(enum_idx)?);
        Err(Error::CompileError(err, pos))
    }

    pub fn resolve_function(
        &self,
        name: FunctionName,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<&FunctionCandidates, Error> {
        self.functions
            .get(&name)
            .ok_or_else(|| Error::CompileError(format!("Function {} not found", name.pretty(pool)), pos))
    }

    pub fn resolve_method(
        &self,
        name: Ident,
        class_idx: PoolIndex<Class>,
        pool: &ConstantPool,
        pos: Pos,
    ) -> Result<FunctionCandidates, Error> {
        let mut current_idx = class_idx;
        let mut candidates = FunctionCandidates { functions: vec![] };

        while current_idx != PoolIndex::UNDEFINED {
            let fun_name = FunctionName::instance(current_idx, name.clone());
            if let Ok(match_) = self.resolve_function(fun_name, pool, pos) {
                candidates.append(match_)
            }
            current_idx = pool.class(current_idx)?.base;
        }
        if candidates.functions.len() == 0 {
            let fun_name = FunctionName::instance(class_idx, name.clone());
            Err(Error::function_not_found(fun_name.pretty(pool), pos))
        } else {
            Ok(candidates)
        }
    }

    pub fn resolve_reference(&self, name: Ident, pos: Pos) -> Result<Reference, Error> {
        self.references
            .get(&name)
            .cloned()
            .ok_or_else(|| Error::CompileError(format!("Unresolved reference {}", name), pos))
    }

    pub fn get_type_index(&mut self, type_: &TypeId, pool: &mut ConstantPool) -> Result<PoolIndex<Type>, Error> {
        let name = type_.repr(pool)?;
        if let Some(type_idx) = self.types.get(&name) {
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
            let type_idx = pool.push_definition(Definition::type_(name_idx, value)).cast();
            self.types.insert(name, type_idx);
            Ok(type_idx)
        }
    }

    pub fn resolve_type(&self, name: &TypeName, pool: &ConstantPool, pos: Pos) -> Result<TypeId, Error> {
        let result = if let Some(res) = self.types.get(&name.repr()) {
            self.resolve_type_from_pool(*res, pool, pos)?
        } else {
            match (name.name.as_ref(), name.arguments.as_slice()) {
                ("ref", [nested]) => TypeId::Ref(Box::new(self.resolve_type(nested, pool, pos)?)),
                ("wref", [nested]) => TypeId::WeakRef(Box::new(self.resolve_type(nested, pool, pos)?)),
                ("script_ref", [nested]) => TypeId::ScriptRef(Box::new(self.resolve_type(nested, pool, pos)?)),
                ("array", [nested]) => TypeId::Array(Box::new(self.resolve_type(nested, pool, pos)?)),
                _ => match self.references.get(&name.repr()) {
                    Some(Reference::Class(idx)) => TypeId::Class(*idx),
                    Some(Reference::Struct(idx)) => TypeId::Struct(*idx),
                    Some(Reference::Enum(idx)) => TypeId::Enum(*idx),
                    _ => return Err(Error::CompileError(format!("Unresolved type {}", name), pos)),
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
                let ident = Ident::Owned(pool.definition_name(index)?);
                match self.references.get(&ident) {
                    Some(Reference::Class(class_idx)) => TypeId::Class(*class_idx),
                    Some(Reference::Struct(struct_idx)) => TypeId::Struct(*struct_idx),
                    Some(Reference::Enum(enum_idx)) => TypeId::Enum(*enum_idx),
                    _ => return Err(Error::CompileError(format!("Unresolved reference to {}", ident), pos)),
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
