use redscript::ast::{Expr, Ident, Pos, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::definition::{
    AnyDefinition, Class, Definition, Enum, Field, Function, Local, Parameter, Type, Visibility
};
use redscript::error::Error;
use sequence_trie::SequenceTrie;

use crate::typechecker::TypedAst;
use crate::{FunctionSignature, Import, ModulePath, Reference, Symbol, TypeId, Value};

#[derive(Debug, Clone)]
pub struct Scope {
    symbols: im::HashMap<Ident, Symbol>,
    references: im::HashMap<Ident, Value>,
    types: im::HashMap<Ident, PoolIndex<Type>>,

    pub this: Option<PoolIndex<Class>>,
    pub function: Option<PoolIndex<Function>>,
}

impl Scope {
    pub fn new(pool: &ConstantPool) -> Result<Self, Error> {
        let types = pool
            .roots()
            .filter_map(|(idx, def)| match def.value {
                AnyDefinition::Type(_) => {
                    let ident = Ident::Owned(pool.definition_name(idx).ok()?);
                    Some((ident, idx.cast()))
                }
                _ => None,
            })
            .collect();

        let result = Scope {
            symbols: im::HashMap::new(),
            references: im::HashMap::new(),
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
        self.references.insert(name, Value::Local(local));
    }

    pub fn add_parameter(&mut self, name: Ident, param: PoolIndex<Parameter>) {
        self.references.insert(name, Value::Parameter(param));
    }

    pub fn add_type(&mut self, name: Ident, typ: PoolIndex<Type>) {
        self.types.insert(name, typ);
    }

    pub fn add_symbol(&mut self, name: Ident, symbol: Symbol) {
        match symbol {
            Symbol::Functions(funs) => match self.symbols.get_mut(&name) {
                Some(Symbol::Functions(existing)) => existing.extend(funs),
                _ => {
                    self.symbols.insert(name.clone(), Symbol::Functions(funs));
                }
            },
            _ => {
                self.symbols.insert(name.clone(), symbol);
            }
        }
    }

    pub fn resolve_function(&self, name: Ident, pos: Pos) -> Result<FunctionCandidates, Error> {
        if let Some(Symbol::Functions(functions)) = self.symbols.get(&name) {
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
            .get(&name)
            .cloned()
            .ok_or_else(|| Error::unresolved_reference(name, pos))
    }

    pub fn resolve_symbol(&self, name: Ident, pos: Pos) -> Result<Symbol, Error> {
        self.symbols
            .get(&name)
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
            let type_idx = pool.add_definition(Definition::type_(name_idx, value)).cast();
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
                _ => match self.symbols.get(&name.repr()) {
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
                match self.symbols.get(&ident) {
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

pub struct SymbolMap {
    symbols: SequenceTrie<Ident, Symbol>,
}

impl SymbolMap {
    pub fn new(pool: &ConstantPool) -> Result<SymbolMap, Error> {
        let mut symbols: SequenceTrie<Ident, Symbol> = SequenceTrie::new();

        for (idx, def) in pool.roots() {
            let name = pool.definition_name(idx)?;
            let symbol = match def.value {
                AnyDefinition::Class(ref class) if class.flags.is_struct() => {
                    Symbol::Struct(idx.cast(), class.visibility)
                }
                AnyDefinition::Class(ref class) => Symbol::Class(idx.cast(), class.visibility),
                AnyDefinition::Enum(_) => Symbol::Enum(idx.cast()),
                AnyDefinition::Function(ref fun) => Symbol::Functions(vec![(idx.cast(), fun.visibility)]),
                _ => continue,
            };
            let path = ModulePath::parse(&name);
            match (symbols.get_mut(&path), symbol) {
                (Some(Symbol::Functions(existing)), Symbol::Functions(new)) => {
                    existing.extend(new);
                }
                (_, symbol) => {
                    symbols.insert(&path, symbol);
                }
            }
        }

        Ok(SymbolMap { symbols })
    }

    pub fn add_class(&mut self, path: &ModulePath, class: PoolIndex<Class>, visibility: Visibility) {
        self.symbols.insert(path, Symbol::Class(class, visibility));
    }

    pub fn add_enum(&mut self, path: &ModulePath, enum_: PoolIndex<Enum>) {
        self.symbols.insert(path, Symbol::Enum(enum_));
    }

    pub fn add_function(&mut self, path: &ModulePath, index: PoolIndex<Function>, visibility: Visibility) {
        match self.symbols.get_mut(path) {
            Some(Symbol::Functions(existing)) => {
                existing.push((index, visibility));
            }
            _ => {
                self.symbols.insert(path, Symbol::Functions(vec![(index, visibility)]));
            }
        }
    }

    pub fn populate_import(&self, import: Import, scope: &mut Scope, visibility: Visibility) -> Result<(), Error> {
        match import {
            Import::Exact(path, pos) => {
                if let Some(symbol) = self.get_symbol(&path, pos)?.visible(visibility) {
                    scope.add_symbol(path.last().unwrap(), symbol);
                }
            }
            Import::All(path, pos) => {
                for (ident, symbol) in self.get_direct_children(&path, pos)? {
                    if let Some(symbol) = symbol.clone().visible(visibility) {
                        scope.add_symbol(ident, symbol.clone());
                    }
                }
            }
            Import::Selected(path, names, pos) => {
                for name in names {
                    let path = path.with_child(name);
                    if let Some(symbol) = self.get_symbol(&path, pos)?.visible(visibility) {
                        scope.add_symbol(path.last().unwrap(), symbol);
                    }
                }
            }
        };
        Ok(())
    }

    fn get_symbol(&self, path: &ModulePath, pos: Pos) -> Result<Symbol, Error> {
        self.symbols
            .get(path)
            .cloned()
            .ok_or_else(|| Error::unresolved_import(path.render(), pos))
    }

    fn get_direct_children(
        &self,
        path: &ModulePath,
        pos: Pos,
    ) -> Result<impl Iterator<Item = (Ident, &Symbol)>, Error> {
        let node = self
            .symbols
            .get_node(path)
            .ok_or_else(|| Error::unresolved_import(path.render(), pos))?;
        let res = node
            .iter()
            .filter(|(parts, _)| parts.len() == 1)
            .map(|(mut parts, sym)| (parts.pop().unwrap().clone(), sym));
        Ok(res)
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
