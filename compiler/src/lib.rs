#![feature(option_result_contains)]
use core::panic;
use std::rc::Rc;

use assembler::Assembler;
use im::hashmap::Entry;
use parser::Annotation;
use redscript::ast::{Ident, Seq, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::Code;
use redscript::definition::{
    Class, ClassFlags, Enum, FieldFlags, FunctionFlags, Local, Parameter, ParameterFlags, SourceReference, Visibility,
};
use redscript::definition::{Definition, Field, Function, Type};
use redscript::error::Error;
use scope::{FunctionId, Scope};

use crate::parser::{ClassSource, Declaration, FunctionSource, MemberSource, Qualifier, SourceEntry};

pub mod assembler;
pub mod parser;
pub mod scope;

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
        let name_idx = self.pool.names.add(class.name.clone());
        let type_idx = self.pool.push_definition(Definition::type_(name_idx, Type::Class));
        let idx = self.pool.reserve().cast();
        let name = Ident(self.pool.names.get(name_idx)?);

        self.scope.types.insert(name.clone(), type_idx.cast());
        self.scope.names.insert(name, Reference::Class(idx));

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
        let name_idx = self.pool.names.add(source.name);
        let name = Ident(self.pool.names.get(name_idx)?);

        if let Reference::Class(class_idx) = self.scope.resolve(name)? {
            let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);
            let flags = ClassFlags::new();
            let mut functions = vec![];
            let mut fields = vec![];

            for member in source.members {
                match member {
                    MemberSource::Function(fun) => {
                        functions.push(self.define_function(fun, class_idx)?);
                    }
                    MemberSource::Field(field) => {
                        fields.push(self.define_field(field, class_idx)?);
                    }
                }
            }

            let base_idx = if let Some(base_name) = source.base {
                let base_ident = Ident::new(base_name);
                if let Reference::Class(base_idx) = self.scope.resolve(base_ident.clone())? {
                    base_idx
                } else {
                    Err(Error::CompileError(format!("{} is not a class", base_ident.0)))?
                }
            } else {
                PoolIndex::UNDEFINED
            };

            let class = Class {
                visibility,
                flags,
                base: base_idx,
                functions,
                fields,
                overrides: vec![],
            };
            self.pool
                .put_definition(class_idx.cast(), Definition::class(name_idx, class));
            Ok(())
        } else {
            panic!("Shouldn't get here")
        }
    }

    fn define_function(
        &mut self,
        source: FunctionSource,
        parent_idx: PoolIndex<Class>,
    ) -> Result<PoolIndex<Function>, Error> {
        let name_idx = self.pool.names.add(FunctionId::from_source(&source)?.mangled());
        let name = Ident(self.pool.names.get(name_idx)?);

        let decl = source.declaration;
        let (parent_idx, fun_idx) = self.determine_function_location(name, &decl.annotations, parent_idx)?;
        let visibility = decl.qualifiers.visibility().unwrap_or(Visibility::Private);
        let return_type = if decl.type_.name == "void" {
            None
        } else {
            Some(self.define_type(&decl.type_)?)
        };
        let flags = FunctionFlags::new()
            .with_is_static(decl.qualifiers.contain(Qualifier::Static))
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_final(decl.qualifiers.contain(Qualifier::Final))
            .with_is_exec(decl.qualifiers.contain(Qualifier::Exec));
        let mut parameters = Vec::new();

        for decl in &source.parameters {
            let type_ = self.define_type(&decl.type_)?;
            let flags = ParameterFlags::new();
            let param = Parameter { type_, flags };
            let name = self.pool.names.add(decl.name.clone());
            let idx = self
                .pool
                .push_definition(Definition::param(name, fun_idx.cast(), param));
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
        let definition = Definition::function(name_idx, parent_idx.cast(), function);
        self.pool.put_definition(fun_idx.cast(), definition);
        if let Some(seq) = source.body {
            self.backlog.push((parent_idx, fun_idx, seq))
        }
        Ok(fun_idx)
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

    fn determine_function_location(
        &mut self,
        name: Ident,
        annotations: &[Annotation],
        parent: PoolIndex<Class>,
    ) -> Result<(PoolIndex<Class>, PoolIndex<Function>), Error> {
        if let Some(target_name) = annotations.iter().find_map(|ann| ann.get_insert_target()) {
            if let Reference::Class(target_class) = self.scope.resolve(Ident::new(target_name.to_owned()))? {
                let class = self.pool.class(target_class)?;
                let existing_idx = class
                    .functions
                    .iter()
                    .find(|fun| self.pool.definition_name(**fun).unwrap() == name.0);
                if let Some(idx) = existing_idx {
                    Ok((target_class, *idx))
                } else {
                    Ok((target_class, self.pool.reserve().cast()))
                }
            } else {
                let error = format!("Can't find object {} to insert method in", name.0);
                Err(Error::CompileError(error))
            }
        } else {
            Ok((parent, self.pool.reserve().cast()))
        }
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
    Function(PoolIndex<Function>),
    Class(PoolIndex<Class>),
    Enum(PoolIndex<Enum>),
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
    pub fn unwrapped(&self) -> &TypeId {
        match self {
            TypeId::Ref(_, inner) => inner.unwrapped(),
            TypeId::WeakRef(_, inner) => inner.unwrapped(),
            TypeId::ScriptRef(_, inner) => inner.unwrapped(),
            other => other,
        }
    }

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
            "
            public class A {
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

    #[test]
    fn compile_ext_class() -> Result<(), Error> {
        let sources = parser::parse(
            "
            public class X {
                private const Int32 m_base_field;

                public Int32 BaseMethod() {
                    return this.m_base_field;
                }
            }

            public class Y extends X {
                public static Int32 CallBase() {
                  return this.BaseMethod();
                }
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool);
        compiler.compile(sources)
    }
}
