#![feature(option_result_contains)]

use assembler::Assembler;
use parser::Annotation;
use redscript::ast::{Ident, Seq, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::{Code, Instr};
use redscript::definition::{
    Class, ClassFlags, Enum, FieldFlags, FunctionFlags, Local, Parameter, ParameterFlags, SourceReference, Visibility,
};
use redscript::definition::{Definition, Field, Function, Type};
use redscript::error::Error;
use scope::{FunctionId, FunctionName, Scope};

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
    pub fn new(pool: &'a mut ConstantPool) -> Result<Compiler<'a>, Error> {
        let scope = Scope::new(pool)?;
        let backlog = Vec::new();
        Ok(Compiler { pool, scope, backlog })
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
        self.scope.references.insert(name, Reference::Class(idx));

        Ok(())
    }

    fn define_class(&mut self, source: ClassSource) -> Result<(), Error> {
        let name_idx = self.pool.names.add(source.name);
        let name = Ident(self.pool.names.get(name_idx)?);

        if let Reference::Class(class_idx) = self.scope.resolve_reference(name)? {
            let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);
            let flags = ClassFlags::new();
            let mut functions = vec![];
            let mut fields = vec![];

            for member in source.members {
                match member {
                    MemberSource::Function(fun) => {
                        functions.push(self.define_function(fun, class_idx)?);
                    }
                    MemberSource::Field(field, type_) => {
                        fields.push(self.define_field(field, type_, class_idx)?);
                    }
                }
            }

            let base_idx = if let Some(base_name) = source.base {
                let base_ident = Ident::new(base_name);
                if let Reference::Class(base_idx) = self.scope.resolve_reference(base_ident.clone())? {
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
        let decl = &source.declaration;
        let flags = FunctionFlags::new()
            .with_is_static(decl.qualifiers.contain(Qualifier::Static))
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_final(decl.qualifiers.contain(Qualifier::Final))
            .with_is_exec(decl.qualifiers.contain(Qualifier::Exec))
            .with_is_callback(decl.qualifiers.contain(Qualifier::Callback));

        let fun_id = FunctionId::from_source(&source)?;
        let ident = Ident::new(decl.name.clone());
        let name = if parent_idx.is_undefined() {
            FunctionName::global(ident)
        } else {
            FunctionName::instance(parent_idx, ident)
        };
        let (parent_idx, base_method, fun_idx) =
            self.determine_function_location(&fun_id, &decl.annotations, parent_idx)?;
        let name_idx = self.pool.names.add(fun_id.mangled());

        let visibility = decl.qualifiers.visibility().unwrap_or(Visibility::Private);
        let return_type = match source.type_ {
            None => None,
            Some(type_) => Some(self.scope.resolve_type_index(Ident::new(type_.repr()))?),
        };

        let mut parameters = Vec::new();

        for param in &source.parameters {
            let type_ = self.scope.resolve_type_index(Ident::new(param.type_.repr()))?;
            let flags = ParameterFlags::new()
                .with_is_out(param.declaration.qualifiers.contain(Qualifier::Out))
                .with_is_optional(param.declaration.qualifiers.contain(Qualifier::Optional));
            let name = self.pool.names.add(param.declaration.name.clone());
            let param = Parameter { type_, flags };
            let idx = self
                .pool
                .push_definition(Definition::param(name, fun_idx.cast(), param));
            parameters.push(idx.cast());
        }

        let source_ref = SourceReference {
            file: PoolIndex::DEFAULT_SOURCE,
            line: 0,
        };

        let function = Function {
            visibility,
            flags,
            source: Some(source_ref),
            return_type,
            unk1: false,
            base_method,
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
        self.scope.push_function(name, fun_idx);
        Ok(fun_idx)
    }

    fn define_field(
        &mut self,
        field: Declaration,
        type_: TypeName,
        parent: PoolIndex<Class>,
    ) -> Result<PoolIndex<Field>, Error> {
        let name = self.pool.names.add(field.name);
        let visibility = field.qualifiers.visibility().unwrap_or(Visibility::Private);
        let type_ = self.scope.resolve_type_index(Ident::new(type_.repr()))?;
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
        name: &FunctionId,
        annotations: &[Annotation],
        parent: PoolIndex<Class>,
    ) -> Result<(PoolIndex<Class>, Option<PoolIndex<Function>>, PoolIndex<Function>), Error> {
        if let Some(target_name) = annotations.iter().find_map(|ann| ann.get_insert_target()) {
            if let Reference::Class(target_class) = self.scope.resolve_reference(Ident::new(target_name.to_owned()))? {
                let class = self.pool.class(target_class)?;
                let existing_idx = class.functions.iter().find(|fun| {
                    let str = self.pool.definition_name(**fun).unwrap();
                    str.as_str() == name.mangled() || str.as_str() == name.0
                });
                if let Some(idx) = existing_idx {
                    let fun = self.pool.function(*idx)?;
                    Ok((target_class, fun.base_method, *idx))
                } else {
                    let error = format!("Method {} not found on {}", name.0, target_name);
                    Err(Error::CompileError(error))
                }
            } else {
                let error = format!("Can't find object {} to insert method in", name.0);
                Err(Error::CompileError(error))
            }
        } else {
            Ok((parent, None, self.pool.reserve().cast()))
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
            scope.references.insert(ident, Reference::Parameter(*param));
        }

        let assembler = Assembler::from_seq(&seq, pool, &mut scope)?;
        let function = pool.function_mut(fun_idx)?;
        function.code = Code(assembler.code.into_iter().collect());
        function.code.0.push(Instr::Nop);
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeId {
    Prim(PoolIndex<Type>),
    Class(Option<PoolIndex<Type>>, PoolIndex<Class>),
    Struct(PoolIndex<Type>, PoolIndex<Class>),
    Enum(PoolIndex<Type>, PoolIndex<Enum>),
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

    fn index(&self) -> Option<PoolIndex<Type>> {
        match self {
            TypeId::Prim(idx) => Some(*idx),
            TypeId::Class(idx, _) => *idx,
            TypeId::Enum(idx, _) => Some(*idx),
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

    fn pretty(&self, pool: &ConstantPool) -> Result<Ident, Error> {
        match self {
            TypeId::Prim(idx) => Ok(Ident(pool.definition_name(*idx)?)),
            TypeId::Class(_, idx) => Ok(Ident(pool.definition_name(*idx)?)),
            TypeId::Struct(_, idx) => Ok(Ident(pool.definition_name(*idx)?)),
            TypeId::Enum(_, idx) => Ok(Ident(pool.definition_name(*idx)?)),
            TypeId::Ref(_, idx) => Ok(Ident::new(format!("ref<{}>", idx.pretty(pool)?.0))),
            TypeId::WeakRef(_, idx) => Ok(Ident::new(format!("wref<{}>", idx.pretty(pool)?.0))),
            TypeId::Array(_, idx) => Ok(Ident::new(format!("array<{}>", idx.pretty(pool)?.0))),
            TypeId::StaticArray(_, idx, size) => Ok(Ident::new(format!("array<{}, {}>", idx.pretty(pool)?.0, size))),
            TypeId::ScriptRef(_, idx) => Ok(Ident::new(format!("ref<{}>", idx.pretty(pool)?.0))),
            TypeId::Null => Ok(Ident::new("null".to_owned())),
            TypeId::Void => Ok(Ident::new("void".to_owned())),
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
                private const m_field: Int32;

                public DoStuff(fieldOrNot: Bool): Int32 {
                    return fieldOrNot ? this.m_field : A.Ten();
                }

                public static Ten(): Int32 {
                  return 10;
                }
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile(sources)
    }

    #[test]
    fn compile_ext_class() -> Result<(), Error> {
        let sources = parser::parse(
            "
            public class X {
                private const m_base_field: Int32;

                public BaseMethod(): Int32 {
                    return this.m_base_field;
                }
            }

            public class Y extends X {
                public static CallBase(): Int32 {
                  return this.BaseMethod();
                }
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile(sources)
    }
}
