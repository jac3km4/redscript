#![feature(option_result_contains)]

use std::borrow::Cow;
use std::collections::HashSet;
use std::rc::Rc;

use assembler::Assembler;
use parser::{Annotation, AnnotationName, FieldSource};
use redscript::ast::{Ident, Pos, Seq, SourceAst};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::Code;
use redscript::definition::{
    Class, ClassFlags, Definition, Enum, Field, FieldFlags, Function, FunctionFlags, Local, Parameter, ParameterFlags, SourceReference, Type, Visibility
};
use redscript::error::Error;
use scope::{FunctionName, Scope};
use source_map::Files;
use sugar::Desugar;
use transform::ExprTransformer;
use typechecker::Typechecker;

use crate::parser::{ClassSource, FunctionSource, MemberSource, Qualifier, SourceEntry};

pub mod assembler;
pub mod parser;
pub mod scope;
pub mod source_map;
pub mod sugar;
pub mod transform;
pub mod typechecker;

pub struct Compiler<'a> {
    pool: &'a mut ConstantPool,
    backlog: Vec<BacklogItem>,
    scope: Scope,
}

impl<'a> Compiler<'a> {
    pub fn new(pool: &'a mut ConstantPool) -> Result<Compiler<'a>, Error> {
        let scope = Scope::new(pool)?;
        let backlog = Vec::new();
        Ok(Compiler { pool, backlog, scope })
    }

    pub fn compile(&mut self, files: &Files) -> Result<(), Error> {
        log::info!("Compiling files: {}", files);

        match self.try_compile(files) {
            Ok(diagnostics) => {
                for diagnostic in diagnostics {
                    Self::print_diagnostic(&files, diagnostic);
                }
                log::info!("Compilation complete");
                Ok(())
            }
            Err(Error::CompileError(err, pos)) => {
                Self::print_error(files, &err, pos);
                Err(Error::CompileError(err, pos))
            }
            Err(Error::SyntaxError(err, pos)) => {
                Self::print_error(files, &err, pos);
                Err(Error::SyntaxError(err, pos))
            }
            Err(other) => {
                log::error!("{}: {:?}", "Unexpected error during compilation", other);
                Err(other)
            }
        }
    }

    fn try_compile(&mut self, files: &Files) -> Result<Vec<Diagnostic>, Error> {
        let entries = parser::parse(files.sources()).map_err(|err| {
            let message = format!("Syntax error, expected {}", err.expected);
            Error::SyntaxError(message, Pos::new(err.location.offset))
        })?;

        self.compile_entries(entries)
    }

    fn print_diagnostic(files: &Files, diagnostic: Diagnostic) {
        match diagnostic {
            Diagnostic::MethodConflict(_, pos) => {
                let loc = files.lookup(pos).unwrap();
                log::warn!("Conflicting method replacement at {}", loc)
            }
        }
    }

    fn print_error(files: &Files, error: &str, pos: Pos) {
        let loc = files.lookup(pos).unwrap();
        let line = files.enclosing_line(&loc).trim_end();
        log::error!(
            "Failed at {}:\n \
             {}\n \
             {}^^^\n \
             {}",
            loc,
            line,
            " ".repeat(loc.position.col),
            error
        );
    }

    fn compile_entries(&mut self, sources: Vec<SourceEntry>) -> Result<Vec<Diagnostic>, Error> {
        let mut compiled_funs = HashSet::new();
        let mut diagnostics = Vec::new();

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
                    let pos = fun.declaration.pos;
                    let idx = self.define_function(fun, PoolIndex::UNDEFINED)?;
                    if compiled_funs.contains(&idx) {
                        diagnostics.push(Diagnostic::MethodConflict(idx, pos));
                    } else {
                        compiled_funs.insert(idx);
                    }
                }
                SourceEntry::GlobalLet(let_) => {
                    self.define_global_let(let_)?;
                }
            }
        }
        for item in self.backlog.drain(..) {
            Self::compile_function(item.function, item.class, &item.code, &self.scope, self.pool)?;
        }
        Ok(diagnostics)
    }

    fn stub_type(&mut self, class: &ClassSource) -> Result<(), Error> {
        let name_idx = self.pool.names.add(Rc::new(class.name.clone()));
        let type_idx = self.pool.push_definition(Definition::type_(name_idx, Type::Class));
        let class_idx = self.pool.push_definition(Definition::type_(name_idx, Type::Class));
        let name = Ident::Owned(self.pool.names.get(name_idx)?);

        self.scope.types.insert(name.clone(), type_idx.cast());
        self.scope.references.insert(name, Reference::Class(class_idx.cast()));

        Ok(())
    }

    fn define_class(&mut self, source: ClassSource) -> Result<(), Error> {
        let name_idx = self.pool.names.add(Rc::new(source.name));
        let name = Ident::Owned(self.pool.names.get(name_idx)?);

        if let Reference::Class(class_idx) = self.scope.resolve_reference(name, source.pos)? {
            let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);
            let flags = ClassFlags::new();
            let mut functions = vec![];
            let mut fields = vec![];

            for member in source.members {
                match member {
                    MemberSource::Function(fun) => {
                        functions.push(self.define_function(fun, class_idx)?);
                    }
                    MemberSource::Field(let_) => {
                        fields.push(self.define_field(let_, class_idx)?);
                    }
                }
            }

            let base_idx = if let Some(base_name) = source.base {
                let base_ident = Ident::new(base_name);
                if let Reference::Class(base_idx) = self.scope.resolve_reference(base_ident.clone(), source.pos)? {
                    base_idx
                } else {
                    return Err(Error::CompileError(
                        format!("{} is not a class", base_ident),
                        source.pos,
                    ));
                }
            } else if let Ok(Reference::Class(class)) =
                self.scope.resolve_reference(Ident::Static("IScriptable"), source.pos)
            {
                class
            } else {
                log::warn!("No IScriptable in scope, defaulting to no implicit base class");
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
        let fun_id = FunctionId::from_source(&source);
        let ident = Ident::new(decl.name.clone());

        let (parent_idx, base_method, fun_idx) =
            self.determine_function_location(&fun_id, ident.clone(), &decl.annotations, parent_idx)?;

        let name_idx = if let Some(fun) = self
            .pool
            .definition(fun_idx)
            .ok()
            .filter(|d| d.name != PoolIndex::UNDEFINED)
        {
            fun.name
        } else {
            self.pool.names.add(Rc::new(fun_id.into_owned()))
        };

        let name = if parent_idx.is_undefined() {
            FunctionName::global(ident)
        } else {
            FunctionName::instance(parent_idx, ident)
        };

        let flags = FunctionFlags::new()
            .with_is_static(decl.qualifiers.contain(Qualifier::Static) || parent_idx == PoolIndex::UNDEFINED)
            .with_is_final(decl.qualifiers.contain(Qualifier::Final))
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_exec(decl.qualifiers.contain(Qualifier::Exec))
            .with_is_callback(decl.qualifiers.contain(Qualifier::Callback));

        let visibility = decl
            .qualifiers
            .visibility()
            .unwrap_or(if parent_idx == PoolIndex::UNDEFINED {
                Visibility::Public
            } else {
                Visibility::Private
            });

        let return_type = match source.type_ {
            None => None,
            Some(type_) if type_.name.as_ref() == "Void" => None,
            Some(type_) => {
                let type_ = self.scope.resolve_type(&type_, self.pool, decl.pos)?;
                Some(self.scope.get_type_index(&type_, self.pool)?)
            }
        };

        let mut parameters = Vec::new();

        for param in &source.parameters {
            let type_ = self.scope.resolve_type(&param.type_, self.pool, decl.pos)?;
            let type_idx = self.scope.get_type_index(&type_, self.pool)?;
            let flags = ParameterFlags::new()
                .with_is_out(param.qualifiers.contain(Qualifier::Out))
                .with_is_optional(param.qualifiers.contain(Qualifier::Optional));
            let name = self.pool.names.add(Rc::new(param.name.clone()));
            let param = Parameter { type_: type_idx, flags };
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
            base_method: base_method.filter(|_| !flags.is_static()),
            parameters,
            locals: vec![],
            operator: None,
            cast: 0,
            code: Code::EMPTY,
        };
        let definition = Definition::function(name_idx, parent_idx.cast(), function);
        self.pool.put_definition(fun_idx.cast(), definition);
        if let Some(code) = source.body {
            let item = BacklogItem {
                class: parent_idx,
                function: fun_idx,
                code,
            };
            self.backlog.push(item)
        }
        self.scope.push_function(name, fun_idx);
        Ok(fun_idx)
    }

    fn define_field(&mut self, source: FieldSource, parent: PoolIndex<Class>) -> Result<PoolIndex<Field>, Error> {
        let decl = source.declaration;
        let name = self.pool.names.add(Rc::new(decl.name));
        let visibility = decl.qualifiers.visibility().unwrap_or(Visibility::Private);
        let type_ = self.scope.resolve_type(&source.type_, self.pool, decl.pos)?;
        let type_idx = self.scope.get_type_index(&type_, self.pool)?;
        let flags = FieldFlags::new().with_is_mutable(true);
        let field = Field {
            visibility,
            type_: type_idx,
            flags,
            hint: None,
            attributes: vec![],
            defaults: vec![],
        };
        let definition = Definition::field(name, parent.cast(), field);
        let idx = self.pool.push_definition(definition).cast();
        Ok(idx)
    }

    fn define_global_let(&mut self, source: FieldSource) -> Result<(), Error> {
        let decl = &source.declaration;
        for ann in &source.declaration.annotations {
            match ann.name {
                AnnotationName::AddField => {
                    let value = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let ident = Ident::new(value.clone());
                    if let Reference::Class(target_class) = self.scope.resolve_reference(ident.clone(), ann.pos)? {
                        let idx = self.define_field(source, target_class)?;
                        self.pool.class_mut(target_class)?.fields.push(idx);
                        return Ok(());
                    } else {
                        return Err(Error::class_not_found(ident.as_ref(), ann.pos));
                    }
                }
                _ => {}
            }
        }
        Err(Error::CompileError(
            "Global let binding not allowed".to_owned(),
            decl.pos,
        ))
    }

    fn determine_function_location(
        &mut self,
        name: &FunctionId,
        ident: Ident,
        annotations: &[Annotation],
        parent: PoolIndex<Class>,
    ) -> Result<(PoolIndex<Class>, Option<PoolIndex<Function>>, PoolIndex<Function>), Error> {
        for ann in annotations {
            match ann.name {
                AnnotationName::ReplaceMethod => {
                    let value = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let ident = Ident::new(value.clone());
                    if let Reference::Class(target_class) = self.scope.resolve_reference(ident, ann.pos)? {
                        let class = self.pool.class(target_class)?;
                        let idx = class
                            .functions
                            .iter()
                            .find(|fun| self.pool.definition_name(**fun).unwrap().as_str() == name.as_ref())
                            .ok_or_else(|| Error::method_not_found(name.as_ref(), value, ann.pos))?;
                        let fun = self.pool.function(*idx)?;
                        return Ok((target_class, fun.base_method, *idx));
                    } else {
                        return Err(Error::class_not_found(value, ann.pos));
                    }
                }
                AnnotationName::ReplaceGlobal => {
                    let fun_name = FunctionName::global(ident);
                    let overloads = self
                        .scope
                        .functions
                        .get(&fun_name)
                        .ok_or_else(|| Error::global_not_found(&fun_name.pretty(self.pool), ann.pos))?;
                    let fun_idx = overloads
                        .functions
                        .iter()
                        .find(|f| self.pool.definition_name(**f).unwrap().as_str() == name.as_ref())
                        .ok_or_else(|| Error::global_not_found(&fun_name.pretty(self.pool), ann.pos))?;
                    return Ok((PoolIndex::UNDEFINED, None, *fun_idx));
                }
                AnnotationName::AddMethod => {
                    let value = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let ident = Ident::new(value.clone());
                    if let Reference::Class(target_class) = self.scope.resolve_reference(ident, ann.pos)? {
                        let class = self.pool.class(target_class)?;
                        let base_method = if class.base != PoolIndex::UNDEFINED {
                            let base = self.pool.class(class.base)?;
                            base.functions
                                .iter()
                                .find(|fun| self.pool.definition_name(**fun).unwrap().as_str() == name.as_ref())
                                .cloned()
                        } else {
                            None
                        };

                        let idx = self.pool.reserve().cast();
                        self.pool.class_mut(target_class)?.functions.push(idx);
                        return Ok((target_class, base_method, idx));
                    } else {
                        return Err(Error::class_not_found(&value, ann.pos));
                    }
                }
                AnnotationName::AddField => {}
            }
        }

        Ok((parent, None, self.pool.reserve().cast()))
    }

    fn compile_function(
        fun_idx: PoolIndex<Function>,
        class_idx: PoolIndex<Class>,
        seq: &Seq<SourceAst>,
        scope: &Scope,
        pool: &mut ConstantPool,
    ) -> Result<(), Error> {
        let fun = pool.function(fun_idx)?;
        let mut local_scope = if fun.flags.is_static() {
            scope.with_context(None, fun_idx)
        } else {
            scope.with_context(Some(class_idx), fun_idx)
        };

        for param in &fun.parameters {
            let ident = Ident::Owned(pool.definition_name(*param)?);
            local_scope.references.insert(ident, Reference::Parameter(*param));
        }

        let mut checker = Typechecker::new(pool);
        let checked = checker.check_seq(seq, &mut local_scope)?;
        let mut locals = checker.locals;

        let mut desugar = Desugar::new(&mut local_scope, pool);
        let desugared = desugar.on_seq(checked)?;
        locals.append(&mut desugar.locals);

        let code = Assembler::from_body(desugared, &mut local_scope, pool)?;
        let function = pool.function_mut(fun_idx)?;
        function.code = code;
        function.locals = locals;
        Ok(())
    }
}

#[derive(Debug)]
pub struct BacklogItem {
    class: PoolIndex<Class>,
    function: PoolIndex<Function>,
    code: Seq<SourceAst>,
}

#[derive(Debug)]
pub enum Diagnostic {
    MethodConflict(PoolIndex<Function>, Pos),
}

#[derive(Debug, Clone)]
pub enum Reference {
    Local(PoolIndex<Local>),
    Parameter(PoolIndex<Parameter>),
    Class(PoolIndex<Class>),
    Struct(PoolIndex<Class>),
    Enum(PoolIndex<Enum>),
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
            TypeId::Null => panic!(),
            TypeId::Void => panic!(),
        }
    }

    fn pretty(&self, pool: &ConstantPool) -> Result<Ident, Error> {
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

pub struct FunctionId<'a>(Cow<'a, str>);

impl<'a> FunctionId<'a> {
    pub fn from_source(source: &'a FunctionSource) -> Self {
        let qs = &source.declaration.qualifiers;
        if qs.contain(Qualifier::Callback) || qs.contain(Qualifier::Exec) || qs.contain(Qualifier::Native) {
            FunctionId(Cow::Borrowed(&source.declaration.name))
        } else {
            let mut signature = String::new();
            for arg in &source.parameters {
                signature.push_str(arg.type_.mangled().as_ref());
            }
            let mangled = format!("{};{}", source.declaration.name, signature);
            FunctionId(Cow::Owned(mangled))
        }
    }

    pub fn into_owned(self) -> String {
        self.0.into_owned()
    }
}

impl<'a> AsRef<str> for FunctionId<'a> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use redscript::bundle::{PoolIndex, ScriptBundle};
    use redscript::bytecode::{Code, Instr, Offset};
    use redscript::definition::DefinitionValue;
    use redscript::error::Error;

    use crate::{parser, Compiler};

    const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

    #[test]
    fn compile_simple_class() -> Result<(), Error> {
        let sources = parser::parse(
            "
            public class A {
                private const let m_field: Int32;

                public func DoStuff(fieldOrNot: Bool) -> Int32 {
                    return fieldOrNot ? this.m_field : A.Ten();
                }

                public static func Ten() -> Int32 {
                    return 10;
                }
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile_entries(sources)?;
        Ok(())
    }

    #[test]
    fn compile_ext_class() -> Result<(), Error> {
        let sources = parser::parse(
            "
            public class X {
                private const let m_base_field: Int32;

                public func BaseMethod() -> Int32 {
                    return this.m_base_field;
                }
            }

            public class Y extends X {
                public func CallBase() -> Int32 {
                  return this.BaseMethod();
                }
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile_entries(sources)?;
        Ok(())
    }

    #[test]
    fn compile_class_with_forward_ref() -> Result<(), Error> {
        let sources = parser::parse(
            "
            public class MyTestClass456 {
                public let myOtherTestClass: ref<MyTestClass123>;

                public func DoStuff() -> ref<MyTestClass123> {
                    return this.myOtherTestClass;
                }
            }
            
            public class MyTestClass123 {
                public let myTestVar: String;
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile_entries(sources)?;
        Ok(())
    }

    #[test]
    fn compile_class_with_shorthand_funcs() -> Result<(), Error> {
        let sources = parser::parse(
            "
            public class ShorthandTest {
                public func InstanceVal() -> String = ShorthandTest.StaticVal()
                public static func StaticVal() -> String = \"static\"
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile_entries(sources)?;
        Ok(())
    }

    #[test]
    fn compile_dynamic_casts() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let b: wref<B> = new B();
                let a: wref<A> = b as A;
            }

            class A {}
            class B extends A {}
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(27)),
            Instr::RefToWeakRef,
            Instr::New(PoolIndex::new(24)),
            Instr::Assign,
            Instr::Local(PoolIndex::new(29)),
            Instr::RefToWeakRef,
            Instr::DynamicCast(PoolIndex::new(22), 0),
            Instr::WeakRefToRef,
            Instr::Local(PoolIndex::new(27)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_basic_casts() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let a: Float = Cast(1);
                let b: String = Cast(2);
            }

            func Cast(i: Int32) -> Float = 0.0
            func Cast(i: Int32) -> String = \"\"
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(26)),
            Instr::InvokeStatic(Offset::new(19), 0, PoolIndex::new(22)),
            Instr::I32Const(1),
            Instr::ParamEnd,
            Instr::Assign,
            Instr::Local(PoolIndex::new(27)),
            Instr::InvokeStatic(Offset::new(19), 0, PoolIndex::new(24)),
            Instr::I32Const(2),
            Instr::ParamEnd,
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_overloaded_call() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let val = new B();
                TestingTarget(val, val);
            }

            func TestingTarget(x: wref<A>, y: ref<B>) {}
            func TestingTarget(x: wref<A>, y: ref<C>) {}

            class A {}
            class B extends A {}
            class C extends A {}
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(37)),
            Instr::New(PoolIndex::new(24)),
            Instr::InvokeStatic(Offset::new(33), 0, PoolIndex::new(28)),
            Instr::RefToWeakRef,
            Instr::Local(PoolIndex::new(37)),
            Instr::Local(PoolIndex::new(37)),
            Instr::ParamEnd,
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_for_loop() -> Result<(), Error> {
        let sources = "
            func Testing() {
                for i in [0, 1] {
                    Log(ToString(i));
                }
            }

            func Log(str: String) {}
            func OperatorAssignAdd(l: Int32, r: Int32) -> Int32 = 0
            func OperatorLess(l: Int32, r: Int32) -> Bool = true
            ";
        let expected = vec![
            Instr::ArrayPush(PoolIndex::new(31)),
            Instr::Local(PoolIndex::new(32)),
            Instr::I32Const(0),
            Instr::ArrayPush(PoolIndex::new(31)),
            Instr::Local(PoolIndex::new(32)),
            Instr::I32Const(1),
            Instr::Assign,
            Instr::Local(PoolIndex::new(33)),
            Instr::Local(PoolIndex::new(32)),
            Instr::Assign,
            Instr::Local(PoolIndex::new(34)),
            Instr::I32Const(0),
            Instr::JumpIfFalse(Offset::new(144)),
            Instr::InvokeStatic(Offset::new(41), 0, PoolIndex::new(27)),
            Instr::Local(PoolIndex::new(34)),
            Instr::ArraySize(PoolIndex::new(31)),
            Instr::Local(PoolIndex::new(33)),
            Instr::ParamEnd,
            Instr::Assign,
            Instr::Local(PoolIndex::new(30)),
            Instr::ArrayElement(PoolIndex::new(31)),
            Instr::Local(PoolIndex::new(33)),
            Instr::Local(PoolIndex::new(34)),
            Instr::InvokeStatic(Offset::new(32), 0, PoolIndex::new(22)),
            Instr::ToString(PoolIndex::new(8)),
            Instr::Local(PoolIndex::new(30)),
            Instr::ParamEnd,
            Instr::InvokeStatic(Offset::new(28), 0, PoolIndex::new(24)),
            Instr::Local(PoolIndex::new(34)),
            Instr::I32Const(1),
            Instr::ParamEnd,
            Instr::Jump(Offset::new(-141)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_nested_array_literals() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let x = [[1, 2], [3, 4], [5, 6]];
            }
            ";
        let expected = vec![
            Instr::ArrayPush(PoolIndex::new(22)),
            Instr::Local(PoolIndex::new(26)),
            Instr::I32Const(1),
            Instr::ArrayPush(PoolIndex::new(22)),
            Instr::Local(PoolIndex::new(26)),
            Instr::I32Const(2),
            Instr::ArrayPush(PoolIndex::new(23)),
            Instr::Local(PoolIndex::new(25)),
            Instr::Local(PoolIndex::new(26)),
            Instr::ArrayPush(PoolIndex::new(22)),
            Instr::Local(PoolIndex::new(27)),
            Instr::I32Const(3),
            Instr::ArrayPush(PoolIndex::new(22)),
            Instr::Local(PoolIndex::new(27)),
            Instr::I32Const(4),
            Instr::ArrayPush(PoolIndex::new(23)),
            Instr::Local(PoolIndex::new(25)),
            Instr::Local(PoolIndex::new(27)),
            Instr::ArrayPush(PoolIndex::new(22)),
            Instr::Local(PoolIndex::new(28)),
            Instr::I32Const(5),
            Instr::ArrayPush(PoolIndex::new(22)),
            Instr::Local(PoolIndex::new(28)),
            Instr::I32Const(6),
            Instr::ArrayPush(PoolIndex::new(23)),
            Instr::Local(PoolIndex::new(25)),
            Instr::Local(PoolIndex::new(28)),
            Instr::Assign,
            Instr::Local(PoolIndex::new(24)),
            Instr::Local(PoolIndex::new(25)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_variant_conversions() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let x = ToVariant(new A());
                let y: ref<A> = FromVariant(x);
            }

            class A {}
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(24)),
            Instr::ToVariant(PoolIndex::new(25)),
            Instr::New(PoolIndex::new(22)),
            Instr::Assign,
            Instr::Local(PoolIndex::new(26)),
            Instr::FromVariant(PoolIndex::new(25)),
            Instr::Local(PoolIndex::new(24)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_switch_case() -> Result<(), Error> {
        let sources = "
            func Testing(val: Int32) -> Bool {
                switch val % 4 {
                    case 1:
                    case 2:
                    case 3:
                        break;
                    default:
                        return true;
                }
                return false;
            }

            func OperatorModulo(l: Int32, r: Int32) -> Int32 = 0
            ";
        let expected = vec![
            Instr::Switch(PoolIndex::new(8), Offset::new(39)),
            Instr::InvokeStatic(Offset::new(28), 0, PoolIndex::new(23)),
            Instr::Param(PoolIndex::new(22)),
            Instr::I32Const(4),
            Instr::ParamEnd,
            Instr::SwitchLabel(Offset::new(10), Offset::new(30)),
            Instr::I32Const(1),
            Instr::SwitchLabel(Offset::new(10), Offset::new(20)),
            Instr::I32Const(2),
            Instr::SwitchLabel(Offset::new(13), Offset::new(10)),
            Instr::I32Const(3),
            Instr::Jump(Offset::new(6)),
            Instr::SwitchDefault,
            Instr::Return,
            Instr::TrueConst,
            Instr::Return,
            Instr::FalseConst,
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_ternary_op() -> Result<(), Error> {
        let sources = "
            func Testing(val: Int32) -> Bool = val % 2 == 0 ? true : false

            func OperatorModulo(l: Int32, r: Int32) -> Int32 = 0
            func OperatorEqual(l: Int32, r: Int32) -> Bool = false
            ";
        let expected = vec![
            Instr::Return,
            Instr::Conditional(Offset::new(53), Offset::new(54)),
            Instr::InvokeStatic(Offset::new(47), 0, PoolIndex::new(26)),
            Instr::InvokeStatic(Offset::new(28), 0, PoolIndex::new(23)),
            Instr::Param(PoolIndex::new(22)),
            Instr::I32Const(2),
            Instr::ParamEnd,
            Instr::I32Const(0),
            Instr::ParamEnd,
            Instr::TrueConst,
            Instr::FalseConst,
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_if_else() -> Result<(), Error> {
        let sources = "
            func Testing(bool: Bool) -> Int32 {
                if bool {
                    return 1;
                } else {
                    return 0;
                }
            }
            ";
        let expected = vec![
            Instr::JumpIfFalse(Offset::new(21)),
            Instr::Param(PoolIndex::new(22)),
            Instr::Return,
            Instr::I32Const(1),
            Instr::Jump(Offset::new(9)),
            Instr::Return,
            Instr::I32Const(0),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_method_overload_call() -> Result<(), Error> {
        let sources = "
            class B extends A {
                func Testing() -> Int32 = super.Testing()
            }

            class A {
                func Testing() -> Int32 = 0
            }
            ";
        let expected = vec![
            Instr::Return,
            Instr::Context(Offset::new(18)),
            Instr::This,
            Instr::InvokeStatic(Offset::new(14), 0, PoolIndex::new(26)),
            Instr::ParamEnd,
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_to_bool() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let x = new A();
                if ToBool(x) {}
                let y: wref<A> = new A();
                if ToBool(y) {}
            }

            class A {}
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(25)),
            Instr::New(PoolIndex::new(22)),
            Instr::JumpIfFalse(Offset::new(13)),
            Instr::RefToBool,
            Instr::Local(PoolIndex::new(25)),
            Instr::Assign,
            Instr::Local(PoolIndex::new(27)),
            Instr::RefToWeakRef,
            Instr::New(PoolIndex::new(22)),
            Instr::JumpIfFalse(Offset::new(13)),
            Instr::WeakRefToBool,
            Instr::Local(PoolIndex::new(27)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    fn check_function_bytecode(code: &str, instrs: Vec<Instr<Offset>>) -> Result<(), Error> {
        let entries = parser::parse(code).unwrap();
        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile_entries(entries)?;
        let match_ = scripts
            .pool
            .definitions()
            .find(|(_, def)| matches!(def.value, DefinitionValue::Function(_)))
            .map(|(_, def)| &def.value);

        if let Some(DefinitionValue::Function(fun)) = match_ {
            assert_eq!(fun.code, Code(instrs))
        } else {
            assert!(false, "No function found in the pool")
        }
        Ok(())
    }
}
