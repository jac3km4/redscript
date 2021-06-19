#![feature(stmt_expr_attributes)]

use std::borrow::Cow;
use std::collections::HashSet;
use std::rc::Rc;
use std::str::FromStr;

use assembler::Assembler;
use parser::{AnnotationName, EnumSource, FieldSource, SourceModule};
use redscript::ast::{BinOp, Ident, Pos, Seq, SourceAst, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex};
use redscript::bytecode::Code;
use redscript::definition::{
    Class, ClassFlags, Definition, Enum, Field, FieldFlags, Function, FunctionFlags, Local, Parameter, ParameterFlags, SourceReference, Type, Visibility
};
use redscript::error::Error;
use scope::{Scope, SymbolMap};
use source_map::{Files, SourceLocation};
use sugar::Desugar;
use transform::ExprTransformer;
use typechecker::TypeChecker;

use crate::parser::{ClassSource, FunctionSource, MemberSource, Qualifier, SourceEntry};

pub mod assembler;
#[allow(clippy::redundant_closure_call)]
pub mod parser;
pub mod scope;
pub mod source_map;
pub mod sugar;
pub mod transform;
pub mod typechecker;

pub struct Compiler<'a> {
    pool: &'a mut ConstantPool,
    symbols: SymbolMap,
    scope: Scope,
    backlog: Vec<BacklogItem>,
}

impl<'a> Compiler<'a> {
    pub fn new(pool: &'a mut ConstantPool) -> Result<Compiler<'a>, Error> {
        let symbols = SymbolMap::new(pool)?;
        let backlog = Vec::new();
        let mut scope = Scope::new(pool)?;

        symbols.populate_import(
            Import::All(ModulePath::EMPTY, Pos::ZERO),
            &mut scope,
            Visibility::Private,
        )?;

        Ok(Compiler {
            pool,
            symbols,
            scope,
            backlog,
        })
    }

    pub fn compile(&mut self, files: &Files) -> Result<(), Error> {
        log::info!("Compiling files: {}", files);

        match self.try_compile(files) {
            Ok(diagnostics) => {
                for diagnostic in diagnostics {
                    Self::print_diagnostic(files, diagnostic);
                }
                log::info!("Compilation complete");
                Ok(())
            }
            Err(Error::CompileError(err, pos)) => {
                Self::print_error(&err, files.lookup(pos).expect("Unknown file"));
                Err(Error::CompileError(err, pos))
            }
            Err(Error::SyntaxError(err, pos)) => {
                Self::print_error(&err, files.lookup(pos).expect("Unknown file"));
                Err(Error::SyntaxError(err, pos))
            }
            Err(other) => {
                log::error!("{}: {:?}", "Unexpected error during compilation", other);
                Err(other)
            }
        }
    }

    fn try_compile(&mut self, files: &Files) -> Result<Vec<Diagnostic>, Error> {
        let mut modules = vec![];
        for file in files.files() {
            let parsed = parser::parse_file(file).map_err(|err| {
                let message = format!("Syntax error, expected {}", err.expected);
                Error::SyntaxError(message, file.byte_offset() + err.location.offset)
            })?;
            modules.push(parsed);
        }
        self.compile_modules(modules)
    }

    fn print_diagnostic(files: &Files, diagnostic: Diagnostic) {
        match diagnostic {
            Diagnostic::MethodConflict(_, pos) => {
                let loc = files.lookup(pos).unwrap();
                log::warn!("At {}:\n Conflicting method replacement", loc)
            }
        }
    }

    fn print_error(error: &str, loc: SourceLocation) {
        let line = loc.enclosing_line().trim_end();
        log::error!(
            "At {}:\n \
             {}\n \
             {}^^^\n \
             {}",
            loc,
            line,
            " ".repeat(loc.position.col),
            error
        );
    }

    fn compile_modules(&mut self, modules: Vec<SourceModule>) -> Result<Vec<Diagnostic>, Error> {
        let mut compiled_funs = HashSet::new();
        let mut diagnostics = Vec::new();
        let mut queue = Vec::with_capacity(modules.len());

        for module in modules {
            let path = module.path.unwrap_or(ModulePath::EMPTY);
            let mut slots = Vec::with_capacity(module.entries.len());
            for entry in module.entries {
                slots.push(self.define_symbol(entry, &path)?);
            }
            queue.push((path, module.imports, slots));
        }

        for (path, imports, slots) in queue {
            let mut module_scope = self.scope.clone();

            if !path.is_empty() {
                self.symbols
                    .populate_import(Import::All(path, Pos::ZERO), &mut module_scope, Visibility::Private)?;
            };

            for import in imports {
                self.symbols
                    .populate_import(import, &mut module_scope, Visibility::Public)?;
            }

            for slot in slots {
                match slot {
                    Slot::Function {
                        index,
                        parent,
                        base,
                        source,
                        visibility,
                        wrapped,
                    } => {
                        let pos = source.declaration.pos;
                        self.define_function(index, parent, base, wrapped, visibility, source, &mut module_scope)?;
                        if compiled_funs.contains(&index) {
                            diagnostics.push(Diagnostic::MethodConflict(index, pos));
                        } else {
                            compiled_funs.insert(index);
                        }
                    }
                    Slot::Class {
                        index,
                        source,
                        visibility,
                    } => {
                        self.define_class(index, visibility, source, &mut module_scope)?;
                    }
                    Slot::Field {
                        index,
                        source,
                        visibility,
                    } => {
                        self.define_global_let(index, visibility, source, &mut module_scope)?;
                    }
                    Slot::Enum { index, source } => {
                        self.define_enum(index, source)?;
                    }
                }
            }
        }

        for item in self.backlog.drain(..) {
            Self::compile_function(item, self.pool)?;
        }
        Ok(diagnostics)
    }

    fn define_symbol(&mut self, entry: SourceEntry, module: &ModulePath) -> Result<Slot, Error> {
        match entry {
            SourceEntry::Class(source) => {
                let path = module.with_child(source.name.clone());
                let name_index = self.pool.names.add(path.render().to_owned());
                let type_index = self.pool.add_definition(Definition::type_(name_index, Type::Class));
                let index = self
                    .pool
                    .add_definition(Definition::type_(name_index, Type::Prim))
                    .cast();
                let visibility = source.qualifiers.visibility().unwrap_or(Visibility::Private);

                self.scope.add_type(path.render(), type_index.cast());
                self.symbols.add_class(&path, index, visibility);

                // add to globals when no module
                if module.is_empty() {
                    self.scope
                        .add_symbol(source.name.clone(), Symbol::Class(index, visibility));
                }

                let slot = Slot::Class {
                    index,
                    source,
                    visibility,
                };
                Ok(slot)
            }
            SourceEntry::Function(fun) => {
                let slot = self.determine_function_location(fun, module)?;
                Ok(slot)
            }
            SourceEntry::GlobalLet(source) => {
                let name_index = self.pool.names.add(source.declaration.name.to_owned());
                let index = self
                    .pool
                    .add_definition(Definition::type_(name_index, Type::Prim))
                    .cast();
                let visibility = source
                    .declaration
                    .qualifiers
                    .visibility()
                    .unwrap_or(Visibility::Private);

                let slot = Slot::Field {
                    index,
                    source,
                    visibility,
                };
                Ok(slot)
            }
            SourceEntry::Enum(source) => {
                let path = module.with_child(source.name.clone());
                let name_index = self.pool.names.add(path.render().to_owned());
                let type_index = self.pool.add_definition(Definition::type_(name_index, Type::Class));
                let index = self
                    .pool
                    .add_definition(Definition::type_(name_index, Type::Prim))
                    .cast();

                self.scope.add_type(path.render(), type_index.cast());
                self.symbols.add_enum(&path, index);

                // add to globals when no module
                if module.is_empty() {
                    self.scope.add_symbol(source.name.clone(), Symbol::Enum(index));
                }

                let slot = Slot::Enum { index, source };
                Ok(slot)
            }
        }
    }

    fn define_class(
        &mut self,
        class_idx: PoolIndex<Class>,
        visibility: Visibility,
        source: ClassSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let flags = ClassFlags::new()
            .with_is_abstract(source.qualifiers.contain(Qualifier::Abstract))
            .with_is_final(source.qualifiers.contain(Qualifier::Final));
        let mut functions = vec![];
        let mut fields = vec![];

        for member in source.members {
            match member {
                MemberSource::Function(fun) => {
                    let fun_sig = FunctionSignature::from_source(&fun);
                    let name_idx = self.pool.names.add(Rc::new(fun_sig.into_owned()));
                    let fun_idx = self.pool.add_definition(Definition::type_(name_idx, Type::Prim)).cast();

                    self.define_function(fun_idx, class_idx, None, None, visibility, fun, scope)?;
                    functions.push(fun_idx);
                }
                MemberSource::Field(let_) => {
                    let name_idx = self.pool.names.add(let_.declaration.name.to_owned());
                    let field_idx = self.pool.add_definition(Definition::type_(name_idx, Type::Prim)).cast();

                    self.define_field(field_idx, class_idx, visibility, let_, scope)?;
                    fields.push(field_idx);
                }
            }
        }

        let base_idx = if let Some(base_name) = source.base {
            if let Symbol::Class(base_idx, _) = scope.resolve_symbol(base_name.clone(), source.pos)? {
                base_idx
            } else {
                return Err(Error::class_not_found(base_name, source.pos));
            }
        } else if let Ok(Symbol::Class(class, _)) = scope.resolve_symbol(Ident::Static("IScriptable"), source.pos) {
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
        let name_idx = self.pool.definition(class_idx)?.name;

        self.pool
            .put_definition(class_idx.cast(), Definition::class(name_idx, class));
        Ok(())
    }

    fn define_function(
        &mut self,
        fun_idx: PoolIndex<Function>,
        parent_idx: PoolIndex<Class>,
        base_method: Option<PoolIndex<Function>>,
        wrapped: Option<PoolIndex<Function>>,
        visibility: Visibility,
        source: FunctionSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = &source.declaration;
        let flags = FunctionFlags::new()
            .with_is_static(decl.qualifiers.contain(Qualifier::Static) || parent_idx == PoolIndex::UNDEFINED)
            .with_is_final(decl.qualifiers.contain(Qualifier::Final))
            .with_is_const(decl.qualifiers.contain(Qualifier::Const))
            .with_is_exec(decl.qualifiers.contain(Qualifier::Exec))
            .with_is_callback(decl.qualifiers.contain(Qualifier::Callback));

        let return_type = match source.type_ {
            None => None,
            Some(type_) if type_.name.as_ref() == "Void" => None,
            Some(type_) => {
                let type_ = scope.resolve_type(&type_, self.pool, decl.pos)?;
                Some(scope.get_type_index(&type_, self.pool)?)
            }
        };

        let mut parameters = Vec::new();

        for param in &source.parameters {
            let type_ = scope.resolve_type(&param.type_, self.pool, decl.pos)?;
            let type_idx = scope.get_type_index(&type_, self.pool)?;
            let flags = ParameterFlags::new()
                .with_is_out(param.qualifiers.contain(Qualifier::Out))
                .with_is_optional(param.qualifiers.contain(Qualifier::Optional));
            let name = self.pool.names.add(param.name.to_owned());
            let param = Parameter { type_: type_idx, flags };
            let idx = self.pool.add_definition(Definition::param(name, fun_idx.cast(), param));
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
        let name_idx = self.pool.definition(fun_idx)?.name;
        let definition = Definition::function(name_idx, parent_idx.cast(), function);

        if let Some(code) = source.body {
            let item = BacklogItem {
                class: parent_idx,
                function: fun_idx,
                wrapped,
                code,
                scope: scope.clone(),
            };
            self.backlog.push(item)
        }

        self.pool.put_definition(fun_idx.cast(), definition);
        Ok(())
    }

    fn define_field(
        &mut self,
        index: PoolIndex<Field>,
        parent: PoolIndex<Class>,
        visibility: Visibility,
        source: FieldSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = source.declaration;
        let type_ = scope.resolve_type(&source.type_, self.pool, decl.pos)?;
        let type_idx = scope.get_type_index(&type_, self.pool)?;
        let flags = FieldFlags::new()
            .with_is_mutable(true)
            .with_is_native(decl.qualifiers.contain(Qualifier::Native));
        let field = Field {
            visibility,
            type_: type_idx,
            flags,
            hint: None,
            attributes: vec![],
            defaults: vec![],
        };
        let name_index = self.pool.definition(index)?.name;
        let definition = Definition::field(name_index, parent.cast(), field);

        self.pool.put_definition(index.cast(), definition);
        Ok(())
    }

    fn define_enum(&mut self, index: PoolIndex<Enum>, source: EnumSource) -> Result<(), Error> {
        let mut members = Vec::with_capacity(source.members.len());

        for member in source.members {
            let name_index = self.pool.names.add(member.name.to_owned());
            let def = Definition::enum_value(name_index, index, member.value);
            members.push(self.pool.add_definition(def).cast());
        }

        let enum_ = Enum {
            flags: 0,
            size: 4,
            members,
            unk1: false,
        };
        let name_index = self.pool.definition(index)?.name;
        self.pool
            .put_definition(index.cast(), Definition::enum_(name_index, enum_));
        Ok(())
    }

    fn define_global_let(
        &mut self,
        index: PoolIndex<Field>,
        visibility: Visibility,
        source: FieldSource,
        scope: &mut Scope,
    ) -> Result<(), Error> {
        let decl = &source.declaration;
        for ann in &source.declaration.annotations {
            match ann.name {
                AnnotationName::AddField => {
                    let ident = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    if let Symbol::Class(target_class, _) = scope.resolve_symbol(ident.clone(), ann.pos)? {
                        self.define_field(index, target_class, visibility, source, scope)?;
                        self.pool.class_mut(target_class)?.fields.push(index);
                        return Ok(());
                    } else {
                        return Err(Error::class_not_found(ident, ann.pos));
                    }
                }
                _ => {}
            }
        }
        Err(Error::unsupported("Let binding", decl.pos))
    }

    fn determine_function_location(&mut self, source: FunctionSource, module: &ModulePath) -> Result<Slot, Error> {
        let name = source.declaration.name.clone();
        let sig = FunctionSignature::from_source(&source);
        let visibility = source
            .declaration
            .qualifiers
            .visibility()
            .unwrap_or(Visibility::Private);

        for ann in &source.declaration.annotations {
            match ann.name {
                AnnotationName::WrapMethod => {
                    let class_name = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone(), ann.pos)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Error::class_not_found(class_name, ann.pos)),
                    };
                    let index = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool, ann.pos)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Error::function_not_found(name, ann.pos))?;

                    let name_idx = self.pool.names.add(Rc::new(sig.into_owned()));
                    let swap_idx = self.pool.add_definition(Definition::type_(name_idx, Type::Prim)).cast();
                    let base = self.pool.function(index)?.base_method;

                    self.pool.swap_definition(index, swap_idx);
                    self.pool.rename(swap_idx, PoolIndex::UNDEFINED);
                    self.pool.class_mut(target_class_idx)?.functions.push(swap_idx);

                    let slot = Slot::Function {
                        index,
                        parent: target_class_idx,
                        base,
                        wrapped: Some(swap_idx),
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::ReplaceMethod => {
                    let class_name = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone(), ann.pos)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Error::class_not_found(class_name, ann.pos)),
                    };
                    let index = self
                        .scope
                        .resolve_method(name.clone(), target_class_idx, self.pool, ann.pos)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Error::function_not_found(name, ann.pos))?;
                    let base = self.pool.function(index)?.base_method;
                    let slot = Slot::Function {
                        index,
                        parent: target_class_idx,
                        base,
                        wrapped: None,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::ReplaceGlobal => {
                    let index = self
                        .scope
                        .resolve_function(name.clone(), ann.pos)?
                        .by_id(&sig, self.pool)
                        .ok_or_else(|| Error::function_not_found(name, ann.pos))?;

                    let slot = Slot::Function {
                        index,
                        parent: PoolIndex::UNDEFINED,
                        base: None,
                        wrapped: None,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::AddMethod => {
                    let class_name = ann
                        .values
                        .first()
                        .ok_or_else(|| Error::invalid_annotation_args(ann.pos))?;
                    let target_class_idx = match self.scope.resolve_symbol(class_name.clone(), ann.pos)? {
                        Symbol::Class(idx, _) => idx,
                        Symbol::Struct(idx, _) => idx,
                        _ => return Err(Error::class_not_found(class_name, ann.pos)),
                    };
                    let class = self.pool.class(target_class_idx)?;
                    let base_method = if class.base != PoolIndex::UNDEFINED {
                        let base = self.pool.class(class.base)?;
                        base.functions
                            .iter()
                            .find(|fun| self.pool.definition_name(**fun).unwrap().as_str() == sig.as_ref())
                            .cloned()
                    } else {
                        None
                    };
                    let name_idx = self.pool.names.add(Rc::new(sig.into_owned()));
                    let index = self.pool.add_definition(Definition::type_(name_idx, Type::Prim)).cast();
                    self.pool.class_mut(target_class_idx)?.functions.push(index);

                    let slot = Slot::Function {
                        index,
                        parent: target_class_idx,
                        base: base_method,
                        wrapped: None,
                        source,
                        visibility,
                    };
                    return Ok(slot);
                }
                AnnotationName::AddField => {}
            }
        }

        let name_idx = self.pool.names.add(module.with_function(sig).render().to_owned());
        let fun_idx = self.pool.add_definition(Definition::type_(name_idx, Type::Prim)).cast();

        let path = module.with_child(name.clone());
        self.symbols.add_function(&path, fun_idx, visibility);

        // add to globals when no module
        if module.is_empty() {
            self.scope
                .add_symbol(name, Symbol::Functions(vec![(fun_idx, visibility)]));
        }

        let slot = Slot::Function {
            index: fun_idx.cast(),
            parent: PoolIndex::UNDEFINED,
            base: None,
            wrapped: None,
            source,
            visibility,
        };
        Ok(slot)
    }

    fn compile_function(item: BacklogItem, pool: &mut ConstantPool) -> Result<(), Error> {
        let fun = pool.function(item.function)?;

        let mut local_scope = if fun.flags.is_static() {
            item.scope.with_context(None, item.function)
        } else {
            item.scope.with_context(Some(item.class), item.function)
        };

        for param in &fun.parameters {
            let ident = Ident::Owned(pool.definition_name(*param)?);
            local_scope.add_parameter(ident, *param);
        }

        if let Some(wrapped) = item.wrapped {
            let wrapped_ident = Ident::Static("wrappedMethod");
            local_scope.add_symbol(wrapped_ident, Symbol::Functions(vec![(wrapped, Visibility::Public)]));
        }

        let mut checker = TypeChecker::new(pool);
        let checked = checker.check_seq(&item.code, &mut local_scope)?;
        let mut locals = checker.locals();

        let mut desugar = Desugar::new(&mut local_scope, pool);
        let desugared = desugar.on_seq(checked)?;
        locals.extend(desugar.locals());

        let code = Assembler::from_body(desugared, &mut local_scope, pool)?;
        let function = pool.function_mut(item.function)?;
        function.code = code;
        function.locals = locals;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    parts: Vec<Ident>,
}

impl ModulePath {
    pub const EMPTY: ModulePath = ModulePath { parts: vec![] };

    pub fn new(parts: Vec<Ident>) -> ModulePath {
        ModulePath { parts }
    }

    pub fn parse(str: &str) -> ModulePath {
        let parts = str
            .split('.')
            .map(|str| Ident::new(str.split(';').next().unwrap().to_owned()))
            .collect();
        ModulePath { parts }
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn with_child(&self, child: Ident) -> ModulePath {
        let mut copy = self.clone();
        copy.parts.push(child);
        copy
    }

    pub fn with_function(&self, fun_sig: FunctionSignature) -> ModulePath {
        let ident = Ident::new(fun_sig.into_owned());
        self.with_child(ident)
    }

    pub fn last(&self) -> Option<Ident> {
        self.parts.last().cloned()
    }

    pub fn render(&self) -> Ident {
        self.parts
            .iter()
            .cloned()
            .reduce(|acc, m| Ident::new(format!("{}.{}", acc, m)))
            .unwrap_or(Ident::Static(""))
    }
}

impl<'a> IntoIterator for &'a ModulePath {
    type Item = &'a Ident;

    type IntoIter = std::slice::Iter<'a, Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.parts.iter()
    }
}

pub struct BacklogItem {
    class: PoolIndex<Class>,
    function: PoolIndex<Function>,
    wrapped: Option<PoolIndex<Function>>,
    code: Seq<SourceAst>,
    scope: Scope,
}

#[derive(Debug)]
pub enum Diagnostic {
    MethodConflict(PoolIndex<Function>, Pos),
}

#[derive(Debug, Clone)]
pub enum Value {
    Local(PoolIndex<Local>),
    Parameter(PoolIndex<Parameter>),
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Class(PoolIndex<Class>, Visibility),
    Struct(PoolIndex<Class>, Visibility),
    Enum(PoolIndex<Enum>),
    Functions(Vec<(PoolIndex<Function>, Visibility)>),
}

impl Symbol {
    pub fn visible(self, visibility: Visibility) -> Option<Symbol> {
        match self {
            Symbol::Class(_, v) if v <= visibility => Some(self),
            Symbol::Struct(_, v) if v <= visibility => Some(self),
            Symbol::Enum(_) => Some(self),
            Symbol::Functions(funs) => {
                let visible_funs: Vec<_> = funs.into_iter().filter(|(_, v)| *v <= visibility).collect();
                if visible_funs.is_empty() {
                    None
                } else {
                    Some(Symbol::Functions(visible_funs))
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Import {
    Exact(ModulePath, Pos),
    Selected(ModulePath, Vec<Ident>, Pos),
    All(ModulePath, Pos),
}

#[derive(Debug, Clone)]
pub enum Reference {
    Value(Value),
    Symbol(Symbol),
}

pub enum Slot {
    Function {
        index: PoolIndex<Function>,
        parent: PoolIndex<Class>,
        base: Option<PoolIndex<Function>>,
        wrapped: Option<PoolIndex<Function>>,
        source: FunctionSource,
        visibility: Visibility,
    },
    Class {
        index: PoolIndex<Class>,
        source: ClassSource,
        visibility: Visibility,
    },
    Field {
        index: PoolIndex<Field>,
        source: FieldSource,
        visibility: Visibility,
    },
    Enum {
        index: PoolIndex<Enum>,
        source: EnumSource,
    },
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

pub struct FunctionSignature<'a>(Cow<'a, str>);

impl<'a> FunctionSignature<'a> {
    pub fn from_source(source: &'a FunctionSource) -> Self {
        let qs = &source.declaration.qualifiers;
        let name = source.declaration.name.as_ref();
        let is_operator = BinOp::from_str(name).is_ok();

        if !is_operator
            && (qs.contain(Qualifier::Callback) || qs.contain(Qualifier::Exec) || qs.contain(Qualifier::Native))
        {
            FunctionSignature(Cow::Borrowed(name))
        } else {
            let builder = source
                .parameters
                .iter()
                .fold(FunctionSignatureBuilder::new(name.to_owned()), |acc, param| {
                    acc.parameter(&param.type_, param.qualifiers.contain(Qualifier::Out) && is_operator)
                });
            if is_operator {
                builder.return_type(source.type_.as_ref().unwrap_or(&TypeName::VOID))
            } else {
                builder.build()
            }
        }
    }

    pub fn name(&self) -> &str {
        self.as_ref().split(';').next().unwrap()
    }

    pub fn into_owned(self) -> String {
        self.0.into_owned()
    }
}

impl<'a> AsRef<str> for FunctionSignature<'a> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

pub struct FunctionSignatureBuilder {
    signature: String,
}

impl FunctionSignatureBuilder {
    pub fn new(name: String) -> FunctionSignatureBuilder {
        FunctionSignatureBuilder { signature: name + ";" }
    }

    pub fn parameter(self, typ: &TypeName, is_out: bool) -> FunctionSignatureBuilder {
        let mut signature = self.signature;
        if is_out {
            signature.push_str("Out");
        }
        signature.push_str(typ.mangled().as_ref());
        FunctionSignatureBuilder { signature }
    }

    pub fn return_type<'a>(self, typ: &TypeName) -> FunctionSignature<'a> {
        let mut signature = self.signature;
        signature.push(';');
        signature.push_str(typ.mangled().as_ref());
        FunctionSignature(Cow::Owned(signature))
    }

    pub fn build<'a>(self) -> FunctionSignature<'a> {
        FunctionSignature(Cow::Owned(self.signature))
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use redscript::bundle::{ConstantPool, PoolIndex, ScriptBundle};
    use redscript::bytecode::{Code, Instr, Offset};
    use redscript::definition::{AnyDefinition, ClassFlags};
    use redscript::error::Error;

    use crate::{parser, Compiler};

    const PREDEF: &[u8] = include_bytes!("../../resources/predef.redscripts");

    #[test]
    fn compile_simple_class() -> Result<(), Error> {
        let sources = "
            public class A {
                private const let m_field: Int32;

                public func DoStuff(fieldOrNot: Bool) -> Int32 {
                    return fieldOrNot ? this.m_field : A.Ten();
                }

                public static func Ten() -> Int32 {
                    return 10;
                }
            }";

        check_compilation(sources)
    }

    #[test]
    fn compile_ext_class() -> Result<(), Error> {
        let sources = "
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
            }";

        check_compilation(sources)
    }

    #[test]
    fn compile_class_with_forward_ref() -> Result<(), Error> {
        let sources = "
            public class MyTestClass456 {
                public let myOtherTestClass: ref<MyTestClass123>;

                public func DoStuff() -> ref<MyTestClass123> {
                    return this.myOtherTestClass;
                }
            }
            
            public class MyTestClass123 {
                public let myTestVar: String;
            }";

        check_compilation(sources)
    }

    #[test]
    fn compile_class_with_shorthand_funcs() -> Result<(), Error> {
        let sources = "
            public class ShorthandTest {
                public func InstanceVal() -> String = ShorthandTest.StaticVal()
                public static func StaticVal() -> String = \"static\"
            }";

        check_compilation(sources)
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
            Instr::New(PoolIndex::new(25)),
            Instr::Assign,
            Instr::Local(PoolIndex::new(29)),
            Instr::RefToWeakRef,
            Instr::DynamicCast(PoolIndex::new(23), 0),
            Instr::WeakRefToRef,
            Instr::Local(PoolIndex::new(27)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_base_class_overload() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let b = new B();
                b.Testing(1, 2);
            }

            class A {
                final func Testing(a: Int32) -> Int32 = a
            }
            class B extends A {
                final func Testing(a: Int32, b: Int32) -> Int32 = b
            }
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(32)),
            Instr::New(PoolIndex::new(25)),
            Instr::Context(Offset::new(36)),
            Instr::Local(PoolIndex::new(32)),
            Instr::InvokeStatic(Offset::new(24), 0, PoolIndex::new(28)),
            Instr::I32Const(1),
            Instr::I32Const(2),
            Instr::ParamEnd,
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_class_attributes() -> Result<(), Error> {
        let source = r#"
            public abstract class Base {}

            public final class Derived extends Base {}
        "#;

        let expected_base_flags = ClassFlags::new().with_is_abstract(true);
        let expected_derived_flags = ClassFlags::new().with_is_final(true);

        let pool = check_compilation_pool(source)?;
        check_class_flags(&pool, "Base", expected_base_flags)?;
        check_class_flags(&pool, "Derived", expected_derived_flags)?;
        Ok(())
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
            Instr::InvokeStatic(Offset::new(19), 0, PoolIndex::new(23)),
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
            Instr::Local(PoolIndex::new(38)),
            Instr::New(PoolIndex::new(27)),
            Instr::InvokeStatic(Offset::new(33), 0, PoolIndex::new(22)),
            Instr::RefToWeakRef,
            Instr::Local(PoolIndex::new(38)),
            Instr::Local(PoolIndex::new(38)),
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
            func OperatorAssignAdd(out l: Int32, r: Int32) -> Int32 = 0
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
            Instr::InvokeStatic(Offset::new(41), 0, PoolIndex::new(24)),
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
            Instr::InvokeStatic(Offset::new(28), 0, PoolIndex::new(23)),
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
            Instr::New(PoolIndex::new(23)),
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
            Instr::InvokeStatic(Offset::new(28), 0, PoolIndex::new(22)),
            Instr::Param(PoolIndex::new(23)),
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
            Instr::InvokeStatic(Offset::new(47), 0, PoolIndex::new(23)),
            Instr::InvokeStatic(Offset::new(28), 0, PoolIndex::new(22)),
            Instr::Param(PoolIndex::new(24)),
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
    fn compile_null_wref_assignment() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let a: wref<A> = null;
            }

            class A {}
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(25)),
            Instr::RefToWeakRef,
            Instr::Null,
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_number_literals() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let a: Float = 1;
                let b: Float = 2.0;
                let c: Double = 3;
                let d: Double = 4.0;
                let e: Double = 5.0d;
                let f: Int32 = 6;
                let g: Int64 = 7;
                let h: Int64 = 8l;
                let i: Uint32 = 9u;
                let j: Uint64 = 10u;
            }
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(22)),
            Instr::F32Const(1.0),
            Instr::Assign,
            Instr::Local(PoolIndex::new(23)),
            Instr::F32Const(2.0),
            Instr::Assign,
            Instr::Local(PoolIndex::new(24)),
            Instr::F64Const(3.0),
            Instr::Assign,
            Instr::Local(PoolIndex::new(25)),
            Instr::F64Const(4.0),
            Instr::Assign,
            Instr::Local(PoolIndex::new(26)),
            Instr::F64Const(5.0),
            Instr::Assign,
            Instr::Local(PoolIndex::new(27)),
            Instr::I32Const(6),
            Instr::Assign,
            Instr::Local(PoolIndex::new(28)),
            Instr::I64Const(7),
            Instr::Assign,
            Instr::Local(PoolIndex::new(29)),
            Instr::I64Const(8),
            Instr::Assign,
            Instr::Local(PoolIndex::new(30)),
            Instr::U32Const(9),
            Instr::Assign,
            Instr::Local(PoolIndex::new(31)),
            Instr::U64Const(10),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_is_defined() -> Result<(), Error> {
        let sources = "
            func Testing() {
                let x = new A();
                if IsDefined(x) {}
                let y: wref<A> = new A();
                if IsDefined(y) {}
            }

            class A {}
            ";
        let expected = vec![
            Instr::Assign,
            Instr::Local(PoolIndex::new(25)),
            Instr::New(PoolIndex::new(23)),
            Instr::JumpIfFalse(Offset::new(13)),
            Instr::RefToBool,
            Instr::Local(PoolIndex::new(25)),
            Instr::Assign,
            Instr::Local(PoolIndex::new(27)),
            Instr::RefToWeakRef,
            Instr::New(PoolIndex::new(23)),
            Instr::JumpIfFalse(Offset::new(13)),
            Instr::WeakRefToBool,
            Instr::Local(PoolIndex::new(27)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    #[test]
    fn compile_mutually_dependent_modules() -> Result<(), Error> {
        let sources1 = parser::parse_str(
            "
            module MyModule.Module1
            import MyModule.Module2.{B, Func2}

            public func Func1() -> Int32 = 2

            class A {
                func Thing() -> Int32 {
                    Func2();
                    return new B().Thing();
                }
            }",
        )
        .unwrap();

        let sources2 = parser::parse_str(
            "
            module MyModule.Module2
            import MyModule.Module1.*

            public func Func2() -> Int32 = 2

            public class B {
                func Thing() -> Int32 = Func1()
            }",
        )
        .unwrap();

        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile_modules(vec![sources1, sources2])?;
        Ok(())
    }

    #[test]
    fn compile_enum() -> Result<(), Error> {
        let sources = r#"
            func Testing(dir: Direction) -> Int32 {
                switch dir {
                    case Direction.Left:
                        return EnumInt(dir);
                    case Direction.Right:
                        return EnumInt(dir);
                }
            }

            enum Direction {
                Left = 0,
                Right = 1,
            }
            "#;

        let expected = vec![
            Instr::Switch(PoolIndex::new(22), Offset::new(20)),
            Instr::Param(PoolIndex::new(24)),
            Instr::SwitchLabel(Offset::new(42), Offset::new(22)),
            Instr::EnumConst(PoolIndex::new(23), PoolIndex::new(25)),
            Instr::Return,
            Instr::EnumToI32(PoolIndex::new(22), 4),
            Instr::Param(PoolIndex::new(24)),
            Instr::SwitchLabel(Offset::new(42), Offset::new(22)),
            Instr::EnumConst(PoolIndex::new(23), PoolIndex::new(26)),
            Instr::Return,
            Instr::EnumToI32(PoolIndex::new(22), 4),
            Instr::Param(PoolIndex::new(24)),
            Instr::Nop,
        ];
        check_function_bytecode(sources, expected)
    }

    fn check_compilation_pool(code: &str) -> Result<ConstantPool, Error> {
        let module = parser::parse_str(code).unwrap();
        let mut scripts = ScriptBundle::load(&mut Cursor::new(PREDEF))?;
        let mut compiler = Compiler::new(&mut scripts.pool)?;
        compiler.compile_modules(vec![module])?;

        Ok(scripts.pool)
    }

    fn check_compilation(code: &str) -> Result<(), Error> {
        check_compilation_pool(code)?;
        Ok(())
    }

    fn check_function_bytecode(code: &str, instrs: Vec<Instr<Offset>>) -> Result<(), Error> {
        let pool = check_compilation_pool(code)?;
        let match_ = pool
            .definitions()
            .find(|(_, def)| matches!(def.value, AnyDefinition::Function(_)))
            .map(|(_, def)| &def.value);

        if let Some(AnyDefinition::Function(fun)) = match_ {
            assert_eq!(fun.code, Code(instrs))
        } else {
            assert!(false, "No function found in the pool")
        }
        Ok(())
    }

    fn check_class_flags(pool: &ConstantPool, name: &str, flags: ClassFlags) -> Result<(), Error> {
        let name_index = pool.names.get_index(&String::from(name))?;
        let match_ = pool
            .definitions()
            .filter(|(_, def)| matches!(def.value, AnyDefinition::Class(_)))
            .find(|(_, def)| def.name == name_index)
            .map(|(_, def)| &def.value);

        if let Some(AnyDefinition::Class(ref class)) = match_ {
            assert_eq!(class.flags, flags)
        } else {
            assert!(false, "Class definition {} not found in the pool", name)
        }
        Ok(())
    }
}
