//! Purely syntactic knowledge about the module being converted: declared classes, their fields
//! and method return types, local variable types and identifiers in use.

use hashbrown::{HashMap, HashSet};
use redscript_ast::{
    AstVisitor, BinOp, Constant, Expr, FunctionBody, Item, SourceFunction, SourceItemDecl,
    SourceModule, SourcePattern, SourceType, Spanned, Type, UnOp, WithSpan,
};

use crate::{Cx, NameArena, TypeQuery};

const NUMERIC: [&str; 10] = [
    "Int8", "Int16", "Int32", "Int64", "Uint8", "Uint16", "Uint32", "Uint64", "Float", "Double",
];
use crate::walk::{SExpr, SType, annotation_target, elem_type, pattern_bindings, type_name};

#[derive(Debug, Default)]
pub struct ClassInfo<'src> {
    /// Whether the class is declared in the module (as opposed to only being patched).
    pub declared: bool,
    pub is_struct: bool,
    /// The name of the base class, when declared in the module.
    pub base: Option<&'src str>,
    pub fields: HashMap<&'src str, SType<'src>>,
    /// The names of the fields declared in the body of the class, in declaration order.
    pub own_fields: Vec<&'src str>,
    pub methods: HashMap<&'src str, Option<SType<'src>>>,
}

/// Declarations found in the module, used to recover types without a type checker.
#[derive(Debug, Default)]
pub struct ModuleIndex<'src> {
    pub classes: HashMap<&'src str, ClassInfo<'src>>,
    pub functions: HashMap<&'src str, Option<SType<'src>>>,
    pub enums: HashSet<&'src str>,
}

impl<'src> ModuleIndex<'src> {
    pub fn new(module: &SourceModule<'src>) -> Self {
        let mut index = Self::default();
        for (decl, _) in &module.items[..] {
            index.add_decl(None, decl);
        }
        index
    }

    fn add_decl(&mut self, class: Option<&'src str>, decl: &SourceItemDecl<'src>) {
        let owner = annotation_target(&decl.annotations).or(class);
        match &decl.item {
            Item::Class(agg) | Item::Struct(agg) => {
                let name = agg.name.0;
                let info = self.classes.entry(name).or_default();
                info.declared = true;
                info.is_struct = matches!(decl.item, Item::Struct(_));
                info.base = agg.extends.as_ref().and_then(|typ| type_name(&typ.0));
                for (decl, _) in &agg.items {
                    self.add_decl(Some(name), decl);
                }
            }
            Item::Enum(enum_) => {
                self.enums.insert(enum_.name.0);
            }
            Item::Function(func) => {
                let ret = func.return_type.as_deref().cloned();
                match owner {
                    Some(owner) => {
                        self.classes
                            .entry(owner)
                            .or_default()
                            .methods
                            .insert(func.name.0, ret);
                    }
                    None => {
                        self.functions.insert(func.name.0, ret);
                    }
                }
            }
            Item::Let(field) => {
                if let Some(owner) = owner {
                    let info = self.classes.entry(owner).or_default();
                    info.fields.insert(field.name.0, (*field.typ).clone());
                    if class.is_some() && annotation_target(&decl.annotations).is_none() {
                        info.own_fields.push(field.name.0);
                    }
                }
            }
            Item::Import(_) => {}
        }
    }

    /// The names of the fields of the struct `name` declared in the module, in declaration order.
    pub fn struct_fields(&self, name: &str) -> Option<&[&'src str]> {
        let info = self
            .classes
            .get(name)
            .filter(|c| c.declared && c.is_struct)?;
        Some(&info.own_fields)
    }

    pub fn is_declared_class(&self, name: &str) -> bool {
        self.classes.get(name).is_some_and(|c| c.declared)
    }

    /// Whether `class` or one of its bases known to the module has a field `field`.
    pub fn has_field(&self, class: &str, field: &str) -> bool {
        let mut seen = HashSet::new();
        let mut current = Some(class);
        while let Some(name) = current
            && seen.insert(name)
        {
            let Some(info) = self.classes.get(name) else {
                break;
            };
            if info.fields.contains_key(field) {
                return true;
            }
            current = info.base;
        }
        false
    }
}

/// Declared types of the parameters and locals of a function. Names declared several times with
/// different types map to `None`.
#[derive(Debug, Default)]
pub struct LocalTypes<'src> {
    types: HashMap<&'src str, (Option<SType<'src>>, usize)>,
}

impl<'src> LocalTypes<'src> {
    pub fn of(func: &SourceFunction<'src>) -> Self {
        let mut this = Self::default();
        for (param, _) in &func.params[..] {
            this.add(param.name, param.typ.clone());
        }
        let mut collector = LetCollector(&mut this);
        if let Some(body) = &func.body {
            match body {
                FunctionBody::Block(block) => collector.visit_block(block).ok(),
                FunctionBody::Inline(expr) => collector.visit_expr(expr).ok(),
            };
        }
        this
    }

    fn add(&mut self, name: &'src str, typ: Option<SType<'src>>) {
        match self.types.get_mut(name) {
            Some((existing, count)) => {
                *count += 1;
                let same = matches!((&existing, &typ), (Some(a), Some(b)) if a.0 == b.0);
                if !same {
                    *existing = None;
                }
            }
            None => {
                self.types.insert(name, (typ, 1));
            }
        }
    }

    /// Records the inferred type of a local declared without one. Ignored for names declared
    /// more than once, since this map is not scope-aware.
    pub fn infer(&mut self, name: &str, typ: SType<'src>) {
        if let Some((existing @ None, 1)) = self.types.get_mut(name) {
            *existing = Some(typ);
        }
    }

    pub fn get(&self, name: &str) -> Option<&SType<'src>> {
        self.types.get(name)?.0.as_ref()
    }

    /// How many times `name` is declared, as a parameter or a local.
    pub fn declarations(&self, name: &str) -> usize {
        self.types.get(name).map_or(0, |(_, count)| *count)
    }
}

struct LetCollector<'a, 'src>(&'a mut LocalTypes<'src>);

impl<'src> AstVisitor<'src, WithSpan> for LetCollector<'_, 'src> {
    type Error = ();

    fn visit_pattern(&mut self, pattern: &Spanned<SourcePattern<'src>>) -> Result<(), Self::Error> {
        for (name, _) in pattern_bindings(&pattern.0) {
            self.0.add(name, None);
        }
        Ok(())
    }

    fn visit_let(
        &mut self,
        name: &Spanned<&'src str>,
        typ: &Option<Box<SType<'src>>>,
        value: &Option<Box<SExpr<'src>>>,
    ) -> Result<(), Self::Error> {
        self.0.add(name.0, typ.as_deref().cloned());
        value.iter().try_for_each(|value| self.visit_expr(value))
    }

    fn visit_for_in(
        &mut self,
        name: &Spanned<&'src str>,
        iter: &SExpr<'src>,
        body: &redscript_ast::SourceBlock<'src>,
    ) -> Result<(), Self::Error> {
        self.0.add(name.0, None);
        self.visit_expr(iter)?;
        self.visit_block(body)
    }
}

/// Everything needed to recover the type of an expression: the type information provided by the
/// caller if any, and the declarations of the module otherwise.
pub struct TypeEnv<'a, 'src> {
    pub index: &'a ModuleIndex<'src>,
    pub locals: &'a LocalTypes<'src>,
    pub class: Option<&'src str>,
    /// Type information provided by the caller, consulted first.
    pub oracle: Option<&'a dyn TypeQuery>,
    pub arena: &'src NameArena,
}

impl<'a, 'src> TypeEnv<'a, 'src> {
    pub fn new<'b: 'a>(
        index: &'a ModuleIndex<'src>,
        locals: &'a LocalTypes<'src>,
        class: Option<&'src str>,
        cx: &Cx<'b, 'src>,
    ) -> Self {
        Self {
            index,
            locals,
            class,
            oracle: cx.opts.types,
            arena: cx.arena,
        }
    }

    /// Returns the type of `expr`, from the type information provided by the caller when it has
    /// an answer for this exact expression, otherwise when it can be determined from
    /// declarations alone.
    pub fn type_of(&self, (expr, span): &SExpr<'src>) -> Option<SourceType<'src>> {
        if let Some(typ) = self
            .oracle
            .and_then(|oracle| oracle.type_of(*span))
            .and_then(|hint| hint.to_ast_type(self.arena, *span))
        {
            return Some(typ);
        }
        self.declared_type_of(expr)
    }

    /// Returns the type of `expr` when it can be determined from declarations, using the type
    /// information provided by the caller for its subexpressions.
    fn declared_type_of(&self, expr: &Expr<'src, WithSpan>) -> Option<SourceType<'src>> {
        match expr {
            Expr::Ident(name) => self.locals.get(name).map(|t| t.0.clone()),
            Expr::Constant(c) => Some(Type::plain(match c {
                Constant::String(_) => "String",
                Constant::CName(_) => "CName",
                Constant::Resource(_) => "ResRef",
                Constant::TweakDbId(_) => "TweakDBID",
                Constant::F32(_) => "Float",
                Constant::F64(_) => "Double",
                Constant::I32(_) => "Int32",
                Constant::I64(_) => "Int64",
                Constant::U32(_) => "Uint32",
                Constant::U64(_) => "Uint64",
                Constant::Bool(_) => "Bool",
            })),
            Expr::InterpolatedString(_) => Some(Type::plain("String")),
            Expr::Member { expr, member } => {
                let class = self.class_of(expr)?;
                let field = self.index.classes.get(class)?.fields.get(member)?;
                Some(field.0.clone())
            }
            Expr::Call { expr, args, .. } => {
                if let Some(typ) = self.builtin_call_type(&expr.0, args) {
                    return Some(typ);
                }
                let ret = match &expr.0 {
                    Expr::Ident(name) => self
                        .class
                        .and_then(|c| self.index.classes.get(c))
                        .and_then(|c| c.methods.get(name))
                        .or_else(|| self.index.functions.get(name))?,
                    Expr::Member { expr, member } => {
                        let class = self.class_of(expr)?;
                        self.index.classes.get(class)?.methods.get(member)?
                    }
                    _ => return None,
                };
                ret.as_ref().map(|t| t.0.clone())
            }
            Expr::Index { expr, .. } => {
                let typ = self.type_of(expr)?;
                elem_type(&typ).map(|t| t.0.clone())
            }
            Expr::DynCast { typ, .. } | Expr::New { typ, .. } => Some(typ.0.clone()),
            Expr::BinOp { lhs, op, rhs } => {
                let lhs = self.type_of(lhs);
                let rhs = self.type_of(rhs);
                let (lhs_name, rhs_name) = (
                    lhs.as_ref().and_then(type_name),
                    rhs.as_ref().and_then(type_name),
                );
                match op {
                    BinOp::Add if lhs_name == Some("String") || rhs_name == Some("String") => {
                        Some(Type::plain("String"))
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod
                        if lhs_name.is_some_and(|n| NUMERIC.contains(&n))
                            && lhs_name == rhs_name =>
                    {
                        lhs
                    }
                    BinOp::Or
                    | BinOp::And
                    | BinOp::Eq
                    | BinOp::Ne
                    | BinOp::Lt
                    | BinOp::Le
                    | BinOp::Gt
                    | BinOp::Ge => Some(Type::plain("Bool")),
                    _ => None,
                }
            }
            Expr::UnOp { op, expr } => match op {
                UnOp::Not => Some(Type::plain("Bool")),
                UnOp::Neg | UnOp::BitNot => self.type_of(expr),
            },
            _ => None,
        }
    }

    /// Return types of the intrinsics and array methods that matter for the conversion.
    fn builtin_call_type(
        &self,
        callee: &Expr<'src, WithSpan>,
        args: &[SExpr<'src>],
    ) -> Option<SourceType<'src>> {
        let (method, array) = match callee {
            Expr::Ident(name) => match *name {
                "EnumInt" => return Some(Type::plain("Int32")),
                "IntToString" | "FloatToString" | "BoolToString" | "NameToString" => {
                    return Some(Type::plain("String"));
                }
                _ => (name.strip_prefix("Array")?, args.first()?),
            },
            Expr::Member { expr, member } => (*member, &**expr),
            _ => return None,
        };
        let array = self.type_of(array)?;
        let elem = elem_type(&array)?;
        match method {
            "Size" | "FindFirst" | "FindLast" => Some(Type::plain("Int32")),
            "Contains" => Some(Type::plain("Bool")),
            "Last" => Some(elem.0.clone()),
            _ => None,
        }
    }

    fn class_of(&self, expr: &SExpr<'src>) -> Option<&'src str> {
        match expr.0 {
            Expr::This => self.class,
            _ => type_name(&self.type_of(expr)?),
        }
    }
}

/// Identifiers in use in a function, used to generate fresh names that cannot clash with them.
pub struct Names<'src> {
    taken: HashSet<&'src str>,
    arena: &'src NameArena,
}

impl<'src> Names<'src> {
    pub fn of(func: &SourceFunction<'src>, arena: &'src NameArena) -> Self {
        let mut collector = NameCollector(HashSet::new());
        collector.0.insert(func.name.0);
        for (param, _) in &func.params[..] {
            collector.0.insert(param.name);
        }
        collector.visit_function(func).ok();
        Self {
            taken: collector.0,
            arena,
        }
    }

    /// Returns `base` if it's not in use yet, otherwise `base_1`, `base_2`... The returned name
    /// is reserved.
    pub fn fresh(&mut self, base: &str) -> &'src str {
        let name = if self.taken.contains(base) {
            (1..)
                .map(|i| format!("{base}_{i}"))
                .find(|n| !self.taken.contains(n.as_str()))
                .expect("infinite iterator")
        } else {
            base.to_owned()
        };
        let name = self.arena.alloc(name);
        self.taken.insert(name);
        name
    }
}

struct NameCollector<'src>(HashSet<&'src str>);

impl<'src> AstVisitor<'src, WithSpan> for NameCollector<'src> {
    type Error = ();

    fn visit_pattern(&mut self, pattern: &Spanned<SourcePattern<'src>>) -> Result<(), Self::Error> {
        self.0.extend(
            pattern_bindings(&pattern.0)
                .into_iter()
                .map(|(name, _)| name),
        );
        Ok(())
    }

    fn visit_ident(&mut self, ident: &'src str) -> Result<(), Self::Error> {
        self.0.insert(ident);
        Ok(())
    }

    fn visit_let(
        &mut self,
        name: &Spanned<&'src str>,
        _typ: &Option<Box<SType<'src>>>,
        value: &Option<Box<SExpr<'src>>>,
    ) -> Result<(), Self::Error> {
        self.0.insert(name.0);
        value.iter().try_for_each(|value| self.visit_expr(value))
    }

    fn visit_for_in(
        &mut self,
        name: &Spanned<&'src str>,
        iter: &SExpr<'src>,
        body: &redscript_ast::SourceBlock<'src>,
    ) -> Result<(), Self::Error> {
        self.0.insert(name.0);
        self.visit_expr(iter)?;
        self.visit_block(body)
    }

    fn visit_lambda(
        &mut self,
        params: &[Spanned<redscript_ast::SourceParam<'src>>],
        body: &redscript_ast::SourceFunctionBody<'src>,
    ) -> Result<(), Self::Error> {
        self.0.extend(params.iter().map(|(p, _)| p.name));
        match body {
            FunctionBody::Inline(expr) => self.visit_expr(expr),
            FunctionBody::Block(block) => self.visit_block(block),
        }
    }
}

/// Number of declarations and of references of each name.
#[derive(Debug, Default)]
pub struct NameCounts<'src> {
    pub decls: HashMap<&'src str, usize>,
    pub refs: HashMap<&'src str, usize>,
}

impl<'src> NameCounts<'src> {
    pub fn of_function(func: &SourceFunction<'src>) -> Self {
        let mut this = Self::default();
        for (param, _) in &func.params[..] {
            *this.decls.entry(param.name).or_default() += 1;
        }
        this.visit_function(func).ok();
        this
    }

    pub fn of_block(block: &redscript_ast::SourceBlock<'src>) -> Self {
        let mut this = Self::default();
        this.visit_block(block).ok();
        this
    }

    pub fn decls(&self, name: &str) -> usize {
        self.decls.get(name).copied().unwrap_or(0)
    }

    pub fn refs(&self, name: &str) -> usize {
        self.refs.get(name).copied().unwrap_or(0)
    }
}

impl<'src> AstVisitor<'src, WithSpan> for NameCounts<'src> {
    type Error = ();

    fn visit_ident(&mut self, ident: &'src str) -> Result<(), Self::Error> {
        *self.refs.entry(ident).or_default() += 1;
        Ok(())
    }

    fn visit_pattern(&mut self, pattern: &Spanned<SourcePattern<'src>>) -> Result<(), Self::Error> {
        for (name, _) in pattern_bindings(&pattern.0) {
            *self.decls.entry(name).or_default() += 1;
        }
        Ok(())
    }

    fn visit_let(
        &mut self,
        name: &Spanned<&'src str>,
        _typ: &Option<Box<SType<'src>>>,
        value: &Option<Box<SExpr<'src>>>,
    ) -> Result<(), Self::Error> {
        *self.decls.entry(name.0).or_default() += 1;
        value.iter().try_for_each(|value| self.visit_expr(value))
    }

    fn visit_for_in(
        &mut self,
        name: &Spanned<&'src str>,
        iter: &SExpr<'src>,
        body: &redscript_ast::SourceBlock<'src>,
    ) -> Result<(), Self::Error> {
        *self.decls.entry(name.0).or_default() += 1;
        self.visit_expr(iter)?;
        self.visit_block(body)
    }

    fn visit_lambda(
        &mut self,
        params: &[Spanned<redscript_ast::SourceParam<'src>>],
        body: &redscript_ast::SourceFunctionBody<'src>,
    ) -> Result<(), Self::Error> {
        for (param, _) in params {
            *self.decls.entry(param.name).or_default() += 1;
        }
        match body {
            FunctionBody::Inline(expr) => self.visit_expr(expr),
            FunctionBody::Block(block) => self.visit_block(block),
        }
    }
}
