//! Mutable AST traversal and small node-building helpers shared by the conversion passes.

use redscript_ast::{
    Condition, ConditionalBlock, Constant, Expr, FunctionBody, Item, ItemDecl, ItemQualifiers,
    LetCondition, Pattern, SourceAnnotation, SourceExpr, SourceFunction, SourceFunctionBody,
    SourceItemDecl, SourceModule, SourceParam, SourcePattern, SourceStmt, SourceType, Span,
    Spanned, Stmt, StrPart, Type,
};

pub type SExpr<'src> = Spanned<SourceExpr<'src>>;
pub type SStmt<'src> = Spanned<SourceStmt<'src>>;
pub type SType<'src> = Spanned<SourceType<'src>>;
pub type SParam<'src> = Spanned<SourceParam<'src>>;

/// A mutable visitor over function bodies. Every hook defaults to walking into the children,
/// overriding implementations decide whether to transform before or after walking.
pub trait MutVisitor<'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        walk_stmt(self, stmt);
    }

    /// Called for every statement list: blocks, switch cases and switch defaults.
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        walk_stmts(self, stmts);
    }

    fn visit_lambda(
        &mut self,
        _params: &mut Box<[SParam<'src>]>,
        body: &mut SourceFunctionBody<'src>,
    ) {
        walk_body(self, body);
    }
}

pub fn walk_body<'src, V: MutVisitor<'src> + ?Sized>(
    v: &mut V,
    body: &mut SourceFunctionBody<'src>,
) {
    match body {
        FunctionBody::Block(block) => v.visit_stmts(&mut block.stmts),
        FunctionBody::Inline(expr) => v.visit_expr(expr),
    }
}

pub fn walk_stmts<'src, V: MutVisitor<'src> + ?Sized>(v: &mut V, stmts: &mut [SStmt<'src>]) {
    stmts.iter_mut().for_each(|stmt| v.visit_stmt(stmt));
}

fn walk_let_condition<'src, V: MutVisitor<'src> + ?Sized>(
    v: &mut V,
    cond: &mut LetCondition<'src, redscript_ast::WithSpan>,
) {
    match cond {
        LetCondition::Expr(expr) | LetCondition::LetPattern(_, expr) => v.visit_expr(expr),
    }
}

pub fn walk_stmt<'src, V: MutVisitor<'src> + ?Sized>(v: &mut V, (stmt, _): &mut SStmt<'src>) {
    match stmt {
        Stmt::Let { value, .. } => {
            if let Some(value) = value {
                v.visit_expr(value);
            }
        }
        Stmt::Switch {
            expr,
            cases,
            default,
        } => {
            v.visit_expr(expr);
            for case in cases.iter_mut() {
                if let Condition::Expr(expr) = &mut case.condition {
                    v.visit_expr(expr);
                }
                v.visit_stmts(&mut case.body);
            }
            if let Some(default) = default {
                v.visit_stmts(default);
            }
        }
        Stmt::If { blocks, else_ } => {
            for block in blocks.iter_mut() {
                walk_let_condition(v, &mut block.condition);
                v.visit_stmts(&mut block.body.stmts);
            }
            if let Some(else_) = else_ {
                v.visit_stmts(&mut else_.stmts);
            }
        }
        Stmt::While(block) => {
            walk_let_condition(v, &mut block.condition);
            v.visit_stmts(&mut block.body.stmts);
        }
        Stmt::ForIn { iter, body, .. } => {
            v.visit_expr(iter);
            v.visit_stmts(&mut body.stmts);
        }
        Stmt::For {
            init,
            cond,
            step,
            body,
        } => {
            if let Some(init) = init {
                v.visit_stmt(init);
            }
            if let Some(cond) = cond {
                v.visit_expr(cond);
            }
            if let Some(step) = step {
                v.visit_expr(step);
            }
            v.visit_stmts(&mut body.stmts);
        }
        Stmt::Return(Some(expr)) | Stmt::Expr(expr) => v.visit_expr(expr),
        Stmt::Return(None) | Stmt::Break | Stmt::Continue => {}
    }
}

pub fn walk_expr<'src, V: MutVisitor<'src> + ?Sized>(v: &mut V, (expr, _): &mut SExpr<'src>) {
    match expr {
        Expr::ArrayLit(elems) => elems.iter_mut().for_each(|e| v.visit_expr(e)),
        Expr::InterpolatedString(parts) => {
            for part in parts.iter_mut() {
                if let StrPart::Expr(expr) = part {
                    v.visit_expr(expr);
                }
            }
        }
        Expr::Assign { lhs, rhs }
        | Expr::BinOp { lhs, rhs, .. }
        | Expr::Index {
            expr: lhs,
            index: rhs,
        } => {
            v.visit_expr(lhs);
            v.visit_expr(rhs);
        }
        Expr::UnOp { expr, .. } | Expr::Member { expr, .. } | Expr::DynCast { expr, .. } => {
            v.visit_expr(expr);
        }
        Expr::Call { expr, args, .. } => {
            v.visit_expr(expr);
            args.iter_mut().for_each(|e| v.visit_expr(e));
        }
        Expr::New { args, .. } => args.iter_mut().for_each(|e| v.visit_expr(e)),
        Expr::Conditional { cond, then, else_ } => {
            v.visit_expr(cond);
            v.visit_expr(then);
            v.visit_expr(else_);
        }
        Expr::Lambda { params, body } => v.visit_lambda(params, body),
        Expr::Ident(_)
        | Expr::Constant(_)
        | Expr::This
        | Expr::Super
        | Expr::Null
        | Expr::Error => {}
    }
}

/// Replaces every statement of the list with the statements returned by `f`.
pub fn expand_stmts<'src>(
    stmts: &mut Box<[SStmt<'src>]>,
    mut f: impl FnMut(SStmt<'src>, &mut Vec<SStmt<'src>>),
) {
    let mut out = Vec::with_capacity(stmts.len());
    for stmt in std::mem::take(stmts).into_vec() {
        f(stmt, &mut out);
    }
    *stmts = out.into_boxed_slice();
}

/// Information about the declaration a function belongs to.
pub struct FnInfo<'a, 'src> {
    /// The class whose members `this` refers to: the target of a patching annotation, or the
    /// enclosing class.
    pub class: Option<&'src str>,
    pub annotations: &'a [Spanned<SourceAnnotation<'src>>],
    pub qualifiers: ItemQualifiers,
}

impl FnInfo<'_, '_> {
    pub fn has_annotation(&self, name: &str) -> bool {
        self.annotations.iter().any(|(a, _)| a.name == name)
    }
}

pub const PATCH_ANNOTATIONS: [&str; 4] = ["wrapMethod", "replaceMethod", "addMethod", "addField"];

/// Words REDmod rejects as the names of parameters, locals and fields.
pub const REDMOD_RESERVED: [&str; 28] = [
    "name",
    "event",
    "out",
    "hint",
    "optional",
    "var",
    "function",
    "array",
    "ref",
    "wref",
    "script_ref",
    "virtual",
    "override",
    "default",
    "cast",
    "editable",
    "instanceeditable",
    "operator",
    "implicit",
    "opt",
    "cb",
    "exec",
    "quest",
    "persistent",
    "native",
    "importonly",
    "replicated",
    "static",
];

/// Returns the class a patching annotation targets, if any.
pub fn annotation_target<'src>(
    annotations: &[Spanned<SourceAnnotation<'src>>],
) -> Option<&'src str> {
    annotations.iter().find_map(|(ann, _)| {
        if !PATCH_ANNOTATIONS.contains(&ann.name) {
            return None;
        }
        match ann.args.first() {
            Some((Expr::Ident(name), _)) => Some(*name),
            _ => None,
        }
    })
}

/// Calls `f` for every function (free or member) of the module.
pub fn for_each_function<'src>(
    module: &mut SourceModule<'src>,
    mut f: impl FnMut(&FnInfo<'_, 'src>, &mut SourceFunction<'src>),
) {
    for (decl, _) in &mut module.items {
        function_in_decl(None, decl, &mut f);
    }
}

fn function_in_decl<'src>(
    class: Option<&'src str>,
    decl: &mut SourceItemDecl<'src>,
    f: &mut impl FnMut(&FnInfo<'_, 'src>, &mut SourceFunction<'src>),
) {
    let ItemDecl {
        annotations,
        qualifiers,
        item,
        ..
    } = decl;
    match item {
        Item::Function(func) => {
            let info = FnInfo {
                class: annotation_target(annotations).or(class),
                annotations,
                qualifiers: *qualifiers,
            };
            f(&info, func);
        }
        Item::Class(agg) | Item::Struct(agg) => {
            let name = agg.name.0;
            for (decl, _) in &mut agg.items {
                function_in_decl(Some(name), decl, f);
            }
        }
        Item::Import(_) | Item::Let(_) | Item::Enum(_) => {}
    }
}

/// Calls `f` for every item declaration of the module, including class members.
pub fn for_each_decl<'src>(
    module: &mut SourceModule<'src>,
    mut f: impl FnMut(&mut Spanned<SourceItemDecl<'src>>),
) {
    fn go<'src>(
        decl: &mut Spanned<SourceItemDecl<'src>>,
        f: &mut impl FnMut(&mut Spanned<SourceItemDecl<'src>>),
    ) {
        f(decl);
        if let Item::Class(agg) | Item::Struct(agg) = &mut decl.0.item {
            agg.items.iter_mut().for_each(|d| go(d, f));
        }
    }
    module.items.iter_mut().for_each(|d| go(d, &mut f));
}

/// A zero-length span at the start of `span`. Used for synthesized nodes so that they never
/// collide with the span of an original node (and thus never steal its comments).
pub fn synth(span: Span) -> Span {
    Span::new(span.start, span.start, span.file)
}

pub fn ident<'src>(name: &'src str, span: Span) -> SExpr<'src> {
    (Expr::Ident(name), span)
}

pub fn int<'src>(value: i32, span: Span) -> SExpr<'src> {
    (Expr::Constant(Constant::I32(value)), span)
}

pub fn plain_type<'src>(name: &'src str, span: Span) -> SType<'src> {
    (Type::plain(name), span)
}

pub fn call<'src>(
    callee: SExpr<'src>,
    type_args: Vec<SType<'src>>,
    args: Vec<SExpr<'src>>,
    span: Span,
) -> SExpr<'src> {
    (
        Expr::Call {
            expr: Box::new(callee),
            type_args: type_args.into(),
            args: args.into(),
        },
        span,
    )
}

pub fn method_call<'src>(
    receiver: SExpr<'src>,
    method: &'src str,
    args: Vec<SExpr<'src>>,
    span: Span,
) -> SExpr<'src> {
    let callee_span = synth(span);
    let callee = (
        Expr::Member {
            expr: Box::new(receiver),
            member: method,
        },
        callee_span,
    );
    call(callee, vec![], args, span)
}

pub fn let_stmt<'src>(
    name: &'src str,
    typ: Option<SType<'src>>,
    value: Option<SExpr<'src>>,
    span: Span,
) -> SStmt<'src> {
    (
        Stmt::Let {
            name: (name, span),
            typ: typ.map(Box::new),
            value: value.map(Box::new),
        },
        span,
    )
}

/// Returns the name of the free function called by `expr`, if it is a call to an identifier.
pub fn free_call_name<'src>(expr: &SourceExpr<'src>) -> Option<&'src str> {
    match expr {
        Expr::Call { expr, .. } => match &expr.0 {
            Expr::Ident(name) => Some(*name),
            _ => None,
        },
        _ => None,
    }
}

/// Whether `expr` is a plain variable path: an identifier, `this.field` or a chain of field
/// accesses rooted at one of these.
pub fn is_variable_path(expr: &SourceExpr<'_>) -> bool {
    match expr {
        Expr::Ident(_) => true,
        Expr::Member { expr, .. } => matches!(expr.0, Expr::This) || is_variable_path(&expr.0),
        _ => false,
    }
}

/// The element type of an array type.
pub fn elem_type<'a, 'src>(typ: &'a SourceType<'src>) -> Option<&'a SType<'src>> {
    match typ {
        Type::Array(elem) | Type::StaticArray(elem, _) => Some(elem),
        Type::Named {
            name: "array",
            args,
        } if args.len() == 1 => Some(&args[0]),
        _ => None,
    }
}

/// Strips `ref<..>`/`wref<..>` from a type.
pub fn strip_ref<'a, 'src>(typ: &'a SourceType<'src>) -> &'a SourceType<'src> {
    match typ {
        Type::Named {
            name: "ref" | "wref" | "script_ref",
            args,
        } if args.len() == 1 => strip_ref(&args[0].0),
        _ => typ,
    }
}

/// The name of a plain named type (without arguments, after stripping references).
pub fn type_name<'src>(typ: &SourceType<'src>) -> Option<&'src str> {
    match strip_ref(typ) {
        Type::Named { name, args } if args.is_empty() => Some(*name),
        _ => None,
    }
}

/// Calls `f` for every variable bound by a pattern.
pub fn pattern_bindings_mut<'src>(
    pattern: &mut SourcePattern<'src>,
    f: &mut impl FnMut(&mut Spanned<&'src str>),
) {
    match pattern {
        Pattern::Name(name) => f(name),
        Pattern::As(inner, _) | Pattern::Nullable(inner) => pattern_bindings_mut(&mut inner.0, f),
        Pattern::Aggregate(_, fields) => fields
            .iter_mut()
            .for_each(|(_, p)| pattern_bindings_mut(p, f)),
        Pattern::Array(_, (pats, _)) => pats.iter_mut().for_each(|p| pattern_bindings_mut(p, f)),
    }
}

/// Returns the variables bound by a pattern.
pub fn pattern_bindings<'src>(pattern: &SourcePattern<'src>) -> Vec<Spanned<&'src str>> {
    let mut names = vec![];
    pattern_bindings_mut(&mut pattern.clone(), &mut |name| names.push(*name));
    names
}

/// Renames references to a variable, respecting shadowing by later declarations.
pub struct RenameIdent<'src> {
    pub old: &'src str,
    pub new: &'src str,
}

impl<'src> RenameIdent<'src> {
    /// Visits a conditional block; a pattern condition binding `old` shadows it in the body.
    fn visit_conditional_block(
        &mut self,
        block: &mut ConditionalBlock<'src, redscript_ast::WithSpan>,
    ) {
        let shadows = matches!(&block.condition, LetCondition::LetPattern((pat, _), _)
            if pattern_bindings(pat).iter().any(|n| n.0 == self.old));
        if shadows {
            if let LetCondition::LetPattern(_, value) = &mut block.condition {
                self.visit_expr(value);
            }
        } else {
            walk_let_condition(self, &mut block.condition);
            self.visit_stmts(&mut block.body.stmts);
        }
    }
}

impl<'src> MutVisitor<'src> for RenameIdent<'src> {
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        for stmt in stmts.iter_mut() {
            self.visit_stmt(stmt);
            if let Stmt::Let { name, .. } = &stmt.0
                && name.0 == self.old
            {
                // shadowed for the rest of the list
                return;
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        match &mut stmt.0 {
            Stmt::ForIn { name, iter, .. } if name.0 == self.old => self.visit_expr(iter),
            Stmt::If { blocks, else_ } => {
                for block in blocks.iter_mut() {
                    self.visit_conditional_block(block);
                }
                if let Some(else_) = else_ {
                    self.visit_stmts(&mut else_.stmts);
                }
            }
            Stmt::While(block) => self.visit_conditional_block(block),
            Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                self.visit_expr(expr);
                for case in cases.iter_mut() {
                    match &mut case.condition {
                        Condition::Expr(expr) => {
                            self.visit_expr(expr);
                            self.visit_stmts(&mut case.body);
                        }
                        Condition::Pattern((pat, _)) => {
                            if !pattern_bindings(pat).iter().any(|n| n.0 == self.old) {
                                self.visit_stmts(&mut case.body);
                            }
                        }
                    }
                }
                if let Some(default) = default {
                    self.visit_stmts(default);
                }
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        if let Expr::Ident(name) = &mut expr.0
            && *name == self.old
        {
            *name = self.new;
        }
        walk_expr(self, expr);
    }

    fn visit_lambda(
        &mut self,
        params: &mut Box<[SParam<'src>]>,
        body: &mut SourceFunctionBody<'src>,
    ) {
        if params.iter().all(|(p, _)| p.name != self.old) {
            walk_body(self, body);
        }
    }
}
