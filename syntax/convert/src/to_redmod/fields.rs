//! REDmod rejects the reserved words (`hint`, `event`...) as the names of fields. The fields of
//! the classes and structs declared by the sources that use one are renamed (`hint_`), along with
//! their uses:
//! - the declarations,
//! - the accesses through `this`, explicit or implicit, in the methods of the class and of its
//!   subclasses,
//! - the member accesses whose receiver has the type of the class or of a subclass, as far as the
//!   type information or the declarations of the module tell. An access to a member with the
//!   name of a renamed field on a value of unknown type gets a warning.
//!
//! The classes of every file of the conversion are collected beforehand ([`SourceFields`]), so
//! that the uses in the other files and in the subclasses declared there are renamed too. When a
//! module is converted on its own, only its own classes are known.
//!
//! The fields of the game classes, including the ones added with `@addField`, are never renamed:
//! they keep the `reserved-field-name` warning. So are the fields of the `native` and `importonly`
//! classes declared by the sources, which describe classes of the game.

use hashbrown::{HashMap, HashSet};
use redscript_ast::{
    Condition, Expr, Import, Item, ItemQualifiers, LetCondition, Pattern, SourceConditionalBlock,
    SourceFunctionBody, SourceModule, SourcePattern, Span, Stmt,
};

use crate::Cx;
use crate::index::{LocalTypes, ModuleIndex, TypeEnv};
use crate::walk::{
    MutVisitor, REDMOD_RESERVED, SExpr, SParam, SStmt, for_each_function, pattern_bindings,
    type_name, walk_body, walk_expr, walk_stmt, walk_stmts,
};

/// The classes and structs declared by the converted sources, with their fields whose names
/// REDmod rejects and the names they get. Collected from every file before the conversion, see
/// [`ConvertOptions::fields`](crate::ConvertOptions::fields).
#[derive(Debug, Default)]
pub struct SourceFields {
    /// The classes, by qualified name.
    classes: HashMap<String, SourceClass>,
    /// The qualified names of the classes, by name without the module path.
    by_simple: HashMap<String, Vec<String>>,
    /// The original names of the renamed fields.
    old_names: HashSet<String>,
}

#[derive(Debug, Default)]
struct SourceClass {
    /// The qualified name of the base class, when it's declared by the sources.
    base: Option<String>,
    /// The names of all the fields declared by the class.
    fields: HashSet<String>,
    /// The new names of the renamed fields declared by the class, by original name.
    renamed: HashMap<String, String>,
}

impl SourceFields {
    /// Collects the classes declared by `modules`, the modules of all the files converted
    /// together.
    pub fn collect<'a, 'src: 'a>(
        modules: impl IntoIterator<Item = &'a SourceModule<'src>>,
    ) -> Self {
        let mut this = Self::default();
        let mut bases = vec![];
        let mut reserved = vec![];
        for module in modules {
            let resolver = Resolver::of(module);
            for (decl, _) in &module.items[..] {
                let (Item::Class(agg) | Item::Struct(agg)) = &decl.item else {
                    continue;
                };
                if decl
                    .qualifiers
                    .intersects(ItemQualifiers::NATIVE | ItemQualifiers::IMPORT_ONLY)
                {
                    continue;
                }
                let name = agg.name.0;
                let qualified = resolver.qualify(name);
                let class = this.classes.entry(qualified.clone()).or_default();
                for (member, _) in &agg.items {
                    if let Item::Let(field) = &member.item {
                        let field = field.name.0;
                        class.fields.insert(field.to_owned());
                        if is_renamed_field(member.qualifiers, field) {
                            reserved.push((qualified.clone(), field.to_owned()));
                        }
                    }
                }
                this.by_simple
                    .entry(name.to_owned())
                    .or_default()
                    .push(qualified.clone());
                if let Some(base) = agg.extends.as_deref().and_then(|(t, _)| type_name(t)) {
                    bases.push((qualified, base.to_owned(), resolver.clone()));
                }
            }
        }
        for (class, base, resolver) in bases {
            let base = this.resolve(&base, &resolver).filter(|base| *base != class);
            if let Some(info) = this.classes.get_mut(&class) {
                info.base = base;
            }
        }
        for (class, field) in reserved {
            let taken: HashSet<String> = this
                .related(&class)
                .into_iter()
                .flat_map(|name| {
                    let info = &this.classes[name.as_str()];
                    info.fields.iter().chain(info.renamed.values()).cloned()
                })
                .collect();
            let base = format!("{field}_");
            let new = if taken.contains(&base) {
                (1..)
                    .map(|i| format!("{base}_{i}"))
                    .find(|name| !taken.contains(name))
                    .expect("infinite iterator")
            } else {
                base
            };
            this.old_names.insert(field.clone());
            if let Some(info) = this.classes.get_mut(&class) {
                info.renamed.insert(field, new);
            }
        }
        this
    }

    /// Whether no field is renamed.
    pub fn is_empty(&self) -> bool {
        self.old_names.is_empty()
    }

    /// The new names of the renamed fields of the class `class`, as named in the module of
    /// `resolver`, and of its bases.
    pub(crate) fn new_names_in(&self, resolver: &Resolver, class: &str) -> HashSet<String> {
        let Some(class) = self.resolve(class, resolver) else {
            return HashSet::new();
        };
        self.ancestors(&class)
            .into_iter()
            .flat_map(|name| self.classes[name].renamed.values().cloned())
            .collect()
    }

    /// The new name of the field `field` of the class `class` (qualified) or of one of its
    /// bases, when it's renamed.
    fn renamed(&self, class: &str, field: &str) -> Option<&str> {
        self.ancestors(class)
            .into_iter()
            .find_map(|name| self.classes[name].renamed.get(field).map(String::as_str))
    }

    /// The classes declaring a field `field` that is renamed.
    fn declaring(&self, field: &str) -> Vec<&str> {
        let mut classes: Vec<_> = self
            .classes
            .iter()
            .filter(|(_, info)| info.renamed.contains_key(field))
            .map(|(name, _)| name.as_str())
            .collect();
        classes.sort_unstable();
        classes
    }

    /// `class` and its bases declared by the sources, from the most derived.
    fn ancestors<'a>(&'a self, class: &'a str) -> Vec<&'a str> {
        let mut res = vec![];
        let mut current = self.classes.get_key_value(class).map(|(k, _)| k.as_str());
        while let Some(name) = current.filter(|name| !res.contains(name)) {
            res.push(name);
            current = self.classes[name].base.as_deref();
        }
        res
    }

    /// `class`, its bases and its subclasses declared by the sources.
    fn related(&self, class: &str) -> Vec<String> {
        let mut res: Vec<String> = self
            .ancestors(class)
            .into_iter()
            .map(str::to_owned)
            .collect();
        for name in self.classes.keys() {
            if name != class && self.ancestors(name).contains(&class) {
                res.push(name.clone());
            }
        }
        res
    }

    /// The qualified name of the class of the sources named `name` in a module.
    fn resolve(&self, name: &str, resolver: &Resolver) -> Option<String> {
        let exists = |name: &str| self.classes.contains_key(name);
        if name.contains('.') {
            return exists(name).then(|| name.to_owned());
        }
        let in_module = resolver.qualify(name);
        if exists(&in_module) {
            return Some(in_module);
        }
        for import in &resolver.imports {
            let candidate = match import {
                ImportSpec::Exact(path) => path
                    .rsplit_once('.')
                    .filter(|(_, last)| *last == name)
                    .map(|_| path.clone()),
                ImportSpec::Select(path, names) => names
                    .iter()
                    .any(|n| n == name)
                    .then(|| format!("{path}.{name}")),
                ImportSpec::All(path) => Some(format!("{path}.{name}")),
            };
            if let Some(candidate) = candidate.filter(|c| exists(c)) {
                return Some(candidate);
            }
        }
        if exists(name) {
            return Some(name.to_owned());
        }
        match self.by_simple.get(name).map(Vec::as_slice) {
            Some([only]) => Some(only.clone()),
            _ => None,
        }
    }
}

/// Whether a field named `field` declared with `qualifiers` in a class of the sources is renamed.
fn is_renamed_field(qualifiers: ItemQualifiers, field: &str) -> bool {
    REDMOD_RESERVED.contains(&field) && !qualifiers.contains(ItemQualifiers::NATIVE)
}

/// The spans of the field declarations of `module` that are renamed, which need no other
/// diagnostic.
pub fn renamed_field_spans(module: &SourceModule<'_>) -> HashSet<Span> {
    let mut spans = HashSet::new();
    for (decl, _) in &module.items[..] {
        let (Item::Class(agg) | Item::Struct(agg)) = &decl.item else {
            continue;
        };
        if decl
            .qualifiers
            .intersects(ItemQualifiers::NATIVE | ItemQualifiers::IMPORT_ONLY)
        {
            continue;
        }
        for (member, span) in &agg.items {
            if let Item::Let(field) = &member.item
                && is_renamed_field(member.qualifiers, field.name.0)
            {
                spans.insert(*span);
            }
        }
    }
    spans
}

/// How the names of the types are resolved in a module.
#[derive(Debug, Clone)]
pub(crate) struct Resolver {
    /// The module path.
    module: Option<String>,
    imports: Vec<ImportSpec>,
}

#[derive(Debug, Clone)]
enum ImportSpec {
    /// The qualified name of an imported item.
    Exact(String),
    /// A module path and the names imported from it.
    Select(String, Vec<String>),
    /// A module path whose items are all imported.
    All(String),
}

impl Resolver {
    pub(crate) fn of(module: &SourceModule<'_>) -> Self {
        let module_path = module.path.as_ref().map(|path| path.segments.join("."));
        let imports = module
            .items
            .iter()
            .filter_map(|(decl, _)| match &decl.item {
                Item::Import(import) => Some(match import {
                    Import::Exact(path) => ImportSpec::Exact(path.segments.join(".")),
                    Import::Select(path, names) => ImportSpec::Select(
                        path.segments.join("."),
                        names.iter().map(|n| (*n).to_owned()).collect(),
                    ),
                    Import::All(path) => ImportSpec::All(path.segments.join(".")),
                }),
                _ => None,
            })
            .collect();
        Self {
            module: module_path,
            imports,
        }
    }

    /// The qualified name of an item named `name` declared in the module.
    fn qualify(&self, name: &str) -> String {
        match &self.module {
            Some(module) => format!("{module}.{name}"),
            None => name.to_owned(),
        }
    }
}

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    let owned;
    let fields = if let Some(fields) = cx.opts.fields {
        fields
    } else {
        owned = SourceFields::collect([&*module]);
        &owned
    };
    if fields.is_empty() {
        return;
    }
    let resolver = Resolver::of(module);
    // built before the declarations are renamed, the uses are looked up with their old names
    let index = ModuleIndex::new(module);

    for_each_function(module, |info, func| {
        let locals = LocalTypes::of(func);
        let env = TypeEnv::new(&index, &locals, info.class, cx);
        let this = info
            .class
            .and_then(|class| fields.resolve(class, &resolver));
        let mut uses = Uses {
            cx: &mut *cx,
            env: &env,
            fields,
            resolver: &resolver,
            this,
            scopes: vec![func.params.iter().map(|(p, _)| p.name).collect()],
        };
        if let Some(body) = &mut func.body {
            walk_body(&mut uses, body);
        }
    });

    for (decl, _) in &mut module.items {
        let (Item::Class(agg) | Item::Struct(agg)) = &mut decl.item else {
            continue;
        };
        if decl
            .qualifiers
            .intersects(ItemQualifiers::NATIVE | ItemQualifiers::IMPORT_ONLY)
        {
            continue;
        }
        let class = resolver.qualify(agg.name.0);
        let Some(info) = fields.classes.get(&class) else {
            continue;
        };
        for (member, span) in &mut agg.items {
            let Item::Let(field) = &mut member.item else {
                continue;
            };
            let old = field.name.0;
            let Some(new) = info.renamed.get(old) else {
                continue;
            };
            field.name.0 = cx.arena.alloc(new.as_str());
            if member.qualifiers.contains(ItemQualifiers::PERSISTENT) {
                cx.warn(
                    *span,
                    "renamed-persistent-field",
                    format!(
                        "`{old}` is a reserved word in REDmod, this persistent field was renamed \
                         to `{new}` with its uses: the values saved under the old name are not \
                         loaded into it"
                    ),
                );
            } else {
                cx.info(
                    *span,
                    "renamed-field",
                    format!(
                        "`{old}` is a reserved word in REDmod, this field was renamed to `{new}` \
                         with its uses"
                    ),
                );
            }
        }
    }
}

/// Renames the uses of the renamed fields in a function body.
struct Uses<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
    env: &'a TypeEnv<'a, 'src>,
    fields: &'a SourceFields,
    resolver: &'a Resolver,
    /// The class of the sources `this` refers to, qualified.
    this: Option<String>,
    /// The locals in scope.
    scopes: Vec<HashSet<&'src str>>,
}

/// The class of the value a member is read from.
enum Receiver {
    /// A class of the sources, qualified.
    Source(String),
    /// Another type.
    Other,
    Unknown,
}

impl<'src> Uses<'_, '_, 'src> {
    fn is_local(&self, name: &str) -> bool {
        self.scopes.iter().any(|scope| scope.contains(name))
    }

    fn bind(&mut self, name: &'src str) {
        self.scopes
            .last_mut()
            .expect("there should be a scope")
            .insert(name);
    }

    fn scoped(&mut self, f: impl FnOnce(&mut Self)) {
        self.scopes.push(HashSet::new());
        f(self);
        self.scopes.pop();
    }

    fn alloc(&self, name: &str) -> &'src str {
        self.cx.arena.alloc(name)
    }

    fn receiver(&self, receiver: &SExpr<'src>) -> Receiver {
        let from_name = |name: Option<String>| match name {
            Some(name) => Receiver::Source(name),
            None => Receiver::Other,
        };
        if matches!(receiver.0, Expr::This | Expr::Super) {
            return from_name(self.this.clone());
        }
        let oracle = self
            .cx
            .opts
            .types
            .and_then(|types| types.type_of(receiver.1));
        if let Some(hint) = oracle {
            let class = hint
                .name()
                .filter(|name| self.fields.classes.contains_key(*name))
                .map(str::to_owned);
            return from_name(class);
        }
        match self.env.type_of(receiver) {
            Some(typ) => {
                from_name(type_name(&typ).and_then(|name| self.fields.resolve(name, self.resolver)))
            }
            None => Receiver::Unknown,
        }
    }

    /// Renames the field read from `receiver` or warns when its type is unknown.
    fn member(&mut self, receiver: &SExpr<'src>, member: &mut &'src str, span: Span) {
        match self.receiver(receiver) {
            Receiver::Source(class) => {
                if let Some(new) = self.fields.renamed(&class, member) {
                    *member = self.alloc(new);
                }
            }
            Receiver::Other => {}
            Receiver::Unknown => {
                let classes = self.fields.declaring(member);
                let new: Vec<_> = classes
                    .iter()
                    .filter_map(|class| self.fields.renamed(class, member))
                    .collect();
                self.cx.warn(
                    span,
                    "reserved-field-unresolved",
                    format!(
                        "`.{member}` is read from a value of unknown type: if it's the field of \
                         `{}`, renamed to `{}`, rename it here too",
                        classes.join("`, `"),
                        new.first().copied().unwrap_or_default()
                    ),
                );
            }
        }
    }

    /// Renames the fields of the aggregate patterns of a class of the sources and declares the
    /// variables bound by `pattern`.
    fn pattern(&mut self, pattern: &mut SourcePattern<'src>) {
        rename_pattern_fields(self, pattern);
        for (name, _) in pattern_bindings(pattern) {
            self.bind(name);
        }
    }

    fn conditional(&mut self, block: &mut SourceConditionalBlock<'src>) {
        match &mut block.condition {
            LetCondition::Expr(expr) => {
                self.visit_expr(expr);
                self.visit_stmts(&mut block.body.stmts);
            }
            LetCondition::LetPattern((pattern, _), value) => {
                self.visit_expr(value);
                self.scoped(|this| {
                    this.pattern(pattern);
                    this.visit_stmts(&mut block.body.stmts);
                });
            }
        }
    }
}

fn rename_pattern_fields<'src>(uses: &Uses<'_, '_, 'src>, pattern: &mut SourcePattern<'src>) {
    match pattern {
        Pattern::Name(_) => {}
        Pattern::As(inner, _) | Pattern::Nullable(inner) => {
            rename_pattern_fields(uses, &mut inner.0);
        }
        Pattern::Aggregate((name, _), fields) => {
            let class = uses.fields.resolve(name, uses.resolver);
            for ((field, _), pattern) in fields.iter_mut() {
                if let Some(new) = class
                    .as_deref()
                    .and_then(|class| uses.fields.renamed(class, field))
                {
                    *field = uses.alloc(new);
                }
                rename_pattern_fields(uses, pattern);
            }
        }
        Pattern::Array(_, (patterns, _)) => {
            for pattern in patterns.iter_mut() {
                rename_pattern_fields(uses, pattern);
            }
        }
    }
}

impl<'src> MutVisitor<'src> for Uses<'_, '_, 'src> {
    fn visit_stmts(&mut self, stmts: &mut Box<[SStmt<'src>]>) {
        self.scoped(|this| walk_stmts(this, stmts));
    }

    fn visit_stmt(&mut self, stmt: &mut SStmt<'src>) {
        match &mut stmt.0 {
            Stmt::Let { name, value, .. } => {
                if let Some(value) = value {
                    self.visit_expr(value);
                }
                self.bind(name.0);
            }
            Stmt::ForIn { name, iter, body } => {
                self.visit_expr(iter);
                self.scoped(|this| {
                    this.bind(name.0);
                    this.visit_stmts(&mut body.stmts);
                });
            }
            Stmt::For { .. } => self.scoped(|this| walk_stmt(this, stmt)),
            Stmt::If { blocks, else_ } => {
                for block in blocks.iter_mut() {
                    self.conditional(block);
                }
                if let Some(else_) = else_ {
                    self.visit_stmts(&mut else_.stmts);
                }
            }
            Stmt::While(block) => self.conditional(block),
            Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                self.visit_expr(expr);
                for case in cases.iter_mut() {
                    self.scoped(|this| {
                        match &mut case.condition {
                            Condition::Expr(expr) => this.visit_expr(expr),
                            Condition::Pattern((pattern, _)) => this.pattern(pattern),
                        }
                        this.visit_stmts(&mut case.body);
                    });
                }
                if let Some(default) = default {
                    self.visit_stmts(default);
                }
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        let span = expr.1;
        match &mut expr.0 {
            Expr::Ident(name) => {
                if !self.is_local(name)
                    && let Some(class) = &self.this
                    && let Some(new) = self.fields.renamed(class, name)
                {
                    *name = self.alloc(new);
                }
            }
            Expr::Member {
                expr: receiver,
                member,
            } if self.fields.old_names.contains(*member) => {
                // the receiver keeps its original names while its type is looked up
                self.member(receiver, member, span);
            }
            // a method name is not a field
            Expr::Call {
                expr: callee, args, ..
            } => {
                match &mut callee.0 {
                    Expr::Ident(_) => {}
                    Expr::Member { expr: receiver, .. } => self.visit_expr(receiver),
                    _ => self.visit_expr(callee),
                }
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
                return;
            }
            _ => {}
        }
        walk_expr(self, expr);
    }

    fn visit_lambda(
        &mut self,
        params: &mut Box<[SParam<'src>]>,
        body: &mut SourceFunctionBody<'src>,
    ) {
        self.scoped(|this| {
            for (param, _) in params.iter() {
                this.bind(param.name);
            }
            walk_body(this, body);
        });
    }
}

#[cfg(test)]
mod tests {
    use redscript_ast::Dialect;
    use redscript_formatter::{ParseOptions, parse_document_with_options};

    use super::*;
    use crate::test_util::{
        MapTypeQuery, codes, run_pass_with_options, span_in, span_of, squash, test_file_id,
        to_redmod, to_redmod_with_types,
    };
    use crate::{Severity, TypeHint};

    const SRC: &str = "module Mod.M

public class Callback extends DelayCallback {
  public let event: ref<Event>;
  public let hint: Bool;
  public let other: Int32;

  public func Call(hint: Bool) -> Void {
    this.event = null;
    let x = event;
    this.hint = hint;
    Queue(this.event, this.event());
  }
}

class Sub extends Callback {
  func Use() {
    let e = this.event;
    let f = event;
    let event = 1;
    let g = event;
  }
}

struct Consumed {
  let hint: Bool;
  persistent let static: Int32;
}

func Free(c: ref<Callback>, d: Consumed, u: ref<Unknown>, w: wref<inkWidget>) {
  c.event = null;
  let h = d.hint;
  let i = u.hint;
  let j = w.name;
  let k = Make().hint;
}
";

    #[test]
    fn fields_and_their_uses_are_renamed() {
        let (out, diags) = to_redmod(SRC, run);
        let out = squash(&out);
        for expected in [
            "public let event_: Event;",
            "public let hint_: Bool;",
            "public let other: Int32;",
            "this.event_ = NULL;",
            "let x = event_;",
            // the parameter shadows the field
            "this.hint_ = hint;",
            // a method call is not a field
            "Queue(this.event_, this.event());",
            "let e = this.event_;",
            "let f = event_;",
            "let g = event;",
            "let hint_: Bool;",
            "persistent let static_: Int32;",
            "c.event_ = NULL;",
            "let h = d.hint_;",
            // of unknown or other types
            "let i = u.hint;",
            "let j = w.name;",
            "let k = Make().hint;",
        ] {
            assert!(out.contains(expected), "{expected}: {out}");
        }
        let mut codes = codes(&diags);
        codes.sort_unstable();
        assert_eq!(
            codes,
            [
                "renamed-field",
                "renamed-field",
                "renamed-field",
                "renamed-persistent-field",
                "reserved-field-unresolved",
            ]
        );
        let unresolved = diags
            .iter()
            .find(|d| d.code == "reserved-field-unresolved")
            .unwrap();
        assert_eq!(unresolved.severity, Severity::Warning);
        assert!(
            unresolved
                .message
                .contains("`Mod.M.Callback`, `Mod.M.Consumed`")
                && unresolved.message.contains("`hint_`"),
            "{unresolved}"
        );
        assert_eq!(unresolved.span, span_of(SRC, "Make().hint", 0));
        let persistent = diags
            .iter()
            .find(|d| d.code == "renamed-persistent-field")
            .unwrap();
        assert_eq!(persistent.severity, Severity::Warning);
    }

    #[test]
    fn receivers_are_typed_with_type_information() {
        let types = MapTypeQuery::new()
            .with_type(
                span_in(SRC, "u.hint", 0, "u"),
                TypeHint::Class("Mod.N.Unknown".to_owned()),
            )
            .with_type(
                span_in(SRC, "Make().hint", 0, "Make()"),
                TypeHint::Ref(
                    crate::RefKind::Weak,
                    TypeHint::Class("Mod.M.Sub".to_owned()).into(),
                ),
            );
        let (out, diags) = to_redmod_with_types(SRC, &types, run);
        let out = squash(&out);
        assert!(out.contains("let i = u.hint;"), "{out}");
        assert!(out.contains("let k = Make().hint_;"), "{out}");
        assert!(!codes(&diags).contains(&"reserved-field-unresolved"));
    }

    fn parse(src: &str) -> redscript_formatter::Document<'_> {
        let opts = ParseOptions::compat(Dialect::Community);
        let (doc, errors) = parse_document_with_options(src, test_file_id(), opts);
        assert!(errors.is_empty(), "{errors:?}");
        doc.unwrap()
    }

    #[test]
    fn fields_of_other_files_are_renamed() {
        let base = "module Mod.A
public class Base {
  public let hint: Int32;
  public let hint_: Int32;
}
";
        let sub = "module Mod.B
import Mod.A.*

class Sub extends Base {
  func F(b: ref<Base>) -> Int32 {
    return hint + this.hint_ + b.hint;
  }
}
";
        let user = "module Mod.C
import Mod.B.*
func G(s: ref<Sub>, t: ref<Other>) -> Int32 = s.hint + t.hint;
";
        let (base_doc, sub_doc, user_doc) = (parse(base), parse(sub), parse(user));
        let fields = SourceFields::collect([&base_doc.module, &sub_doc.module, &user_doc.module]);
        let (out, diags) = run_pass_with_options(sub, None, Some(&fields), run);
        assert!(
            squash(&out).contains("return hint__1 + this.hint_ + b.hint__1;"),
            "{out}"
        );
        assert!(diags.is_empty(), "{diags:?}");

        let (out, diags) = run_pass_with_options(user, None, Some(&fields), run);
        assert!(squash(&out).contains("s.hint__1 + t.hint;"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");

        // the declaring file
        let (out, diags) = run_pass_with_options(base, None, Some(&fields), run);
        let out = squash(&out);
        assert!(
            out.contains("public let hint__1: Int32; public let hint_: Int32;"),
            "{out}"
        );
        assert_eq!(codes(&diags), ["renamed-field"]);
    }

    #[test]
    fn game_and_native_fields_are_not_renamed() {
        let src = "native class N {
  native let name: CName;
}

@addField(PlayerPuppet)
let hint: Int32;

class C {
  native let event: Int32;
}

func F(n: ref<N>, p: ref<PlayerPuppet>) -> CName {
  p.hint = 1;
  return n.name;
}
";
        let (out, diags) = to_redmod(src, run);
        assert_eq!(out.matches("hint_").count(), 0, "{out}");
        assert_eq!(out.matches("name_").count(), 0, "{out}");
        assert_eq!(out.matches("event_").count(), 0, "{out}");
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn nothing_changes_without_reserved_fields() {
        let src = "class C {
  let other: Int32;
  func F(x: ref<Unknown>) -> Int32 = x.hint + this.other;
}
";
        let (out, diags) = to_redmod(src, run);
        assert!(squash(&out).contains("x.hint + this.other"), "{out}");
        assert!(diags.is_empty(), "{diags:?}");
    }
}
