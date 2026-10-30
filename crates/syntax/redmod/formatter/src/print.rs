//! Syntax-only printing of the native REDmod AST, without semantic lowering.

use std::fmt::{self, Write};

use redscript_redmod_ast::*;

/// Machine-readable reason a native tree cannot be emitted faithfully.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrintErrorKind {
    ImportsAfterDeclarations,
    InvalidIdentifier,
    EmptyModulePath,
    ImportModifiers,
    MemberImport,
    UnsupportedDocumentation,
    EmptyNameGroup,
    InvalidConstReturn,
    EnumValueOutOfRange,
    ArraySizeOutOfRange,
    InvalidTypeArguments,
    MissingCondition,
    InvalidForInitializer,
    EmptyArrayLiteral,
    InvalidSequence,
    InvalidOmittedArgument,
    RecoveredExpression,
    NonFiniteFloat,
    NonFiniteDouble,
    UnsignedLiteral,
    LiteralDelimiter,
    LiteralNul,
    LiteralControl,
    UnverifiedLiteral,
}

impl fmt::Display for PrintErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ImportsAfterDeclarations => "imports must precede other declarations",
            Self::InvalidIdentifier => "invalid REDmod identifier in this syntax position",
            Self::EmptyModulePath => "empty module path",
            Self::ImportModifiers => "using directives cannot carry declaration prefixes",
            Self::MemberImport => "using directives must be at module scope",
            Self::UnsupportedDocumentation => "unsupported documentation comment",
            Self::EmptyNameGroup => "a declaration requires at least one name",
            Self::InvalidConstReturn => "const return requires a return type",
            Self::EnumValueOutOfRange => "enum value exceeds REDmod literal range",
            Self::ArraySizeOutOfRange => "static array size exceeds REDmod integer range",
            Self::InvalidTypeArguments => "invalid native type arguments",
            Self::MissingCondition => "if requires a condition",
            Self::InvalidForInitializer => "for initializer must be a local or expression",
            Self::EmptyArrayLiteral => "empty array literals are unsupported",
            Self::InvalidSequence => {
                "a comma sequence requires at least two discarded or for-header expressions"
            }
            Self::InvalidOmittedArgument => "omitted expressions require an argument slot",
            Self::RecoveredExpression => "cannot print a recovered error expression",
            Self::NonFiniteFloat => "non-finite float literal",
            Self::NonFiniteDouble => "non-finite double literal",
            Self::UnsignedLiteral => "unsigned literals require type-aware lowering",
            Self::LiteralDelimiter => "embedded delimiter has no verified REDmod literal spelling",
            Self::LiteralNul => "NUL has no verified REDmod literal spelling",
            Self::LiteralControl => "unverified control character in REDmod literal",
            Self::UnverifiedLiteral => "raw backslashes or controls are unverified in R/T literals",
        })
    }
}

/// A syntax or literal fidelity error, with the nearest available source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrintError {
    pub kind: PrintErrorKind,
    pub span: Option<Span>,
}

impl fmt::Display for PrintError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::error::Error for PrintError {}

type Result<T> = std::result::Result<T, PrintError>;

fn error(kind: PrintErrorKind, span: Option<Span>) -> PrintError {
    PrintError { kind, span }
}

fn at<T>(result: Result<T>, span: Span) -> Result<T> {
    result.map_err(|mut error| {
        error.span.get_or_insert(span);
        error
    })
}

/// Print a native module. Modifier and switch-label order is retained.
pub fn print_module(module: &Module<'_>) -> Result<String> {
    let mut printer = Printer::default();
    if let Some(path) = &module.path {
        printer.out.push_str("@module \"");
        printer.path(path)?;
        printer.out.push_str("\";\n\n");
    }
    printer.decls(&module.items, DeclScope::Module)?;
    Ok(printer.out)
}

/// Print a standalone declaration at module scope.
pub fn print_item_decl(decl: &Declaration<'_>) -> Result<String> {
    let mut printer = Printer::default();
    printer.decl(decl, DeclScope::Module)?;
    Ok(printer.out)
}

/// Print a standalone declaration in class/struct member context.
pub fn print_member_decl(decl: &Declaration<'_>) -> Result<String> {
    let mut printer = Printer::default();
    printer.decl(decl, DeclScope::Member)?;
    Ok(printer.out)
}

#[derive(Clone, Copy)]
enum DeclScope {
    Module,
    Member,
}

#[derive(Default)]
struct Printer {
    out: String,
    indent: usize,
}

impl Printer {
    fn indent(&mut self) {
        for _ in 0..self.indent {
            self.out.push_str("    ");
        }
    }

    fn indented(&mut self, print: impl FnOnce(&mut Self) -> Result<()>) -> Result<()> {
        let indent = self.indent;
        self.indent += 1;
        let result = print(self);
        self.indent = indent;
        result
    }

    fn ident(&mut self, name: &str) -> Result<()> {
        // Contextual keywords are legal native declaration/type/member names.
        let mut chars = name.chars();
        if !chars
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
            || !chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            || matches!(
                name,
                "true" | "false" | "NULL" | "this" | "super" | "case" | "default"
            )
        {
            return Err(error(PrintErrorKind::InvalidIdentifier, None));
        }
        self.out.push_str(name);
        Ok(())
    }

    fn name(&mut self, name: &Name<'_>) -> Result<()> {
        at(self.ident(name.0), name.1)
    }

    fn names(&mut self, names: &[Name<'_>]) -> Result<()> {
        if names.is_empty() {
            return Err(error(PrintErrorKind::EmptyNameGroup, None));
        }
        self.comma_list(names, Self::name)
    }

    fn comma_list<T>(
        &mut self,
        items: &[T],
        mut print: impl FnMut(&mut Self, &T) -> Result<()>,
    ) -> Result<()> {
        for (i, item) in items.iter().enumerate() {
            if i != 0 {
                self.out.push_str(", ");
            }
            print(self, item)?;
        }
        Ok(())
    }

    fn path(&mut self, path: &Path<'_>) -> Result<()> {
        if path.segments.is_empty() {
            return Err(error(PrintErrorKind::EmptyModulePath, None));
        }
        for (i, part) in path.segments.iter().enumerate() {
            let mut chars = part.chars();
            if !chars
                .next()
                .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                || !chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            {
                return Err(error(PrintErrorKind::InvalidIdentifier, None));
            }
            if i != 0 {
                self.out.push('.');
            }
            self.out.push_str(part);
        }
        Ok(())
    }

    fn decls(&mut self, decls: &[Spanned<Declaration<'_>>], scope: DeclScope) -> Result<()> {
        let mut saw_non_import = false;
        for (decl, span) in decls {
            if matches!(scope, DeclScope::Module) {
                if matches!(decl.item, Item::Import(_)) {
                    if saw_non_import {
                        return Err(error(PrintErrorKind::ImportsAfterDeclarations, Some(*span)));
                    }
                } else {
                    saw_non_import = true;
                }
            }
            at(self.decl(decl, scope), *span)?;
        }
        Ok(())
    }

    fn decl(&mut self, decl: &Declaration<'_>, scope: DeclScope) -> Result<()> {
        if let Item::Import(path) = &decl.item {
            if matches!(scope, DeclScope::Member) {
                return Err(error(PrintErrorKind::MemberImport, None));
            }
            if decl.visibility.is_some()
                || !decl.modifiers.is_empty()
                || !decl.metadata.is_empty()
                || !decl.annotations.is_empty()
            {
                return Err(error(PrintErrorKind::ImportModifiers, None));
            }
            self.decl_prefix(decl)?;
            self.out.push_str("@using \"");
            self.path(path)?;
            self.out.push_str("\";\n");
            return Ok(());
        }
        self.decl_prefix(decl)?;
        self.item(&decl.item)
    }

    fn decl_prefix(&mut self, decl: &Declaration<'_>) -> Result<()> {
        for doc in &decl.docs {
            if !doc.starts_with("///") || doc.starts_with("////") || doc.contains(['\r', '\n']) {
                return Err(error(PrintErrorKind::UnsupportedDocumentation, None));
            }
            self.indent();
            self.out.push_str(doc);
            self.out.push('\n');
        }
        for (meta, span) in &decl.metadata {
            self.indent();
            self.out.push('[');
            at(self.name(&meta.key), *span)?;
            self.out.push_str(" = ");
            self.expr(&meta.value)?;
            self.out.push_str("]\n");
        }
        for (annotation, span) in &decl.annotations {
            self.indent();
            self.out.push('@');
            at(self.name(&annotation.name), *span)?;
            self.out.push('(');
            self.exprs(&annotation.args)?;
            self.out.push_str(")\n");
        }
        self.indent();
        if let Some(visibility) = decl.visibility {
            self.out.push_str(match visibility {
                Visibility::Public => "public ",
                Visibility::Protected => "protected ",
                Visibility::Private => "private ",
            });
        }
        for (modifier, _) in &decl.modifiers {
            self.out.push_str(match modifier {
                Modifier::Abstract => "abstract ",
                Modifier::Const => "const ",
                Modifier::Exec => "exec ",
                Modifier::Final => "final ",
                Modifier::ImportOnly => "importonly ",
                Modifier::Import => "import ",
                Modifier::Persistent => "persistent ",
                Modifier::Quest => "quest ",
                Modifier::Static => "static ",
                Modifier::Editable => "editable ",
                Modifier::InstanceEditable => "instanceeditable ",
                Modifier::Inlined => "inlined ",
                Modifier::Replicated => "replicated ",
                Modifier::TestOnly => "testonly ",
                Modifier::Override => "override ",
                Modifier::Export => "export ",
                Modifier::Virtual => "virtual ",
                Modifier::Constexpr => "constexpr ",
                Modifier::Mutable => "mutable ",
            });
        }
        Ok(())
    }

    fn item(&mut self, item: &Item<'_>) -> Result<()> {
        match item {
            Item::Import(_) => unreachable!(),
            Item::Field(field) => {
                self.out.push_str("var ");
                self.names(&field.names)?;
                self.out.push_str(": ");
                self.typ(&field.typ)?;
                if let Some(default) = &field.default {
                    self.out.push_str(" = ");
                    self.expr(default)?;
                }
                self.out.push_str(";\n");
            }
            Item::Aggregate {
                is_struct,
                name,
                extends,
                items,
            } => {
                self.out
                    .push_str(if *is_struct { "struct " } else { "class " });
                self.name(name)?;
                if let Some(extends) = extends {
                    self.out.push_str(" extends ");
                    self.typ(extends)?;
                }
                self.out.push_str(" {\n");
                self.indented(|printer| printer.decls(items, DeclScope::Member))?;
                self.indent();
                self.out.push_str("}\n");
            }
            Item::Function {
                kind,
                params,
                return_type,
                const_return,
                body,
            } => {
                match kind {
                    FunctionKind::Function(name) | FunctionKind::Event(name) => {
                        self.out
                            .push_str(if matches!(kind, FunctionKind::Event(_)) {
                                "event "
                            } else {
                                "function "
                            });
                        self.name(name)?;
                    }
                    FunctionKind::Operator((op, _)) => {
                        self.out.push_str("operator ");
                        self.out.push_str(operator(*op));
                    }
                    FunctionKind::Cast { implicit, .. } => {
                        self.out.push_str(if implicit.is_some() {
                            "implicit cast"
                        } else {
                            "cast"
                        });
                    }
                }
                self.out.push('(');
                self.comma_list(params, |printer, (param, span)| {
                    at(printer.param(param), *span)
                })?;
                self.out.push(')');
                if let Some(typ) = return_type {
                    self.out.push_str(" -> ");
                    if const_return.is_some() {
                        self.out.push_str("const ");
                    }
                    self.typ(typ)?;
                } else if const_return.is_some() {
                    return Err(error(PrintErrorKind::InvalidConstReturn, *const_return));
                }
                if let Some(body) = body {
                    self.out.push(' ');
                    self.block(body)?;
                    self.out.push('\n');
                } else {
                    self.out.push_str(";\n");
                }
            }
            Item::Enum {
                name,
                variants,
                storage,
            } => {
                self.out.push_str("enum ");
                self.name(name)?;
                if let Some(typ) = storage {
                    self.out.push_str(": ");
                    self.typ(typ)?;
                }
                self.out.push_str(" {\n");
                self.indented(|printer| {
                    for (variant, _) in variants {
                        printer.indent();
                        printer.name(&variant.name)?;
                        if let Some((value, _)) = variant.value {
                            write!(printer.out, " = {value}").unwrap();
                        }
                        printer.out.push_str(",\n");
                    }
                    Ok(())
                })?;
                self.indent();
                self.out.push_str("}\n");
            }
            Item::Directive {
                kind,
                target,
                value,
            } => {
                self.out.push_str(match kind {
                    DirectiveKind::Default => "default ",
                    DirectiveKind::Hint => "hint ",
                });
                self.name(target)?;
                self.out.push_str(" = ");
                self.expr(value)?;
                self.out.push_str(";\n");
            }
        }
        Ok(())
    }

    fn param(&mut self, param: &ParamGroup<'_>) -> Result<()> {
        for (modifier, _) in &param.modifiers {
            self.out.push_str(match modifier {
                ParamModifier::Optional => "optional ",
                ParamModifier::Out => "out ",
                ParamModifier::Const => "const ",
                ParamModifier::Skip => "skip ",
            });
        }
        self.names(&param.names)?;
        self.out.push_str(": ");
        self.typ(&param.typ)
    }

    fn typ(&mut self, typ: &Spanned<Type<'_>>) -> Result<()> {
        at(self.type_inner(&typ.0), typ.1)
    }

    fn type_inner(&mut self, typ: &Type<'_>) -> Result<()> {
        match typ {
            Type::Named { name, args } => {
                let valid = match *name {
                    "ref" | "wref" | "weak" | "script_ref" => args.len() == 1,
                    "mixedRef" => args.len() <= 1,
                    "array" => false, // Native arrays have their own node.
                    _ => args.is_empty(),
                };
                if !valid {
                    return Err(error(PrintErrorKind::InvalidTypeArguments, None));
                }
                self.ident(name)?;
                if !args.is_empty() {
                    self.out.push('<');
                    self.comma_list(args, Self::typ)?;
                    self.out.push('>');
                }
            }
            Type::Array(inner) => {
                self.out.push_str("array<");
                self.typ(inner)?;
                self.out.push('>');
            }
            Type::StaticArray(inner, size) => {
                self.typ(inner)?;
                if *size > i32::MAX as usize {
                    return Err(error(PrintErrorKind::ArraySizeOutOfRange, None));
                }
                write!(self.out, "[{size}]").unwrap();
            }
        }
        Ok(())
    }

    fn block(&mut self, block: &Block<'_>) -> Result<()> {
        self.out.push_str("{\n");
        self.indented(|printer| printer.stmts(block))?;
        self.indent();
        self.out.push('}');
        Ok(())
    }

    fn stmts(&mut self, block: &Block<'_>) -> Result<()> {
        for (stmt, span) in block {
            self.indent();
            at(self.stmt(stmt), *span)?;
            self.out.push('\n');
        }
        Ok(())
    }

    fn local(&mut self, local: &Local<'_>) -> Result<()> {
        if local.const_.is_some() {
            self.out.push_str("const ");
        }
        self.out.push_str("var ");
        self.names(&local.names)?;
        self.out.push_str(": ");
        self.typ(&local.typ)?;
        if let Some(value) = &local.value {
            self.out.push_str(" = ");
            self.expr(value)?;
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &Statement<'_>) -> Result<()> {
        match stmt {
            Statement::Local(local) => {
                self.local(local)?;
                self.out.push(';');
            }
            Statement::Expr(expr) => {
                self.header_expr(expr)?;
                self.out.push(';');
            }
            Statement::Return(expr) => {
                self.out.push_str("return");
                if let Some(expr) = expr {
                    self.out.push(' ');
                    self.expr(expr)?;
                }
                self.out.push(';');
            }
            Statement::Break => self.out.push_str("break;"),
            Statement::Continue => self.out.push_str("continue;"),
            Statement::Empty => self.out.push(';'),
            Statement::Block(block) => self.block(block)?,
            Statement::If { blocks, else_ } => {
                if blocks.is_empty() {
                    return Err(error(PrintErrorKind::MissingCondition, None));
                }
                for (i, (condition, block)) in blocks.iter().enumerate() {
                    self.out
                        .push_str(if i == 0 { "if (" } else { " else if (" });
                    self.expr(condition)?;
                    self.out.push_str(") ");
                    self.block(block)?;
                }
                if let Some(block) = else_ {
                    self.out.push_str(" else ");
                    self.block(block)?;
                }
            }
            Statement::While { condition, body } => {
                self.out.push_str("while (");
                self.expr(condition)?;
                self.out.push_str(") ");
                self.block(body)?;
            }
            Statement::DoWhile { body, condition } => {
                self.out.push_str("do ");
                self.block(body)?;
                self.out.push_str(" while (");
                self.expr(condition)?;
                self.out.push_str(");");
            }
            Statement::For {
                init,
                condition,
                step,
                body,
            } => {
                self.out.push_str("for (");
                if let Some(init) = init {
                    match &init.0 {
                        Statement::Local(local) => at(self.local(local), init.1)?,
                        Statement::Expr(expr) => self.header_expr(expr)?,
                        _ => {
                            return Err(error(PrintErrorKind::InvalidForInitializer, Some(init.1)));
                        }
                    }
                }
                self.out.push_str("; ");
                if let Some(expr) = condition {
                    self.header_expr(expr)?;
                }
                self.out.push_str("; ");
                if let Some(expr) = step {
                    self.header_expr(expr)?;
                }
                self.out.push_str(") ");
                self.block(body)?;
            }
            Statement::Switch { expr, labels } => {
                self.out.push_str("switch (");
                self.expr(expr)?;
                self.out.push_str(") {\n");
                self.indented(|printer| {
                    for (label, _) in labels {
                        printer.indent();
                        if let Some(value) = &label.value {
                            printer.out.push_str("case ");
                            printer.expr(value)?;
                            printer.out.push(':');
                        } else {
                            printer.out.push_str("default:");
                        }
                        printer.out.push('\n');
                        printer.indented(|printer| printer.stmts(&label.body))?;
                    }
                    Ok(())
                })?;
                self.indent();
                self.out.push('}');
            }
        }
        Ok(())
    }

    fn header_expr(&mut self, expr: &Spanned<Expr<'_>>) -> Result<()> {
        if let Expr::Sequence(items) = &expr.0 {
            if items.len() < 2 {
                return Err(error(PrintErrorKind::InvalidSequence, Some(expr.1)));
            }
            self.exprs(items)
        } else {
            self.expr(expr)
        }
    }

    fn exprs(&mut self, exprs: &[Spanned<Expr<'_>>]) -> Result<()> {
        self.comma_list(exprs, Self::expr)
    }

    fn receiver(&mut self, expr: &Spanned<Expr<'_>>) -> Result<()> {
        // Allocation absorbs suffixes and numbers absorb dots. Grouping an
        // identifier call would instead parse as a C-style conversion.
        let grouped = matches!(expr.0, Expr::New { .. } | Expr::Constant(_));
        if grouped {
            self.out.push('(');
        }
        self.expr(expr)?;
        if grouped {
            self.out.push(')');
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Spanned<Expr<'_>>) -> Result<()> {
        at(self.expr_inner(&expr.0), expr.1)
    }

    fn expr_inner(&mut self, expr: &Expr<'_>) -> Result<()> {
        match expr {
            Expr::Ident(name) => {
                if matches!(
                    *name,
                    "new"
                        | "do"
                        | "const"
                        | "var"
                        | "let"
                        | "if"
                        | "else"
                        | "while"
                        | "switch"
                        | "for"
                        | "return"
                        | "break"
                        | "continue"
                        | "function"
                        | "event"
                        | "class"
                        | "struct"
                        | "enum"
                        | "public"
                        | "private"
                        | "protected"
                        | "override"
                ) {
                    return Err(error(PrintErrorKind::InvalidIdentifier, None));
                }
                self.ident(name)?;
            }
            Expr::Constant(value) => self.constant(value)?,
            Expr::Null => self.out.push_str("NULL"),
            Expr::This => self.out.push_str("this"),
            Expr::Super => self.out.push_str("super"),
            Expr::ArrayLit(items) => {
                if items.is_empty() {
                    return Err(error(PrintErrorKind::EmptyArrayLiteral, None));
                }
                self.out.push('[');
                self.exprs(items)?;
                self.out.push(']');
            }
            Expr::New { typ } => {
                self.out.push_str("new ");
                self.typ(typ)?;
            }
            Expr::Call { expr, args } => {
                self.receiver(expr)?;
                self.out.push('(');
                // One omitted slot cannot be distinguished from an empty list.
                if matches!(&**args, [(Expr::Omitted, _)]) {
                    return Err(error(PrintErrorKind::InvalidOmittedArgument, None));
                }
                self.comma_list(args, |printer, arg| {
                    if !matches!(arg.0, Expr::Omitted) {
                        printer.expr(arg)?;
                    }
                    Ok(())
                })?;
                self.out.push(')');
            }
            Expr::Member { expr, member } => {
                self.receiver(expr)?;
                self.out.push('.');
                self.ident(member)?;
            }
            Expr::Index { expr, index } => {
                self.receiver(expr)?;
                self.out.push('[');
                self.expr(index)?;
                self.out.push(']');
            }
            Expr::Cast { typ, expr, style } => {
                self.out.push('(');
                match style {
                    CastStyle::CStyle => {
                        self.out.push('(');
                        self.typ(typ)?;
                        self.out.push(')');
                    }
                    CastStyle::Intrinsic => {
                        self.out.push_str("Cast<");
                        self.typ(typ)?;
                        self.out.push('>');
                    }
                }
                self.out.push('(');
                self.expr(expr)?;
                self.out.push_str("))");
            }
            Expr::DynCast { expr, typ } => {
                self.out.push('(');
                self.expr(expr)?;
                self.out.push_str(" as ");
                self.typ(typ)?;
                self.out.push(')');
            }
            Expr::UnOp { op, expr } => {
                self.out.push('(');
                self.out.push_str(op.symbol());
                self.expr(expr)?;
                self.out.push(')');
            }
            Expr::Assign { lhs, rhs } | Expr::BinOp { lhs, rhs, .. } => {
                self.out.push('(');
                self.expr(lhs)?;
                self.out.push(' ');
                self.out.push_str(match expr {
                    Expr::BinOp { op, .. } => op.symbol(),
                    _ => "=",
                });
                self.out.push(' ');
                self.expr(rhs)?;
                self.out.push(')');
            }
            Expr::Conditional { cond, then, else_ } => {
                self.out.push('(');
                self.expr(cond)?;
                self.out.push_str(" ? ");
                self.expr(then)?;
                self.out.push_str(" : ");
                self.expr(else_)?;
                self.out.push(')');
            }
            Expr::Sequence(_) => return Err(error(PrintErrorKind::InvalidSequence, None)),
            Expr::Omitted => return Err(error(PrintErrorKind::InvalidOmittedArgument, None)),
            Expr::Error => return Err(error(PrintErrorKind::RecoveredExpression, None)),
        }
        Ok(())
    }

    fn string(&mut self, prefix: &str, value: &str) -> Result<()> {
        let delimiter = if prefix == "n" && !value.contains('\'') {
            '\''
        } else {
            '"'
        };
        for c in value.chars() {
            let kind = if c == delimiter {
                Some(PrintErrorKind::LiteralDelimiter)
            } else if c == '\0' {
                Some(PrintErrorKind::LiteralNul)
            } else if c.is_control() && c != '\n' {
                Some(PrintErrorKind::LiteralControl)
            } else if matches!(prefix, "R" | "T") && (c == '\\' || c.is_control()) {
                Some(PrintErrorKind::UnverifiedLiteral)
            } else {
                None
            };
            if let Some(kind) = kind {
                return Err(error(kind, None));
            }
        }
        if delimiter != '\'' {
            self.out.push_str(prefix);
        }
        self.out.push(delimiter);
        self.out.push_str(value);
        self.out.push(delimiter);
        Ok(())
    }

    fn constant(&mut self, constant: &Constant<'_>) -> Result<()> {
        match constant {
            Constant::String(s) => self.string("", s)?,
            Constant::CName(s) => self.string("n", s)?,
            Constant::Resource(s) => self.string("R", s)?,
            Constant::TweakDbId(s) => self.string("T", s)?,
            Constant::I32(n) => write!(self.out, "{n}").unwrap(),
            Constant::I64(n) => write!(self.out, "{n}l").unwrap(),
            Constant::U32(_) | Constant::U64(_) => {
                return Err(error(PrintErrorKind::UnsignedLiteral, None));
            }
            Constant::F32(n) => {
                if !n.is_finite() {
                    return Err(error(PrintErrorKind::NonFiniteFloat, None));
                }
                write!(
                    self.out,
                    "{}",
                    DecimalFloat {
                        value: n,
                        integral: n.fract() == 0.0
                    }
                )
                .unwrap();
            }
            Constant::F64(n) => {
                if !n.is_finite() {
                    return Err(error(PrintErrorKind::NonFiniteDouble, None));
                }
                write!(
                    self.out,
                    "{}d",
                    DecimalFloat {
                        value: n,
                        integral: n.fract() == 0.0
                    }
                )
                .unwrap();
            }
            Constant::Bool(b) => self.out.push_str(if *b { "true" } else { "false" }),
        }
        Ok(())
    }
}

fn operator(op: Operator) -> &'static str {
    match op {
        Operator::Add => "+",
        Operator::Subtract => "-",
        Operator::Multiply => "*",
        Operator::Divide => "/",
        Operator::Modulo => "%",
        Operator::Equal => "==",
        Operator::NotEqual => "!=",
        Operator::Less => "<",
        Operator::LessEqual => "<=",
        Operator::Greater => ">",
        Operator::GreaterEqual => ">=",
        Operator::And => "&&",
        Operator::Or => "||",
        Operator::BitAnd => "&",
        Operator::BitOr => "|",
        Operator::BitXor => "^",
        Operator::Not => "!",
        Operator::BitNot => "~",
        Operator::AssignAdd => "+=",
        Operator::AssignSubtract => "-=",
        Operator::AssignMultiply => "*=",
        Operator::AssignDivide => "/=",
        Operator::AssignBitAnd => "&=",
        Operator::AssignBitOr => "|=",
        Operator::Index => "[]",
    }
}

// Integral values need a decimal point to remain floating-point tokens, including -0.
struct DecimalFloat<T> {
    value: T,
    integral: bool,
}

impl<T: fmt::Display> fmt::Display for DecimalFloat<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)?;
        if self.integral {
            f.write_str(".0")?;
        }
        Ok(())
    }
}
