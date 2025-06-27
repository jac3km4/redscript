use std::{fmt, mem};

use hashbrown::HashMap;
use pretty_dtoa::{FmtFloatConfig, dtoa, ftoa};
use redscript_ast::{
    Aggregate, Annotation, ArraySpread, Assoc, AstKind, AstNode, AstVisitor, BinOp, Block,
    Condition, Constant, Enum, EnumVariant, Expr, Field, FileId, Function, FunctionBody, Import,
    Item, ItemDecl, ItemQualifiers, LetCondition, Module, NodeId, Param, ParamQualifiers, Path,
    Pattern, SourceAstNode, SourceBlock, SourceStmt, Span, Spanned, Stmt, StrPart, Type, TypeParam,
    UnOp, Variance, Visibility, WithSpan, Wrapper,
};
use redscript_parser::{ParseResult, Token, lex_with_lf_and_comments, parse, parser};

#[derive(Debug, Clone)]
pub struct FormatSettings {
    pub indent: u16,
    pub max_width: u16,
    pub max_chain_fields: u8,
    pub max_chain_calls: u8,
    pub max_chain_operators: u8,
    pub max_chain_total: u8,
    pub trunc_sig_digits: Option<u8>,
}

impl FormatSettings {
    pub const DEFAULT: FormatSettings = FormatSettings {
        indent: 2,
        max_width: 80,
        max_chain_fields: 4,
        max_chain_calls: 2,
        max_chain_operators: 4,
        max_chain_total: 4,
        trunc_sig_digits: None,
    };
}

impl Default for FormatSettings {
    fn default() -> Self {
        Self::DEFAULT
    }
}

pub fn format_document<'a>(
    source: &'a str,
    id: FileId,
    settings: &'a FormatSettings,
) -> ParseResult<impl fmt::Display + use<'a>> {
    let mut errors = vec![];

    let (tokens, e) = lex_with_lf_and_comments(source, id);
    errors.extend(e);
    let Some(tokens) = tokens else {
        return (None, errors);
    };
    let (ws, tokens): (Vec<_>, Vec<_>) =
        tokens.into_iter().partition(|(t, _)| t.is_ws_or_comment());

    let (module, e) = parse(parser::module(), &tokens, id);
    errors.extend(e);
    let Some(module) = module else {
        return (None, errors);
    };

    let mut collector = PrefixCollector::new(&ws);
    collector.visit_module(&module).ok();
    let (prefixes, remainder) = collector.into_inner();
    let remainder = remainder
        .iter()
        .filter(|(t, _)| !matches!(t, Token::LineFeed))
        .map(|(t, _)| t.clone())
        .collect::<Vec<_>>();

    let display = DisplayFn(move |f: &mut fmt::Formatter<'_>| {
        let ctx = FormatCtx::new(settings, &prefixes);
        writeln!(f, "{}", module.as_fmt(ctx))?;
        remainder.iter().try_for_each(|t| writeln!(f, "{t}"))
    });
    (Some(display), errors)
}

#[derive(Debug, Clone, Copy)]
pub struct FormatCtx<'a> {
    settings: &'a FormatSettings,
    prefixes: &'a HashMap<NodeId, Vec<Prefix<'a>>>,
    depth: u16,
    parent: Option<ParentOp>,
}

impl<'a> FormatCtx<'a> {
    #[inline]
    pub fn new(
        settings: &'a FormatSettings,
        prefixes: &'a HashMap<NodeId, Vec<Prefix<'a>>>,
    ) -> Self {
        Self {
            settings,
            prefixes,
            depth: 0,
            parent: None,
        }
    }

    #[inline]
    fn ws(&self) -> Indent {
        Indent(self.depth * self.settings.indent)
    }

    fn bump(&self, n: u16) -> Self {
        Self {
            depth: self.depth + n,
            ..*self
        }
    }

    fn decrement(&self) -> Self {
        Self {
            depth: self.depth - 1,
            ..*self
        }
    }

    #[inline]
    fn current_indent(&self) -> u16 {
        self.depth * self.settings.indent
    }

    fn with_parent_op(&self, parent: ParentOp) -> Self {
        Self {
            parent: Some(parent),
            ..*self
        }
    }

    fn without_parent_op(&self) -> Self {
        Self {
            parent: None,
            ..*self
        }
    }

    fn node_prefix(&self, id: NodeId, indent: Option<Indent>) -> impl fmt::Display + use<'_> {
        DisplayFn(move |f: &mut fmt::Formatter<'_>| {
            self.prefixes
                .get(&id)
                .map(Vec::as_slice)
                .unwrap_or_default()
                .iter()
                .try_for_each(|prefix| match (prefix, indent) {
                    (Prefix::LineBreak, _) => writeln!(f),
                    (
                        Prefix::LineComment(comment) | Prefix::BlockComment(comment),
                        Some(indent),
                    ) => {
                        writeln!(f, "{indent}{comment}")
                    }
                    (Prefix::LineComment(comment), None) => {
                        // convert line comments to block comments for inline nodes
                        write!(f, "/*{} */ ", &comment[2..].trim_end())
                    }
                    (Prefix::BlockComment(comment), None) => {
                        write!(f, "{comment} ")
                    }
                })
        })
    }
}

pub trait Formattable: Sized {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result;
}

impl<A: Formattable, B: Formattable> Formattable for (A, B) {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        self.0.format(f, ctx)?;
        self.1.format(f, ctx)
    }
}

impl<A: Formattable, B: Formattable, C: Formattable> Formattable for (A, B, C) {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        self.0.format(f, ctx)?;
        self.1.format(f, ctx)?;
        self.2.format(f, ctx)
    }
}

impl<A: Formattable> Formattable for Option<A> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        if let Some(value) = self {
            value.format(f, ctx)
        } else {
            Ok(())
        }
    }
}

impl<K: AstKind> Formattable for Module<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        if let Some(path) = &self.path {
            writeln!(f, "module {}", path.as_fmt(ctx))?;
        }
        writeln!(f)?;
        format_items(self.items.iter().map(Wrapper::as_wrapped), f, ctx)
    }
}

impl<K: AstKind> Formattable for ItemDecl<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            ctx.node_prefix(NodeId::item_decl(self), Some(ctx.ws()))
        )?;

        for doc in &self.doc[..] {
            writeln!(f, "{}{}", ctx.ws(), doc)?;
        }
        for ann in &self.annotations[..] {
            writeln!(f, "{}{}", ctx.ws(), ann.as_wrapped().as_fmt(ctx))?;
        }
        write!(f, "{}", ctx.ws())?;
        if let Some(visibility) = &self.visibility {
            write!(f, "{} ", visibility.as_fmt(ctx))?;
        }
        writeln!(
            f,
            "{}{}",
            self.qualifiers.as_fmt(ctx),
            self.item.as_fmt(ctx)
        )
    }
}

impl<K: AstKind> Formattable for Item<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Item::Import(import) => write!(f, "{}", import.as_fmt(ctx)),
            Item::Class(class) => write!(f, "class {}", class.as_fmt(ctx)),
            Item::Struct(struct_) => write!(f, "struct {}", struct_.as_fmt(ctx)),
            Item::Function(func) => write!(f, "{}", func.as_fmt(ctx)),
            Item::Let(field) => write!(f, "{}", field.as_fmt(ctx)),
            Item::Enum(enum_) => write!(f, "{}", enum_.as_fmt(ctx)),
        }
    }
}

impl Formattable for Import<'_> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Import::Exact(path) => write!(f, "import {}", path.as_fmt(ctx)),
            Import::Select(path, items) => write!(
                f,
                "import {}.{{{}}}",
                path.as_fmt(ctx),
                SepBy(items.iter().map(Wrapper::as_wrapped), ", ", ctx)
            ),
            Import::All(path) => write!(f, "import {}.*", path.as_fmt(ctx)),
        }
    }
}

impl<K: AstKind> Formattable for Aggregate<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}", self.name.as_wrapped())?;
        if !self.type_params.is_empty() {
            write!(
                f,
                "<{}>",
                SepBy(self.type_params.iter().map(Wrapper::as_wrapped), ", ", ctx)
            )?;
        }
        if let Some(extends) = &self.extends {
            write!(f, " extends {}", (**extends).as_wrapped().as_fmt(ctx))?;
        }
        writeln!(f, " {{")?;
        format_items(self.items.iter().map(Wrapper::as_wrapped), f, ctx.bump(1))?;
        write!(f, "{}}}", ctx.ws())
    }
}

impl<K: AstKind> Formattable for Field<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "let {}: {}",
            self.name.as_wrapped(),
            (*self.typ).as_wrapped().as_fmt(ctx)
        )?;
        if let Some(value) = &self.default {
            write!(f, " = {}", (**value).as_wrapped().as_fmt(ctx))?;
        }
        write!(f, ";")
    }
}

impl<K: AstKind> Formattable for Function<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "func {}", self.name.as_wrapped())?;
        if !self.type_params.is_empty() {
            write!(
                f,
                "<{}>",
                SepBy(self.type_params.iter().map(Wrapper::as_wrapped), ", ", ctx)
            )?;
        }
        write!(
            f,
            "({})",
            SepByMultiline::new(self.params.iter().map(Wrapper::as_wrapped), ", ", ctx)
        )?;
        if let Some(typ) = &self.return_type {
            write!(f, " -> {}", (**typ).as_wrapped().as_fmt(ctx))?;
        }
        match &self.body {
            Some(FunctionBody::Block(block)) => write!(f, " {}", block.as_fmt(ctx)),
            Some(FunctionBody::Inline(expr)) => {
                write!(f, " = {};", (**expr).as_wrapped().as_fmt(ctx))
            }
            None => write!(f, ";"),
        }
    }
}

impl<K: AstKind> Formattable for Enum<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        writeln!(f, "enum {} {{", self.name.as_wrapped())?;
        for variant in &self.variants[..] {
            let ctx = ctx.bump(1);
            writeln!(f, "{}{},", ctx.ws(), variant.as_wrapped().as_fmt(ctx))?;
        }
        write!(f, "}}")
    }
}

impl Formattable for EnumVariant<'_> {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(value) = &self.value {
            write!(f, " = {value}")?;
        }
        Ok(())
    }
}

impl<K: AstKind> Formattable for Block<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.stmts[..] {
            writeln!(f, "{}", stmt.as_wrapped().as_fmt(ctx.bump(1)))?;
        }
        write!(f, "{}}}", ctx.ws())
    }
}

impl<K: AstKind> Formattable for Stmt<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}", ctx.node_prefix(NodeId::stmt(self), Some(ctx.ws())))?;

        match self {
            Stmt::Let { name, typ, value } => {
                write!(f, "{}let {}", ctx.ws(), name.as_wrapped())?;
                if let Some(typ) = typ {
                    write!(f, ": {}", (**typ).as_wrapped().as_fmt(ctx))?;
                }
                if let Some(value) = value {
                    write!(f, " = {}", (**value).as_wrapped().as_fmt(ctx))?;
                }
                write!(f, ";")
            }
            Stmt::Switch {
                expr,
                cases,
                default,
            } => {
                writeln!(
                    f,
                    "{}switch {} {{",
                    ctx.ws(),
                    (**expr).as_wrapped().as_fmt(ctx)
                )?;
                for case in &cases[..] {
                    let ctx = ctx.bump(1);
                    writeln!(
                        f,
                        "{}case {}:",
                        ctx.ws(),
                        case.condition.as_wrapped().as_fmt(ctx)
                    )?;
                    for stmt in &case.body[..] {
                        writeln!(f, "{}", stmt.as_wrapped().as_fmt(ctx.bump(1)))?;
                    }
                }
                if let Some(default) = default {
                    let ctx = ctx.bump(1);
                    writeln!(f, "{}default:", ctx.ws())?;
                    for stmt in &default[..] {
                        writeln!(f, "{}", stmt.as_wrapped().as_fmt(ctx.bump(1)))?;
                    }
                }
                write!(f, "{}}}", ctx.ws())
            }
            Stmt::If { blocks, else_ } => {
                write!(f, "{}", ctx.ws())?;
                let mut it = blocks.iter();
                if let Some(block) = it.next() {
                    write!(
                        f,
                        "if {} {}",
                        block.condition.as_wrapped().as_fmt(ctx),
                        block.body.as_fmt(ctx)
                    )?;
                }
                for block in it {
                    write!(
                        f,
                        " else if {} {}",
                        block.condition.as_wrapped().as_fmt(ctx),
                        block.body.as_fmt(ctx)
                    )?;
                }
                if let Some(else_) = else_ {
                    write!(f, " else {}", else_.as_fmt(ctx))?;
                }
                Ok(())
            }
            Stmt::While(block) => {
                write!(
                    f,
                    "{}while {} ",
                    ctx.ws(),
                    block.condition.as_wrapped().as_fmt(ctx)
                )?;
                write!(f, "{}", block.body.as_fmt(ctx))
            }
            Stmt::ForIn { name, iter, body } => {
                write!(
                    f,
                    "{}for {} in {} ",
                    ctx.ws(),
                    name.as_wrapped(),
                    (**iter).as_wrapped().as_fmt(ctx)
                )?;
                write!(f, "{}", body.as_fmt(ctx))
            }
            Stmt::Return(Some(expr)) => {
                write!(
                    f,
                    "{}return {};",
                    ctx.ws(),
                    (**expr).as_wrapped().as_fmt(ctx)
                )
            }
            Stmt::Return(None) => {
                write!(f, "{}return;", ctx.ws())
            }
            Stmt::Break => {
                write!(f, "{}break;", ctx.ws())
            }
            Stmt::Continue => {
                write!(f, "{}continue;", ctx.ws())
            }
            Stmt::Expr(expr) => write!(f, "{}{};", ctx.ws(), (**expr).as_wrapped().as_fmt(ctx)),
        }
    }
}

impl<K: AstKind> Formattable for Expr<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}", ctx.node_prefix(NodeId::expr(self), None))?;

        match self {
            Expr::Ident(name) => write!(f, "{name}"),
            Expr::Constant(value) => match (value, ctx.settings.trunc_sig_digits) {
                (Constant::String(s), _) => write!(f, "\"{}\"", str::escape_default(s)),
                (Constant::CName(s), _) => write!(f, "n\"{}\"", str::escape_default(s)),
                (Constant::Resource(s), _) => write!(f, "r\"{}\"", str::escape_default(s)),
                (Constant::TweakDbId(s), _) => write!(f, "t\"{}\"", str::escape_default(s)),
                (&Constant::F32(v), Some(max_sig_digits)) => {
                    let conf = FmtFloatConfig::default()
                        .max_significant_digits(max_sig_digits)
                        .round();
                    write!(f, "{}", ftoa(v, conf))
                }
                (&Constant::F32(v), _) => {
                    write!(f, "{}", ftoa(v, FmtFloatConfig::default()))
                }
                (&Constant::F64(v), Some(max_sig_digits)) => {
                    let conf = FmtFloatConfig::default()
                        .max_significant_digits(max_sig_digits)
                        .round();
                    write!(f, "{}d", dtoa(v, conf))
                }
                (&Constant::F64(v), _) => {
                    write!(f, "{}", dtoa(v, FmtFloatConfig::default()))
                }
                (Constant::I32(v), _) => write!(f, "{v}"),
                (Constant::I64(v), _) => write!(f, "{v}l"),
                (Constant::U32(v), _) => write!(f, "{v}u"),
                (Constant::U64(v), _) => write!(f, "{v}ul"),
                (Constant::Bool(v), _) => write!(f, "{v}"),
            },
            Expr::ArrayLit(elems) => {
                let ctx = ctx.without_parent_op();
                write!(
                    f,
                    "[{}]",
                    SepByMultiline::new(elems.iter().map(Wrapper::as_wrapped), ", ", ctx)
                )
            }
            Expr::InterpolatedString(parts) => {
                let ctx = ctx.without_parent_op();
                write!(f, "s\"")?;
                for part in &parts[..] {
                    match part {
                        StrPart::Expr(expr) => write!(f, "\\({})", expr.as_wrapped().as_fmt(ctx))?,
                        StrPart::Str(s) => write!(f, "{}", str::escape_default(s))?,
                    }
                }
                write!(f, "\"")?;
                Ok(())
            }
            Expr::Assign { lhs, rhs } => {
                let ctx = ctx.without_parent_op();
                write!(
                    f,
                    "{} = {}",
                    (**lhs).as_wrapped().as_fmt(ctx),
                    (**rhs).as_wrapped().as_fmt(ctx)
                )
            }
            Expr::Member { .. } | Expr::BinOp { .. } => format_chain(self, f, ctx),
            Expr::UnOp { op, expr } => {
                let parenthesize = matches!(ctx.parent, Some(ParentOp::TopPrec));
                let ctx = ctx.with_parent_op(ParentOp::Unary);
                let expr = (**expr).as_wrapped().as_fmt(ctx);
                if parenthesize {
                    write!(f, "({}{})", op.as_fmt(ctx), expr)
                } else {
                    write!(f, "{}{}", op.as_fmt(ctx), expr)
                }
            }
            Expr::Call {
                expr,
                type_args,
                args,
            } => {
                if let Expr::Member { .. } = (**expr).as_wrapped() {
                    format_chain(self, f, ctx)
                } else {
                    write!(
                        f,
                        "{}",
                        (**expr)
                            .as_wrapped()
                            .as_fmt(ctx.with_parent_op(ParentOp::TopPrec))
                    )?;
                    format_call_args::<K>(type_args, args, f, ctx)
                }
            }
            Expr::Index { expr, index } => {
                write!(
                    f,
                    "{}[{}]",
                    (**expr)
                        .as_wrapped()
                        .as_fmt(ctx.with_parent_op(ParentOp::TopPrec)),
                    (**index).as_wrapped().as_fmt(ctx.without_parent_op())
                )
            }
            Expr::DynCast { expr, typ } => {
                let parenthesize = matches!(ctx.parent, Some(ParentOp::TopPrec | ParentOp::Unary));
                let ctx = ctx.with_parent_op(ParentOp::Unary);
                let expr = (**expr).as_wrapped().as_fmt(ctx);
                let typ = (**typ).as_wrapped().as_fmt(ctx);
                if parenthesize {
                    write!(f, "({expr} as {typ})")
                } else {
                    write!(f, "{expr} as {typ}")
                }
            }
            Expr::New { typ, args } => {
                let ctx = ctx.without_parent_op();
                write!(
                    f,
                    "new {}({})",
                    (**typ).as_wrapped().as_fmt(ctx),
                    SepByMultiline::new(args.iter().map(Wrapper::as_wrapped), ", ", ctx)
                )
            }
            Expr::Conditional { cond, then, else_ } => {
                let parenthesize = ctx.parent.is_some();
                let ctx = ctx.without_parent_op();
                let cond = (**cond).as_wrapped().as_fmt(ctx);
                let then = (**then).as_wrapped().as_fmt(ctx);
                let else_ = (**else_).as_wrapped().as_fmt(ctx);
                if parenthesize {
                    write!(f, "({cond} ? {then} : {else_})")
                } else {
                    write!(f, "{cond} ? {then} : {else_}")
                }
            }
            Expr::Lambda { params, body } => {
                let parenthesize = ctx.parent.is_some();
                let ctx = ctx.without_parent_op();
                let params = params.iter().map(Wrapper::as_wrapped);

                if parenthesize {
                    write!(
                        f,
                        "(({}) -> {})",
                        SepBy(params, ", ", ctx),
                        body.as_fmt(ctx)
                    )
                } else {
                    write!(f, "({}) -> {}", SepBy(params, ", ", ctx), body.as_fmt(ctx))
                }
            }
            Expr::This => write!(f, "this"),
            Expr::Super => write!(f, "super"),
            Expr::Null => write!(f, "null"),
            Expr::Error => Ok(()),
        }
    }
}

impl<K: AstKind> Formattable for FunctionBody<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            FunctionBody::Block(block) => write!(f, "{}", block.as_fmt(ctx)),
            FunctionBody::Inline(expr) => write!(f, "{}", (**expr).as_wrapped().as_fmt(ctx)),
        }
    }
}

impl<K: AstKind> Formattable for Param<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        if self.qualifiers.contains(ParamQualifiers::OUT) {
            write!(f, "out ")?;
        };
        if self.qualifiers.contains(ParamQualifiers::OPTIONAL) {
            write!(f, "opt ")?;
        };
        if self.qualifiers.contains(ParamQualifiers::CONST) {
            write!(f, "const ")?;
        };
        if let Some(typ) = &self.typ {
            write!(f, "{}: {}", self.name, typ.as_wrapped().as_fmt(ctx))
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl<K: AstKind> Formattable for Type<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Type::Named { name, args } if args.is_empty() => write!(f, "{name}"),
            Type::Named { name, args } => write!(
                f,
                "{}<{}>",
                name,
                SepBy(args.iter().map(Wrapper::as_wrapped), ", ", ctx)
            ),
            Type::Array(el) => write!(f, "[{}]", (**el).as_wrapped().as_fmt(ctx)),
            Type::StaticArray(el, size) => {
                write!(f, "[{}; {}]", (**el).as_wrapped().as_fmt(ctx), size)
            }
            Type::Fn {
                params,
                return_type,
            } => {
                write!(
                    f,
                    "({}) -> {}",
                    SepBy(params.iter().map(Wrapper::as_wrapped), ", ", ctx),
                    (**return_type).as_wrapped().as_fmt(ctx)
                )
            }
        }
    }
}

impl<K: AstKind> Formattable for Annotation<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "@{}({})",
            self.name,
            SepBy(self.args.iter().map(Wrapper::as_wrapped), ", ", ctx)
        )
    }
}

impl Formattable for Path<'_> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            SepBy(self.segments.iter().map(Wrapper::as_wrapped), ".", ctx)
        )
    }
}

impl<K: AstKind> Formattable for Condition<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => write!(f, "{}", (*expr).as_wrapped().as_fmt(ctx)),
            Self::Pattern(pattern) => write!(f, "let {}", pattern.as_wrapped().as_fmt(ctx)),
        }
    }
}

impl<K: AstKind> Formattable for LetCondition<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => write!(f, "{}", (*expr).as_wrapped().as_fmt(ctx)),
            Self::LetPattern(pattern, val) => write!(
                f,
                "let {} = {}",
                pattern.as_wrapped().as_fmt(ctx),
                val.as_wrapped().as_fmt(ctx)
            ),
        }
    }
}

impl<K: AstKind> Formattable for Pattern<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "{}", name.as_wrapped()),
            Self::As(inner, typ) => write!(
                f,
                "{} as {}",
                (**inner).as_wrapped().as_fmt(ctx),
                typ.as_wrapped().as_fmt(ctx)
            ),
            Self::Aggregate(name, items) => {
                let items = items.iter().map(|(name, pat)| {
                    if matches!(pat, Pattern::Name(n) if name.as_wrapped() != n.as_wrapped()) {
                        (name.as_wrapped(), Some((": ", pat.as_wrapped())))
                    } else {
                        (name.as_wrapped(), None)
                    }
                });
                write!(
                    f,
                    "{} {{{}}}",
                    name.as_wrapped(),
                    SepByMultiline::new(items, ", ", ctx).with_padding(" ")
                )
            }
            Self::Nullable(nullable) => {
                write!(f, "{}?", (**nullable).as_wrapped().as_fmt(ctx))
            }
            Self::Array(spread, elems) => {
                write!(f, "[")?;
                if matches!(spread, ArraySpread::Start) {
                    write!(f, "..,")?;
                }
                write!(
                    f,
                    "{}",
                    SepByMultiline::new(elems.as_wrapped().iter(), ", ", ctx).with_padding("")
                )?;
                if matches!(spread, ArraySpread::End) {
                    write!(f, ", ..")?;
                }
                write!(f, "]")
            }
        }
    }
}

impl Formattable for BinOp {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::AssignAdd => write!(f, "+="),
            Self::AssignSub => write!(f, "-="),
            Self::AssignMul => write!(f, "*="),
            Self::AssignDiv => write!(f, "/="),
            Self::AssignBitOr => write!(f, "|="),
            Self::AssignBitAnd => write!(f, "&="),
            Self::Or => write!(f, "||"),
            Self::And => write!(f, "&&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::BitAnd => write!(f, "&"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
        }
    }
}

impl Formattable for UnOp {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::BitNot => write!(f, "~"),
            Self::Not => write!(f, "!"),
            Self::Neg => write!(f, "-"),
        }
    }
}

impl Formattable for ItemQualifiers {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        if self.contains(ItemQualifiers::ABSTRACT) {
            write!(f, "abstract ")?;
        }
        if self.contains(ItemQualifiers::CALLBACK) {
            write!(f, "cb ")?;
        }
        if self.contains(ItemQualifiers::CONST) {
            write!(f, "const ")?;
        }
        if self.contains(ItemQualifiers::EXEC) {
            write!(f, "exec ")?;
        }
        if self.contains(ItemQualifiers::FINAL) {
            write!(f, "final ")?;
        }
        if self.contains(ItemQualifiers::IMPORT_ONLY) {
            write!(f, "importonly ")?;
        }
        if self.contains(ItemQualifiers::NATIVE) {
            write!(f, "native ")?;
        }
        if self.contains(ItemQualifiers::PERSISTENT) {
            write!(f, "persistent ")?;
        }
        if self.contains(ItemQualifiers::QUEST) {
            write!(f, "quest ")?;
        }
        if self.contains(ItemQualifiers::STATIC) {
            write!(f, "static ")?;
        }
        Ok(())
    }
}

impl Formattable for Visibility {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::Public => write!(f, "public"),
            Self::Protected => write!(f, "protected"),
            Self::Private => write!(f, "private"),
        }
    }
}

impl<K: AstKind> Formattable for TypeParam<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}{}", self.variance.as_fmt(ctx), self.name.as_wrapped())?;
        if let Some(upper_bound) = &self.upper_bound {
            write!(f, " extends {}", (**upper_bound).as_wrapped().as_fmt(ctx))?;
        }
        Ok(())
    }
}

impl Formattable for Variance {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        match self {
            Self::Covariant => write!(f, "+"),
            Self::Contravariant => write!(f, "-"),
            Self::Invariant => Ok(()),
        }
    }
}

struct DisplayProxy<'s, A>(A, FormatCtx<'s>);

impl<A: Formattable> fmt::Display for DisplayProxy<'_, &A> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.format(f, self.1)
    }
}

#[derive(Debug, Clone, Copy)]
struct Indent(u16);

impl fmt::Display for Indent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:w$}", "", w = usize::from(self.0))
    }
}

struct SepBy<'s, I>(I, &'static str, FormatCtx<'s>);

impl<I: Iterator + Clone> fmt::Display for SepBy<'_, I>
where
    I::Item: Formattable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.0.clone().enumerate() {
            if i > 0 {
                write!(f, "{}", self.1)?;
            }
            write!(f, "{}", item.as_fmt(self.2))?;
        }
        Ok(())
    }
}

struct SepByMultiline<'s, I> {
    iter: I,
    separator: &'static str,
    ctx: FormatCtx<'s>,
    padding: &'static str,
}

impl<'s, I> SepByMultiline<'s, I> {
    fn new(iter: I, separator: &'static str, ctx: FormatCtx<'s>) -> Self {
        Self {
            iter,
            separator,
            ctx,
            padding: "",
        }
    }

    fn with_padding(mut self, padding: &'static str) -> Self {
        self.padding = padding;
        self
    }
}

impl<I: ExactSizeIterator<Item = E> + Clone, E> fmt::Display for SepByMultiline<'_, I>
where
    E: ApproxWidth + Formattable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            iter,
            separator,
            ctx,
            padding,
        } = self;
        let width = iter.clone().map(|e| e.approx_width()).sum::<u16>()
            + (separator.len() * (iter.len().max(1) - 1)) as u16;

        if ctx.current_indent() + width > ctx.settings.max_width {
            let sep = separator.trim_end();
            let ctx = ctx.bump(1);
            for (i, elem) in iter.clone().enumerate() {
                if i > 0 {
                    write!(f, "{sep}")?;
                }
                writeln!(f)?;
                write!(f, "{}{}", ctx.ws(), elem.as_fmt(ctx))?;
            }
            writeln!(f)?;
            write!(f, "{}", ctx.decrement().ws())
        } else {
            write!(f, "{padding}{}{padding}", SepBy(iter.clone(), ", ", *ctx))
        }
    }
}

pub trait SyntaxOps {
    fn as_fmt(&self, ctx: FormatCtx<'_>) -> impl fmt::Display;
}

impl<A> SyntaxOps for A
where
    A: Formattable,
{
    #[inline]
    fn as_fmt(&self, ctx: FormatCtx<'_>) -> impl fmt::Display {
        DisplayProxy(self, ctx)
    }
}

impl Formattable for &str {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<A: Formattable> Formattable for &A {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        (*self).format(f, ctx)
    }
}

enum Chain<'ast, 'src, K: AstKind> {
    Member(&'src str),
    Call(
        &'src str,
        &'ast [K::Inner<Type<'src, K>>],
        &'ast [K::Inner<Expr<'src, K>>],
    ),
    Op(BinOp, &'ast Expr<'src, K>),
    Index(&'ast Expr<'src, K>),
}

fn format_chain<K: AstKind>(
    expr: &Expr<'_, K>,
    f: &mut fmt::Formatter<'_>,
    ctx: FormatCtx<'_>,
) -> fmt::Result {
    let mut cur = expr;
    let mut chain = vec![];

    let mut chain_fields = 0;
    let mut chain_calls = 0;
    let mut chain_ops = 0;

    let mut cur_parent = ctx.parent;
    let ctx = ctx.without_parent_op();

    let parenthesize = loop {
        match cur {
            // handle expressions that might require parentheses first
            Expr::Conditional { .. } => {
                break cur_parent.is_some();
            }
            Expr::DynCast { .. } => {
                break matches!(cur_parent, Some(ParentOp::TopPrec | ParentOp::Unary));
            }
            Expr::UnOp { .. } | Expr::New { .. } => {
                break matches!(cur_parent, Some(ParentOp::TopPrec));
            }
            Expr::BinOp { lhs, op, rhs } => {
                if cur_parent.is_some_and(|parent| !parent.can_assoc(*op)) {
                    break true;
                }
                cur = (**lhs).as_wrapped();
                cur_parent = Some(ParentOp::BinaryLhs(*op));
                chain.push(Chain::Op(*op, (**rhs).as_wrapped()));
                chain_ops += 1;
            }
            // break the chain if we're chaining operators and we encounter a non-operator
            _ if chain_ops > 0 => {
                break false;
            }
            Expr::Member { expr, member } => {
                cur = (**expr).as_wrapped();
                cur_parent = Some(ParentOp::TopPrec);
                chain.push(Chain::Member(member));
                chain_fields += 1;
            }
            Expr::Index { expr, index } => {
                cur = (**expr).as_wrapped();
                cur_parent = Some(ParentOp::TopPrec);
                chain.push(Chain::Index((**index).as_wrapped()));
            }
            Expr::Call {
                expr,
                type_args,
                args,
            } => {
                if let Expr::Member { expr, member } = (**expr).as_wrapped() {
                    cur = (**expr).as_wrapped();
                    cur_parent = Some(ParentOp::TopPrec);
                    chain.push(Chain::Call(member, &type_args[..], &args[..]));
                    chain_calls += 1;
                } else {
                    break false;
                }
            }
            _ => break false,
        };
    };

    if parenthesize {
        write!(f, "({})", cur.as_fmt(ctx))?;
    } else {
        write!(f, "{}", cur.as_fmt(ctx))?;
    }

    let width = chain.iter().map(Chain::approx_width).sum::<u16>();
    let break_line = ctx.current_indent() + width > ctx.settings.max_width
        || chain_fields > usize::from(ctx.settings.max_chain_fields)
        || chain_calls > usize::from(ctx.settings.max_chain_calls)
        || chain_ops > usize::from(ctx.settings.max_chain_operators)
        || chain.len() > usize::from(ctx.settings.max_chain_total);

    let ctx = ctx.bump(1);

    for i in chain.into_iter().rev() {
        match i {
            Chain::Member(member) => {
                if break_line {
                    writeln!(f)?;
                    write!(f, "{}", ctx.ws())?;
                }
                write!(f, ".{member}")?;
            }
            Chain::Call(member, type_args, args) => {
                if break_line {
                    writeln!(f)?;
                    write!(f, "{}", ctx.ws())?;
                }
                write!(f, ".{member}")?;
                format_call_args::<K>(type_args, args, f, ctx)?;
            }
            Chain::Index(index) => write!(f, "[{}]", index.as_fmt(ctx))?,
            Chain::Op(op, rhs) => {
                let ctx = ctx.with_parent_op(ParentOp::BinaryRhs(op));
                if break_line {
                    writeln!(f)?;
                    write!(f, "{}", ctx.ws())?;
                } else {
                    write!(f, " ")?;
                }
                write!(f, "{} {}", op.as_fmt(ctx), rhs.as_fmt(ctx))?;
            }
        }
    }

    Ok(())
}

fn format_items<'a, 'c: 'a, K: AstKind + 'a>(
    items: impl IntoIterator<Item = &'a ItemDecl<'c, K>>,
    f: &mut fmt::Formatter<'_>,
    ctx: FormatCtx<'_>,
) -> fmt::Result {
    let mut it = items.into_iter();
    let Some(first) = it.next() else {
        return Ok(());
    };
    let mut discriminant = mem::discriminant(&first.item);
    let mut annotated = !first.annotations.is_empty();
    write!(f, "{}", first.as_fmt(ctx))?;

    for decl in it {
        let decl = decl.as_wrapped();
        let decl_is_annotated = !decl.annotations.is_empty();
        if !matches!(decl.item, Item::Import(_) | Item::Let(_))
            || discriminant != mem::discriminant(&decl.item)
            || annotated != decl_is_annotated
            || decl_is_annotated
        {
            writeln!(f)?;
        }
        discriminant = mem::discriminant(&decl.item);
        annotated = decl_is_annotated;
        write!(f, "{}", decl.as_fmt(ctx))?;
    }

    Ok(())
}

fn format_call_args<K: AstKind>(
    type_args: &[K::Inner<Type<'_, K>>],
    args: &[K::Inner<Expr<'_, K>>],
    f: &mut fmt::Formatter<'_>,
    ctx: FormatCtx<'_>,
) -> fmt::Result {
    if !type_args.is_empty() {
        write!(
            f,
            "<{}>",
            SepBy(type_args.iter().map(Wrapper::as_wrapped), ", ", ctx)
        )?;
    }
    write!(
        f,
        "({})",
        SepByMultiline::new(args.iter().map(Wrapper::as_wrapped), ", ", ctx)
    )
}

#[derive(Debug, Clone, Copy)]
enum ParentOp {
    Unary,
    BinaryLhs(BinOp),
    BinaryRhs(BinOp),
    TopPrec,
}

impl ParentOp {
    fn can_assoc(&self, op: BinOp) -> bool {
        match self {
            ParentOp::TopPrec | ParentOp::Unary => false,
            ParentOp::BinaryLhs(parent) | ParentOp::BinaryRhs(parent)
                if parent.precedence() < op.precedence() =>
            {
                true
            }
            ParentOp::BinaryLhs(parent) => {
                parent.precedence() == op.precedence() && parent.assoc() == Assoc::Left
            }
            ParentOp::BinaryRhs(parent) => {
                parent.precedence() == op.precedence() && parent.assoc() == Assoc::Right
            }
        }
    }
}

trait ApproxWidth {
    fn approx_width(&self) -> u16;
}

impl<A: ApproxWidth, B: ApproxWidth> ApproxWidth for (A, B) {
    fn approx_width(&self) -> u16 {
        self.0.approx_width() + self.1.approx_width()
    }
}

impl<A: ApproxWidth, B: ApproxWidth, C: ApproxWidth> ApproxWidth for (A, B, C) {
    fn approx_width(&self) -> u16 {
        self.0.approx_width() + self.1.approx_width() + self.2.approx_width()
    }
}

impl<A: ?Sized + ApproxWidth> ApproxWidth for &A {
    fn approx_width(&self) -> u16 {
        (**self).approx_width()
    }
}

impl<A: ApproxWidth> ApproxWidth for Option<A> {
    fn approx_width(&self) -> u16 {
        match self {
            Some(value) => value.approx_width(),
            None => 0,
        }
    }
}

impl ApproxWidth for str {
    fn approx_width(&self) -> u16 {
        self.len() as u16
    }
}

impl<K> ApproxWidth for Expr<'_, K>
where
    K: AstKind,
{
    fn approx_width(&self) -> u16 {
        match self {
            Expr::Ident(ident) => ident.len() as u16,
            Expr::Constant(constant) => match constant {
                Constant::String(s) => s.len() as u16 + 2,
                Constant::CName(s) => s.len() as u16 + 3,
                Constant::Resource(r) => r.len() as u16 + 3,
                Constant::TweakDbId(t) => t.len() as u16 + 3,
                Constant::F32(f) => f.abs().log10().floor() as u16 + 1,
                Constant::F64(f) => f.abs().log10().floor() as u16 + 1,
                &Constant::I32(0) | &Constant::I64(0) | &Constant::U32(0) | &Constant::U64(0) => 1,
                Constant::I32(i) => i.abs().ilog10() as u16 + 1,
                Constant::I64(i) => i.abs().ilog10() as u16 + 1,
                Constant::U32(u) => u.ilog10() as u16 + 1,
                Constant::U64(u) => u.ilog10() as u16 + 1,
                Constant::Bool(_) => 4,
            },
            Expr::ArrayLit(elems) => elems
                .iter()
                .map(|el| el.as_wrapped().approx_width() + 2)
                .sum::<u16>(),
            Expr::InterpolatedString(parts) => {
                parts
                    .iter()
                    .map(|part| match part {
                        StrPart::Expr(expr) => expr.as_wrapped().approx_width() + 3,
                        StrPart::Str(s) => s.len() as u16,
                    })
                    .sum::<u16>()
                    + 3
            }
            Expr::Assign { lhs, rhs } => {
                (**lhs).as_wrapped().approx_width() + (**rhs).as_wrapped().approx_width() + 3
            }
            Expr::BinOp { lhs, op, rhs } => {
                (**lhs).as_wrapped().approx_width()
                    + (**rhs).as_wrapped().approx_width()
                    + op.approx_width()
                    + 2
            }
            Expr::UnOp { expr, op } => (**expr).as_wrapped().approx_width() + op.approx_width(),
            Expr::Call {
                expr,
                args,
                type_args,
            } => {
                (**expr).as_wrapped().approx_width()
                    + args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<u16>()
                    + type_args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<u16>()
            }
            Expr::Member { expr, member } => {
                (**expr).as_wrapped().approx_width() + member.len() as u16 + 1
            }
            Expr::Index { expr, index } => {
                (**expr).as_wrapped().approx_width() + (**index).as_wrapped().approx_width() + 2
            }
            Expr::DynCast { expr, typ } => {
                (**expr).as_wrapped().approx_width() + (**typ).as_wrapped().approx_width() + 4
            }
            Expr::New { typ, args } => {
                (**typ).as_wrapped().approx_width()
                    + args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<u16>()
                    + 4
            }
            Expr::Conditional { cond, then, else_ } => {
                (**cond).as_wrapped().approx_width()
                    + (**then).as_wrapped().approx_width()
                    + (**else_).as_wrapped().approx_width()
                    + 7
            }
            Expr::Lambda { params, body } => {
                params
                    .iter()
                    .map(|param| param.as_wrapped().name.len() as u16 + 2)
                    .sum::<u16>()
                    + body.approx_width()
                    + 4
            }
            Expr::This | Expr::Null => 4,
            Expr::Super => 5,
            Expr::Error => 0,
        }
    }
}

impl<K: AstKind> ApproxWidth for FunctionBody<'_, K> {
    fn approx_width(&self) -> u16 {
        match self {
            FunctionBody::Block(block) => {
                // TODO: come up with something better
                block.stmts.len() as u16 * 10 + 4
            }
            FunctionBody::Inline(expr) => (**expr).as_wrapped().approx_width(),
        }
    }
}

impl<K: AstKind> ApproxWidth for Param<'_, K> {
    fn approx_width(&self) -> u16 {
        self.name.len() as u16
            + self
                .typ
                .as_ref()
                .map(|typ| typ.as_wrapped().approx_width())
                .unwrap_or_default()
            + 2
    }
}

impl<K: AstKind> ApproxWidth for Type<'_, K> {
    fn approx_width(&self) -> u16 {
        match self {
            Type::Named { name, args } if args.is_empty() => name.len() as u16,
            Type::Named { name, args } => {
                name.len() as u16
                    + args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<u16>()
            }
            Type::Array(el) => (**el).as_wrapped().approx_width() + 2,
            Type::StaticArray(el, size) => {
                (**el).as_wrapped().approx_width() + size.ilog10() as u16 + 4
            }
            Type::Fn {
                params,
                return_type,
            } => {
                params
                    .iter()
                    .map(|param| param.as_wrapped().approx_width() + 2)
                    .sum::<u16>()
                    + (**return_type).as_wrapped().approx_width()
                    + 6
            }
        }
    }
}

impl<K: AstKind> ApproxWidth for Condition<'_, K> {
    fn approx_width(&self) -> u16 {
        match self {
            Self::Expr(expr) => expr.as_wrapped().approx_width(),
            Self::Pattern(pattern) => pattern.as_wrapped().approx_width() + 4,
        }
    }
}

impl<K: AstKind> ApproxWidth for LetCondition<'_, K> {
    fn approx_width(&self) -> u16 {
        match self {
            Self::Expr(expr) => expr.as_wrapped().approx_width(),
            Self::LetPattern(pattern, val) => {
                pattern.as_wrapped().approx_width() + val.as_wrapped().approx_width() + 6
            }
        }
    }
}

impl<K: AstKind> ApproxWidth for Pattern<'_, K> {
    fn approx_width(&self) -> u16 {
        match self {
            Pattern::Name(name) => name.as_wrapped().len() as u16,
            Pattern::As(inner, typ) => {
                (**inner).as_wrapped().approx_width() + typ.as_wrapped().approx_width() + 2
            }
            Pattern::Aggregate(name, items) => {
                name.as_wrapped().len() as u16
                    + items
                        .iter()
                        .map(|(name, pat)| {
                            (*name).as_wrapped().len() as u16 + pat.as_wrapped().approx_width() + 2
                        })
                        .sum::<u16>()
                    + 4
            }
            Pattern::Nullable(inner) => (**inner).as_wrapped().approx_width() + 1,
            Pattern::Array(spread, elems) => {
                elems
                    .as_wrapped()
                    .iter()
                    .map(|el| el.approx_width() + 2)
                    .sum::<u16>()
                    + match spread {
                        ArraySpread::End | ArraySpread::Start => 4,
                        ArraySpread::None => 0,
                    }
            }
        }
    }
}

impl ApproxWidth for BinOp {
    fn approx_width(&self) -> u16 {
        match self {
            BinOp::AssignAdd
            | BinOp::AssignSub
            | BinOp::AssignMul
            | BinOp::AssignDiv
            | BinOp::AssignBitOr
            | BinOp::AssignBitAnd
            | BinOp::Or
            | BinOp::And
            | BinOp::Eq
            | BinOp::Ne
            | BinOp::Le
            | BinOp::Ge => 2,
            BinOp::BitOr
            | BinOp::BitXor
            | BinOp::BitAnd
            | BinOp::Lt
            | BinOp::Gt
            | BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Mod => 1,
        }
    }
}

impl ApproxWidth for UnOp {
    fn approx_width(&self) -> u16 {
        match self {
            UnOp::BitNot | UnOp::Not | UnOp::Neg => 1,
        }
    }
}

impl<K: AstKind> ApproxWidth for Chain<'_, '_, K> {
    fn approx_width(&self) -> u16 {
        match self {
            Chain::Member(member) => member.len() as u16 + 1,
            Chain::Call(member, type_args, args) => {
                member.len() as u16
                    + type_args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<u16>()
                    + args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<u16>()
                    + 2
            }
            Chain::Op(op, rhs) => op.approx_width() + rhs.as_wrapped().approx_width() + 1,
            Chain::Index(index) => index.as_wrapped().approx_width() + 2,
        }
    }
}

struct DisplayFn<F>(F);

impl<F> fmt::Display for DisplayFn<F>
where
    F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}

#[derive(Debug)]
pub enum Prefix<'src> {
    LineBreak,
    LineComment(&'src str),
    BlockComment(&'src str),
}

#[derive(Debug)]
struct PrefixCollector<'ctx, 'src> {
    prefixes: HashMap<NodeId, Vec<Prefix<'src>>>,
    remainder: &'ctx [Spanned<Token<'src>>],
}

impl<'ctx, 'src> PrefixCollector<'ctx, 'src> {
    fn new(tokens: &'ctx [Spanned<Token<'src>>]) -> Self {
        Self {
            prefixes: HashMap::new(),
            remainder: tokens,
        }
    }

    fn into_inner(
        self,
    ) -> (
        HashMap<NodeId, Vec<Prefix<'src>>>,
        &'ctx [Spanned<Token<'src>>],
    ) {
        (self.prefixes, self.remainder)
    }

    fn skip_until(&mut self, span: Span) {
        while let [(Token::LineFeed, s), rest @ ..] = self.remainder {
            if s.start > span.start {
                break;
            }
            self.remainder = rest;
        }
    }
}

impl<'src> AstVisitor<'src, WithSpan> for PrefixCollector<'_, 'src> {
    type Error = Never;

    fn visit_node(&mut self, node: SourceAstNode<'_, 'src>) -> Result<(), Self::Error> {
        let mut prefixes = vec![];
        let mut is_lf_seq = false;

        while let [(fst, span), rest @ ..] = self.remainder {
            if span.start > node.span().start {
                break;
            }
            match (fst, rest) {
                // two consecutive line feeds form a line break between statements
                (Token::LineFeed, [(Token::LineFeed, span), rest @ ..])
                    if matches!(node, AstNode::Stmt(_)) && span.start <= node.span().start =>
                {
                    is_lf_seq = true;
                    self.remainder = rest;
                }
                _ => {
                    if is_lf_seq {
                        prefixes.push(Prefix::LineBreak);
                        is_lf_seq = false;
                    }
                    self.remainder = rest;
                }
            }
            match fst {
                Token::LineComment(comment) => prefixes.push(Prefix::LineComment(comment)),
                Token::BlockComment(comment) => prefixes.push(Prefix::BlockComment(comment)),
                _ => {}
            }
        }
        if is_lf_seq {
            prefixes.push(Prefix::LineBreak);
        }
        if !prefixes.is_empty() {
            self.prefixes.insert(node.id(), prefixes);
        }
        Ok(())
    }

    fn post_visit_node(&mut self, node: AstNode<'_, 'src, WithSpan>) -> Result<(), Self::Error> {
        while let [(Token::LineFeed, span), rest @ ..] = self.remainder {
            if span.start >= node.span().end {
                break;
            }
            self.remainder = rest;
        }

        Ok(())
    }

    fn visit_block(&mut self, block: &SourceBlock<'src>) -> Result<(), Self::Error> {
        if let Some(block_span) = block.bounds_span() {
            self.skip_until(block_span);
        };

        block
            .stmts
            .iter()
            .try_for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_default(&mut self, stmts: &[Spanned<SourceStmt<'src>>]) -> Result<(), Self::Error> {
        if let (Some((_, fst)), Some((_, lst))) = (stmts.first(), stmts.last()) {
            self.skip_until(fst.merge(lst));
        };
        stmts.iter().try_for_each(|stmt| self.visit_stmt(stmt))
    }
}

enum Never {}
