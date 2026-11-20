use std::fmt;

use pretty_dtoa::{FmtFloatConfig, dtoa, ftoa};
use redscript_community_ast::{
    ArraySpread, AstKind, BinOp, Block, Condition, Constant, Expr, LetCondition, NodeId, Pattern,
    StrPart, Stmt, UnOp, Wrapper,
};

use crate::layout::{ParentOp, SepBy, SepByMultiline, format_call_args, format_chain};
use crate::{FormatCtx, FormatSettings, Formattable, SyntaxOps};

impl<K: AstKind> Formattable for Block<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for stmt in &self.stmts[..] {
            writeln!(f, "{}", stmt.as_wrapped().as_fmt(ctx.indented()))?;
        }
        write!(f, "{}}}", ctx.indent())
    }
}

impl<K: AstKind> Formattable for Stmt<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            ctx.node_prefix(NodeId::stmt(self), Some(ctx.indent()))
        )?;

        match self {
            Stmt::Let {
                is_const,
                name,
                typ,
                value,
            } => {
                write!(
                    f,
                    "{}{} {}",
                    ctx.indent(),
                    if *is_const { "const" } else { "let" },
                    name.as_wrapped()
                )?;
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
                default_index,
                default,
            } => {
                writeln!(
                    f,
                    "{}switch {} {{",
                    ctx.indent(),
                    (**expr).as_wrapped().as_fmt(ctx)
                )?;
                for index in 0..=cases.len() {
                    let ctx = ctx.indented();
                    if index == *default_index
                        && let Some(default) = default
                    {
                        writeln!(f, "{}default:", ctx.indent())?;
                        for stmt in &default[..] {
                            writeln!(f, "{}", stmt.as_wrapped().as_fmt(ctx.indented()))?;
                        }
                    }
                    let Some(case) = cases.get(index) else {
                        continue;
                    };
                    writeln!(
                        f,
                        "{}case {}:",
                        ctx.indent(),
                        case.condition.as_wrapped().as_fmt(ctx)
                    )?;
                    for stmt in &case.body[..] {
                        writeln!(f, "{}", stmt.as_wrapped().as_fmt(ctx.indented()))?;
                    }
                }

                write!(f, "{}}}", ctx.indent())
            }
            Stmt::If { blocks, else_ } => {
                write!(f, "{}", ctx.indent())?;
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
                    ctx.indent(),
                    block.condition.as_wrapped().as_fmt(ctx)
                )?;
                write!(f, "{}", block.body.as_fmt(ctx))
            }
            Stmt::ForIn { name, iter, body } => {
                write!(
                    f,
                    "{}for {} in {} ",
                    ctx.indent(),
                    name.as_wrapped(),
                    (**iter).as_wrapped().as_fmt(ctx)
                )?;
                write!(f, "{}", body.as_fmt(ctx))
            }
            Stmt::Return(Some(expr)) => {
                write!(
                    f,
                    "{}return {};",
                    ctx.indent(),
                    (**expr).as_wrapped().as_fmt(ctx)
                )
            }
            Stmt::Return(None) => {
                write!(f, "{}return;", ctx.indent())
            }
            Stmt::Break => {
                write!(f, "{}break;", ctx.indent())
            }
            Stmt::Continue => {
                write!(f, "{}continue;", ctx.indent())
            }
            Stmt::Expr(expr) => write!(f, "{}{};", ctx.indent(), (**expr).as_wrapped().as_fmt(ctx)),
        }
    }
}

fn format_constant(
    value: &Constant<'_>,
    f: &mut fmt::Formatter<'_>,
    settings: &FormatSettings,
) -> fmt::Result {
    let config = match settings.trunc_sig_digits {
        Some(digits) => FmtFloatConfig::default()
            .max_significant_digits(digits)
            .round(),
        None => FmtFloatConfig::default(),
    };
    match value {
        Constant::String(s) => write!(f, "\"{}\"", str::escape_default(s)),
        Constant::CName(s) => write!(f, "n\"{}\"", str::escape_default(s)),
        Constant::Resource(s) => write!(f, "r\"{}\"", str::escape_default(s)),
        Constant::TweakDbId(s) => write!(f, "t\"{}\"", str::escape_default(s)),
        // `inf` is an identifier in redscript, so retain infinity as an overflowing literal.
        &Constant::F32(v) if v.is_infinite() => {
            write!(f, "{}1.0e100", if v.is_sign_negative() { "-" } else { "" })
        }
        &Constant::F64(v) if v.is_infinite() => {
            write!(f, "{}1.0e400d", if v.is_sign_negative() { "-" } else { "" })
        }
        &Constant::F32(v) => write!(f, "{}", ftoa(v, config)),
        &Constant::F64(v) => write!(f, "{}d", dtoa(v, config)),
        Constant::I32(v) => write!(f, "{v}"),
        Constant::I64(v) => write!(f, "{v}l"),
        Constant::U32(v) => write!(f, "{v}u"),
        Constant::U64(v) => write!(f, "{v}ul"),
        Constant::Bool(v) => write!(f, "{v}"),
    }
}

impl<K: AstKind> Formattable for Expr<'_, K> {
    fn format(&self, f: &mut fmt::Formatter<'_>, ctx: FormatCtx<'_>) -> fmt::Result {
        write!(f, "{}", ctx.node_prefix(NodeId::expr(self), None))?;
        format_expr_body(self, f, ctx)
    }
}

pub(crate) fn format_expr_body<K: AstKind>(
    expression: &Expr<'_, K>,
    f: &mut fmt::Formatter<'_>,
    ctx: FormatCtx<'_>,
) -> fmt::Result {
    match expression {
        Expr::Ident(name) => write!(f, "{name}"),
        Expr::Constant(value) => {
            let parenthesize = matches!(ctx.parent, Some(ParentOp::TopPrec))
                && (matches!(value, Constant::I32(v) if *v < 0)
                    || matches!(value, Constant::I64(v) if *v < 0));
            if parenthesize {
                write!(f, "(")?;
            }
            format_constant(value, f, ctx.settings)?;
            if parenthesize {
                write!(f, ")")?;
            }
            Ok(())
        }
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
            let parenthesize = ctx.parent.is_some();
            let ctx = ctx.with_parent_op(ParentOp::AssignmentOperand);
            if parenthesize {
                write!(f, "(")?;
            }
            write!(
                f,
                "{} = {}",
                (**lhs).as_wrapped().as_fmt(ctx),
                (**rhs).as_wrapped().as_fmt(ctx)
            )?;
            if parenthesize {
                write!(f, ")")?;
            }
            Ok(())
        }
        Expr::Member { .. } | Expr::BinOp { .. } => format_chain(expression, f, ctx),
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
                format_chain(expression, f, ctx)
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
            let cond = (**cond)
                .as_wrapped()
                .as_fmt(ctx.with_parent_op(ParentOp::ConditionalCondition));
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
                    SepBy::new(params, ", ", ctx),
                    body.as_fmt(ctx)
                )
            } else {
                write!(
                    f,
                    "({}) -> {}",
                    SepBy::new(params, ", ", ctx),
                    body.as_fmt(ctx)
                )
            }
        }
        Expr::This => write!(f, "this"),
        Expr::Super => write!(f, "super"),
        Expr::Null => write!(f, "null"),
        Expr::Omitted | Expr::Error => Ok(()),
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
        write!(f, "{}", ctx.node_prefix(NodeId::condition(self), None))?;
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
                    if matches!(pat, Pattern::Name(n) if name.as_wrapped() == n.as_wrapped()) {
                        (name.as_wrapped(), None)
                    } else {
                        (name.as_wrapped(), Some((": ", pat.as_wrapped())))
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
                    SepByMultiline::new(elems.as_wrapped().iter(), ", ", ctx)
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
        f.write_str(self.symbol())
    }
}

impl Formattable for UnOp {
    fn format(&self, f: &mut fmt::Formatter<'_>, _ctx: FormatCtx<'_>) -> fmt::Result {
        f.write_str(self.symbol())
    }
}
