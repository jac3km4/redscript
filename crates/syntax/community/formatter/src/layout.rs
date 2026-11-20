use std::fmt;

use pretty_dtoa::{FmtFloatConfig, dtoa, ftoa};
use redscript_community_ast::{
    ArraySpread, Assoc, AstKind, BinOp, Condition, Constant, Expr, FunctionBody, LetCondition,
    Param, ParamQualifiers, Pattern, StrPart, Type, UnOp, Wrapper,
};

use crate::exprs::format_expr_body;
use crate::{FormatCtx, Formattable, SyntaxOps};

#[derive(Debug, Clone, Copy)]
pub(crate) struct Indent(pub(crate) usize);

impl fmt::Display for Indent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Formatting widths have a runtime limit, even though they accept usize.
        let mut remaining = self.0;
        while remaining > 0 {
            let width = remaining.min(1024);
            write!(f, "{:width$}", "")?;
            remaining -= width;
        }
        Ok(())
    }
}

pub(crate) struct SepBy<'s, I> {
    iter: I,
    separator: &'static str,
    ctx: FormatCtx<'s>,
}

impl<'s, I> SepBy<'s, I> {
    pub(crate) fn new(iter: I, separator: &'static str, ctx: FormatCtx<'s>) -> Self {
        Self {
            iter,
            separator,
            ctx,
        }
    }
}

impl<I: Iterator + Clone> fmt::Display for SepBy<'_, I>
where
    I::Item: Formattable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.iter.clone().enumerate() {
            if i > 0 {
                write!(f, "{}", self.separator)?;
            }
            write!(f, "{}", item.as_fmt(self.ctx))?;
        }
        Ok(())
    }
}

pub(crate) struct SepByMultiline<'s, I> {
    iter: I,
    separator: &'static str,
    ctx: FormatCtx<'s>,
    padding: &'static str,
    trailing_separator: bool,
}

impl<'s, I> SepByMultiline<'s, I> {
    pub(crate) fn new(iter: I, separator: &'static str, ctx: FormatCtx<'s>) -> Self {
        Self {
            iter,
            separator,
            ctx,
            padding: "",
            trailing_separator: false,
        }
    }

    pub(crate) fn with_trailing_separator(mut self, trailing_separator: bool) -> Self {
        self.trailing_separator = trailing_separator;
        self
    }

    pub(crate) fn with_padding(mut self, padding: &'static str) -> Self {
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
            trailing_separator,
        } = self;
        let trailing = if *trailing_separator {
            separator.trim_end()
        } else {
            ""
        };
        let width = iter.clone().map(|e| e.approx_width()).sum::<usize>()
            + separator.len() * (iter.len().max(1) - 1)
            + trailing.len();

        if ctx.indent_width() + width > usize::from(ctx.settings.max_width) {
            let sep = separator.trim_end();
            let ctx = ctx.indented();
            for (i, elem) in iter.clone().enumerate() {
                if i > 0 {
                    write!(f, "{sep}")?;
                }
                writeln!(f)?;
                write!(f, "{}{}", ctx.indent(), elem.as_fmt(ctx))?;
            }
            writeln!(f, "{trailing}")?;
            write!(f, "{}", ctx.decrement().indent())
        } else {
            write!(
                f,
                "{padding}{}{trailing}{padding}",
                SepBy::new(iter.clone(), separator, *ctx)
            )
        }
    }
}

enum ChainPart<'ast, 'src, K: AstKind> {
    Member(&'src str),
    Call(
        &'src str,
        &'ast [K::Inner<Type<'src, K>>],
        &'ast [K::Inner<Expr<'src, K>>],
    ),
    Op(BinOp, &'ast Expr<'src, K>),
    Index(&'ast Expr<'src, K>),
}

pub(crate) fn format_chain<K: AstKind>(
    expr: &Expr<'_, K>,
    f: &mut fmt::Formatter<'_>,
    ctx: FormatCtx<'_>,
) -> fmt::Result {
    let mut base_expr = expr;
    let mut chain = vec![];

    let mut chain_fields = 0;
    let mut chain_calls = 0;
    let mut chain_ops = 0;

    let mut parent_op = ctx.parent;
    let ctx = ctx.without_parent_op();

    let parenthesize = loop {
        match base_expr {
            // handle expressions that might require parentheses first
            Expr::Assign { .. } | Expr::Conditional { .. } | Expr::Lambda { .. } => {
                break parent_op.is_some();
            }
            Expr::DynCast { .. } => {
                break matches!(parent_op, Some(ParentOp::TopPrec | ParentOp::Unary));
            }
            Expr::UnOp { .. } | Expr::New { .. } => {
                break matches!(parent_op, Some(ParentOp::TopPrec));
            }
            Expr::Constant(Constant::I32(v)) if *v < 0 => {
                break matches!(parent_op, Some(ParentOp::TopPrec));
            }
            Expr::Constant(Constant::I64(v)) if *v < 0 => {
                break matches!(parent_op, Some(ParentOp::TopPrec));
            }
            Expr::BinOp { lhs, op, rhs } => {
                if parent_op.is_some_and(|parent| !parent.can_assoc(*op)) {
                    break true;
                }
                base_expr = (**lhs).as_wrapped();
                parent_op = Some(ParentOp::BinaryLhs(*op));
                chain.push(ChainPart::Op(*op, (**rhs).as_wrapped()));
                chain_ops += 1;
            }
            // break the chain if we're chaining operators and we encounter a non-operator
            _ if chain_ops > 0 => {
                break false;
            }
            Expr::Member { expr, member } => {
                base_expr = (**expr).as_wrapped();
                parent_op = Some(ParentOp::TopPrec);
                chain.push(ChainPart::Member(member));
                chain_fields += 1;
            }
            Expr::Index { expr, index } => {
                base_expr = (**expr).as_wrapped();
                parent_op = Some(ParentOp::TopPrec);
                chain.push(ChainPart::Index((**index).as_wrapped()));
            }
            Expr::Call {
                expr,
                type_args,
                args,
            } => {
                if let Expr::Member { expr, member } = (**expr).as_wrapped() {
                    base_expr = (**expr).as_wrapped();
                    parent_op = Some(ParentOp::TopPrec);
                    chain.push(ChainPart::Call(member, &type_args[..], &args[..]));
                    chain_calls += 1;
                } else {
                    break false;
                }
            }
            _ => break false,
        };
    };

    // Without parentheses, the lexer consumes the member-access dot as part of the number.
    let parenthesize = parenthesize
        || matches!(base_expr, Expr::Constant(Constant::I32(_)))
            && matches!(
                chain.last(),
                Some(ChainPart::Member(_) | ChainPart::Call(..))
            );

    let base = DisplayFn(|f: &mut fmt::Formatter<'_>| {
        // The chain root's prefix was already emitted by Expr::format.
        if std::ptr::eq(base_expr, expr) {
            format_expr_body(base_expr, f, ctx)
        } else {
            base_expr.format(f, ctx)
        }
    });
    if parenthesize {
        write!(f, "({base})")?;
    } else {
        write!(f, "{base}")?;
    }

    let width = base_expr.approx_width()
        + if parenthesize { 2 } else { 0 }
        + chain.iter().map(ChainPart::approx_width).sum::<usize>();
    let break_line = ctx.indent_width() + width > usize::from(ctx.settings.max_width)
        || chain_fields > usize::from(ctx.settings.max_chain_fields)
        || chain_calls > usize::from(ctx.settings.max_chain_calls)
        || chain_ops > usize::from(ctx.settings.max_chain_operators)
        || chain.len() > usize::from(ctx.settings.max_chain_total);

    let ctx = ctx.indented();

    for part in chain.into_iter().rev() {
        match part {
            ChainPart::Member(member) => {
                if break_line {
                    writeln!(f)?;
                    write!(f, "{}", ctx.indent())?;
                }
                write!(f, ".{member}")?;
            }
            ChainPart::Call(member, type_args, args) => {
                if break_line {
                    writeln!(f)?;
                    write!(f, "{}", ctx.indent())?;
                }
                write!(f, ".{member}")?;
                format_call_args::<K>(type_args, args, f, ctx)?;
            }
            ChainPart::Index(index) => write!(f, "[{}]", index.as_fmt(ctx))?,
            ChainPart::Op(op, rhs) => {
                let ctx = ctx.with_parent_op(ParentOp::BinaryRhs(op));
                if break_line {
                    writeln!(f)?;
                    write!(f, "{}", ctx.indent())?;
                } else {
                    write!(f, " ")?;
                }
                write!(f, "{} {}", op.as_fmt(ctx), rhs.as_fmt(ctx))?;
            }
        }
    }

    Ok(())
}

pub(crate) fn format_call_args<K: AstKind>(
    type_args: &[K::Inner<Type<'_, K>>],
    args: &[K::Inner<Expr<'_, K>>],
    f: &mut fmt::Formatter<'_>,
    ctx: FormatCtx<'_>,
) -> fmt::Result {
    if !type_args.is_empty() {
        write!(
            f,
            "<{}>",
            SepBy::new(type_args.iter().map(Wrapper::as_wrapped), ", ", ctx)
        )?;
    }
    write!(
        f,
        "({})",
        SepByMultiline::new(args.iter().map(Wrapper::as_wrapped), ", ", ctx)
            .with_trailing_separator(
                args.last()
                    .is_some_and(|arg| matches!(arg.as_wrapped(), Expr::Omitted))
            )
    )
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ParentOp {
    AssignmentOperand,
    ConditionalCondition,
    Unary,
    BinaryLhs(BinOp),
    BinaryRhs(BinOp),
    TopPrec,
}

impl ParentOp {
    fn can_assoc(&self, op: BinOp) -> bool {
        match self {
            ParentOp::AssignmentOperand | ParentOp::ConditionalCondition => true,
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

pub(crate) trait ApproxWidth {
    fn approx_width(&self) -> usize;
}

impl<A: ApproxWidth, B: ApproxWidth> ApproxWidth for (A, B) {
    fn approx_width(&self) -> usize {
        self.0.approx_width() + self.1.approx_width()
    }
}

impl<A: ApproxWidth, B: ApproxWidth, C: ApproxWidth> ApproxWidth for (A, B, C) {
    fn approx_width(&self) -> usize {
        self.0.approx_width() + self.1.approx_width() + self.2.approx_width()
    }
}

impl<A: ?Sized + ApproxWidth> ApproxWidth for &A {
    fn approx_width(&self) -> usize {
        (**self).approx_width()
    }
}

impl<A: ApproxWidth> ApproxWidth for Option<A> {
    fn approx_width(&self) -> usize {
        match self {
            Some(value) => value.approx_width(),
            None => 0,
        }
    }
}

impl ApproxWidth for str {
    fn approx_width(&self) -> usize {
        self.len()
    }
}

impl<K> ApproxWidth for Expr<'_, K>
where
    K: AstKind,
{
    fn approx_width(&self) -> usize {
        match self {
            Expr::Ident(ident) => ident.len(),
            Expr::Constant(constant) => match constant {
                Constant::String(s) => s.escape_default().count() + 2,
                Constant::CName(s) => s.escape_default().count() + 3,
                Constant::Resource(r) => r.escape_default().count() + 3,
                Constant::TweakDbId(t) => t.escape_default().count() + 3,
                Constant::F32(f) => ftoa(*f, FmtFloatConfig::default()).len(),
                Constant::F64(f) => dtoa(*f, FmtFloatConfig::default()).len() + 1,
                Constant::I32(i) => {
                    i.unsigned_abs().checked_ilog10().unwrap_or(0) as usize
                        + 1
                        + usize::from(*i < 0)
                }
                Constant::I64(i) => {
                    i.unsigned_abs().checked_ilog10().unwrap_or(0) as usize
                        + 2
                        + usize::from(*i < 0)
                }
                Constant::U32(u) => u.checked_ilog10().unwrap_or(0) as usize + 2,
                Constant::U64(u) => u.checked_ilog10().unwrap_or(0) as usize + 3,
                Constant::Bool(true) => 4,
                Constant::Bool(false) => 5,
            },
            Expr::ArrayLit(elems) => elems
                .iter()
                .map(|el| el.as_wrapped().approx_width() + 2)
                .sum::<usize>(),
            Expr::InterpolatedString(parts) => {
                parts
                    .iter()
                    .map(|part| match part {
                        StrPart::Expr(expr) => expr.as_wrapped().approx_width() + 3,
                        StrPart::Str(s) => s.escape_default().count(),
                    })
                    .sum::<usize>()
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
                        .sum::<usize>()
                    + type_args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<usize>()
            }
            Expr::Member { expr, member } => {
                (**expr).as_wrapped().approx_width() + member.len() + 1
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
                        .sum::<usize>()
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
                    .map(|param| param.as_wrapped().name.len() + 2)
                    .sum::<usize>()
                    + body.approx_width()
                    + 4
            }
            Expr::This | Expr::Null => 4,
            Expr::Super => 5,
            Expr::Omitted | Expr::Error => 0,
        }
    }
}

impl<K: AstKind> ApproxWidth for FunctionBody<'_, K> {
    fn approx_width(&self) -> usize {
        match self {
            FunctionBody::Block(block) => {
                // TODO: come up with something better
                block.stmts.len() * 10 + 4
            }
            FunctionBody::Inline(expr) => (**expr).as_wrapped().approx_width(),
        }
    }
}

impl<K: AstKind> ApproxWidth for Param<'_, K> {
    fn approx_width(&self) -> usize {
        self.name.len()
            + self
                .typ
                .as_ref()
                .map(|typ| typ.as_wrapped().approx_width() + 2)
                .unwrap_or_default()
            + usize::from(self.qualifiers.contains(ParamQualifiers::OUT)) * 4
            + usize::from(self.qualifiers.contains(ParamQualifiers::OPTIONAL)) * 4
            + usize::from(self.qualifiers.contains(ParamQualifiers::CONST)) * 6
            + usize::from(self.qualifiers.contains(ParamQualifiers::SKIP)) * 5
    }
}

impl<K: AstKind> ApproxWidth for Type<'_, K> {
    fn approx_width(&self) -> usize {
        match self {
            Type::Named { name, args } if args.is_empty() => name.len(),
            Type::Named { name, args } => {
                name.len()
                    + args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<usize>()
            }
            Type::Array(el) => (**el).as_wrapped().approx_width() + 2,
            Type::StaticArray(el, size) => {
                (**el).as_wrapped().approx_width() + size.checked_ilog10().unwrap_or(0) as usize + 4
            }
            Type::Fn {
                params,
                return_type,
            } => {
                params
                    .iter()
                    .map(|param| param.as_wrapped().approx_width() + 2)
                    .sum::<usize>()
                    + (**return_type).as_wrapped().approx_width()
                    + 6
            }
        }
    }
}

impl<K: AstKind> ApproxWidth for Condition<'_, K> {
    fn approx_width(&self) -> usize {
        match self {
            Self::Expr(expr) => expr.as_wrapped().approx_width(),
            Self::Pattern(pattern) => pattern.as_wrapped().approx_width() + 4,
        }
    }
}

impl<K: AstKind> ApproxWidth for LetCondition<'_, K> {
    fn approx_width(&self) -> usize {
        match self {
            Self::Expr(expr) => expr.as_wrapped().approx_width(),
            Self::LetPattern(pattern, val) => {
                pattern.as_wrapped().approx_width() + val.as_wrapped().approx_width() + 6
            }
        }
    }
}

impl<K: AstKind> ApproxWidth for Pattern<'_, K> {
    fn approx_width(&self) -> usize {
        match self {
            Pattern::Name(name) => name.as_wrapped().len(),
            Pattern::As(inner, typ) => {
                (**inner).as_wrapped().approx_width() + typ.as_wrapped().approx_width() + 2
            }
            Pattern::Aggregate(name, items) => {
                name.as_wrapped().len()
                    + items
                        .iter()
                        .map(|(name, pat)| {
                            (*name).as_wrapped().len() + pat.as_wrapped().approx_width() + 2
                        })
                        .sum::<usize>()
                    + 4
            }
            Pattern::Nullable(inner) => (**inner).as_wrapped().approx_width() + 1,
            Pattern::Array(spread, elems) => {
                elems
                    .as_wrapped()
                    .iter()
                    .map(|el| el.approx_width() + 2)
                    .sum::<usize>()
                    + match spread {
                        ArraySpread::End | ArraySpread::Start => 4,
                        ArraySpread::None => 0,
                    }
            }
        }
    }
}

impl ApproxWidth for BinOp {
    fn approx_width(&self) -> usize {
        self.symbol().len()
    }
}

impl ApproxWidth for UnOp {
    fn approx_width(&self) -> usize {
        self.symbol().len()
    }
}

impl<K: AstKind> ApproxWidth for ChainPart<'_, '_, K> {
    fn approx_width(&self) -> usize {
        match self {
            ChainPart::Member(member) => member.len() + 1,
            ChainPart::Call(member, type_args, args) => {
                member.len()
                    + type_args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<usize>()
                    + args
                        .iter()
                        .map(|arg| arg.as_wrapped().approx_width() + 2)
                        .sum::<usize>()
                    + 2
            }
            ChainPart::Op(op, rhs) => op.approx_width() + rhs.as_wrapped().approx_width() + 2,
            ChainPart::Index(index) => index.as_wrapped().approx_width() + 2,
        }
    }
}

pub(crate) struct DisplayFn<F>(pub(crate) F);

impl<F> fmt::Display for DisplayFn<F>
where
    F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0(f)
    }
}
