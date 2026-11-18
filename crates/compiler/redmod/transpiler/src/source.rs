//! Source capability checks for semantic facts not recoverable from lowered IR.

use redscript_community_ast::{self as ast, AstVisitor, SourceExpr, Spanned, StrPart, WithSpan};
use redscript_compiler_frontend::ArrayPrefixInfo as ExprInfo;

use crate::{Result, TranspileDiagnostic, error};

pub fn validate(function: &ast::SourceFunction<'_>) -> Result<()> {
    Guard { lazy: false }.visit_function(function)
}

struct ArrayFinder;

// Short-circuit traversal once an array literal is found, not a capability failure.
struct ArrayFound;

impl<'src> AstVisitor<'src, WithSpan> for ArrayFinder {
    type Error = ArrayFound;

    fn visit_array_lit(
        &mut self,
        _: &[Spanned<SourceExpr<'src>>],
    ) -> std::result::Result<(), ArrayFound> {
        Err(ArrayFound)
    }
}

fn has_array(expr: &Spanned<SourceExpr<'_>>) -> bool {
    ArrayFinder.visit_expr(expr).is_err()
}

fn is_reordering_unsafe(before: ExprInfo, after: ExprInfo) -> bool {
    ExprInfo::is_reordering_unsafe(before, after)
}

fn inspect_siblings<'a, 'src: 'a>(
    exprs: impl IntoIterator<Item = &'a Spanned<SourceExpr<'src>>>,
) -> ExprInfo {
    exprs
        .into_iter()
        .fold(ExprInfo::default(), |acc, expr| {
            acc.combine_prefixes(inspect(expr))
        })
}

fn inspect((expr, _): &Spanned<SourceExpr<'_>>) -> ExprInfo {
    match expr {
        ast::Expr::ArrayLit(elems) => {
            let inner = inspect_siblings(elems);
            ExprInfo {
                has_array: true,
                array_effects: inner.has_side_effects,
                array_reads_state: inner.reads_state,
                has_side_effects: inner.has_side_effects,
                reads_state: inner.reads_state,
            }
        }
        ast::Expr::Call { expr, args, .. } => {
            let callee = inspect(expr);
            let args_info = inspect_siblings(args);
            ExprInfo {
                has_array: callee.has_array || args_info.has_array,
                array_effects: callee.array_effects || args_info.array_effects,
                array_reads_state: callee.array_reads_state || args_info.array_reads_state,
                has_side_effects: true,
                reads_state: true,
            }
        }
        ast::Expr::New { args, .. } => inspect_siblings(args),
        ast::Expr::InterpolatedString(parts) => inspect_siblings(parts.iter().filter_map(|part| {
            match part {
                StrPart::Expr(expr) => Some(expr),
                StrPart::Str(_) => None,
            }
        })),
        ast::Expr::BinOp { lhs, rhs, .. } => inspect(lhs).combine_prefixes(inspect(rhs)),
        ast::Expr::Index { expr, index } => {
            let mut info = inspect(expr).combine_prefixes(inspect(index));
            info.reads_state = true;
            info
        }
        ast::Expr::Assign { lhs, rhs } => {
            let mut info = inspect(lhs).combine_prefixes(inspect(rhs));
            info.has_side_effects = true;
            info.reads_state = true;
            info
        }
        ast::Expr::Conditional { cond, then, else_ } => inspect(cond)
            .combine_prefixes(inspect(then))
            .combine_prefixes(inspect(else_)),
        ast::Expr::Member { expr, .. } => {
            let mut info = inspect(expr);
            info.reads_state = true;
            info
        }
        ast::Expr::DynCast { expr, .. } | ast::Expr::UnOp { expr, .. } => inspect(expr),
        ast::Expr::Ident(_) => ExprInfo {
            reads_state: true,
            ..ExprInfo::default()
        },
        ast::Expr::Lambda { .. }
        | ast::Expr::Constant(_)
        | ast::Expr::Null
        | ast::Expr::This
        | ast::Expr::Super
        | ast::Expr::Omitted
        | ast::Expr::Error => ExprInfo::default(),
    }
}

fn check_pair(before: &Spanned<SourceExpr<'_>>, after: &Spanned<SourceExpr<'_>>) -> Result<()> {
    if is_reordering_unsafe(inspect(before), inspect(after)) {
        return Err(error(
            "this array may run before the left operand; evaluate both in order using local variables",
            Some(after.1),
        ));
    }
    Ok(())
}

fn check_siblings(exprs: &[Spanned<SourceExpr<'_>>]) -> Result<()> {
    let mut earlier = ExprInfo::default();
    for expr in exprs {
        let info = inspect(expr);
        if is_reordering_unsafe(earlier, info) {
            return Err(error(
                "this array may run before an earlier argument; evaluate arguments in order using local variables",
                Some(expr.1),
            ));
        }
        earlier = earlier.combine_prefixes(info);
    }
    Ok(())
}

struct Guard {
    lazy: bool,
}

impl<'src> AstVisitor<'src, WithSpan> for Guard {
    type Error = TranspileDiagnostic;

    fn visit_node(&mut self, node: ast::AstNode<'_, 'src, WithSpan>) -> Result<()> {
        if let ast::AstNode::Stmt((ast::Stmt::Let { is_const: true, .. }, span)) = node {
            return Err(error(
                "const locals are not supported; use let if the local does not need to be constant",
                Some(*span),
            ));
        }
        Ok(())
    }

    fn visit_array_lit(&mut self, elems: &[Spanned<SourceExpr<'src>>]) -> Result<()> {
        if self.lazy {
            return Err(error(
                "array literals are not supported here; create the array inside an explicit if branch",
                elems.first().map(|(_, span)| *span),
            ));
        }
        check_siblings(elems)?;
        elems.iter().try_for_each(|e| self.visit_expr(e))
    }

    fn visit_bin_op(
        &mut self,
        lhs: &Spanned<SourceExpr<'src>>,
        op: ast::BinOp,
        rhs: &Spanned<SourceExpr<'src>>,
    ) -> Result<()> {
        check_pair(lhs, rhs)?;
        self.visit_expr(lhs)?;
        let previous = self.lazy;
        self.lazy |= matches!(op, ast::BinOp::And | ast::BinOp::Or);
        let result = self.visit_expr(rhs);
        self.lazy = previous;
        result
    }

    fn visit_call(
        &mut self,
        callee: &Spanned<SourceExpr<'src>>,
        _: &[Spanned<ast::SourceType<'src>>],
        args: &[Spanned<SourceExpr<'src>>],
    ) -> Result<()> {
        check_siblings(args)?;
        if !matches!(callee.0, ast::Expr::Ident(_))
            && is_reordering_unsafe(inspect(callee), inspect_siblings(args))
        {
            return Err(error(
                "this array may run before the receiver; evaluate the receiver and arguments in order using locals",
                Some(callee.1),
            ));
        }
        self.visit_expr(callee)?;
        args.iter().try_for_each(|e| self.visit_expr(e))
    }

    fn visit_interpolated_string(&mut self, parts: &[ast::StrPart<'src, WithSpan>]) -> Result<()> {
        let mut earlier = ExprInfo::default();
        for part in parts {
            if let ast::StrPart::Expr(expr) = part {
                let info = inspect(expr);
                if is_reordering_unsafe(earlier, info) {
                    return Err(error(
                        "this array may reorder string expressions; evaluate them in order using local variables",
                        Some(expr.1),
                    ));
                }
                self.visit_expr(expr)?;
                earlier = earlier.combine_prefixes(info);
            }
        }
        Ok(())
    }

    fn visit_case(&mut self, case: &ast::Case<'src, WithSpan>) -> Result<()> {
        // Matchers are evaluated conditionally, but their array prefixes are
        // currently emitted before the entire switch by shared lowering.
        // Patterns contain bindings/types, not executable expressions.
        if let ast::Condition::Expr(expr) = &case.condition
            && has_array(expr)
        {
            return Err(error(
                "array literals are not supported in case expressions; use if/else branches instead",
                Some(expr.1),
            ));
        }
        self.visit_condition(&case.condition)?;
        case.body.iter().try_for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_assign(
        &mut self,
        lhs: &Spanned<SourceExpr<'src>>,
        rhs: &Spanned<SourceExpr<'src>>,
    ) -> Result<()> {
        if !matches!(lhs.0, ast::Expr::Ident(_)) && is_reordering_unsafe(inspect(lhs), inspect(rhs)) {
            return Err(error(
                "this array may run before the assignment target; evaluate the target before creating the array",
                Some(lhs.1),
            ));
        }
        self.visit_expr(lhs)?;
        self.visit_expr(rhs)
    }

    fn visit_new(
        &mut self,
        _: &Spanned<ast::SourceType<'src>>,
        args: &[Spanned<SourceExpr<'src>>],
    ) -> Result<()> {
        check_siblings(args)?;
        args.iter().try_for_each(|e| self.visit_expr(e))
    }

    fn visit_index(
        &mut self,
        array: &Spanned<SourceExpr<'src>>,
        index: &Spanned<SourceExpr<'src>>,
    ) -> Result<()> {
        check_pair(array, index)?;
        self.visit_expr(array)?;
        self.visit_expr(index)
    }

    fn visit_conditional(
        &mut self,
        cond: &Spanned<SourceExpr<'src>>,
        then: &Spanned<SourceExpr<'src>>,
        else_: &Spanned<SourceExpr<'src>>,
    ) -> Result<()> {
        self.visit_expr(cond)?;
        let previous = self.lazy;
        self.lazy = true;
        let result = self.visit_expr(then).and_then(|()| self.visit_expr(else_));
        self.lazy = previous;
        result
    }
}
