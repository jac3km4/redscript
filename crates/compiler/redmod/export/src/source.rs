//! Source capability checks for semantic facts not recoverable from lowered IR.

use redscript_community_ast::{self as ast, AstVisitor, SourceExpr, Spanned, WithSpan};

use crate::{ExportDiagnostic, Result, error};

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

fn check_pair(before: &Spanned<SourceExpr<'_>>, after: &Spanned<SourceExpr<'_>>) -> Result<()> {
    if has_array(after) && !matches!(before.0, ast::Expr::Constant(_)) {
        return Err(error(
            "array prefix may reorder an earlier operand; bind operands explicitly",
            Some(after.1),
        ));
    }
    Ok(())
}

fn check_siblings(exprs: &[Spanned<SourceExpr<'_>>]) -> Result<()> {
    let mut earlier_expression = false;
    for expr in exprs {
        if earlier_expression && has_array(expr) {
            return Err(error(
                "array prefix may reorder an earlier argument; bind arguments explicitly",
                Some(expr.1),
            ));
        }
        earlier_expression |= !matches!(expr.0, ast::Expr::Constant(_));
    }
    Ok(())
}

struct Guard {
    lazy: bool,
}

impl<'src> AstVisitor<'src, WithSpan> for Guard {
    type Error = ExportDiagnostic;

    fn visit_array_lit(&mut self, elems: &[Spanned<SourceExpr<'src>>]) -> Result<()> {
        if self.lazy {
            return Err(error(
                "array literal in lazy operand has no retained control-flow provenance; bind the array in the selected branch explicitly",
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
        if args.iter().any(has_array) && !matches!(callee.0, ast::Expr::Ident(_)) {
            return Err(error(
                "array prefix may reorder receiver evaluation; bind arguments explicitly",
                Some(callee.1),
            ));
        }
        self.visit_expr(callee)?;
        args.iter().try_for_each(|e| self.visit_expr(e))
    }

    fn visit_interpolated_string(&mut self, parts: &[ast::StrPart<'src, WithSpan>]) -> Result<()> {
        let mut earlier_expression = false;
        for part in parts {
            if let ast::StrPart::Expr(expr) = part {
                if earlier_expression && has_array(expr) {
                    return Err(error(
                        "array prefix may reorder an earlier interpolation expression; bind interpolation operands explicitly",
                        Some(expr.1),
                    ));
                }
                self.visit_expr(expr)?;
                earlier_expression = true;
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
                "array prefix in switch matcher has no retained conditional evaluation provenance; bind the matcher array explicitly",
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
        if has_array(rhs) && !matches!(lhs.0, ast::Expr::Ident(_)) {
            return Err(error(
                "array prefix may reorder assignment place evaluation",
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
