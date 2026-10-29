use hashbrown::HashSet;
use redscript_community_ast::{self as ast, Span, Spanned};
use thiserror::Error;

#[derive(Debug)]
pub struct Evaluator<'a> {
    modules: HashSet<Box<[&'a str]>>,
}

impl<'a> Evaluator<'a> {
    pub fn new(modules: HashSet<Box<[&'a str]>>) -> Self {
        Self { modules }
    }

    pub fn from_modules<'b>(modules: impl IntoIterator<Item = &'b ast::SourceModule<'a>>) -> Self
    where
        'a: 'b,
    {
        let modules = modules
            .into_iter()
            .filter_map(|m| m.path.as_ref())
            .map(|path| path.segments.clone())
            .collect::<HashSet<_>>();
        Self { modules }
    }

    /// Whether the supplied compilation inputs contain this exact module path.
    pub fn module_exists(&self, path: &str) -> bool {
        let path = path.split('.').collect::<Vec<_>>();
        self.modules.contains(&path[..])
    }

    pub fn eval(&self, (expr, span): &Spanned<ast::SourceExpr<'_>>) -> Result<Value, Error> {
        let res = match expr {
            ast::Expr::Constant(ast::Constant::Bool(b)) => Value::Bool(*b),
            ast::Expr::BinOp { lhs, op, rhs } => {
                if !matches!(op, ast::BinOp::And | ast::BinOp::Or) {
                    return Err(Error::UnsupportedBinaryOperator(*op, *span));
                }
                let (Value::Bool(lhs), Value::Bool(rhs)) = (self.eval(lhs)?, self.eval(rhs)?);
                Value::Bool(match op {
                    ast::BinOp::And => lhs && rhs,
                    ast::BinOp::Or => lhs || rhs,
                    _ => unreachable!(),
                })
            }
            ast::Expr::UnOp { op, expr } => {
                if !matches!(op, ast::UnOp::Not) {
                    return Err(Error::UnsupportedUnaryOperator(*op, *span));
                }
                let Value::Bool(b) = self.eval(expr)?;
                Value::Bool(!b)
            }
            ast::Expr::Call {
                expr,
                type_args,
                args,
            } => {
                if !matches!(expr.0, ast::Expr::Ident("ModuleExists")) {
                    return Err(Error::UnknownFunction(callee_name(&expr.0), expr.1));
                }
                if let Some((_, span)) = type_args.first() {
                    return Err(Error::ModuleExistsTypeArguments(*span));
                }
                match &**args {
                    [(ast::Expr::Constant(ast::Constant::String(str)), _)] => {
                        Value::Bool(self.module_exists(str))
                    }
                    [(_, span)] => return Err(Error::ModuleExistsExpectedStringLiteral(*span)),
                    _ => {
                        let span = match (args.first(), args.last()) {
                            (Some((_, first)), Some((_, last))) => first.merge(last),
                            _ => *span,
                        };
                        return Err(Error::ModuleExistsArgumentCount(args.len(), span));
                    }
                }
            }
            _ => return Err(Error::ExpectedBoolean(*span)),
        };
        Ok(res)
    }
}

pub enum Value {
    Bool(bool),
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("expected a compile-time boolean expression")]
    ExpectedBoolean(Span),
    #[error("unsupported compile-time unary operator `{}`; only `!` is supported", unary_symbol(.0))]
    UnsupportedUnaryOperator(ast::UnOp, Span),
    #[error("unsupported compile-time binary operator `{}`; only `&&` and `||` are supported", binary_symbol(.0))]
    UnsupportedBinaryOperator(ast::BinOp, Span),
    #[error("unknown compile-time function{}; only `ModuleExists` is supported", .0.as_ref().map(|name| format!(" `{name}`")).unwrap_or_default())]
    UnknownFunction(Option<String>, Span),
    #[error(
        "`ModuleExists` requires exactly one argument, got {0}; usage: ModuleExists(\"Module.Name\")"
    )]
    ModuleExistsArgumentCount(usize, Span),
    #[error(
        "`ModuleExists` requires a string literal argument; usage: ModuleExists(\"Module.Name\")"
    )]
    ModuleExistsExpectedStringLiteral(Span),
    #[error("`ModuleExists` does not accept type arguments; usage: ModuleExists(\"Module.Name\")")]
    ModuleExistsTypeArguments(Span),
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Self::ExpectedBoolean(span)
            | Self::UnsupportedUnaryOperator(_, span)
            | Self::UnsupportedBinaryOperator(_, span)
            | Self::UnknownFunction(_, span)
            | Self::ModuleExistsArgumentCount(_, span)
            | Self::ModuleExistsExpectedStringLiteral(span)
            | Self::ModuleExistsTypeArguments(span) => *span,
        }
    }
}

fn unary_symbol(op: &ast::UnOp) -> &'static str {
    match op {
        ast::UnOp::BitNot => "~",
        ast::UnOp::Not => "!",
        ast::UnOp::Neg => "-",
    }
}

fn binary_symbol(op: &ast::BinOp) -> &'static str {
    match op {
        ast::BinOp::AssignAdd => "+=",
        ast::BinOp::AssignSub => "-=",
        ast::BinOp::AssignMul => "*=",
        ast::BinOp::AssignDiv => "/=",
        ast::BinOp::AssignBitOr => "|=",
        ast::BinOp::AssignBitAnd => "&=",
        ast::BinOp::Or => "||",
        ast::BinOp::And => "&&",
        ast::BinOp::BitOr => "|",
        ast::BinOp::BitXor => "^",
        ast::BinOp::BitAnd => "&",
        ast::BinOp::Eq => "==",
        ast::BinOp::Ne => "!=",
        ast::BinOp::Lt => "<",
        ast::BinOp::Le => "<=",
        ast::BinOp::Gt => ">",
        ast::BinOp::Ge => ">=",
        ast::BinOp::Add => "+",
        ast::BinOp::Sub => "-",
        ast::BinOp::Mul => "*",
        ast::BinOp::Div => "/",
        ast::BinOp::Mod => "%",
    }
}

fn callee_name(expr: &ast::SourceExpr<'_>) -> Option<String> {
    match expr {
        ast::Expr::Ident(name) => Some((*name).to_owned()),
        ast::Expr::Member { expr, member } => {
            callee_name(&expr.0).map(|name| format!("{name}.{member}"))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Diagnostic;

    fn parse(source: &str) -> Spanned<ast::SourceExpr<'_>> {
        let file = ast::SourceMap::new().push_back("cte.reds", source);
        let (expr, errors) = redscript_community_parser::parse_expr(source, file);
        assert!(errors.is_empty(), "{source}: {errors:?}");
        (
            expr.expect("expected an expression"),
            Span::new(0, source.len() as u32, file),
        )
    }

    fn assert_error(source: &str, highlighted: &str, message: &str) -> Error {
        let expr = parse(source);
        let error = Evaluator::new(HashSet::new())
            .eval(&expr)
            .err()
            .expect("expected an evaluation error");
        let start = source.find(highlighted).unwrap() as u32;
        assert_eq!(
            error.span(),
            Span::new(start, start + highlighted.len() as u32, expr.1.file),
            "{source}"
        );
        assert_eq!(error.to_string(), message, "{source}");
        error
    }

    #[test]
    fn evaluates_boolean_expressions_and_module_exists() {
        let evaluator = Evaluator::new(HashSet::from([Box::from(["Test", "Present"])]));
        for (source, expected) in [
            ("true", true),
            ("false", false),
            ("!true", false),
            ("!false", true),
            ("true && false", false),
            ("true && true", true),
            ("false || true", true),
            ("false || false", false),
            (r#"ModuleExists("Test.Present")"#, true),
            (r#"ModuleExists("Test.Missing")"#, false),
            (r#"!ModuleExists("Test.Missing") && (false || true)"#, true),
        ] {
            let Value::Bool(actual) = evaluator.eval(&parse(source)).unwrap();
            assert_eq!(actual, expected, "{source}");
        }
    }

    #[test]
    fn module_lookup_matches_evaluation_for_exact_supplied_paths() {
        let sources = ast::SourceMap::new();
        sources.push_back("present.reds", "module Test.Present\n");
        sources.push_back("duplicate.reds", "module Test.Present\n");
        sources.push_back("unnamed.reds", "");
        let mut reporter = crate::CompileErrorReporter::default();
        let modules = crate::parse_files(&sources, &mut reporter);
        assert!(reporter.reported().is_empty());
        let evaluator = Evaluator::from_modules(&modules);
        for (path, expected) in [
            ("Test.Present", true),
            ("Test", false),
            ("Present", false),
            ("Test.Missing", false),
            ("test.Present", false),
            ("Test.Present.", false),
            ("", false),
        ] {
            assert_eq!(evaluator.module_exists(path), expected, "{path}");
            let source = format!("ModuleExists(\"{path}\")");
            let Value::Bool(value) = evaluator.eval(&parse(&source)).unwrap();
            assert_eq!(value, expected, "{path}");
        }
    }

    #[test]
    fn expects_compile_time_boolean_expression() {
        for (source, highlighted) in [
            ("42", "42"),
            (r#""text""#, r#""text""#),
            ("value", "value"),
            ("!42", "42"),
            ("true && value", "value"),
            ("false || value", "value"),
            // Keep evaluating both operands, even when the left determines the result.
            ("false && value", "value"),
            ("true || value", "value"),
        ] {
            assert!(matches!(
                assert_error(
                    source,
                    highlighted,
                    "expected a compile-time boolean expression"
                ),
                Error::ExpectedBoolean(_)
            ));
        }
    }

    #[test]
    fn rejects_unsupported_unary_operators_before_operands() {
        for (source, op, symbol) in [
            ("-42", ast::UnOp::Neg, "-"),
            ("~42", ast::UnOp::BitNot, "~"),
        ] {
            let message = format!(
                "unsupported compile-time unary operator `{symbol}`; only `!` is supported"
            );
            assert!(matches!(
                assert_error(source, source, &message),
                Error::UnsupportedUnaryOperator(actual, _) if actual == op
            ));
        }
    }

    #[test]
    fn rejects_unsupported_binary_operators_before_operands() {
        for (source, op, symbol) in [
            ("1 + 2", ast::BinOp::Add, "+"),
            ("1 - 2", ast::BinOp::Sub, "-"),
            ("1 * 2", ast::BinOp::Mul, "*"),
            ("1 / 2", ast::BinOp::Div, "/"),
            ("1 % 2", ast::BinOp::Mod, "%"),
            ("1 | 2", ast::BinOp::BitOr, "|"),
            ("1 & 2", ast::BinOp::BitAnd, "&"),
            ("1 ^ 2", ast::BinOp::BitXor, "^"),
            ("true == false", ast::BinOp::Eq, "=="),
            ("true != false", ast::BinOp::Ne, "!="),
            ("1 < 2", ast::BinOp::Lt, "<"),
            ("1 <= 2", ast::BinOp::Le, "<="),
            ("1 > 2", ast::BinOp::Gt, ">"),
            ("1 >= 2", ast::BinOp::Ge, ">="),
            ("value += 1", ast::BinOp::AssignAdd, "+="),
            ("value -= 1", ast::BinOp::AssignSub, "-="),
            ("value *= 1", ast::BinOp::AssignMul, "*="),
            ("value /= 1", ast::BinOp::AssignDiv, "/="),
            ("value |= 1", ast::BinOp::AssignBitOr, "|="),
            ("value &= 1", ast::BinOp::AssignBitAnd, "&="),
        ] {
            let message = format!(
                "unsupported compile-time binary operator `{symbol}`; only `&&` and `||` are supported"
            );
            assert!(matches!(
                assert_error(source, source, &message),
                Error::UnsupportedBinaryOperator(actual, _) if actual == op
            ));
        }
    }

    #[test]
    fn rejects_unknown_compile_time_functions_at_callee() {
        for (source, highlighted, name) in [
            ("Other()", "Other", Some("Other")),
            ("Other<Int32>(42)", "Other", Some("Other")),
            (
                r#"moduleExists("Test")"#,
                "moduleExists",
                Some("moduleExists"),
            ),
            (
                r#"object.ModuleExists("Test")"#,
                "object.ModuleExists",
                Some("object.ModuleExists"),
            ),
            ("true()", "true", None),
        ] {
            let message = match name {
                Some(name) => format!(
                    "unknown compile-time function `{name}`; only `ModuleExists` is supported"
                ),
                None => {
                    "unknown compile-time function; only `ModuleExists` is supported".to_owned()
                }
            };
            assert!(matches!(
                assert_error(source, highlighted, &message),
                Error::UnknownFunction(actual, _) if actual.as_deref() == name
            ));
        }
    }

    #[test]
    fn rejects_module_exists_argument_count_at_arguments_or_call() {
        for (source, highlighted, count) in [
            ("ModuleExists()", "ModuleExists()", 0),
            ("true && ModuleExists()", "ModuleExists()", 0),
            (r#"ModuleExists("A", "B")"#, r#""A", "B""#, 2),
            ("ModuleExists(42, true, name)", "42, true, name", 3),
        ] {
            let message = format!(
                "`ModuleExists` requires exactly one argument, got {count}; usage: ModuleExists(\"Module.Name\")"
            );
            assert!(matches!(
                assert_error(source, highlighted, &message),
                Error::ModuleExistsArgumentCount(actual, _) if actual == count
            ));
        }
    }

    #[test]
    fn rejects_module_exists_non_string_literal_at_argument() {
        for (source, highlighted) in [
            ("ModuleExists(42)", "42"),
            ("ModuleExists(true)", "true"),
            ("ModuleExists(name)", "name"),
            (r#"ModuleExists("A" + "B")"#, r#""A" + "B""#),
            ("true && ModuleExists(name)", "name"),
        ] {
            assert!(matches!(
                assert_error(
                    source,
                    highlighted,
                    "`ModuleExists` requires a string literal argument; usage: ModuleExists(\"Module.Name\")"
                ),
                Error::ModuleExistsExpectedStringLiteral(_)
            ));
        }
    }

    #[test]
    fn rejects_module_exists_type_arguments_before_value_arguments() {
        for (source, highlighted) in [
            (r#"ModuleExists<Int32>("A")"#, "Int32"),
            ("ModuleExists<Int32>()", "Int32"),
            ("ModuleExists<Int32>(42)", "Int32"),
            ("ModuleExists<Int32, Bool>(42, true)", "Int32"),
        ] {
            assert!(matches!(
                assert_error(
                    source,
                    highlighted,
                    "`ModuleExists` does not accept type arguments; usage: ModuleExists(\"Module.Name\")"
                ),
                Error::ModuleExistsTypeArguments(_)
            ));
        }
    }

    #[test]
    fn diagnostic_preserves_cte_code_and_error_span() {
        for source in [
            "value",
            "-42",
            "1 + 2",
            "Other()",
            "ModuleExists()",
            r#"ModuleExists("A", "B")"#,
            "ModuleExists(42)",
            r#"ModuleExists<Int32>("A")"#,
        ] {
            let error = Evaluator::new(HashSet::new())
                .eval(&parse(source))
                .err()
                .expect("expected an evaluation error");
            let span = error.span();
            let message = error.to_string();
            let diagnostic = Diagnostic::from(error);
            assert_eq!(diagnostic.code(), "CTE_ERR");
            assert_eq!(diagnostic.span(), span);
            assert_eq!(diagnostic.to_string(), message);
        }
    }
}
