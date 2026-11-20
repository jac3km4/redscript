//! Source capability checks for semantic facts not recoverable from lowered IR.
//!
//! Array-prefix safety is diagnosed once on resolved IR by the shared frontend
//! and promoted to a hard failure in [`crate::AnalyzedSources::transpile_selected`].
//! Only source-only facts that lowering erases are checked here.

use redscript_community_ast::{self as ast, AstVisitor, WithSpan};

use crate::{Result, TranspileDiagnostic, error};

pub fn validate(function: &ast::SourceFunction<'_>) -> Result<()> {
    Guard.visit_function(function)
}

struct Guard;

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
}
