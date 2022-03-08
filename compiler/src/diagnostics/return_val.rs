use redscript::ast::{Expr, Seq, Span};
use redscript::definition::FunctionFlags;

use super::DiagnosticPass;
use crate::typechecker::TypedAst;
use crate::unit::Diagnostic;

pub struct ReturnValueCheck;

impl DiagnosticPass for ReturnValueCheck {
    fn diagnose(&self, body: &Seq<TypedAst>, flags: FunctionFlags, fun_span: Span) -> Vec<Diagnostic> {
        if flags.has_return_value() && !flags.is_callback() && !does_seq_return(body) {
            vec![Diagnostic::MissingReturn(fun_span)]
        } else {
            vec![]
        }
    }
}

fn does_always_return(expr: &Expr<TypedAst>) -> bool {
    match expr {
        Expr::Return(_, _) => true,
        Expr::Seq(seq) => does_seq_return(seq),
        Expr::Switch(_, cases, default, _) => {
            let cases_return = cases.iter().all(|case| does_seq_return(&case.body));
            let default_returns = default.as_ref().map_or(true, does_seq_return);
            cases_return && default_returns
        }
        Expr::If(_, if_, Some(else_), _) => {
            let if_returns = does_seq_return(if_);
            let else_returns = does_seq_return(else_);
            if_returns && else_returns
        }
        _ => false,
    }
}

fn does_seq_return(seq: &Seq<TypedAst>) -> bool {
    seq.exprs.iter().any(does_always_return)
}
