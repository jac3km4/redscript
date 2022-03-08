use redscript::ast::{Seq, Span};
use redscript::definition::FunctionFlags;

use crate::typechecker::TypedAst;
use crate::unit::Diagnostic;

pub mod return_val;
pub mod unused;

pub trait DiagnosticPass {
    fn diagnose(&self, body: &Seq<TypedAst>, flags: FunctionFlags, fun_span: Span) -> Vec<Diagnostic>;
}
