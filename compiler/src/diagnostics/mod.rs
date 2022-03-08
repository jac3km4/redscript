use redscript::ast::Seq;

use crate::typechecker::TypedAst;
use crate::unit::Diagnostic;

pub mod unused;

pub trait DiagnosticPass {
    fn diagnose(&self, body: &Seq<TypedAst>) -> Vec<Diagnostic>;
}
