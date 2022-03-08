use redscript::ast::{Seq, Span};
use redscript::definition::FunctionFlags;

use crate::typechecker::TypedAst;
use crate::unit::Diagnostic;

pub mod return_val;
pub mod unused;

pub trait DiagnosticPass {
    fn diagnose(&self, body: &Seq<TypedAst>, metadata: &FunctionMetadata) -> Vec<Diagnostic>;
}

pub struct FunctionMetadata {
    flags: FunctionFlags,
    was_callback: bool,
    span: Span,
}

impl FunctionMetadata {
    pub fn new(flags: FunctionFlags, was_callback: bool, span: Span) -> Self {
        Self {
            flags,
            was_callback,
            span,
        }
    }
}
