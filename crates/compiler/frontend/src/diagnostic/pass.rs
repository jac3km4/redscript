use crate::{CompileErrorReporter, LoweredFunction};

mod unused_locals;

pub use unused_locals::UnusedLocals;

#[cfg(not(feature = "sync"))]
pub trait DiagnosticPass {
    fn run<'ctx>(&self, func: &LoweredFunction<'ctx>, reporter: &mut CompileErrorReporter<'ctx>);
}

#[cfg(feature = "sync")]
pub trait DiagnosticPass: Sync {
    fn run<'ctx>(&self, func: &LoweredFunction<'ctx>, reporter: &mut CompileErrorReporter<'ctx>);
}
