use std::io;

use redscript::ast::Span;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("compilation errors")]
    CompileErrors(Vec<(&'static str, Span)>),
    #[error("I/O error: {0}")]
    IoError(#[from] io::Error),
}
