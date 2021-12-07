use std::{fmt, io};

use redscript::bundle::PoolError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] io::Error),
    #[error("formatter error")]
    FormatError(#[from] fmt::Error),
    #[error("decompilation error: {0}")]
    PoolError(#[from] PoolError),
    #[error("multiple errors")]
    DecompileError(String),
}
