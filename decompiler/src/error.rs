use std::{fmt, io};

use redscript::bundle::PoolError;
use redscript::bytecode::CursorError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] io::Error),
    #[error("formatter error")]
    FormatError(#[from] fmt::Error),
    #[error("pool error: {0}")]
    PoolError(#[from] PoolError),
    #[error("decompile error: {0}")]
    DecompileError(String),
    #[error("code cursor error: {0}")]
    CursorError(#[from] CursorError),
}
