use thiserror::Error;

pub type Result<A, E = Error> = std::result::Result<A, E>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Detached switch bodies are unsupported")]
    DetachedSwitchBodies,
    #[error("Unexpected end of code")]
    UnexpectedEndOfCode,
    #[error(
        "Cannot preserve initializer for const local '{0}': initialization requiring hoisting or expanded writes is unsupported; omit function bodies to decompile declarations only"
    )]
    UnsupportedConstInitializer(String),
    #[error("Missing {1} in the pool at index {0}")]
    MissingPoolItem(u32, &'static str),
    #[error("Missing parameter end instruction")]
    MissingParamEnd,
    #[error("Invalid cast with void return type")]
    VoidCast,
    #[error("Invalid number of arguments for an operator")]
    InvalidOperatorArgs,
    #[error("Invalid enum storage size or values")]
    InvalidEnumStorage,
    #[error("Read error: {0}")]
    ReadFailure(redscript_io::byte::Error),
}

impl From<redscript_io::byte::Error> for Error {
    fn from(err: redscript_io::byte::Error) -> Self {
        Error::ReadFailure(err)
    }
}
