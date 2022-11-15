pub mod ast;
pub mod bundle;
pub mod bytecode;
pub mod decode;
pub mod definition;
pub mod encode;
pub mod io;

#[cfg(not(feature = "arc"))]
pub type Str = flexstr::LocalStr;
#[cfg(feature = "arc")]
pub type Str = flexstr::SharedStr;

/// Type for small string byte-buffers, optimized to match the size of flexstr.
pub type StrBuf = smallvec::SmallVec<[u8; 22]>;

#[cfg(not(feature = "arc"))]
pub use flexstr::local_fmt as str_fmt;
#[cfg(feature = "arc")]
pub use flexstr::shared_fmt as str_fmt;
