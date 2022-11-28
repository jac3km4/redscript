use simple_interner::Interner;

pub mod autobox;
pub mod codegen;
pub mod compiler;
pub mod error;
#[allow(clippy::redundant_closure_call)]
pub mod parser;
mod scoped_map;
pub mod source_map;
pub mod sugar;
pub mod type_repo;
pub mod typer;
pub mod visit;

pub type StringInterner = Interner<str, ahash::RandomState>;
type IndexMap<K, V> = indexmap::IndexMap<K, V, ahash::RandomState>;
