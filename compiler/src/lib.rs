#![feature(macro_metavar_expr)]
#![macro_use]
extern crate paste;
use simple_interner::Interner;

pub mod autobox;
pub mod codegen;
pub mod comb;
pub mod compiler;
pub mod error;
pub mod lexer;
#[allow(clippy::redundant_closure_call)]
pub mod parser;
pub mod parser2;
mod scoped_map;
pub mod source_map;
pub mod type_repo;
pub mod typer;
pub mod validators;
pub mod visit;

pub type StringInterner = Interner<str, ahash::RandomState>;
type IndexMap<K, V> = indexmap::IndexMap<K, V, ahash::RandomState>;
