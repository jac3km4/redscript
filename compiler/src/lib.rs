#![feature(stmt_expr_attributes)]

pub mod assembler;
pub mod cte;
#[allow(clippy::redundant_closure_call)]
pub mod parser;
pub mod scope;
pub mod source_map;
pub mod sugar;
pub mod symbol;
pub mod transform;
pub mod typechecker;
pub mod unit;
