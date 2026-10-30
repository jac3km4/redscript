//! REDmod source indentation and syntax-only AST printing.
//!
//! [`format_source`] preserves source spelling and trivia, changing only leading
//! indentation. The [`mod@print`] APIs emit native REDmod AST nodes, not original
//! source text. Neither path performs type-aware transpilation or semantic validation.

mod format;
pub mod print;

pub use format::format_source;
pub use print::{PrintError, PrintErrorKind, print_item_decl, print_member_decl, print_module};
