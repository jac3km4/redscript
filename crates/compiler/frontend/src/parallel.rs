//! Ordered `map` helper used by the additive `parallel` feature.
//!
//! With `parallel` disabled this is a plain `Iterator::map`, so the result
//! keeps the exact input order in both configurations.

#[cfg(feature = "parallel")]
pub(crate) fn map_ordered<T, R, F>(items: Vec<T>, f: F) -> Vec<R>
where
    T: Send,
    R: Send,
    F: Fn(T) -> R + Send + Sync,
{
    use rayon::prelude::*;
    // Share the parser's pool so a binary that both parses and lowers never
    // spins up two worker pools with reserved 8 MiB stacks. See
    // `redscript_community_parser::parallel_pool` for the stack-size rationale.
    redscript_community_parser::parallel_pool().install(|| items.into_par_iter().map(f).collect())
}

#[cfg(not(feature = "parallel"))]
pub(crate) fn map_ordered<T, R, F>(items: Vec<T>, f: F) -> Vec<R>
where
    F: Fn(T) -> R,
{
    items.into_iter().map(f).collect()
}
