//! Ordered `map` helper used by the additive `parallel` feature.
//!
//! With `parallel` disabled this is a plain `Iterator::map`, so the result
//! keeps the exact input order in both configurations.

use crate::{CompileErrorReporter, Diagnostic};

/// Runs `check` on every item, then drains each item's diagnostics into
/// `reporter` in input order.
///
/// The item and `check` must be `Send + Sync` in the parallel configuration, so
/// callers fold single-threaded state (such as a per-file semantic policy) into
/// the items before calling this instead of capturing it in `check`.
#[cfg(feature = "parallel")]
pub(crate) fn report_ordered<'ctx, T, F>(
    items: Vec<T>,
    check: F,
    reporter: &mut CompileErrorReporter<'ctx>,
) where
    T: Send,
    F: Fn(T) -> Vec<Diagnostic<'ctx>> + Send + Sync,
{
    for diagnostics in map_ordered(items, check) {
        reporter.report_many(diagnostics);
    }
}

/// Runs `check` on every item, then drains each item's diagnostics into
/// `reporter` in input order.
///
/// With `parallel` disabled this runs sequentially in input order, so callers
/// may capture single-threaded state directly in `check`.
#[cfg(not(feature = "parallel"))]
pub(crate) fn report_ordered<'ctx, T, F>(
    items: Vec<T>,
    check: F,
    reporter: &mut CompileErrorReporter<'ctx>,
) where
    F: Fn(T) -> Vec<Diagnostic<'ctx>>,
{
    for diagnostics in map_ordered(items, check) {
        reporter.report_many(diagnostics);
    }
}

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
