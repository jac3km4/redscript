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
    pool().install(|| items.into_par_iter().map(f).collect())
}

/// Dedicated worker pool with generous stacks: lowering and validation recurse
/// over nested source and IR, so the default 2 MiB worker stack can overflow
/// where the sequential path (running on the caller's stack) would not.
#[cfg(feature = "parallel")]
fn pool() -> &'static rayon::ThreadPool {
    use std::sync::OnceLock;

    const STACK_SIZE: usize = 64 * 1024 * 1024;

    static POOL: OnceLock<rayon::ThreadPool> = OnceLock::new();
    POOL.get_or_init(|| {
        rayon::ThreadPoolBuilder::new()
            .stack_size(STACK_SIZE)
            .build()
            .expect("failed to build the parallel compiler thread pool")
    })
}

#[cfg(not(feature = "parallel"))]
pub(crate) fn map_ordered<T, R, F>(items: Vec<T>, f: F) -> Vec<R>
where
    F: Fn(T) -> R,
{
    items.into_iter().map(f).collect()
}
