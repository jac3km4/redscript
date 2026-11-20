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

/// Runs parallel work on a pool whose worker stacks match the project's 8 MiB
/// stack budget (see the `/STACK:8388608` flag in `.cargo/config.toml`).
///
/// Lowering and validation recurse over nested source and IR, so rayon's 2 MiB
/// default can overflow where the sequential path — which runs on the caller's
/// 8 MiB main stack — would not. Set `REDSCRIPT_STACK_SIZE` (bytes) to raise it
/// for an unusually deep corpus.
#[cfg(feature = "parallel")]
fn pool() -> &'static rayon::ThreadPool {
    use std::env;
    use std::sync::OnceLock;

    static POOL: OnceLock<rayon::ThreadPool> = OnceLock::new();
    POOL.get_or_init(|| {
        let stack_size = env::var("REDSCRIPT_STACK_SIZE")
            .ok()
            .and_then(|value| value.parse().ok())
            .unwrap_or(8 * 1024 * 1024);
        rayon::ThreadPoolBuilder::new()
            .stack_size(stack_size)
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
