//! Synchronization primitives selected by the additive `sync` feature.
//!
//! With `sync` disabled (the default) these are plain single-threaded
//! primitives: `Rc`, `RefCell` and `Cell`. Enabling `sync` swaps them for
//! thread-safe counterparts (`Arc`, `Mutex`, atomics) so the public analysis
//! types become `Send + Sync`.
//!
//! The default configuration must stay behaviorally identical, including
//! `Debug` output, so the single-threaded variants are exact type aliases to
//! the original concrete types rather than newtypes.

#[cfg(feature = "sync")]
use std::fmt;

#[cfg(not(feature = "sync"))]
pub type Shared<T> = std::rc::Rc<T>;
#[cfg(feature = "sync")]
pub type Shared<T> = std::sync::Arc<T>;

#[cfg(not(feature = "sync"))]
pub(crate) type SharedWeak<T> = std::rc::Weak<T>;
#[cfg(feature = "sync")]
pub(crate) type SharedWeak<T> = std::sync::Weak<T>;

/// A boxed error trait object that additionally requires `Send + Sync` under
/// `sync`. Used by [`crate::Diagnostic::Custom`] and backend assembly errors.
#[cfg(not(feature = "sync"))]
pub type BoxedError<'ctx> = Box<dyn std::error::Error + 'ctx>;
#[cfg(feature = "sync")]
pub type BoxedError<'ctx> = Box<dyn std::error::Error + Send + Sync + 'ctx>;

// ---------------------------------------------------------------------------
// SharedCell
// ---------------------------------------------------------------------------

#[cfg(not(feature = "sync"))]
pub(crate) type SharedCell<T> = std::cell::RefCell<T>;

#[cfg(feature = "sync")]
pub(crate) struct SharedCell<T>(std::sync::Mutex<T>);

#[cfg(feature = "sync")]
impl<T> SharedCell<T> {
    #[inline]
    pub(crate) fn new(value: T) -> Self {
        Self(std::sync::Mutex::new(value))
    }

    #[inline]
    pub(crate) fn borrow(&self) -> Ref<'_, T> {
        Ref(self
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner))
    }

    #[inline]
    pub(crate) fn borrow_mut(&self) -> RefMut<'_, T> {
        RefMut(
            self.0
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner),
        )
    }

    #[inline]
    pub(crate) fn try_borrow(&self) -> Result<Ref<'_, T>, BorrowError> {
        self.0.try_lock().map(Ref).map_err(|_| BorrowError)
    }
}

#[cfg(feature = "sync")]
impl<T: fmt::Debug> fmt::Debug for SharedCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Mirror `RefCell`'s `Debug` output exactly, without ever blocking.
        if let Ok(value) = self.try_borrow() {
            f.debug_struct("RefCell").field("value", &value).finish()
        } else {
            struct BorrowedPlaceholder;
            impl fmt::Debug for BorrowedPlaceholder {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str("<borrowed>")
                }
            }
            f.debug_struct("RefCell")
                .field("value", &BorrowedPlaceholder)
                .finish()
        }
    }
}

#[cfg(feature = "sync")]
pub(crate) struct Ref<'a, T>(std::sync::MutexGuard<'a, T>);

#[cfg(feature = "sync")]
impl<T> std::ops::Deref for Ref<'_, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.0
    }
}

#[cfg(feature = "sync")]
impl<T: fmt::Debug> fmt::Debug for Ref<'_, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

#[cfg(feature = "sync")]
pub(crate) struct RefMut<'a, T>(std::sync::MutexGuard<'a, T>);

#[cfg(feature = "sync")]
impl<T> std::ops::Deref for RefMut<'_, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.0
    }
}

#[cfg(feature = "sync")]
impl<T> std::ops::DerefMut for RefMut<'_, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

#[cfg(feature = "sync")]
impl<T: fmt::Debug> fmt::Debug for RefMut<'_, T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

#[cfg(feature = "sync")]
#[derive(Debug)]
pub(crate) struct BorrowError;

// ---------------------------------------------------------------------------
// Atomics
// ---------------------------------------------------------------------------

/// Defines a shared counter: a plain `Cell` by default, an atomic under `sync`.
///
/// Both configurations expose `new`, `get`, and `set`, and the atomic `Debug`
/// mirrors `Cell`'s output so diagnostics render identically.
macro_rules! shared_atomic {
    ($name:ident, $atomic:ident, $ty:ty) => {
        #[cfg(not(feature = "sync"))]
        pub(crate) type $name = std::cell::Cell<$ty>;

        #[cfg(feature = "sync")]
        pub(crate) struct $name(std::sync::atomic::$atomic);

        #[cfg(feature = "sync")]
        impl $name {
            #[inline]
            pub(crate) fn new(value: $ty) -> Self {
                Self(std::sync::atomic::$atomic::new(value))
            }

            #[inline]
            pub(crate) fn get(&self) -> $ty {
                self.0.load(std::sync::atomic::Ordering::Relaxed)
            }

            #[inline]
            pub(crate) fn set(&self, value: $ty) {
                self.0.store(value, std::sync::atomic::Ordering::Relaxed);
            }
        }

        #[cfg(feature = "sync")]
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                // Mirror `Cell`'s `Debug` output.
                f.debug_struct("Cell").field("value", &self.get()).finish()
            }
        }
    };
}

shared_atomic!(SharedU16, AtomicU16, u16);
shared_atomic!(SharedU32, AtomicU32, u32);
