use std::fmt;

#[cfg(not(feature = "sync"))]
use crate::sync::SharedCell;

/// Lazily-computed value with cycle detection.
///
/// In the default (single-threaded) configuration the backing cell is a
/// `RefCell`, so re-entrant evaluation on the same thread fails with
/// [`CycleError`].
///
/// Under `sync` the cell is a `Mutex` plus `Condvar`: a concurrent caller waits
/// for the in-flight evaluation instead of reporting a spurious [`CycleError`].
/// Only re-entrant evaluation on the evaluating thread is treated as a cycle.
/// (A cycle that crosses threads cannot be broken by a single cell and would
/// still deadlock, which matches the lifetime-restricted nature of these lazy
/// values.)
pub struct Lazy<A, F> {
    get: F,
    slot: Slot<A>,
}

impl<A, F> Lazy<A, F> {
    #[inline]
    pub fn new(get: F) -> Self {
        Self {
            get,
            slot: Slot::new(),
        }
    }

    #[inline]
    pub fn try_get(&self) -> Option<A>
    where
        A: Clone,
    {
        self.slot.ready()
    }

    pub fn get<E>(&self, env: E) -> Result<A, CycleError>
    where
        A: Clone,
        F: Fn(E) -> A,
    {
        self.slot.evaluate(|| (self.get)(env))
    }
}

impl<A, E> Lazy<A, Box<dyn Fn(&E) -> A>> {
    pub fn resolved(value: A) -> Self {
        Self {
            get: Box::new(move |_| unreachable!()),
            slot: Slot::resolved(value),
        }
    }
}

#[cfg(not(feature = "sync"))]
impl<A: fmt::Debug, F> fmt::Debug for Lazy<A, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Mirror the `sync` representation: `Some`/`None` for the slot value and
        // `<borrowed>` while another evaluation holds the cell.
        let Ok(value) = self.slot.0.try_borrow() else {
            return f
                .debug_struct("Lazy")
                .field("value", &"<borrowed>")
                .finish();
        };
        f.debug_struct("Lazy")
            .field("value", &value.as_ref())
            .finish()
    }
}

#[cfg(feature = "sync")]
impl<A: fmt::Debug, F> fmt::Debug for Lazy<A, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let state = match self.slot.state.try_lock() {
            Ok(state) => state,
            Err(std::sync::TryLockError::Poisoned(error)) => error.into_inner(),
            Err(std::sync::TryLockError::WouldBlock) => {
                return f
                    .debug_struct("Lazy")
                    .field("value", &"<borrowed>")
                    .finish();
            }
        };
        let value = match &*state {
            SlotState::Ready(value) => Some(value),
            SlotState::Uninit | SlotState::Evaluating(_) => None,
        };
        f.debug_struct("Lazy").field("value", &value).finish()
    }
}

/// A single evaluation slot: the stored value plus enough state to distinguish
/// "another thread is computing" from "this thread is re-entering".
#[cfg(not(feature = "sync"))]
struct Slot<A>(SharedCell<Option<A>>);

#[cfg(not(feature = "sync"))]
impl<A> Slot<A> {
    #[inline]
    fn new() -> Self {
        Self(SharedCell::new(None))
    }

    #[inline]
    fn resolved(value: A) -> Self {
        Self(SharedCell::new(Some(value)))
    }
}

#[cfg(not(feature = "sync"))]
impl<A: Clone> Slot<A> {
    #[inline]
    fn ready(&self) -> Option<A> {
        self.0.borrow().clone()
    }

    /// Holds the cell across evaluation, so re-entrant evaluation on the same
    /// thread observes the failed borrow and reports a cycle.
    fn evaluate(&self, f: impl FnOnce() -> A) -> Result<A, CycleError> {
        let mut borrow = self.0.try_borrow_mut().map_err(|_| CycleError)?;
        if let Some(value) = &*borrow {
            return Ok(value.clone());
        }
        let value = f();
        *borrow = Some(value.clone());
        Ok(value)
    }
}

#[cfg(feature = "sync")]
struct Slot<A> {
    state: std::sync::Mutex<SlotState<A>>,
    ready: std::sync::Condvar,
}

#[cfg(feature = "sync")]
enum SlotState<A> {
    Uninit,
    Evaluating(std::thread::ThreadId),
    Ready(A),
}

#[cfg(feature = "sync")]
impl<A> Slot<A> {
    #[inline]
    fn new() -> Self {
        Self {
            state: std::sync::Mutex::new(SlotState::Uninit),
            ready: std::sync::Condvar::new(),
        }
    }

    #[inline]
    fn resolved(value: A) -> Self {
        Self {
            state: std::sync::Mutex::new(SlotState::Ready(value)),
            ready: std::sync::Condvar::new(),
        }
    }
}

#[cfg(feature = "sync")]
impl<A: Clone> Slot<A> {
    #[inline]
    fn ready(&self) -> Option<A> {
        let state = match self.state.try_lock() {
            Ok(state) => state,
            Err(std::sync::TryLockError::Poisoned(error)) => error.into_inner(),
            Err(std::sync::TryLockError::WouldBlock) => return None,
        };
        match &*state {
            SlotState::Ready(value) => Some(value.clone()),
            SlotState::Uninit | SlotState::Evaluating(_) => None,
        }
    }

    fn evaluate(&self, f: impl FnOnce() -> A) -> Result<A, CycleError> {
        let current = std::thread::current().id();
        {
            let mut state = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            loop {
                match &*state {
                    SlotState::Ready(value) => return Ok(value.clone()),
                    SlotState::Evaluating(owner) if *owner == current => return Err(CycleError),
                    // Another thread owns the evaluation: wait for it to finish
                    // rather than misreporting a cycle.
                    SlotState::Evaluating(_) => {
                        state = self
                            .ready
                            .wait(state)
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                    }
                    SlotState::Uninit => {
                        *state = SlotState::Evaluating(current);
                        break;
                    }
                }
            }
        }
        // Evaluate without holding the lock so `f` may force other lazy values,
        // and reset the slot if `f` panics so waiters do not hang forever.
        let guard = ResetGuard {
            slot: self,
            armed: true,
        };
        let value = f();
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *state = SlotState::Ready(value.clone());
        self.ready.notify_all();
        guard.disarm();
        Ok(value)
    }
}

#[cfg(feature = "sync")]
struct ResetGuard<'a, A> {
    slot: &'a Slot<A>,
    armed: bool,
}

#[cfg(feature = "sync")]
impl<A> ResetGuard<'_, A> {
    fn disarm(mut self) {
        self.armed = false;
    }
}

#[cfg(feature = "sync")]
impl<A> Drop for ResetGuard<'_, A> {
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        let mut state = self
            .slot
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if matches!(*state, SlotState::Evaluating(_)) {
            *state = SlotState::Uninit;
        }
        self.slot.ready.notify_all();
    }
}

#[derive(Debug)]
pub struct CycleError;

#[cfg(all(test, feature = "sync"))]
mod sync_tests {
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use std::sync::{Arc, Mutex};
    use std::time::Duration;

    use super::*;

    type BoxedLazy = Arc<Lazy<u32, Box<dyn Fn(()) -> u32 + Send + Sync>>>;

    #[test]
    fn concurrent_get_waits_instead_of_reporting_a_cycle() {
        let started = Arc::new(AtomicBool::new(false));
        let released = Arc::new(AtomicBool::new(false));
        let calls = Arc::new(AtomicUsize::new(0));

        let lazy = Arc::new(Lazy::new({
            let started = Arc::clone(&started);
            let released = Arc::clone(&released);
            let calls = Arc::clone(&calls);
            move |_: ()| {
                calls.fetch_add(1, Ordering::SeqCst);
                started.store(true, Ordering::SeqCst);
                while !released.load(Ordering::SeqCst) {
                    std::thread::sleep(Duration::from_millis(1));
                }
                7u32
            }
        }));

        let first = {
            let lazy = Arc::clone(&lazy);
            std::thread::spawn(move || lazy.get(()).unwrap())
        };
        while !started.load(Ordering::SeqCst) {
            std::thread::sleep(Duration::from_millis(1));
        }
        let second = {
            let lazy = Arc::clone(&lazy);
            std::thread::spawn(move || lazy.get(()).unwrap())
        };
        // Let the second thread reach the blocked wait before releasing.
        std::thread::sleep(Duration::from_millis(20));
        released.store(true, Ordering::SeqCst);

        assert_eq!(first.join().unwrap(), 7);
        assert_eq!(second.join().unwrap(), 7);
        assert_eq!(calls.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn reentrant_get_on_the_evaluating_thread_reports_a_cycle() {
        let holder: Arc<Mutex<Option<BoxedLazy>>> = Arc::new(Mutex::new(None));
        let saw_cycle = Arc::new(AtomicBool::new(false));

        let lazy: BoxedLazy = Arc::new(Lazy::new(Box::new({
            let holder = Arc::clone(&holder);
            let saw_cycle = Arc::clone(&saw_cycle);
            move |_: ()| {
                let inner = holder.lock().unwrap().clone().unwrap();
                saw_cycle.store(inner.get(()).is_err(), Ordering::SeqCst);
                1u32
            }
        })));
        *holder.lock().unwrap() = Some(Arc::clone(&lazy));

        assert_eq!(lazy.get(()).unwrap(), 1);
        assert!(saw_cycle.load(Ordering::SeqCst));
    }
}
