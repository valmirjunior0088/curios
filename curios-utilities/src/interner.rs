//! One process-wide table per interned kind: a value is allocated once, never freed, and handed out as a `&'static` reference, so equal values share one address and a copy of the reference is the identity.
//!
//! **Never freed, deliberately.** An interned identity is `Copy`, so nothing can tell when its last copy is gone; the tables are bounded instead by the distinct values a process ever names — module paths and binder hints — which is small beside the texts and terms it holds. A text is not interned: sources are reference-counted, because a language server loads a new one on every edit.

use std::{
    collections::HashMap,
    hash::Hash,
    sync::{Mutex, PoisonError},
};

/// A table of interned values, keyed by the borrowed form a lookup is asked with — a path's segment slice, a spelling's `str` — so asking for a value already interned allocates nothing.
pub(crate) type Table<K, T> = Mutex<HashMap<&'static K, &'static T>>;

/// The one allocation of the value `key` borrows as, built by `own` and adopted if it is new.
///
/// `key_of` must borrow an adopted value as the key it was asked by, or the table would file it under another; both callers pass the value's own view of itself.
///
/// A poisoned lock is taken as it stands: a table is only ever added to, and an insert that panicked either landed whole or not at all, so what a poisoned lock guards is still a table of distinct values.
pub(crate) fn intern<K: ?Sized + Eq + Hash, T>(
    table: &Table<K, T>,
    key: &K,
    own: impl FnOnce() -> T,
    key_of: impl FnOnce(&'static T) -> &'static K,
) -> &'static T {
    let mut entries = table.lock().unwrap_or_else(PoisonError::into_inner);
    if let Some(&shared) = entries.get(key) {
        return shared;
    }
    let shared: &'static T = Box::leak(Box::new(own()));
    entries.insert(key_of(shared), shared);
    shared
}
