//! A table key told apart by its hash.
//!
//! A hash table compares the key it is probed for with every key it meets under the same control tag, and which keys those are follows the keys the process drew for the table. A derived comparison reads a key's fields in order, so a key whose first field is a graph — a term, a recursive group — equal to the other key's and allocated apart is walked whole before a later field tells the two apart. What a probe reads would then differ from one run of a program to the next, and a count of it would be no count of the program.
//!
//! [`Keyed`] keeps the key's own hash beside it, taken once by a hasher that holds no key of the process's, and compares it before any field. Two keys that differ are told apart there, and a key's fields are compared only against the key it is.

#[cfg(test)]
mod tests;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

/// A key beside its own hash, compared hash first: what a table is keyed by where its key holds a graph beside anything else.
#[derive(Debug, Clone)]
pub struct Keyed<K> {
    hash: u64,
    key: K,
}

impl<K: Hash> Keyed<K> {
    /// `key`, with its hash taken.
    pub fn new(key: K) -> Self {
        // `DefaultHasher::new` is keyed with constants, so the hash is the key's alone.
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);

        Self {
            hash: hasher.finish(),
            key,
        }
    }
}

impl<K> Keyed<K> {
    /// The key itself.
    pub fn key(&self) -> &K {
        &self.key
    }
}

impl<K: PartialEq> PartialEq for Keyed<K> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.key == other.key
    }
}

impl<K: Eq> Eq for Keyed<K> {}

impl<K> Hash for Keyed<K> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}
