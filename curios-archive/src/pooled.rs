//! One archive restored a part at a time, through one pool.
//!
//! **A part is restored when it is asked for, and what two parts share is restored once.** An archive stores a shared node once and points at it from everywhere it stands, so restoring a part restores every node it reaches; restored through one pool kept for as long as the archive is read, a node two parts reach is one allocation, as it is where the archive is restored whole.
//!
//! **A part is reached through a view that was checked.** The archive library checks an archive from its root, each pointer against the bytes the ones before it left, and so cannot check one part of an archive whose parts share: a node first written for an earlier part lies behind where a later part's own data begins. So the archive is checked whole, once, where it is opened ([`access`](crate::access)), and a part is a reference borrowed out of that view, which this restores.

use {
    rkyv::{Archive, Deserialize, api::high::HighDeserializer, de::Pool, rancor::Error},
    std::cell::RefCell,
};

/// The pool the parts of one archive are restored through.
#[derive(Default)]
pub struct Pooled {
    pool: RefCell<Pool>,
}

impl Pooled {
    pub fn new() -> Self {
        Self::default()
    }

    /// `archived`, a part of the archive this pool is kept for, restored: every node it reaches that an earlier restoration through this pool reached too is the one that restoration made.
    pub fn restore<T>(&self, archived: &T::Archived) -> Result<T, String>
    where
        T: Archive,
        T::Archived: Deserialize<T, HighDeserializer<Error>>,
    {
        rkyv::api::deserialize_using::<T, Pool, Error>(archived, &mut self.pool.borrow_mut())
            .map_err(|error| error.to_string())
    }
}
