//! A table a unit stores by entry: a part of the unit's own, and each entry an object of the store, read where the table is first asked for ([`Recorded`]), each entry keeping the digest it is stored under ([`Filed`]).
//!
//! **An entry kept is digested once.** A unit compiled over a baseline holds the baseline's entries shared, and an entry carries the digest of the object it is stored as, the one it was restored with or the first it was filed as: so a filing names what it kept by the digest it had and digests what it made alone.

use {
    crate::{Digest, Object},
    curios_archive::Bytes,
    std::{cell::OnceCell, ops::Deref},
};

/// A value beside the digest of the object a store keeps it as, once that is known: restored, it holds the digest it was found by, and made here, the one it is first filed as. Shared by every version that holds it, so a value kept from a baseline is never digested again.
#[derive(Debug, Clone)]
pub struct Filed<T> {
    value: T,
    digest: OnceCell<Digest>,
}

impl<T> Filed<T> {
    /// `value`, made here and filed under no digest yet.
    pub fn new(value: T) -> Self {
        Self {
            value,
            digest: OnceCell::new(),
        }
    }

    /// `value`, restored out of the object named `digest`.
    pub fn stored(value: T, digest: Digest) -> Self {
        Self {
            value,
            digest: OnceCell::from(digest),
        }
    }

    /// The digest this value is stored under: the one it was restored with or first filed under, and otherwise what `digest` gives, kept for every later filing.
    pub fn digest(
        &self,
        digest: impl FnOnce(&T) -> Result<Digest, String>,
    ) -> Result<Digest, String> {
        if let Some(held) = self.digest.get() {
            return Ok(*held);
        }
        let made = digest(&self.value)?;

        Ok(*self.digest.get_or_init(|| made))
    }
}

impl<T> Deref for Filed<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

/// A table that can be restored out of what a unit stores of it: a part of the unit's own, and the object of each entry.
pub trait Records: Sized {
    /// The table `table` and `entries` store. `Err` is bytes that hold no such table, or an object that is no entry of it.
    fn restored(table: &[u8], entries: &[Object]) -> Result<Self, String>;
}

/// A table in hand, or what a unit stores of it until something asks for it: its own part and the object of each entry, kept as what the table is stored as again.
///
/// Never changed once made, as [`curios_archive::Deferred`] is not: a unit compiled over another makes a table of its own, sharing the other's entries.
#[derive(Debug, Clone)]
pub struct Recorded<T> {
    value: OnceCell<T>,
    stored: Option<(Bytes, Vec<Object>)>,
}

impl<T: Default> Default for Recorded<T> {
    fn default() -> Self {
        Self::held(T::default())
    }
}

impl<T> Recorded<T> {
    /// A table in hand: one made in this process.
    pub fn held(value: T) -> Self {
        Self {
            value: OnceCell::from(value),
            stored: None,
        }
    }

    /// A table stored as its own part `table` and the objects of its entries, read when it is first asked for.
    pub fn stored(table: impl Into<Bytes>, entries: Vec<Object>) -> Self {
        Self {
            value: OnceCell::new(),
            stored: Some((table.into(), entries)),
        }
    }

    /// What the table was restored from, where it was: what it is stored as again.
    pub fn as_stored(&self) -> Option<(&Bytes, &[Object])> {
        self.stored
            .as_ref()
            .map(|(table, entries)| (table, entries.as_slice()))
    }

    /// The table, restored now where nothing has asked for it before. `Err` is what it was stored as holding no such table.
    pub fn get(&self) -> Result<&T, String>
    where
        T: Records,
    {
        if let Some(value) = self.value.get() {
            return Ok(value);
        }
        let (table, entries) = self
            .stored
            .as_ref()
            .expect("a recorded table is in hand or stored");
        let value = T::restored(table.as_slice(), entries)?;

        Ok(self.value.get_or_init(|| value))
    }

    /// The table where it is in hand, taken: one made here, or already restored.
    pub fn into_held(self) -> Option<T> {
        self.value.into_inner()
    }
}
