//! A value read out of its stored bytes when it is first asked for.
//!
//! **What only some readers want is restored by the reader that wants it.** A value restored with everything around it is paid for by every reader of what is around it. One held as the bytes it was stored as costs a reader that never asks nothing but the bytes, and the first that asks one decoding.
//!
//! The bytes are the value's own archive, written apart from the archive of whatever holds the [`Deferred`]: the holder skips the field (`#[archived_with(curios_archive::Skip)]`), stores [`Deferred::bytes`] where it stores itself, and hands them back through [`Deferred::stored`] where it is restored. So nothing of the value is shared by pointer with the holder's archive, and a value that shares nodes with it does not belong in one.

use std::{borrow::Cow, cell::OnceCell};

#[cfg(feature = "enabled")]
use rkyv::{
    Archive, Deserialize, Serialize,
    api::high::{HighDeserializer, HighSerializer, HighValidator},
    bytecheck::CheckBytes,
    rancor::Error,
    ser::allocator::ArenaHandle,
    util::AlignedVec,
};

/// A value in hand, or the bytes it was stored as until something asks for it.
///
/// Never changed once made: what was restored is what would be stored again, which is why the bytes a value was read from are kept and handed back as its bytes.
#[derive(Debug, Clone)]
pub struct Deferred<T> {
    value: OnceCell<T>,
    /// The bytes the value was restored from, where it was. Borrowed where they are a constant of the executable, owned where they were read from a file.
    stored: Option<Cow<'static, [u8]>>,
}

impl<T: Default> Default for Deferred<T> {
    fn default() -> Self {
        Self::held(T::default())
    }
}

impl<T> Deferred<T> {
    /// A value in hand: one made in this process.
    pub fn held(value: T) -> Self {
        Self {
            value: OnceCell::from(value),
            stored: None,
        }
    }

    /// The value where it is in hand: made here, or already read out of its bytes.
    pub fn peek(&self) -> Option<&T> {
        self.value.get()
    }
}

#[cfg(feature = "enabled")]
impl<T> Deferred<T> {
    /// A value stored as `bytes`, which are read when it is first asked for.
    pub fn stored(bytes: Cow<'static, [u8]>) -> Self {
        Self {
            value: OnceCell::new(),
            stored: Some(bytes),
        }
    }

    /// The value, read out of its bytes now where nothing has asked before. `Err` is bytes that hold no such value.
    pub fn get(&self) -> Result<&T, String>
    where
        T: Archive,
        T::Archived:
            for<'a> CheckBytes<HighValidator<'a, Error>> + Deserialize<T, HighDeserializer<Error>>,
    {
        if let Some(value) = self.value.get() {
            return Ok(value);
        }

        let bytes = self
            .stored
            .as_deref()
            .expect("a deferred value is in hand or stored");
        let value = crate::from_bytes::<T>(bytes)?;

        Ok(self.value.get_or_init(|| value))
    }

    /// The bytes the value is stored as: the ones it was restored from, and otherwise its archive, made now.
    pub fn bytes(&self) -> Result<Cow<'_, [u8]>, String>
    where
        T: for<'a> Serialize<HighSerializer<AlignedVec, ArenaHandle<'a>, Error>>,
    {
        if let Some(stored) = &self.stored {
            return Ok(Cow::Borrowed(&stored[..]));
        }

        let value = self
            .value
            .get()
            .expect("a deferred value is in hand or stored");

        crate::to_bytes(value).map(|bytes| Cow::Owned(bytes.to_vec()))
    }
}
