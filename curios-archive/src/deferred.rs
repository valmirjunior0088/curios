//! A value read out of its stored bytes when it is first asked for.
//!
//! **What only some readers want is restored by the reader that wants it.** A value restored with everything around it is paid for by every reader of what is around it. One held as the bytes it was stored as costs a reader that never asks nothing but the bytes, and the first that asks one decoding.
//!
//! The bytes are the value's own archive, written apart from the archive of whatever holds the [`Deferred`]: the holder skips the field (`#[archived_with(curios_archive::Skip)]`), stores [`Deferred::bytes`] where it stores itself, and hands them back through [`Deferred::stored`] where it is restored. So nothing of the value is shared by pointer with the holder's archive, and a value that shares nodes with it does not belong in one.
//!
//! **Many values stored apart share the buffer they were read from.** A holder of one deferred value for each of its declarations stores their bytes one after another ([`joined`]) and hands each back as a run of the one buffer ([`parted`]), so opening it copies nothing and decodes nothing, and a holder made from another's values stores the bytes of each one it kept as they are.

use std::{borrow::Cow, cell::OnceCell, ops::Range, rc::Rc};

#[cfg(feature = "enabled")]
use rkyv::{
    Archive, Deserialize, Serialize,
    api::high::{HighDeserializer, HighSerializer, HighValidator},
    bytecheck::CheckBytes,
    rancor::Error,
    ser::allocator::ArenaHandle,
    util::AlignedVec,
};

/// The bytes a value was stored as, held as what they were read from allows: a constant of the executable, or a run of a buffer read for it, which every value read from that buffer shares.
#[derive(Debug, Clone)]
pub enum Bytes {
    Constant(&'static [u8]),
    Run {
        buffer: Rc<Vec<u8>>,
        run: Range<usize>,
    },
}

impl Bytes {
    pub fn as_slice(&self) -> &[u8] {
        match self {
            Bytes::Constant(bytes) => bytes,
            Bytes::Run { buffer, run } => &buffer[run.clone()],
        }
    }

    /// The run `run` of these bytes, in the buffer they are in. `None` where they are shorter than it.
    pub fn run(&self, run: Range<usize>) -> Option<Bytes> {
        match self {
            Bytes::Constant(bytes) => bytes.get(run).map(Bytes::Constant),
            Bytes::Run { buffer, run: held } => {
                let start = held.start.checked_add(run.start)?;
                let end = held.start.checked_add(run.end)?;

                (start <= end && end <= held.end).then(|| Bytes::Run {
                    buffer: Rc::clone(buffer),
                    run: start..end,
                })
            }
        }
    }
}

impl From<&'static [u8]> for Bytes {
    fn from(bytes: &'static [u8]) -> Self {
        Bytes::Constant(bytes)
    }
}

impl From<Vec<u8>> for Bytes {
    fn from(bytes: Vec<u8>) -> Self {
        let run = 0..bytes.len();

        Bytes::Run {
            buffer: Rc::new(bytes),
            run,
        }
    }
}

impl From<Cow<'static, [u8]>> for Bytes {
    fn from(bytes: Cow<'static, [u8]>) -> Self {
        match bytes {
            Cow::Borrowed(bytes) => bytes.into(),
            Cow::Owned(bytes) => bytes.into(),
        }
    }
}

/// The bytes of several values, one after another behind how many they are and how long each is: what [`parted`] reads back.
///
/// A count and every length ahead of the first value, since an archive is read from its end and so does not know its own extent, and since finding one value must read none of the others.
pub fn joined<B: AsRef<[u8]>>(parts: &[B]) -> Vec<u8> {
    let held = parts.iter().map(|part| part.as_ref().len()).sum::<usize>();
    let mut bytes = Vec::with_capacity(8 + 8 * parts.len() + held);
    bytes.extend_from_slice(&(parts.len() as u64).to_le_bytes());
    for part in parts {
        bytes.extend_from_slice(&(part.as_ref().len() as u64).to_le_bytes());
    }
    for part in parts {
        bytes.extend_from_slice(part.as_ref());
    }

    bytes
}

/// The values [`joined`] laid out in `stored`, each a run of its buffer: nothing is copied and nothing decoded. `None` for bytes that are no such layout.
pub fn parted(stored: &Bytes) -> Option<Vec<Bytes>> {
    let bytes = stored.as_slice();
    let word = |at: usize| {
        let word = bytes.get(at..at.checked_add(8)?)?;

        usize::try_from(u64::from_le_bytes(word.try_into().ok()?)).ok()
    };
    let count = word(0)?;
    let mut next = count.checked_mul(8)?.checked_add(8)?;
    // Refused before anything is reserved for it: a count is read from the bytes, and a layout holds a length for each part it claims.
    if next > bytes.len() {
        return None;
    }

    let mut parts = Vec::with_capacity(count);
    for part in 0..count {
        let length = word(8 + 8 * part)?;
        let end = next.checked_add(length)?;
        parts.push(stored.run(next..end)?);
        next = end;
    }

    (next == bytes.len()).then_some(parts)
}

/// A value in hand, or the bytes it was stored as until something asks for it.
///
/// Never changed once made: what was restored is what would be stored again, which is why the bytes a value was read from are kept and handed back as its bytes.
#[derive(Debug, Clone)]
pub struct Deferred<T> {
    value: OnceCell<T>,
    /// The bytes the value was restored from, where it was.
    stored: Option<Bytes>,
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

    /// The value where it is in hand, taken: one made here, or already read out of its bytes.
    pub fn into_held(self) -> Option<T> {
        self.value.into_inner()
    }

    /// The bytes the value was restored from, where it was: what it is stored as again, by whoever holds it or keeps it of another.
    pub fn as_stored(&self) -> Option<&Bytes> {
        self.stored.as_ref()
    }
}

#[cfg(feature = "enabled")]
impl<T> Deferred<T> {
    /// A value stored as `bytes`, which are read when it is first asked for.
    pub fn stored(bytes: impl Into<Bytes>) -> Self {
        Self {
            value: OnceCell::new(),
            stored: Some(bytes.into()),
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

        let stored = self
            .stored
            .as_ref()
            .expect("a deferred value is in hand or stored");
        let value = crate::from_bytes::<T>(stored.as_slice())?;

        Ok(self.value.get_or_init(|| value))
    }

    /// The bytes the value is stored as: the ones it was restored from, and otherwise its archive, made now.
    pub fn bytes(&self) -> Result<Cow<'_, [u8]>, String>
    where
        T: for<'a> Serialize<HighSerializer<AlignedVec, ArenaHandle<'a>, Error>>,
    {
        if let Some(stored) = &self.stored {
            return Ok(Cow::Borrowed(stored.as_slice()));
        }

        let value = self
            .value
            .get()
            .expect("a deferred value is in hand or stored");

        crate::to_bytes(value).map(|bytes| Cow::Owned(bytes.to_vec()))
    }
}

#[cfg(test)]
mod tests;
