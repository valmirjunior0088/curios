//! One archive read a part at a time, through one pool.
//!
//! **A part is restored when it is asked for, and what two parts share is restored once.** An archive stores a shared node once and points at it from everywhere it stands, so restoring a part restores every node it reaches; restored through one pool kept for as long as the archive is read, a node two parts reach is one allocation, as it is where the archive is restored whole.
//!
//! **Nothing is checked that is not read.** A part is found by its position in the bytes, which whoever wrote the archive took from a checked view of them ([`position_of`]) and stored beside it. Reading a part checks it there, with what it reaches, and no other part: an archive read this way is never validated whole.

use {
    crate::Bytes,
    rkyv::{
        Archive, Deserialize, Portable,
        api::high::{HighDeserializer, HighValidator},
        bytecheck::CheckBytes,
        de::Pool,
        rancor::Error,
    },
    std::cell::RefCell,
};

/// Where `part` lies in `bytes`, the archive it was borrowed out of: what [`Pooled::restore`] finds it by. `None` for a part that is not in them.
pub fn position_of<A>(bytes: &[u8], part: &A) -> Option<usize> {
    let position = (std::ptr::from_ref(part) as usize).checked_sub(bytes.as_ptr() as usize)?;

    (position.checked_add(size_of::<A>())? <= bytes.len()).then_some(position)
}

/// An archive's bytes, and the pool its parts are restored through.
pub struct Pooled {
    bytes: Bytes,
    pool: RefCell<Pool>,
}

impl Pooled {
    /// `bytes` as an archive to restore parts of.
    pub fn over(bytes: impl Into<Bytes>) -> Self {
        Self {
            bytes: bytes.into(),
            pool: RefCell::new(Pool::new()),
        }
    }

    /// The `T` archived at `position`, restored: checked where it lies, with what it reaches, and sharing every node an earlier restoration through this pool reached too. `Err` is bytes that hold no such value there.
    pub fn restore<T>(&self, position: usize) -> Result<T, String>
    where
        T: Archive,
        T::Archived: Portable
            + for<'a> CheckBytes<HighValidator<'a, Error>>
            + Deserialize<T, HighDeserializer<Error>>,
    {
        let archived =
            rkyv::api::high::access_pos::<T::Archived, Error>(self.bytes.as_slice(), position)
                .map_err(|error| error.to_string())?;

        rkyv::api::deserialize_using::<T, Pool, Error>(archived, &mut self.pool.borrow_mut())
            .map_err(|error| error.to_string())
    }
}
