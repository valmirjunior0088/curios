//! The four serialization entry points, with rkyv's vocabulary kept inside.
//!
//! rkyv's own signatures are generic over an error type and expose a serializer, a validator and a deserializer alias in their bounds. Every call site in this workspace instantiates all of that identically — `rancor::Error` throughout — so the generality bought nothing and cost each caller a `curios_archive::rkyv::` path in return.
//!
//! These fix the error type, hand back a `String`, and take the bounds on themselves. A caller says [`to_bytes`] and [`from_bytes`]; the crate that owns the dependency says the rest.

use {
    rkyv::{
        Archive, Deserialize, Portable, Serialize,
        api::high::{HighDeserializer, HighSerializer, HighValidator},
        bytecheck::CheckBytes,
        rancor::Error,
        ser::allocator::ArenaHandle,
        util::AlignedVec,
    },
    std::ops::Deref,
};

/// Bytes written by [`to_bytes`], kept in rkyv's aligned buffer rather than copied into a `Vec`.
///
/// The alignment is why this is a type rather than a `Vec<u8>`: rkyv writes its archive expecting a buffer aligned for the largest type it contains, and converting to a `Vec` to hand back would copy every byte of an image that can run to megabytes. Deref and `AsRef` cover what callers do with it — compare, hash, write to a file.
pub struct Serialized(AlignedVec);

impl Deref for Serialized {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.0
    }
}

impl AsRef<[u8]> for Serialized {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

/// Serialize `value` into a fresh aligned buffer.
pub fn to_bytes(
    value: &impl for<'a> Serialize<HighSerializer<AlignedVec, ArenaHandle<'a>, Error>>,
) -> Result<Serialized, String> {
    rkyv::to_bytes::<Error>(value)
        .map(Serialized)
        .map_err(|error| error.to_string())
}

/// Validate `bytes` and deserialize the whole value out of them — the checked path, so malformed bytes are an error rather than undefined behaviour.
pub fn from_bytes<T>(bytes: &[u8]) -> Result<T, String>
where
    T: Archive,
    T::Archived:
        for<'a> CheckBytes<HighValidator<'a, Error>> + Deserialize<T, HighDeserializer<Error>>,
{
    rkyv::from_bytes::<T, Error>(bytes).map_err(|error| error.to_string())
}

/// Validate `bytes` and borrow the archived value in place, deserializing nothing.
///
/// The zero-copy half of [`from_bytes`]: use it when the archived form is enough, and [`deserialize`] later if an owned value turns out to be needed.
pub fn access<T>(bytes: &[u8]) -> Result<&T, String>
where
    T: Portable + for<'a> CheckBytes<HighValidator<'a, Error>>,
{
    rkyv::access::<T, Error>(bytes).map_err(|error| error.to_string())
}

/// Deserialize an owned value from one already validated by [`access`].
pub fn deserialize<T>(value: &impl Deserialize<T, HighDeserializer<Error>>) -> Result<T, String> {
    rkyv::deserialize::<T, Error>(value).map_err(|error| error.to_string())
}
