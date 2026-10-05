//! How the arbitrary-precision integers are archived: as their little-endian bytes. And how a binary's buffer is: as bytes of its own.
//!
//! Bytes rather than rkyv's own integer support because these are unbounded — there is no fixed width to archive them at. The signed and unsigned twins differ only in which pair of `num-bigint` conversions they name.

use {
    curios_archive::{Proxy, Via},
    num_bigint::{BigInt, BigUint},
    std::{borrow::Borrow, sync::Arc},
};

/// A [`Binary`](crate::Binary)'s buffer, archived as bytes of its own.
///
/// **By value, because which values share a buffer is how they were come by, not what they are.** A shared pointer is written once however many values hold it, so a literal a unit lowers, elaborates and erases is written once, and twice where an item holding it was reused from an earlier compilation: two archives of equal values, differing by the literal's length. The cost is a copy per holder, and a window writes the whole buffer it reads from.
pub struct OwnedBuffer;

impl Proxy<Arc<[u8]>> for OwnedBuffer {
    type Archivable = Vec<u8>;

    fn to_archivable(value: &Arc<[u8]>) -> impl Borrow<Vec<u8>> {
        value.to_vec()
    }

    // Infallible: every byte sequence is a buffer.
    fn from_archivable(bytes: Vec<u8>) -> Result<Arc<[u8]>, String> {
        Ok(bytes.into())
    }
}

pub type BufferBytes = Via<OwnedBuffer>;

/// Declares one proxy archiving `$int` as the little-endian byte vector `$to` produces and `$from` reads back, plus the adapter alias fields name.
macro_rules! big_bytes {
    ($proxy:ident, $adapter:ident, $int:ident, $to:ident, $from:ident) => {
        pub struct $proxy;

        impl Proxy<$int> for $proxy {
            type Archivable = Vec<u8>;

            fn to_archivable(value: &$int) -> impl Borrow<Vec<u8>> {
                value.$to()
            }

            // Infallible: every byte sequence denotes some integer, including the empty one.
            fn from_archivable(bytes: Vec<u8>) -> Result<$int, String> {
                Ok($int::$from(&bytes))
            }
        }

        pub type $adapter = Via<$proxy>;
    };
}

big_bytes!(
    NaturalBytes,
    BigUintBytes,
    BigUint,
    to_bytes_le,
    from_bytes_le
);
big_bytes!(
    IntegerBytes,
    BigIntBytes,
    BigInt,
    to_signed_bytes_le,
    from_signed_bytes_le
);
