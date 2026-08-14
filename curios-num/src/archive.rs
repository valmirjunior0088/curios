//! How the arbitrary-precision integers are archived: as their little-endian bytes.
//!
//! Bytes rather than rkyv's own integer support because these are unbounded — there is no fixed width to archive them at. The signed and unsigned twins differ only in which pair of `num-bigint` conversions they name.

use {
    curios_archive::{Proxy, Via},
    num_bigint::{BigInt, BigUint},
    std::borrow::Borrow,
};

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
