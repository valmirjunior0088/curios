//! rkyv adapters for arbitrary-precision integers.

use {
    num_bigint::{BigInt, BigUint},
    rkyv::{
        Archive, Archived, Deserialize, Place, Resolver, Serialize,
        rancor::Fallible,
        with::{ArchiveWith, DeserializeWith, SerializeWith},
    },
};

/// Declares one with-adapter archiving `$int` as the little-endian byte vector `$to` produces and `$from` reads back. The twins below differ only in these parameters, so a change to the adapter shape reaches both.
macro_rules! big_bytes {
    ($adapter:ident, $int:ident, $to:ident, $from:ident) => {
        pub struct $adapter;

        impl ArchiveWith<$int> for $adapter {
            type Archived = Archived<Vec<u8>>;
            type Resolver = Resolver<Vec<u8>>;

            fn resolve_with(value: &$int, resolver: Self::Resolver, out: Place<Self::Archived>) {
                value.$to().resolve(resolver, out);
            }
        }

        impl<S> SerializeWith<$int, S> for $adapter
        where
            S: Fallible + ?Sized,
            Vec<u8>: Serialize<S>,
        {
            fn serialize_with(
                value: &$int,
                serializer: &mut S,
            ) -> Result<Self::Resolver, S::Error> {
                value.$to().serialize(serializer)
            }
        }

        impl<D> DeserializeWith<Archived<Vec<u8>>, $int, D> for $adapter
        where
            D: Fallible + ?Sized,
            Archived<Vec<u8>>: Deserialize<Vec<u8>, D>,
        {
            fn deserialize_with(
                value: &Archived<Vec<u8>>,
                deserializer: &mut D,
            ) -> Result<$int, D::Error> {
                Ok($int::$from(&value.deserialize(deserializer)?))
            }
        }
    };
}

big_bytes!(BigUintBytes, BigUint, to_bytes_le, from_bytes_le);
big_bytes!(
    BigIntBytes,
    BigInt,
    to_signed_bytes_le,
    from_signed_bytes_le
);
