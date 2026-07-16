//! rkyv adapters for arbitrary-precision integers.

use {
    num_bigint::{BigInt, BigUint},
    rkyv::{
        Archive, Archived, Deserialize, Place, Resolver, Serialize,
        rancor::Fallible,
        with::{ArchiveWith, DeserializeWith, SerializeWith},
    },
};

pub struct BigUintBytes;

impl ArchiveWith<BigUint> for BigUintBytes {
    type Archived = Archived<Vec<u8>>;
    type Resolver = Resolver<Vec<u8>>;

    fn resolve_with(value: &BigUint, resolver: Self::Resolver, out: Place<Self::Archived>) {
        value.to_bytes_le().resolve(resolver, out);
    }
}

impl<S> SerializeWith<BigUint, S> for BigUintBytes
where
    S: Fallible + ?Sized,
    Vec<u8>: Serialize<S>,
{
    fn serialize_with(value: &BigUint, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
        value.to_bytes_le().serialize(serializer)
    }
}

impl<D> DeserializeWith<Archived<Vec<u8>>, BigUint, D> for BigUintBytes
where
    D: Fallible + ?Sized,
    Archived<Vec<u8>>: Deserialize<Vec<u8>, D>,
{
    fn deserialize_with(
        value: &Archived<Vec<u8>>,
        deserializer: &mut D,
    ) -> Result<BigUint, D::Error> {
        Ok(BigUint::from_bytes_le(&value.deserialize(deserializer)?))
    }
}

pub struct BigIntBytes;

impl ArchiveWith<BigInt> for BigIntBytes {
    type Archived = Archived<Vec<u8>>;
    type Resolver = Resolver<Vec<u8>>;

    fn resolve_with(value: &BigInt, resolver: Self::Resolver, out: Place<Self::Archived>) {
        value.to_signed_bytes_le().resolve(resolver, out);
    }
}

impl<S> SerializeWith<BigInt, S> for BigIntBytes
where
    S: Fallible + ?Sized,
    Vec<u8>: Serialize<S>,
{
    fn serialize_with(value: &BigInt, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
        value.to_signed_bytes_le().serialize(serializer)
    }
}

impl<D> DeserializeWith<Archived<Vec<u8>>, BigInt, D> for BigIntBytes
where
    D: Fallible + ?Sized,
    Archived<Vec<u8>>: Deserialize<Vec<u8>, D>,
{
    fn deserialize_with(
        value: &Archived<Vec<u8>>,
        deserializer: &mut D,
    ) -> Result<BigInt, D::Error> {
        Ok(BigInt::from_signed_bytes_le(
            &value.deserialize(deserializer)?,
        ))
    }
}
