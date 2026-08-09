//! rkyv adapters for validated host-import namespaces.

use curios_archive::rkyv::{
    Archive, Archived, Deserialize, Place, Resolver, Serialize,
    rancor::{Fallible, Source},
    with::{ArchiveWith, DeserializeWith, SerializeWith},
};

pub struct Namespace;

fn code(namespace: &str) -> Option<u8> {
    match namespace {
        "sys" => Some(0),
        "ffi" => Some(1),
        _ => None,
    }
}

impl ArchiveWith<&'static str> for Namespace {
    type Archived = Archived<u8>;
    type Resolver = Resolver<u8>;

    fn resolve_with(value: &&'static str, resolver: Self::Resolver, out: Place<Self::Archived>) {
        code(value)
            .expect("foreign namespace is validated at construction")
            .resolve(resolver, out);
    }
}

impl<S> SerializeWith<&'static str, S> for Namespace
where
    S: Fallible + ?Sized,
    u8: Serialize<S>,
{
    fn serialize_with(
        value: &&'static str,
        serializer: &mut S,
    ) -> Result<Self::Resolver, S::Error> {
        code(value)
            .expect("foreign namespace is validated at construction")
            .serialize(serializer)
    }
}

impl<D> DeserializeWith<Archived<u8>, &'static str, D> for Namespace
where
    D: Fallible + ?Sized,
    D::Error: Source,
    Archived<u8>: Deserialize<u8, D>,
{
    fn deserialize_with(
        value: &Archived<u8>,
        deserializer: &mut D,
    ) -> Result<&'static str, D::Error> {
        match value.deserialize(deserializer)? {
            0 => Ok("sys"),
            1 => Ok("ffi"),
            invalid => Err(D::Error::new(std::io::Error::other(format!(
                "invalid foreign namespace code {invalid}"
            )))),
        }
    }
}
