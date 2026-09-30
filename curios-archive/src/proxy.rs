//! Archiving a type through a stand-in rkyv already understands.
//!
//! A field adapter is one idea around rkyv's three-impl shape — `ArchiveWith`, `SerializeWith`, `DeserializeWith`: *this value is not archivable, but it converts to one that is.* A bignum is its little-endian bytes, a hash map a sorted vector of pairs, an interned path the segments the process table holds. [`Proxy`] states just that conversion, and [`Via`] supplies the three impls.

use {
    crate::rkyv::{
        Archive, Archived, Deserialize, Place, Resolver, Serialize,
        rancor::{Fallible, Source},
        with::{ArchiveWith, DeserializeWith, SerializeWith},
    },
    std::{borrow::Borrow, io, marker::PhantomData},
};

/// How a `Value` rkyv cannot archive is represented by an [`Archivable`](Proxy::Archivable) it can.
///
/// The type implementing this is a marker — it holds nothing and is never constructed. It exists to name the conversion, so one `Value` may have several (a `Vec<u8>` archived as bytes or as a length-prefixed run) without either being privileged.
pub trait Proxy<Value> {
    /// The stand-in actually written to the archive.
    type Archivable;

    /// The value's stand-in. Infallible: a value that exists can always be described.
    ///
    /// Returns anything that *borrows as* the stand-in, so an adapter whose archived form already sits inside the value hands back a reference and pays nothing; `README.md` states why a by-value signature would cost.
    fn to_archivable(value: &Value) -> impl Borrow<Self::Archivable>;

    /// The value a stand-in describes, or why it describes none.
    ///
    /// Fallible because the archive is bytes and a stand-in may describe no value. The error is a `String` so an implementor needs no rkyv vocabulary to report one.
    fn from_archivable(archivable: Self::Archivable) -> Result<Value, String>;
}

/// The field adapter a [`Proxy`] induces: write `#[archived_with(Via<MyProxy>)]`, or name it through a type alias where the proxy is an implementation detail.
pub struct Via<P>(PhantomData<P>);

impl<Value, P> ArchiveWith<Value> for Via<P>
where
    P: Proxy<Value>,
    P::Archivable: Archive,
{
    type Archived = Archived<P::Archivable>;
    type Resolver = Resolver<P::Archivable>;

    fn resolve_with(value: &Value, resolver: Self::Resolver, out: Place<Self::Archived>) {
        P::to_archivable(value).borrow().resolve(resolver, out);
    }
}

impl<Value, P, S> SerializeWith<Value, S> for Via<P>
where
    P: Proxy<Value>,
    P::Archivable: Archive + Serialize<S>,
    S: Fallible + ?Sized,
{
    fn serialize_with(value: &Value, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
        P::to_archivable(value).borrow().serialize(serializer)
    }
}

impl<Value, P, D> DeserializeWith<Archived<P::Archivable>, Value, D> for Via<P>
where
    P: Proxy<Value>,
    P::Archivable: Archive,
    Archived<P::Archivable>: Deserialize<P::Archivable, D>,
    D: Fallible + ?Sized,
    D::Error: Source,
{
    fn deserialize_with(
        archived: &Archived<P::Archivable>,
        deserializer: &mut D,
    ) -> Result<Value, D::Error> {
        let archivable = archived.deserialize(deserializer)?;

        P::from_archivable(archivable).map_err(|error| D::Error::new(io::Error::other(error)))
    }
}
