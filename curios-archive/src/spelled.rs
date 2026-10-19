//! A value archived as its spelling, read back from the spelling where it lies.
//!
//! **What an interned value needs of an archive.** A path or a spelling is held once per process and named by reference; its archived form is the strings it is spelled by. Read through a [`Proxy`](crate::Proxy), each occurrence is restored as strings of its own and then looked up, an allocation for every segment of every occurrence. Read through [`Spelled`], the value is looked up by the archived strings themselves, so an occurrence of a value the process holds already copies nothing.

use {
    crate::rkyv::{
        Archive, Archived, Place, Resolver, Serialize,
        rancor::{Fallible, Source},
        string::ArchivedString,
        vec::ArchivedVec,
        with::{ArchiveWith, DeserializeWith, SerializeWith},
    },
    std::{borrow::Borrow, io, marker::PhantomData},
};

/// The segments of a spelling, read where they lie: an archive's, or one in hand.
pub trait Segments {
    /// How many segments the spelling has.
    fn count(&self) -> usize;

    /// The segment at `index`, below [`Segments::count`].
    fn segment(&self, index: usize) -> &str;
}

impl Segments for ArchivedVec<ArchivedString> {
    fn count(&self) -> usize {
        self.len()
    }

    fn segment(&self, index: usize) -> &str {
        self[index].as_str()
    }
}

impl Segments for [String] {
    fn count(&self) -> usize {
        self.len()
    }

    fn segment(&self, index: usize) -> &str {
        &self[index]
    }
}

/// How a value spelled by a sequence of strings is archived as them, and read back from them where they lie.
///
/// The type implementing this is a marker, as a [`Proxy`](crate::Proxy)'s is.
pub trait Spelled<Value> {
    /// The strings the value is spelled by. Borrowed where the value holds them, as a stand-in is.
    fn spelling(value: &Value) -> impl Borrow<Vec<String>>;

    /// The value `spelling` spells, read where the spelling lies, or why it spells none.
    fn read(spelling: &dyn Segments) -> Result<Value, String>;
}

/// The field adapter a [`Spelled`] induces: `#[archived_with(BySpelling<MySpelling>)]`, or a type alias for it.
pub struct BySpelling<P>(PhantomData<P>);

impl<Value, P: Spelled<Value>> ArchiveWith<Value> for BySpelling<P> {
    type Archived = Archived<Vec<String>>;
    type Resolver = Resolver<Vec<String>>;

    fn resolve_with(value: &Value, resolver: Self::Resolver, out: Place<Self::Archived>) {
        P::spelling(value).borrow().resolve(resolver, out);
    }
}

impl<Value, P, S> SerializeWith<Value, S> for BySpelling<P>
where
    P: Spelled<Value>,
    Vec<String>: Serialize<S>,
    S: Fallible + ?Sized,
{
    fn serialize_with(value: &Value, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
        P::spelling(value).borrow().serialize(serializer)
    }
}

impl<Value, P, D> DeserializeWith<Archived<Vec<String>>, Value, D> for BySpelling<P>
where
    P: Spelled<Value>,
    D: Fallible + ?Sized,
    D::Error: Source,
{
    fn deserialize_with(archived: &Archived<Vec<String>>, _: &mut D) -> Result<Value, D::Error> {
        P::read(archived).map_err(|error| D::Error::new(io::Error::other(error)))
    }
}

/// How a value spelled by one string is archived as it, and read back from it where it lies.
pub trait SpelledText<Value> {
    /// The string the value is spelled by, borrowed where the value holds it.
    fn text(value: &Value) -> impl Borrow<String>;

    /// The value `text` spells, read where the text lies, or why it spells none.
    fn read(text: &str) -> Result<Value, String>;
}

/// The field adapter a [`SpelledText`] induces.
pub struct ByText<P>(PhantomData<P>);

impl<Value, P: SpelledText<Value>> ArchiveWith<Value> for ByText<P> {
    type Archived = Archived<String>;
    type Resolver = Resolver<String>;

    fn resolve_with(value: &Value, resolver: Self::Resolver, out: Place<Self::Archived>) {
        P::text(value).borrow().resolve(resolver, out);
    }
}

impl<Value, P, S> SerializeWith<Value, S> for ByText<P>
where
    P: SpelledText<Value>,
    String: Serialize<S>,
    S: Fallible + ?Sized,
{
    fn serialize_with(value: &Value, serializer: &mut S) -> Result<Self::Resolver, S::Error> {
        P::text(value).borrow().serialize(serializer)
    }
}

impl<Value, P, D> DeserializeWith<Archived<String>, Value, D> for ByText<P>
where
    P: SpelledText<Value>,
    D: Fallible + ?Sized,
    D::Error: Source,
{
    fn deserialize_with(archived: &Archived<String>, _: &mut D) -> Result<Value, D::Error> {
        P::read(archived.as_str()).map_err(|error| D::Error::new(io::Error::other(error)))
    }
}
