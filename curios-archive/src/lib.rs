//! Zero-copy archiving for the Curios workspace: the one crate that names rkyv.
//!
//! Why the dependency is named here and nowhere else, why a type rkyv cannot archive is described by a stand-in written once, and why the entry points fix rkyv's error type are `README.md`'s decisions.
//!
//! **Nothing outside this crate and its `curios-archive-derive` companion spells `rkyv` at all** — not a type, not a trait, not a function, not the helper attribute. That is the whole point, and it is checkable: grep the workspace.
//!
//! # Using it
//!
//! Depend on this crate, gate on your own `archive` feature, and forward it to [`enabled`](#features):
//!
//! ```text
//! [features]
//! archive = ["curios-archive/enabled", "curios-core/archive"]
//! ```
//!
//! Then annotate the types that are archived, and their fields where a field needs saying something:
//!
//! ```text
//! #[curios_archive::archived]
//! pub struct Qualifier {
//!     #[archived_with(Interned)]
//!     segments: Rc<Vec<String>>,
//! }
//! ```
//!
//! [`archived`] takes two keywords, in either order. `recursive` emits the serialize/deserialize/bytecheck bounds a type that reaches itself needs — five hand-written copies of those had already drifted into two spellings before this existed. `always` drops the `cfg_attr` gate, for a crate that archives unconditionally and so has no `archive` feature to name. Anything else is forwarded verbatim into rkyv's own clause, which is how `derive(PartialEq, Eq, Hash)` reaches the archived type.
//!
//! Fields say `#[archived_with(Adapter)]` and `#[archived_omit_bounds]`. Both are inert markers the macro consumes; nothing declares them, so one written outside an `#[archived]` item is an unresolved-attribute error rather than a line that silently does nothing.
//!
//! # When rkyv cannot archive a type directly
//!
//! Implement [`Proxy`] to say what stands in for it — a namespace is a `u8`, a bignum its little-endian bytes, a hash map its sorted entries — and use [`Via`] as the adapter. The three `ArchiveWith`/`SerializeWith`/`DeserializeWith` impls that used to be written by hand at each such type are supplied once, here, which is what keeps rkyv's trait vocabulary from leaking into the crate that owns the type.
//!
//! # Reading and writing
//!
//! [`to_bytes`], [`from_bytes`], [`access`] and [`deserialize`] are rkyv's four entry points with the error type fixed and the bounds taken on. Callers get a `String` back.

pub use curios_archive_derive::archived;

#[cfg(feature = "enabled")]
mod proxy;
#[cfg(feature = "enabled")]
pub use proxy::*;

#[cfg(feature = "enabled")]
mod serialize;
#[cfg(feature = "enabled")]
pub use serialize::*;

#[cfg(feature = "enabled")]
pub use rkyv;

#[cfg(feature = "enabled")]
pub use rkyv::{Archive, Deserialize, Serialize};

/// rkyv's stock field adapters, flat, so a `with =` clause reads `curios_archive::Skip` rather than reaching three modules deep through the [`rkyv`] re-export.
///
/// Only the ones the workspace uses are here, and that is deliberate: a name absent from this list is a name nothing needed, so adding one is a decision someone makes rather than something a glob does silently.
#[cfg(feature = "enabled")]
pub use rkyv::with::{AsString, Map, Skip};
