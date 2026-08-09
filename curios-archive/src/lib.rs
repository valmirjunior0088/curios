//! Zero-copy archiving for the Curios workspace: the one crate that names rkyv.
//!
//! The pattern is `curios-profile`'s. That crate is the workspace's only `tracing` dependency and names its pins in its own manifest; this is the same arrangement for serialization, and for the same reason — a dependency that exists in exactly one manifest cannot be added elsewhere without someone writing the version down again, which is a question a reviewer will ask. A `[workspace.dependencies]` row shares *configuration* and concentrates no authority at all.
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
//! Then annotate the types that are archived:
//!
//! ```text
//! #[cfg_attr(
//!     feature = "archive",
//!     derive(Archive, Serialize, Deserialize),
//!     rkyv(crate = curios_archive::rkyv)
//! )]
//! pub struct Qualifier { … }
//! ```
//!
//! The `rkyv(crate = …)` clause is not decoration. rkyv's derives expand to `::rkyv::` paths, so a crate that no longer depends on rkyv directly has to redirect them; `rkyv_derive` supports exactly this. It is why the annotation is not *shorter* than what it replaced — the goal is one owner, not fewer characters.
//!
//! An attribute macro would collapse those four lines to one, and is deliberately deferred rather than rejected: it needs a `proc-macro = true` crate, which can export nothing else, so it is the serde/serde_derive shape. Because the annotation above already routes through this crate, adding it later rewrites the call sites once and changes nothing else.

pub use curios_archive_derive::archived;

#[cfg(feature = "enabled")]
pub use rkyv;

#[cfg(feature = "enabled")]
pub use rkyv::{Archive, Deserialize, Serialize};
