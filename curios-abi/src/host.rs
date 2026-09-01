//! The builtin host contract, authored once and projected to every consumer.
//!
//! Three concerns live under here, each in its own submodule:
//!
//! - [`store`] — the generic foreign-function substrate ([`WireType`], [`WireSignature`], [`ForeignFunction`], [`ForeignStore`]) that describes any host-provided call, builtin or user `foreign`, as data.
//! - [`types`] — the semantic Rust types a builtin operation speaks in ([`Handle`], [`Status`], [`Poll`], [`Mode`]): the pure halves, free of any native-platform concern, so every host adapter shares them.
//! - [`ops`] — the single authored list of builtin operations (`host_ops!`) and the two projections derived from it: the [`host_ops`](host_ops()) wire store and the typed [`HostOps`] trait. The list is the one place a builtin operation is written; the store and the trait cannot drift because both come off it.
//!
//! Because every consumer — the compiler when it mints the `/sys` prelude and emits the wasm imports, the runtime when it types the `sys.*` imports and binds them — reads these definitions instead of re-spelling them, the two ends cannot drift.

#[cfg(test)]
mod tests;

mod store;
pub use store::*;

mod types;
pub use types::*;

mod ops;
pub use ops::*;
