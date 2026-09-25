//! The builtin host contract, authored once and projected to every consumer.
//!
//! Four concerns live under here, each in its own submodule:
//!
//! - [`store`] — the generic foreign-function substrate ([`WireType`], [`WireSignature`], [`ForeignFunction`], [`ForeignStore`]) that describes any host-provided call, builtin or user `foreign`, as data.
//! - [`types`] — the semantic Rust types a builtin operation speaks in ([`Handle`], [`Failure`], [`Poll`], [`Mode`] and the payloads with several fields): the pure halves, free of any native-platform concern, so every host adapter shares them.
//! - [`crossing`] — how each of those types crosses the wire ([`WireOperand`], [`WirePayload`], [`WireReply`]) and the [`Outcome`] a reply's type states.
//! - [`ops`] — the single authored table of builtin operations (`host_ops!`), each row a typed signature, and what is derived from it: the [`HostOp`] enum, the [`host_ops`](host_ops()) wire store and the typed [`HostOps`] trait. The table is the one place a builtin operation is written; the store, the trait and the runtime's bindings cannot drift because all of them come off it.
//!
//! Because every consumer — the compiler when it mints the `/sys` prelude and emits the wasm imports, the runtime when it types the `sys.*` imports and binds them — reads these definitions instead of re-spelling them, the two ends cannot drift.

#[cfg(test)]
mod tests;

mod store;
pub use store::*;

mod types;
pub use types::*;

mod crossing;
pub use crossing::*;

mod ops;
pub use ops::*;
