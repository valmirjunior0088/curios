//! The host contract, authored once and projected to every consumer.
//!
//! Five concerns live under here, each in its own submodule:
//!
//! - [`store`] — the generic foreign-function substrate ([`WireType`], [`WireSignature`], [`ForeignFunction`], [`ForeignStore`]) that describes the call a `foreign` declaration states, as data.
//! - [`types`] — the semantic Rust types a host operation speaks in ([`Handle`], [`Failure`], [`Poll`], [`Mode`] and the payloads with several fields): the pure halves, free of any native-platform concern, so every host adapter shares them.
//! - [`crossing`] — how each of those types crosses the wire ([`WireOperand`], [`WirePayload`], [`WireReply`]).
//! - [`contract`] — what a row asks of a call's operands beyond their types ([`Requirement`]), and the admission that holds a call to it.
//! - [`ops`] — the single authored table of the host's operations (`for_each_host_op!`), each row a typed signature and the `/std` module that declares it, and what is derived from it: the [`HostOp`] enum and the typed [`HostOps`] trait. The table is the one place a host operation is written; the trait and the runtime's bindings cannot drift because both come off it.

#[cfg(test)]
mod tests;

mod store;
pub use store::*;

mod types;
pub use types::*;

mod crossing;
pub use crossing::*;

mod contract;
pub use contract::*;

mod ops;
pub use ops::*;
