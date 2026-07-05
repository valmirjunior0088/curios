//! The contract shared across the host/guest boundary: the numeric wire codes
//! for `/sys/Io`'s status, poll-event, open-mode, and stdio-handle tags, the
//! [`ForeignStore`] of [`ForeignFunction`]s describing every host operation's
//! import name and [`WireSignature`], and the well-known name constants both
//! ends link on ([`NAMESPACE_SYS`], [`NAMESPACE_ENV`], [`MAIN_EXPORT`]).
//!
//! Both ends cite these definitions: the runtime (`run::host`) when it lowers a
//! `Status`/`Poll`/`Mode` to the wire and when it types the `env.*` imports,
//! and the compiler when it mints the `/sys/Io` prelude declarations, checks
//! host-op operands, and emits the wasm imports. Because the two sides derive
//! from the same definitions, they cannot drift. This crate is a leaf — it
//! depends on nothing, so both the front-end and the runtime can import it
//! without inverting the pipeline's layering.

mod codes;
pub use codes::*;

mod host;
pub use host::*;

mod root;
pub use root::*;

#[cfg(test)]
mod host_tests;

#[cfg(test)]
mod root_tests;
