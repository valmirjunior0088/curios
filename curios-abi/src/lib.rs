//! The contract shared across the host/guest boundary: the numeric wire codes
//! for `/sys/Io`'s status, poll-event, and open-mode tags, and the
//! [`ForeignStore`] of [`ForeignFunction`]s describing every host operation's
//! import name and [`WireSignature`].
//!
//! Both ends cite these definitions: the runtime (`run::host`) when it lowers a
//! `Status`/`Poll`/`Mode` to the wire and when it types the `env.*` imports,
//! and the compiler when it mints the `/sys/Io` prelude declarations, checks
//! host-op operands, and emits the wasm imports. Because the two sides derive
//! from the same definitions, they cannot drift. This crate is a leaf — it
//! depends on nothing, so both the front-end and the runtime can import it
//! without inverting the pipeline's layering.

mod host;

pub use host::*;

#[cfg(test)]
mod host_tests;

/// Status codes of failable IO ops, mirrored by the guest's `/sys/Io/Status` and
/// decoded into `/std/Io/Error`. `Other` carries an OS errno raw, so it has no
/// fixed code here.
pub mod status {
    pub const OK: u32 = 0;
    pub const EOF: u32 = 1;
    pub const NOT_FOUND: u32 = 2;
    pub const PERMISSION_DENIED: u32 = 3;
    pub const ALREADY_EXISTS: u32 = 4;
    pub const CONNECTION_REFUSED: u32 = 5;
    pub const WOULD_BLOCK: u32 = 6;
    pub const TLS_ERROR: u32 = 7;
}

/// `poll` interest/readiness flags — a bitmask, mirrored by `/sys/Io/Poll`.
/// `READ`/`WRITE` are settable interests; `ERR`/`HUP` are result-only.
pub mod poll {
    pub const READ: u32 = 0b0001;
    pub const WRITE: u32 = 0b0010;
    pub const ERR: u32 = 0b0100;
    pub const HUP: u32 = 0b1000;
}

/// `open` modes, mirrored by `/sys/Io/Mode` and the guest's `/std/Io/Mode`.
pub mod mode {
    pub const READ: u32 = 0;
    pub const WRITE: u32 = 1;
    pub const APPEND: u32 = 2;
}
