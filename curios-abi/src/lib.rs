//! The contract shared across the host/guest boundary: the numeric wire codes for `/sys/Handle`'s status, poll-event, open-mode, and stdio-handle tags, the [`ForeignStore`] of [`ForeignFunction`]s describing every host operation's import name and [`WireSignature`], and the well-known import namespaces both ends link on (`sys` for builtins, `ffi` for user foreign declarations).
//!
//! Both ends cite these definitions: `curios-runtime`'s `host` module when it lowers a `Status`/`Poll`/`Mode` to the wire and when it types the `ffi.*` imports, and the compiler when it mints the `/sys/Handle` prelude declarations, checks host-op operands, and emits the wasm imports. Because the two sides derive from the same definitions, they cannot drift. This crate sits below every compiler stage and both runtimes — its only dependencies are the shared foundations `curios-num` and `curios-archive` — so the front-end and the runtime both import it without inverting the pipeline's layering.

mod codes;
pub use codes::*;

mod host;
pub use host::*;
