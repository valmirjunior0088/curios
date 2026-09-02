//! The contract shared across the host/guest boundary: the numeric wire codes for `/sys/Handle`'s status, poll-event, open-mode, file-kind, and stdio-handle tags, the [`ForeignStore`] of [`ForeignFunction`]s describing every host operation's import name and [`WireSignature`], and the well-known import namespaces both ends link on (`sys` for builtins, `ffi` for user foreign declarations).
//!
//! Both ends cite these definitions: `curios-runtime`'s `host` module when it lowers a `Status`/`Poll`/`Mode` to the wire and when it types the `ffi.*` imports, and the compiler when it mints the `/sys/Handle` prelude declarations, checks host-op operands, and emits the wasm imports. This crate sits below every compiler stage and both runtimes — its only dependencies are the shared foundations `curios-num` and `curios-archive` — so the front-end and the runtime both import it without inverting the pipeline's layering. Why the builtin list is authored once and projected, why `exit` is not a row, why the wire vocabulary nests no list, and why a row's identity is its import pair are `README.md`'s decisions.

mod codes;
pub use codes::*;

mod host;
pub use host::*;
