//! The contract shared across the host/guest boundary: the numeric wire codes for `/sys/Handle`'s status, poll-event, open-mode, file-kind, stdio-wiring, serial-parity, serial-flow, serial-op, and stdio-handle tags, the [`ForeignStore`] of [`ForeignFunction`]s describing every host operation's import name and [`WireSignature`], and the well-known import namespaces both ends link on (`sys` for builtins, `ffi` for user foreign declarations).
//!
//! Both ends cite these definitions: `curios-runtime` when it binds each builtin to its method from the same table, decodes the operands and encodes the reply, and when it types the `ffi.*` imports, and the compiler when it mints the `/sys/Handle` prelude declarations, checks host-op operands, and emits the wasm imports. This crate sits below every compiler stage and both runtimes — its only dependencies are the shared foundations `curios-num` and `curios-archive` — so the front-end and the runtime both import it without inverting the pipeline's layering. Why the builtin table is authored once as typed rows and projected, why `exit` is a diverging row, why the wire vocabulary nests no list, and why a builtin is an identity are `README.md`'s decisions.

mod codes;
pub use codes::*;

mod host;
pub use host::*;
