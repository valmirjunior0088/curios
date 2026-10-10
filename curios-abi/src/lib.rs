//! The contract shared across the host/guest boundary: the numeric wire codes for `/sys/Handle`'s status, poll-event, open-mode, file-kind, stdio-wiring, serial-parity, serial-flow, serial-op, and stdio-handle tags, the [`ForeignFunction`] every `foreign` declaration is, with its import name and [`WireSignature`], and the [`ForeignStore`] a unit's are kept in, the table of the host's own operations, and the well-known import namespaces both ends link on (`ffi` for every declaration's row, `sys` for the emitter's `panic` alone).
//!
//! Both ends cite these definitions: `curios-runtime` when it answers each row the standard library declares with its method from the same table, decodes the operands and encodes the reply, and when it types the `ffi.*` imports, and the compiler when it mints `/sys`'s codes, checks a call's operands, and emits the wasm imports. This crate sits below every compiler stage and both runtimes — its only dependencies are the shared foundations `curios-num` and `curios-archive` — so the front-end and the runtime both import it without inverting the pipeline's layering. Why the host's table is authored once as typed rows and projected, why `exit` is a row that does not return, why the wire vocabulary nests no list, and why a function is its import name are `README.md`'s decisions.

mod codes;
pub use codes::*;

mod host;
pub use host::*;
