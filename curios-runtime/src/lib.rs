//! Runtime-only Curios engine: deserialize a precompiled module and run it on an embedded wasmtime, wiring the `sys.*` host imports.
//!
//! **This crate is the workspace's only `wasmtime` dependency**, and nothing above it names a wasmtime type at all. The pin and the feature set live in its manifest — the arrangement `curios-archive` and `curios-num` use for rkyv and num-bigint — so the version `curios` precompiles a `.cwasm` against and the version the launcher deserializes it with cannot drift: there is only one row to change.
//!
//! Nothing is re-exported to achieve that. The wasmtime vocabulary appears in [`Lift`] and [`Lower`], which no crate outside this one implements, and every operation an outside caller needs has a wasmtime-free signature: [`validate`], [`precompile`], [`run_bytes`], and — under `test-support`, so it carries no link here — `test_support::GuestInstance` for driving a module by hand.
//!
//! **Runtime-only is a property of the default build, and it is checked.** The `cranelift` feature is never in `default`, so the isolated `cargo build -p curios-runtime` that `make curios/runtime` runs cannot reach a compiler; `curios` turns it on for itself to get [`precompile`]. What enforces this is not the feature declaration but `curios/src/bundle.rs`, whose guards scan the launcher image that actually ships for backend markers and hold it under a size ceiling — a Cranelift-carrying launcher was built and measured against them, and both refuse it.
//!
//! The split of the two AOT operations follows wasmtime's own: [`validate`] needs no compiler and is always present, [`precompile`] is gated. Binaryen is named nowhere here and never will be — optimization belongs to the native product.

mod bundle;
pub use bundle::*;

mod host;
pub use host::*;

mod table;
use table::*;

mod os_host;
pub use os_host::*;

mod os_resolver;
use os_resolver::*;

mod mock_host;
pub use mock_host::*;

mod lift;
pub use lift::*;

mod lower;
pub use lower::*;

mod engine;
pub use engine::*;

/// Deliberately a namespace rather than flattened into the root, and the one place in this workspace that keeps one. The other crates' namespaces existed to hold colliding names apart and were dissolved into crate boundaries; this one is not about ambiguity at all. `curios_runtime::test_support::GuestInstance` says at its use site that the caller reached for scaffolding rather than product API, which a flat `curios_runtime::GuestInstance` beside `run_bytes` would not. The path is the warning label.
#[cfg(feature = "test-support")]
pub mod test_support;
