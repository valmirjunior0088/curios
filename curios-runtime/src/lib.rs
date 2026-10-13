//! Runtime-only Curios engine: deserialize a precompiled module and run it on an embedded wasmtime, answering the rows it imports.
//!
//! **This crate is the engine, and `curios-cranelift` beside it is its compiler.** The two take Wasmtime from one row of the workspace manifest, so the version `curios` precompiles a `.cwasm` against and the version the launcher deserializes it with cannot drift, and nothing above the pair names a wasmtime type at all.
//!
//! Nothing is re-exported to achieve that. The wasmtime vocabulary appears in [`Lift`] and [`Lower`], which no crate outside this one implements, and in what `curios-cranelift` alone reaches for: [`shared_engine`], and the module a [`MachineCode`] is made of and hands back. Every operation another caller needs has a wasmtime-free signature: [`validate`], [`Compiled`], [`plugin_bindings`], and — under `test-support`, so it carries no link here — `test_support::GuestInstance` for driving a module by hand.
//!
//! **Runtime-only is a property of the crate, and it is checked.** No feature here reaches a compiler, so the isolated `cargo build -p curios-runtime` that `cargo xtask launcher build` runs cannot reach one, and every check builds the one configuration the launcher ships in. What holds it is `README.md`'s "The launcher is slim by exclusion": the guards in `curios/src/bundle.rs` scan the launcher image that ships.
//!
//! The split of the AOT operations follows wasmtime's own: [`validate`] needs no compiler and is here, while everything that does need one is `curios-cranelift`'s. A module compiled there arrives as a [`MachineCode`], which only an engine makes; bytes are read back through [`MachineCode::deserialize`] alone, the one door whose caller vouches for what it hands over. Binaryen is named nowhere here and never will be — optimization belongs to the native product.

mod bundle;
pub use bundle::*;

mod plugin;
pub use plugin::*;

mod host;
pub use host::*;

mod table;
use table::*;

mod os_host;
pub use os_host::*;

mod os_resolver;
use os_resolver::*;

mod os_child;
use os_child::*;

mod os_serial;
use os_serial::*;

mod mock_host;
pub use mock_host::*;

mod lift;
pub use lift::*;

mod lower;
pub use lower::*;

mod engine;
pub use engine::*;

/// Deliberately a namespace rather than flattened into the root, and one of the places in this workspace that keep one — `curios-ersd`'s, `curios-elab`'s, `curios-core`'s, `curios-utilities`'s and `curios-analysis`'s `test_support` are the others, for the same reason, which is not ambiguity at all. `curios_runtime::test_support::GuestInstance` says at its use site that the caller reached for scaffolding rather than product API, which a flat `curios_runtime::GuestInstance` beside `Compiled` would not. The path is the warning label.
#[cfg(feature = "test-support")]
pub mod test_support;
