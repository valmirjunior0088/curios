//! Runtime-only curios engine: deserialize a precompiled module and run it on an
//! embedded wasmtime, wiring the `env.io_*` host imports. This crate never names
//! Cranelift or Binaryen — JIT and precompilation live in `curios-cli`, which
//! reuses [`shared_engine`], [`instantiate`], and [`run_bytes`] from here.

mod bundle;
pub use bundle::*;

mod host;
pub use host::*;

mod table;
pub use table::*;

mod os_host;
pub use os_host::*;

mod os_resolver;
pub use os_resolver::*;

mod mock_host;
pub use mock_host::*;

mod lift;
pub use lift::*;

mod lower;
pub use lower::*;

mod engine;
pub use engine::*;
