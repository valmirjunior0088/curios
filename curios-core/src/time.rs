//! `Instant::now()` panics on `wasm32-unknown-unknown` (no OS clock); on that
//! target alone, back it with `web-time`'s `Performance.now()` shim instead.
//! Every other target (native, `wasm32-wasi`) uses `std::time::Instant`
//! directly, so this is a no-op everywhere except a bare-wasm build.

#[cfg(target_arch = "wasm32")]
pub use web_time::Instant;

#[cfg(not(target_arch = "wasm32"))]
pub use std::time::Instant;
