//! The batteries-included compiler crate: the compile-and-run helpers from `compile` ([`to_cwasm`], [`run_wasm`], [`load`], …). This is the only crate that links both native backends — Cranelift via wasmtime and Binaryen via `curios-binaryen` — so the CLI binary and the integration suite build on it, while runtime-only embedders (the bundled-executable launcher) stay on the slim `curios-rt`.

mod compile;
pub use compile::*;

#[cfg(feature = "profile")]
pub mod profile;

#[cfg(test)]
mod tests;
