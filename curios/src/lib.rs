//! The native back end: [`to_cwasm`] and [`run_wasm`] from `compile`, and the store-backed [`Verdicts`] the fold consults. This is the only crate that links both native backends — Cranelift via wasmtime and Binaryen via `curios-binaryen` — so the CLI binary and the cross-stage suite build on it, while runtime-only embedders (the bundled-executable launcher) stay on the slim `curios-runtime`.
//!
//! Compiling is not here. `curios_pipeline::compile_with_prelude` and its siblings put the fixed prelude in front of the fold, next to the fold they configure; this crate takes the `curios_wasm::Module` that comes out and is indifferent to where it came from.

mod cache;
pub use cache::*;

mod compile;
pub use compile::*;

#[cfg(test)]
mod tests;
