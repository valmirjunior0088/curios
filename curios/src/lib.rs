//! The native back end: [`to_cwasm`] and [`run_wasm`] from `compile`, and the [`engine`] a stored payload is addressed under. This is the only crate that brings both native backends together — Cranelift by enabling `curios-runtime`'s `cranelift` feature, Binaryen via `curios-binaryen` — so the CLI binary and the cross-stage suite build on it, while runtime-only embedders (the bundled-executable launcher) stay on the slim default `curios-runtime`, and the questions `wonder` answers and the pages `document` writes stay in `curios-wonder` and `curios-document`, under both.
//!
//! Compiling is not here. `curios_pipeline::compile_with_prelude` and its siblings put the fixed prelude in front of the fold, next to the fold they configure; this crate takes the `curios_wasm::Module` that comes out and is indifferent to where it came from.

mod compile;
pub use compile::*;

#[cfg(test)]
mod tests;
