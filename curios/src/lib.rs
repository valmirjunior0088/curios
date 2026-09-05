//! The native back end: [`to_cwasm`] and [`run_wasm`] from `compile`, the [`engine`] a stored payload is addressed under, and the pages `curios document` renders. This is the only crate that brings both native backends together — Cranelift by enabling `curios-runtime`'s `cranelift` feature, Binaryen via `curios-binaryen` — so the CLI binary and the cross-stage suite build on it, while runtime-only embedders (the bundled-executable launcher) stay on the slim default `curios-runtime`, and the questions `wonder` answers stay in `curios-wonder`, under both.
//!
//! Compiling is not here. `curios_pipeline::compile_with_prelude` and its siblings put the fixed prelude in front of the fold, next to the fold they configure; this crate takes the `curios_wasm::Module` that comes out and is indifferent to where it came from.

mod compile;
pub use compile::*;

mod document;
pub use document::*;

#[cfg(test)]
mod tests;
