//! The native back end: [`to_cwasm`] and [`run_wasm`] from `compile`, and the store-backed [`Verdicts`] the fold consults. This is the only crate that brings both native backends together — Cranelift by enabling `curios-runtime`'s `cranelift` feature, Binaryen via `curios-binaryen` — so the CLI binary and the cross-stage suite build on it, while runtime-only embedders (the bundled-executable launcher) stay on the slim default `curios-runtime`.
//!
//! Compiling is not here. `curios_pipeline::compile_with_prelude` and its siblings put the fixed prelude in front of the fold, next to the fold they configure; this crate takes the `curios_wasm::Module` that comes out and is indifferent to where it came from.

mod cache;
pub use cache::*;

mod compile;
pub use compile::*;

mod wonder;
pub use wonder::*;

/// What a program read from standard input is called: in a status line, and — because the source it is parsed from is labelled with this too — in the `--> <stdin>:2:1` header of any diagnostic about it. One constant, so a reader is never told two names for one program. Angle brackets because no file is spelled that way, so neither line reads as naming something openable.
pub const STDIN_LABEL: &str = "<stdin>";

#[cfg(test)]
mod tests;
