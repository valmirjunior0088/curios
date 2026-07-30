//! The compile driver: the one crate that strings the pipeline stages together, from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module`. [`compile_entrypoint`] runs the full `into_core → elaborate → zonk → erase → ersd optimize → into_cont → cont optimize → into_wasm` sequence. Each stage is passed to the caller's observer as a borrowed [`Stage`], which is how `--print` dumps IRs without the driver retaining them.
//!
//! The fixed `sys`/`syn`/`std` prelude is restored from `curios-prelude`'s build-scoped archive; every compile replays prepared Text/Core state and restores a fresh Ersd prefix, so production compilation never source-builds the prelude. Everything wasm-native — Binaryen, Cranelift precompilation, execution — lives downstream in `curios`/`curios-runtime`: this crate stops at the wasm module plus the program's harvested `ForeignStore`.

#[cfg(test)]
mod tests;

mod stage;
pub use stage::*;

mod compile;
pub use compile::*;

/// The default reduction budget, re-exported so every caller of [`compile_entrypoint`] can name it without depending on `curios-elab`.
pub use curios_elab::DEFAULT_STEP_BUDGET;
