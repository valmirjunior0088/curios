//! The batteries-included compiler crate: every pipeline stage re-exported under its short name (`base`, `abi`, `text`, `core`, `ersd`, `cont`, `wasm`, `pipeline`), plus the compile-and-run helpers from `compile` ([`to_cwasm`], [`run_wasm`], [`load`], …). This is the only crate that links both native backends — Cranelift via wasmtime and Binaryen via `curios-binaryen` — so the CLI binary and the integration suite build on it, while runtime-only embedders (the bundled-executable launcher) stay on the slim `curios-rt`.

pub use curios_base as base;

pub use curios_abi as abi;

pub use curios_text as text;

pub use curios_core as core;

pub use curios_ersd as ersd;

pub use curios_cont as cont;

pub use curios_wasm as wasm;

pub use curios_pipeline as pipeline;

mod compile;
pub use compile::*;

#[cfg(test)]
mod tests;
