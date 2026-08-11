//! The compile driver: the one crate that strings the pipeline stages together, from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module`. [`compile_entrypoint`] runs the full `into_core → elaborate → zonk → erase → ersd optimize → into_cont → cont optimize → into_wasm` sequence. Each stage is passed to the caller's observer as a borrowed [`Stage`], which is how `--print` dumps IRs without the driver retaining them.
//!
//! **A compilation is units folded over a dependency order, and this crate is the fold.** [`compile_units`] takes a [`Prefix`](curios_unit::Prefix) — every unit already compiled, borrowed in order — and a slice of [`UnitSource`](curios_text::UnitSource)s, and runs each through *lower → elaborate → judge → erase* against the base and everything before it. [`compile_entrypoint`] is that fold's last step, for the one unit that carries an entrypoint and owns the empty prefix.
//!
//! Judgment sits *between* elaboration and erasure rather than after both, so a module the kernel refuses never reaches erasure's budget. That ordering is a property of the sequence, which is why it lives here: [`recheck`] assembles the [`Globals`](curios_cert::Globals) environment from the scope, because a `curios_unit::Unit` is defined to stay below the kernel and cannot name it.
//!
//! **This crate does not know which unit is the standard library.** It has no `curios-prelude` dependency outside `[dev-dependencies]`, where its own fixtures need one because a program needs a prelude to compile against. A product names its standard library — `curios` and `curios-web` restore the archive and pass it as the first unit in scope — exactly as the `/syn` registry is already the caller's to supply. Everything wasm-native — Binaryen, Cranelift precompilation, execution — lives downstream in `curios`/`curios-runtime`: this crate stops at the wasm module plus the program's harvested `ForeignStore`.

#[cfg(test)]
mod tests;

mod stage;
pub use stage::*;

mod compile;
pub use compile::*;

mod standard;
pub use standard::*;

/// The default reduction budget, re-exported so every caller of [`compile_entrypoint`] can name it without depending on `curios-elab`.
pub use curios_elab::DEFAULT_STEP_BUDGET;
