//! The compile driver: the one crate that strings the pipeline stages together, from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module`. [`compile_entrypoint`] runs the full `into_core → elaborate → zonk → erase → ersd optimize → into_cont → cont optimize → into_wasm` sequence. Each stage is passed to the caller's observer as a borrowed [`Stage`], which is how `wonder stage` dumps IRs without the driver retaining them.
//!
//! **A compilation is units folded over a dependency order, and this crate is the fold.** [`compile_units`] takes a [`Prefix`](curios_unit::Prefix) — every unit already compiled, borrowed in order — and a slice of [`UnitSource`](curios_text::UnitSource)s, and runs each through *lower → elaborate → judge → erase* against the base and everything before it. [`compile_entrypoint`] is that fold's last step, for the one unit that carries an entrypoint and owns the empty prefix.
//!
//! Judgment sits *between* elaboration and erasure rather than after both, so a module the kernel refuses never reaches erasure's budget. That ordering is a property of the sequence, which is why it lives here: [`recheck`] assembles the [`Globals`](curios_cert::Globals) environment from the scope, because a `curios_unit::Unit` is defined to stay below the kernel and cannot name it.
//!
//! **A unit the cache holds is a baseline, not a hit or a miss.** The fold asks its [`Cache`] for a unit whose record agrees and reuses it whole; when none agrees it asks for a baseline — a unit compiled from an earlier text of the same sources — and [`compile_unit_over`] compiles the edited unit over it, re-elaborating and judging only the closure of what changed while every other item is replayed as scope. The cache decides what a baseline is, and whether what came of one is filed: a question takes one and files nothing, a build compiles whole and files. The argument that the two are the same unit is [a stored unit is a baseline for an item-level recompile](../../documentation/design/toolchain/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md).
//!
//! **The fold does not know which unit is the standard library.** [`compile_entrypoint`] and [`compile_units`] take a `Prefix` and cannot tell which unit is `/std`; naming it is a product's decision, exactly as the syntax registry is the caller's to supply. What this crate adds above them, in `standard.rs`, is that decision written once — [`compile_with_prelude`] and its siblings put the fixed prelude in front of the fold, because `curios`, `curios-js` and this crate's own fixtures had each spelled the same prefix by hand. The boundary this crate keeps is against Binaryen, Cranelift precompilation and execution, which stay downstream in `curios`/`curios-runtime`: this crate stops at the wasm module plus the program's harvested `ForeignStore`.

#[cfg(test)]
mod tests;

mod stage;
pub use stage::*;

mod compile;
pub use compile::*;

mod recompile;
pub use recompile::*;

mod standard;
pub use standard::*;

/// The default reduction budget, re-exported so every caller of [`compile_entrypoint`] can name it without depending on `curios-elab`.
pub use curios_elab::DEFAULT_STEP_BUDGET;
