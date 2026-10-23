//! The compile driver: the one crate that strings the pipeline stages together, from a parsed `curios_text::Entrypoint` to a `curios_wasm::Module`. [`compile_entrypoint`] runs the full `into_core → elaborate → zonk → judge → erase → ersd optimize → into_cont → cont optimize → into_wasm` sequence. Each stage is passed to the caller's observer as a borrowed [`Stage`], which is how `wonder stage` dumps IRs without the driver retaining them.
//!
//! **A compilation is units folded over a dependency order, and this crate is the fold.** [`compile_units`] takes a [`Predecessors`](curios_unit::Predecessors) — every unit already compiled, borrowed in order — and a slice of [`UnitSource`](curios_text::UnitSource)s, and runs each through *lower → elaborate → judge → erase* against the base and everything before it. [`compile_entrypoint`] is that fold's last step, for the one unit that carries an entrypoint and owns the empty prefix.
//!
//! Judgment sits *between* elaboration and erasure rather than after both, so a module the kernel refuses never reaches erasure's budget. That ordering is a property of the sequence, which is why it lives here: [`recheck`] assembles the [`Globals`](curios_cert::Globals) environment from the predecessors, because a `curios_unit::Unit` is defined to stay below the kernel and cannot name it.
//!
//! A unit the cache holds is a baseline as well as a hit: where none agrees with the record, [`compile_unit_over`] compiles the edited unit over the one the [`Cache`] offers, as `README.md`'s "A baseline crosses the cache seam as a unit, and the cache decides" states, and [a stored unit is a baseline for an item-level recompile](../../documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md) argues that the two are one unit.
//!
//! [`compile_entrypoint`] and [`compile_units`] cannot tell which unit is `/std`; in `standard.rs`, [`compile_with_prelude`] and its siblings put the fixed prelude in front of the fold. The crate stops at the wasm module plus the program's harvested `ForeignStore`, and Binaryen, Cranelift precompilation and execution stay downstream in `curios` and `curios-native`. Both boundaries are `README.md`'s decisions.

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
