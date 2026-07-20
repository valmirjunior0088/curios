//! The arena-level transformations — the three the specification forces and
//! nothing else.
//!
//! Today this driver runs pruning; partial evaluation and the monoid
//! worker/wrapper join it as their phases land. All structural and local
//! optimization — folding, dead code, inlining, contification, specialization
//! — belongs to Cont, which runs after the lowering; the arena's leverage is
//! semantic: don't hand Cont work (pruning), run what is already decided
//! (partial evaluation), and re-base what would exhaust the stack
//! (worker/wrapper).

mod prune;

#[cfg(test)]
mod prune_tests;

use super::ErasedModule;

/// Run the arena transformations in place. The module must verify on entry;
/// it verifies again on exit (each pass re-verifies after mutating).
pub fn optimize_ir(module: &mut ErasedModule) {
    module
        .verify()
        .expect("a module entering optimization verifies");
    prune::prune_unreachable(module);
}
