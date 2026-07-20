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

mod evaluate;

mod rebase;

#[cfg(test)]
mod prune_tests;

#[cfg(test)]
mod rebase_tests;

use super::ErasedModule;

/// Run the arena transformations in place: prune, evaluate, specialize, and
/// prune again (evaluation and specialization strand the code they collapse).
/// The module must verify on entry; the final prune re-verifies on exit.
/// Taking a match arm during specialization orphans the untaken arms' values
/// until that final prune tombstones them, so no intermediate verify runs
/// after specialization.
pub fn optimize_ir(module: &mut ErasedModule) {
    module
        .verify()
        .expect("a module entering optimization verifies");
    let proven_pure = evaluate::prove_eager_groups_pure(module);
    prune::prune_unreachable(module, &proven_pure);
    evaluate::evaluate_closed_terms(module);
    evaluate::specialize_literal_spines(module);
    rebase::rebase_monoid_recursion(module);
    let proven_pure = evaluate::prove_eager_groups_pure(module);
    prune::prune_unreachable(module, &proven_pure);
}
