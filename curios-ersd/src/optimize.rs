//! The arena-level transformations — the three the specification forces and nothing else.
//!
//! Today this driver runs pruning; partial evaluation and the monoid worker/wrapper join it as their phases land. All structural and local optimization — folding, dead code, inlining, contification, specialization — belongs to Cont, which runs after the lowering; the arena's leverage is semantic: don't hand Cont work (pruning), run what is already decided (partial evaluation), and re-base what would exhaust the stack (worker/wrapper).

mod prune;

mod evaluate;

mod rebase;

use super::{Analysis, Module};

/// Run the arena transformations in place: prune, evaluate, specialize, and prune again (evaluation and specialization strand the code they collapse). The module must verify on entry; the final prune re-verifies on exit. Taking a match arm during specialization orphans the untaken arms' values until that final prune tombstones them, so no intermediate verify runs after specialization.
pub fn optimize_ir(module: &mut Module) {
    module
        .verify()
        .expect("a module entering optimization verifies");
    // One snapshot serves both: proving purity does not mutate, and pruning reads its analysis before it tombstones anything, so the module they see is the same module.
    let analysis = Analysis::analyze(module);
    let proven_pure = evaluate::prove_eager_groups_pure(module, &analysis);
    prune::prune_unreachable(module, &proven_pure, &analysis);
    // A curried chain folds one application per round; eight rounds cover any corpus chain with room to spare, and the shared pass budget caps the total work regardless.
    for _ in 0..8 {
        if !evaluate::evaluate_closed_terms(module) {
            break;
        }
    }
    evaluate::specialize_literal_spines(module);
    rebase::rebase_monoid_recursion(module);
    let analysis = Analysis::analyze(module);
    let proven_pure = evaluate::prove_eager_groups_pure(module, &analysis);
    prune::prune_unreachable(module, &proven_pure, &analysis);
}
