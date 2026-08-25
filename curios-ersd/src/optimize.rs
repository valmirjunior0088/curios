//! The Ersd transformations — the ones whose leverage is semantic, and nothing else.
//!
//! Pruning, compile-time partial evaluation with literal-spine specialization beside it, and the monoid worker/wrapper rebase. All structural and local optimization — folding, dead code, inlining, contification, specialization — belongs to Cont, which runs after the lowering; Ersd's leverage is what it still knows: don't hand Cont work it can delete (pruning), run what compile time has already decided (partial evaluation), and re-base what would exhaust the runtime stack (worker/wrapper).

mod prune;

mod evaluate;

mod rebase;

use super::{Analysis, Module};

/// Run the Ersd transformations in place: prune, evaluate, specialize, and prune again (evaluation and specialization strand the code they collapse). The module must verify on entry; the final prune re-verifies on exit. Taking a match arm during specialization orphans the untaken arms' values until that final prune tombstones them, so no intermediate verify runs after specialization.
pub fn optimize(module: &mut Module) {
    module
        .verify()
        .expect("a module entering optimization verifies");
    let analysis = Analysis::analyze(module);
    prune::prune_unreachable(module, &analysis);
    compact(module);
    // A curried chain folds one application per round; eight rounds cover any corpus chain with room to spare, and each round's reification draws on one shared node pool, so a round cannot multiply the module however many candidates it found.
    for _ in 0..8 {
        if !evaluate::evaluate_closed_terms(module) {
            break;
        }
    }
    evaluate::specialize_literal_spines(module);
    rebase::rebase_monoid_recursion(module);
    let analysis = Analysis::analyze(module);
    prune::prune_unreachable(module, &analysis);
    compact(module);
}

/// Compact after a prune, and check the result.
///
/// Pruning tombstones; every later walk then steps over the dead slots, and both the verifier and the analysis walk the whole arena. Measured over one program before this existed: 22,477 live slots on entry against 721 live in 29,153 at exit, with nine verifications and eight analyses in between.
///
/// The verification is not belt-and-braces. A compaction that misses an identity rewrites nothing and reports nothing — the stale index still addresses a live slot, just the wrong entity — so this is the one call site where the structural check is the only thing standing between a remap gap and silent miscompilation.
fn compact(module: &mut Module) {
    module.compact();
    module
        .verify()
        .expect("compaction preserves the representation contract");
}
