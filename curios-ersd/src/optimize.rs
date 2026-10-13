//! The Ersd transformations — the ones whose leverage is semantic, and nothing else.
//!
//! Pruning, compile-time partial evaluation with literal-spine specialization beside it, and the monoid worker/wrapper rebase. All structural and local optimization — folding, dead code, inlining, contification, specialization — belongs to Cont, which runs after the lowering; Ersd's leverage is what it still knows: don't hand Cont work it can delete (pruning), run what compile time has already decided (partial evaluation), and re-base what would exhaust the runtime stack (worker/wrapper).

mod prune;
use prune::*;

mod evaluate;
pub(crate) use evaluate::*;

mod rebase;
use rebase::*;

use super::{Analysis, Mark, Module};

/// The rounds a fold of closed applications runs at most. A curried chain folds one application per round. A cap the loop reaches, not a bound it stays under: every program in `programs/` installs replacements in all of them, because each reified closure copy carries closed applications of its own into the next round.
const ROUNDS: usize = 8;

/// Run the Ersd transformations in place: prune, evaluate, specialize, rebase, and prune again (evaluation and specialization strand the code they collapse). The module must verify on entry; the compaction after the final prune re-verifies on exit. Taking a match arm during specialization orphans the untaken arms' values until that final prune tombstones them, so no intermediate verify runs after specialization.
///
/// Each prune is followed by a compaction whose [`Compaction`](super::Compaction) is dropped, so this must run on a module nothing else indexes into: the pipeline hands it the module `ErasedProgram::into_module` released, a linked program no fragment holds an identity of.
pub fn optimize(module: &mut Module) {
    module
        .verify()
        .expect("a module entering optimization verifies");
    optimize_verified(module);
}

/// [`optimize`] over a module the caller has just verified, skipping the entry check.
///
/// **The check it skips is a second reading of the same bytes.** A program's module arrives here straight from its link, which verifies the whole module ([`Module::verify`]) and hands it over unmutated; verifying again walks everything it linked a second time to reach the same verdict. Every other verification stays: erasure's, the one after closed-term evaluation, and the one each prune and compaction ends with.
///
/// Not a debug assertion either. Restating the check under `cfg(debug_assertions)` would put the whole cost back exactly where the suite runs, in exchange for a verdict the line above it already gave.
pub fn optimize_verified(module: &mut Module) {
    let analysis = Analysis::analyze(module);
    prune_unreachable(module, &analysis);
    compact(module);
    // What keeps the rounds from multiplying the module is each round's reification drawing on one shared node pool, and the prune after the loop drops the copies nothing kept.
    for _ in 0..ROUNDS {
        if !evaluate_closed_terms(module) {
            break;
        }
    }
    // Verified once for the loop rather than once per round. Every round installs only what `apply` proved closed and in scope, so a violation is a defect of that proof either way, and naming the round it happened in is all a per-round walk would buy — eight walks over a module that grows tenfold under reification.
    module
        .verify()
        .expect("closed-term evaluation preserves a verifiable module");
    specialize_literal_spines(module);
    rebase_monoid_recursion(module);
    let analysis = Analysis::analyze(module);
    prune_unreachable(module, &analysis);
    compact(module);
}

/// Fold the closed applications among the items `module` gained since `mark`, a round at a time until one installs nothing, and say whether any round installed anything.
///
/// What a fragment's top-level statements compute, computed where the fragment is: linked last into a module of what it reaches, so that everything at or past `mark` is its own, and taken back out with what the rounds appended for it ([`Module::taken`]). No statement before `mark` is a candidate and none is rewritten, and each round draws on a pool of its own, as a round over a whole program does.
pub fn evaluate_since(module: &mut Module, mark: &Mark) -> bool {
    let mut installed = false;
    for _ in 0..ROUNDS {
        if !evaluate_closed_items(module, mark.items) {
            break;
        }
        installed = true;
    }

    installed
}

/// Compact after a prune, and check the result.
///
/// Pruning tombstones; every later walk then steps over the dead slots, and both the verifier and the analysis walk the whole arena — which, once what a program links and does not reach is pruned, is mostly tombstones.
///
/// The verification is not belt-and-braces. A compaction that misses an identity rewrites nothing and reports nothing — the stale index still addresses a live slot, just the wrong entity — so this is the one call site where the structural check is the only thing standing between a remap gap and silent miscompilation.
fn compact(module: &mut Module) {
    module.compact();
    module
        .verify()
        .expect("compaction preserves the representation contract");
}
