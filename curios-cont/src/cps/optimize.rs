//! Deterministic high-CPS canonicalization and propagation.
//!
//! The pipeline never keys on input names: every rewrite depends only on graph structure and the enforced budget constants below, so the same module always optimizes identically. Performance is investigated with revision worktrees and temporary instrumentation, never a permanent metrics API.

#[cfg(test)]
mod tests;

use super::{
    CpsModule,
    analysis::known_values,
    contify::contify_calls,
    inline::{inline_known_calls, inline_single_use_continuations},
    reachable::prune_unreachable,
    simplify::{
        dissolve_rec_init, eliminate_dead_bindings, eliminate_dead_parameters,
        forward_aggregate_projections, forward_continuations, rewrite_atoms, simplify_nodes,
    },
    specialize::{specialize_call_patterns, specialize_scc_calls},
};

pub(super) const MULTI_SITE_INLINE_LIMIT: usize = 8;
pub(super) const BRANCH_SPECIALIZATION_GROWTH_LIMIT: usize = 24;
pub(super) const SCC_CLONE_LIMIT: usize = 64;
pub(super) const SCC_CLONE_NODE_LIMIT: usize = 256;
pub(super) const BRANCH_CLONE_LIMIT: usize = 64;

/// How many times the pass sequence may be re-run before the fixpoint gives up.
///
/// A backstop against a pass pair that undoes each other's work, not a budget: what bounds the *real* work is the growth limits above, each of which refuses an individual rewrite. Reaching this limit therefore means the sequence did not converge, and the module is emitted in whatever half-optimized state the last round left it — silently, since nothing downstream can tell a fixpoint that finished from one that ran out.
///
/// It was 32, and 32 bound: 33 programs in the corpus stopped there, and raising the limit showed them converging anywhere up to 191 rounds with every test still passing, so the truncation was pure loss rather than a tradeoff anything depended on. The value is set far above that measured maximum because a backstop that a real program can reach is indistinguishable from a budget nobody documented.
pub(super) const ROUND_LIMIT: usize = 1024;

/// Run the verifier-delimited, FIFO high-CPS simplifier. Phase analyses are rebuilt at deterministic boundaries instead of being kept as shadow state.
pub fn optimize(module: &mut CpsModule) {
    module
        .verify()
        .expect("invalid high CPS before optimization");

    let mut scc_clone_budget = SCC_CLONE_LIMIT;
    let mut branch_clone_budget = BRANCH_CLONE_LIMIT;
    let mut converged = false;
    for _ in 0..ROUND_LIMIT {
        let substitutions = known_values(module);
        let changed = rewrite_atoms(module, &substitutions)
            | forward_continuations(module)
            | forward_aggregate_projections(module)
            | simplify_nodes(module)
            | eliminate_dead_bindings(module)
            | eliminate_dead_parameters(module)
            | inline_single_use_continuations(module)
            | inline_known_calls(module)
            | contify_calls(module)
            | specialize_scc_calls(module, &mut scc_clone_budget)
            | specialize_call_patterns(module, &mut branch_clone_budget)
            | dissolve_rec_init(module)
            | prune_unreachable(module);
        if !changed {
            converged = true;
            break;
        }
    }

    // Loud rather than silent: a module that exhausts the limit is emitted less optimized than an equivalent one that did not, and no later stage can detect the difference. Debug-only because the consequence is worse code rather than wrong code — a release compile should still produce a working program.
    debug_assert!(
        converged,
        "cont optimization did not converge within {ROUND_LIMIT} rounds"
    );

    module
        .verify()
        .expect("invalid high CPS after optimization");
}
