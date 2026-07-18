//! Deterministic high-CPS canonicalization and propagation.
//!
//! The pipeline never keys on input names: every rewrite depends only on graph
//! structure and the enforced budget constants below, so the same module always
//! optimizes identically. Performance is investigated with revision worktrees and
//! temporary instrumentation, never a permanent metrics API.

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

/// Run the verifier-delimited, FIFO high-CPS simplifier. Phase analyses are
/// rebuilt at deterministic boundaries instead of being kept as shadow state.
pub fn optimize(module: &mut CpsModule) {
    module
        .verify()
        .expect("invalid high CPS before optimization");

    let mut scc_clone_budget = SCC_CLONE_LIMIT;
    let mut branch_clone_budget = BRANCH_CLONE_LIMIT;
    for _ in 0..32 {
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
            break;
        }
    }

    module
        .verify()
        .expect("invalid high CPS after optimization");
}
