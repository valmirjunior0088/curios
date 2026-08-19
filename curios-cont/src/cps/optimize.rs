//! Deterministic high-CPS canonicalization and propagation.
//!
//! The pipeline never keys on input names: every rewrite depends only on graph structure and the enforced budget constants below, so the same module always optimizes identically. Performance is investigated with revision worktrees and temporary instrumentation, never a permanent metrics API.

#[cfg(test)]
mod tests;

use super::{
    CpsModule,
    analysis::known_values,
    contify::contify_calls,
    cse::dedupe_intrinsics,
    fields::{split_parameters, split_windows, split_workers},
    inline::{inline_known_calls, inline_single_use_continuations},
    protocol::split_returns,
    reachable::prune_unreachable,
    simplify::{
        dissolve_rec_init, eliminate_dead_bindings, eliminate_dead_parameters,
        fold_intrinsic_identities, forward_aggregate_projections, forward_continuations,
        fuse_append_chains, rewrite_atoms, simplify_nodes,
    },
    specialize::{specialize_call_patterns, specialize_jump_patterns, specialize_scc_calls},
    uncurry::uncurry_returns,
};

/// How many live nodes a callee with more than one call site may have and still be inlined into each of them.
///
/// It was 8, and 8 bound something specific: `/std/State/bind` is a nine-node extent — two of its own and seven in the `bind/1` it nests — so a monadic step kept a shared generic `bind` that received both the action and the continuation as arguments and reached each through `call_ref`. `programs/rng_state.crs` spent 0.825 s there against its hand-threaded control's 0.025 s, and one node of budget was the whole of what stood between them.
///
/// That argues for nine. The value is twice the old one instead, because a budget tuned to clear one measured callee is a budget that clears exactly that callee, and the next one a node larger pays the same price with nobody watching.
pub(super) const MULTI_SITE_INLINE_LIMIT: usize = 16;
pub(super) const BRANCH_SPECIALIZATION_GROWTH_LIMIT: usize = 24;
/// How many parameters a continuation may hold after a fields split. The M0 census's largest admitted aggregate is the four-field scan state riding beside loop state, so sixteen clears every observed candidate with headroom for one level of nesting — while refusing the unbounded flattening a recursive structure's constructions would otherwise invite, which is what makes the split's termination independent of `ROUND_LIMIT`.
pub(super) const PARAM_SPLIT_GROWTH_LIMIT: usize = 16;
pub(super) const SCC_CLONE_LIMIT: usize = 64;
pub(super) const SCC_CLONE_NODE_LIMIT: usize = 256;
pub(super) const BRANCH_CLONE_LIMIT: usize = 64;
pub(super) const JUMP_CLONE_LIMIT: usize = 64;

/// How many times the pass sequence may be re-run before the fixpoint gives up.
///
/// A backstop against a pass pair that undoes each other's work, not a budget: what bounds the *real* work is the growth limits above, each of which refuses an individual rewrite. Reaching this limit therefore means the sequence did not converge, and the module is emitted in whatever half-optimized state the last round left it — silently, since nothing downstream can tell a fixpoint that finished from one that ran out.
///
/// It was 32, and 32 bound: 33 programs in the corpus stopped there, and raising the limit showed them converging anywhere up to 191 rounds with every test still passing, so the truncation was pure loss rather than a tradeoff anything depended on. The value is set far above that measured maximum because a backstop that a real program can reach is indistinguishable from a budget nobody documented.
pub(super) const ROUND_LIMIT: usize = 1024;

/// Run the verifier-delimited, FIFO high-CPS simplifier. Phase analyses are rebuilt at deterministic boundaries instead of being kept as shadow state.
pub fn optimize(module: &mut CpsModule) {
    curios_profile::profile!("cont_optimize");
    module
        .verify()
        .expect("invalid high CPS before optimization");

    let mut scc_clone_budget = SCC_CLONE_LIMIT;
    let mut branch_clone_budget = BRANCH_CLONE_LIMIT;
    let mut jump_clone_budget = JUMP_CLONE_LIMIT;
    let mut converged = false;
    for _ in 0..ROUND_LIMIT {
        let substitutions = known_values(module);
        let changed = rewrite_atoms(module, &substitutions)
            | forward_continuations(module)
            | forward_aggregate_projections(module)
            | dedupe_intrinsics(module)
            | simplify_nodes(module)
            | fold_intrinsic_identities(module)
            | fuse_append_chains(module)
            | eliminate_dead_bindings(module)
            | eliminate_dead_parameters(module)
            | inline_single_use_continuations(module)
            | inline_known_calls(module)
            | contify_calls(module)
            | specialize_scc_calls(module, &mut scc_clone_budget)
            | specialize_call_patterns(module, &mut branch_clone_budget)
            | specialize_jump_patterns(module, &mut jump_clone_budget)
            | split_returns(module)
            | split_parameters(module)
            | split_workers(module)
            | split_windows(module)
            | uncurry_returns(module)
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
