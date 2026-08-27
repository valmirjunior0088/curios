//! Deterministic high-CPS canonicalization and propagation.
//!
//! The pipeline never keys on input names: every rewrite depends only on graph structure and the enforced budget constants below, so the same module always optimizes identically. Every pass carries a permanent span and a sample of whether it fired, as `curios-profile` prescribes for optimizer passes: the span's `calls` is the round count and its total the pass's share, and the sample's total is the number of rounds that pass kept the fixpoint alive. Anything finer is investigated with revision worktrees and temporary instrumentation, never a permanent metrics API.

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
        eliminate_dead_bindings, eliminate_dead_parameters, flatten_indexed_lists,
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
    // Name and time one pass, and record whether it fired. The two together are what separate the fixpoint's hypotheses: a pass that admits one candidate per call fires on as many rounds as it has candidates, while a pair undoing each other's work fires in lockstep for rounds neither needed.
    macro_rules! pass {
        ($name:literal, $pass:expr) => {{
            let changed = curios_profile::profile_span!($name, $pass);
            curios_profile::sample!($name, changed as u64);
            changed
        }};
    }

    // The round on which the sequence settled, or `None` if it never did.
    let mut converged = None;
    for round in 1..=ROUND_LIMIT {
        let substitutions =
            curios_profile::profile_span!("cont::known_values", known_values(module));
        let changed = pass!("cont::rewrite_atoms", rewrite_atoms(module, &substitutions))
            | pass!("cont::forward_continuations", forward_continuations(module))
            | pass!(
                "cont::forward_aggregate_projections",
                forward_aggregate_projections(module)
            )
            | pass!("cont::dedupe_intrinsics", dedupe_intrinsics(module))
            | pass!("cont::simplify_nodes", simplify_nodes(module))
            | pass!(
                "cont::fold_intrinsic_identities",
                fold_intrinsic_identities(module)
            )
            | pass!("cont::fuse_append_chains", fuse_append_chains(module))
            | pass!("cont::flatten_indexed_lists", flatten_indexed_lists(module))
            | pass!(
                "cont::eliminate_dead_bindings",
                eliminate_dead_bindings(module)
            )
            | pass!(
                "cont::eliminate_dead_parameters",
                eliminate_dead_parameters(module)
            )
            | pass!(
                "cont::inline_single_use_continuations",
                inline_single_use_continuations(module)
            )
            | pass!("cont::inline_known_calls", inline_known_calls(module))
            | pass!("cont::contify_calls", contify_calls(module))
            | pass!(
                "cont::specialize_scc_calls",
                specialize_scc_calls(module, &mut scc_clone_budget)
            )
            | pass!(
                "cont::specialize_call_patterns",
                specialize_call_patterns(module, &mut branch_clone_budget)
            )
            | pass!(
                "cont::specialize_jump_patterns",
                specialize_jump_patterns(module, &mut jump_clone_budget)
            )
            | pass!("cont::split_returns", split_returns(module))
            | pass!("cont::split_parameters", split_parameters(module))
            | pass!("cont::split_workers", split_workers(module))
            | pass!("cont::uncurry_returns", uncurry_returns(module))
            | pass!("cont::prune_unreachable", prune_unreachable(module));
        // Windows are virtualized only once everything else has settled, because a window split is irrevocable in a way no other rewrite here is: it records a group over every position the region spans, and a later region that transfers into one of those positions is declined whole. A region's extent is a fact of the *converged* graph — the continuations inlining, contification and specialization mint do not exist in the round that split a region they will turn out to flow into — so deciding it earlier measures something transient and then freezes it. `programs/walk_mirror_held_scan.crs` was the case: its walk's continuation was minted a round after the sub-region below it had been split, and the walk sliced a fresh rope per character from then on.
        let changed = changed || pass!("cont::split_windows", split_windows(module));
        if !changed {
            converged = Some(round);
            break;
        }
    }
    curios_profile::sample!("cont_optimize::rounds", converged.unwrap_or(ROUND_LIMIT));

    // Loud rather than silent: a module that exhausts the limit is emitted less optimized than an equivalent one that did not, and no later stage can detect the difference. Debug-only because the consequence is worse code rather than wrong code — a release compile should still produce a working program.
    debug_assert!(
        converged.is_some(),
        "cont optimization did not converge within {ROUND_LIMIT} rounds"
    );

    module
        .verify()
        .expect("invalid high CPS after optimization");
}
