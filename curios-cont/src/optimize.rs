//! Deterministic high-CPS canonicalization and propagation.
//!
//! The pipeline never keys on input names: every rewrite depends only on graph structure and the enforced budget constants below, so the same module always optimizes identically. Every pass carries a permanent span and a sample of whether it fired, as `curios-profile` prescribes for optimizer passes: the span's `calls` is the round count and its total the pass's share, and the sample's total is the number of rounds that pass kept the fixpoint alive. Anything finer is investigated with revision worktrees and temporary instrumentation, never a permanent metrics API.

mod clone;
pub(crate) use clone::*;

mod contify;
pub(crate) use contify::*;

mod cse;
pub(crate) use cse::*;

mod evaluate;
pub(crate) use evaluate::*;

mod fields;
pub(crate) use fields::*;

mod inline;
pub(crate) use inline::*;

mod protocol;
pub(crate) use protocol::*;

mod reachable;
pub(crate) use reachable::*;

mod simplify;
pub(crate) use simplify::*;

mod specialize;
pub(crate) use specialize::*;

mod uncurry;
pub(crate) use uncurry::*;

use super::{Module, known_values};
// Only the instrument below reads the demand lattice, and it compiles away without the feature.
#[cfg(feature = "profile")]
use super::{Demand, demand_of, demands};

/// How many live nodes a callee with more than one call site may have and still be inlined into each of them.
///
/// `/std/State/bind` is a nine-node extent — two of its own and seven in the `bind/1` it nests — so under a limit of eight a monadic step keeps a shared generic `bind` that receives both the action and the continuation as arguments and reaches each through a closure call, an indirect call per step that the hand-threaded control never pays.
///
/// That argues for nine. The value is sixteen instead, because a budget tuned to clear one measured callee is a budget that clears exactly that callee, and the next one a node larger pays the same price with nobody watching.
pub(crate) const MULTI_SITE_INLINE_LIMIT: usize = 16;
pub(crate) const BRANCH_SPECIALIZATION_GROWTH_LIMIT: usize = 24;
/// How many parameters a continuation may hold after a fields split. The largest admitted aggregate `curios`'s corpus census finds is the four-field scan state riding beside loop state, so sixteen clears every observed candidate with headroom for one level of nesting — while refusing the unbounded flattening a recursive structure's constructions would otherwise invite, which is what makes the split's termination independent of `ROUND_LIMIT`.
pub(crate) const PARAM_SPLIT_GROWTH_LIMIT: usize = 16;
pub(crate) const SCC_CLONE_LIMIT: usize = 64;
pub(crate) const SCC_CLONE_NODE_LIMIT: usize = 256;
pub(crate) const BRANCH_CLONE_LIMIT: usize = 64;
pub(crate) const JUMP_CLONE_LIMIT: usize = 64;

/// How many times the pass sequence may be re-run before the fixpoint gives up.
///
/// A backstop against a pass pair that undoes each other's work, not a budget: what bounds the *real* work is the growth limits above, each of which refuses an individual rewrite. Reaching this limit therefore means the sequence did not converge, and the module is emitted in whatever half-optimized state the last round left it — silently, since nothing downstream can tell a fixpoint that finished from one that ran out.
///
/// A limit in the tens would truncate real programs for pure loss rather than a tradeoff anything depends on, so the value is set far above any round count the corpus reaches: a backstop that a real program can reach is indistinguishable from a budget nobody documented.
pub(crate) const ROUND_LIMIT: usize = 1024;

/// Run the verifier-delimited, FIFO high-CPS simplifier. Phase analyses are rebuilt at deterministic boundaries instead of being kept as shadow state.
pub fn optimize(module: &mut Module) {
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
            let changed = curios_profile::profile!($name => $pass);
            curios_profile::sample!($name, changed as u64);
            // The growth ledger beside the fired ledger: what each pass left standing, so a size regression names its pass the way a time regression names its span.
            curios_profile::sample!(
                concat!($name, "::nodes"),
                module.live_nodes().count()
            );
            curios_profile::sample!(
                concat!($name, "::conts"),
                module.live_continuations().count()
            );
            curios_profile::sample!(concat!($name, "::cap"), module.nodes().len());
            curios_profile::sample!(concat!($name, "::values"), module.value_ids().count());
            changed
        }};
    }

    // The round on which the sequence settled, or `None` if it never did.
    let mut converged = None;
    for round in 1..=ROUND_LIMIT {
        let substitutions = curios_profile::profile!("cont::known_values" => known_values(module));
        let changed = pass!("cont::rewrite_atoms", rewrite_atoms(module, &substitutions))
            | pass!("cont::forward_continuations", forward_continuations(module))
            | pass!("cont::forward_calls", forward_calls(module))
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
            | {
                sample_droppable_dead_calls(module);
                false
            }
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
        // Windows are virtualized only once everything else has settled, because a window split is irrevocable in a way no other rewrite here is: it records a group over every position the region spans, and a later region that transfers into one of those positions is declined whole. A region's extent is a fact of the *converged* graph — the continuations inlining, contification and specialization mint do not exist in the round that split a region they will turn out to flow into — so deciding it earlier measures something transient and then freezes it. `programs/walk_mirror_held_scan.crs` is the case: its walk's continuation is minted a round after the sub-region below it would be split, which would leave the walk slicing a fresh rope per character.
        let changed = changed || pass!("cont::split_windows", split_windows(module));
        // Debug builds verify at each round boundary, so an invalid rewrite is named within one round of its pass instead of surfacing at the exit gate arbitrarily later. The boundary is the round and not the pass on purpose: mid-round states are transiently unscoped by design, and only the round's close, behind its prune, promises a structurally canonical module. The row-vocabulary clause is deliberately not part of that promise — `verify_structure` states why — so it is checked at the entry and exit gates alone.
        #[cfg(debug_assertions)]
        if let Err(error) = module.verify_structure() {
            panic!("invalid high CPS at the close of round {round}: {error:?}");
        }
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

/// How many calls stand where a dead-call deletion would remove one — a rule no pass applies, so this sampler is the `droppable` flag's one reader: a known callee whose definition was proved total and performs nothing, with a result nothing reads.
///
/// **A measurement, not a rule.** Whether such a call is ever *observed* here is the question the deletion rests on, and reading the pass list does not answer it: the result becomes unread only once [`eliminate_dead_parameters`] has stripped it, and inlining and contification consume the call in the same round — so the call may be gone before it is ever dead. Counting at the one point the rule would fire is what settles that for the price of a counter.
///
/// The condition is the deletion's own, and it is read off [`super::demand`]'s lattice: the value the call returns into — its return continuation's parameter — is `Unused`, which is the state in which the call could be spliced to a jump. A tail call is never counted, because its `return_to` is the function's own bodyless sentinel and the result is what the caller resumes on, so there is no parameter to read a demand off and none is read.
///
/// **Not whether the return continuation holds no parameter at all**, a state nothing in the pipeline produces: a non-tail resume is minted at arity one by the Ersd lowering, and of the three writers of a continuation's parameter list, [`eliminate_dead_parameters`] skips every continuation that is a return target, [`split_returns`] sets a width of two or more, and specialization copies a clone's list — so a count on that condition could only ever read zero, whether or not a call was dead. Asking the lattice asks the question the deletion actually rests on.
///
/// # What it last reported
///
/// `present` 7 over `total(Vec/of_list(List/replicate(List/len(args), 7)))`; `dead` has not been taken under this condition.
///
/// Retake it with `cargo xtask profile <source>` and read `cont::droppable_dead_calls` in the folded output, or `curios/.artifacts/profile.tsv` directly when the program exits non-zero, since the recipe folds nothing then. Perturb the source first: a cached unit skips the optimizer entirely and reports no sample at all.
fn sample_droppable_dead_calls(module: &Module) {
    let _ = module;
    #[cfg(feature = "profile")]
    {
        // Two numbers, because one cannot tell the two causes of a zero apart: a call already consumed by inlining or contification, and a call still standing whose result is still read.
        let mut present = 0u64;
        let mut dead = 0u64;
        let demands = demands(module);
        for node in module.nodes().iter().flatten() {
            let super::Node::ApplyFun {
                callee: super::Callee::Known(callee),
                return_to,
                ..
            } = node
            else {
                continue;
            };
            if !module
                .function(*callee)
                .is_some_and(|function| function.droppable)
            {
                continue;
            }
            present += 1;
            if module
                .continuation(*return_to)
                .and_then(|continuation| continuation.params.first())
                .is_some_and(|result| demand_of(&demands, *result) == Demand::Unused)
            {
                dead += 1;
            }
        }
        curios_profile::sample!("cont::droppable_calls_present", present);
        curios_profile::sample!("cont::droppable_dead_calls", dead);
    }
}

#[cfg(test)]
mod tests;
