//! Ersd → Ersd optimization. A façade over its submodules, exposing one
//! entrypoint that runs the whole pipeline (as [`curios_cont::optimize`] does for
//! Cont):
//!
//! - `call_graph` — the [`CallGraph`] purity oracle: the top-level reference
//!   graph plus its transitive effect taint, shared by `prune` and the engine.
//! - `rewrite` — shared term utilities (ordered child traversal, path lookup,
//!   deep copy, capture refresh) for the passes that plan against an immutable
//!   module and rewrite it after.
//! - `prune` — drops the items the entrypoint cannot reach, so only the
//!   program's actual slice is lowered.
//! - `evaluate` — folds closed subterms by big-step interpretation: constant
//!   data, closures with baked captures, and tail-effect residuals replace the
//!   computations the type checker already evaluated (`Fmt/parse` of a format
//!   string literal, and everything downstream of it).
//! - `specialize` — unrolls a recursive function over a literal constructor
//!   spine `evaluate` produced, one minted first-order item per spine node
//!   (`go_with` over a constant format spine becomes a straight chain).
//! - `worker_wrapper` — the worker/wrapper engine: generalizes a linear non-tail
//!   self-recursion into a tail-recursive worker behind a thin wrapper, composing a
//!   result-side monoid accumulator and an argument-side suffix cursor.

mod call_graph;
pub(crate) use call_graph::*;

mod rewrite;
use rewrite::*;

mod prune;
pub(crate) use prune::*;

mod evaluate;
pub(crate) use evaluate::*;

mod specialize;
pub(crate) use specialize::*;

mod worker_wrapper;
pub(crate) use worker_wrapper::*;

use super::Module;

/// Run the Ersd optimization pipeline in place.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn optimize(module: &mut Module) {
    // Drop the items the entrypoint cannot reach, so only the program's actual
    // slice is lowered. This keeps `into_cont` from eagerly initializing the
    // unused prelude — chiefly the `Parse`/`Json`/`Http` combinator CAFs — in
    // `main`'s entry region, a closure web the optimizer would otherwise drag
    // through lifting, specialization, and inlining on every compile. It runs
    // first (after erase has type-checked everything) so the passes below only
    // ever walk the reachable subset.
    prune_unreachable(module);

    // Fold what the compiler can already run: closed calls evaluate to data,
    // closures, or tail-effect residuals. A literal format string's parse (and
    // its UTF-8 revalidation) happens here instead of at runtime.
    evaluate_closed_terms(module);

    // Unroll the recursions the fold above left applied to constant spines
    // (`go_with` over a parsed format string), one first-order item per node.
    specialize_literal_spines(module);

    // The folds above drop the last references into whole subsystems (the
    // `Parse` combinator web behind a folded `Fmt/parse`); sweep again so the
    // passes below — and `into_cont` — never see them.
    prune_unreachable(module);

    // Wrap each linear non-tail self-recursion (e.g. `Str/len`'s `count_w`) in a
    // tail-recursive worker: the result-side monoid accumulator reassociates a
    // deferred `… + 1`/`… ++ k` into tail position (O(1) stack instead of a frame
    // per element), and the argument-side suffix cursor threads an integer offset
    // over the original buffer instead of re-slicing the tail (O(1) per step
    // instead of O(n)). The two compose; `count_w` needs both.
    introduce_worker_wrappers(module);
}
