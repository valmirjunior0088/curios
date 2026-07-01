//! Ersd → Ersd optimization. A façade over its submodules, exposing one
//! entrypoint that runs the whole pipeline (as [`crate::cont::optm`] does for
//! Cont):
//!
//! - [`call_graph`] — the [`CallGraph`] purity oracle: the top-level reference
//!   graph plus its transitive effect taint, shared by `prune` and the engine.
//! - the shared slice re-base laws (`Carrier`/`SuffixRead`) live in
//!   `curios_base::suffix_view`, read here by `worker_wrapper` and by `cont`'s
//!   `slice_forwarding`.
//! - [`prune`] — drops the items the entrypoint cannot reach, so only the
//!   program's actual slice is lowered.
//! - [`worker_wrapper`] — the worker/wrapper engine: generalizes a linear non-tail
//!   self-recursion into a tail-recursive worker behind a thin wrapper, composing a
//!   result-side monoid accumulator and an argument-side suffix cursor.

mod call_graph;
pub use call_graph::*;

mod prune;
pub use prune::*;

mod worker_wrapper;
pub use worker_wrapper::*;

use super::Module;

/// Run the Ersd optimization pipeline in place.
pub fn optimize(module: &mut Module) {
    // Drop the items the entrypoint cannot reach, so only the program's actual
    // slice is lowered. This keeps `to_cont` from eagerly initializing the
    // unused prelude — chiefly the `Parse`/`Json`/`Http` combinator CAFs — in
    // `main`'s entry region, a closure web the optimizer would otherwise drag
    // through lifting, specialization, and inlining on every compile. It runs
    // first (after erase has type-checked everything) so the passes below only
    // ever walk the reachable subset.
    prune_unreachable(module);

    // Wrap each linear non-tail self-recursion (e.g. `Str/len`'s `count_w`) in a
    // tail-recursive worker: the result-side monoid accumulator reassociates a
    // deferred `… + 1`/`… ++ k` into tail position (O(1) stack instead of a frame
    // per element), and the argument-side suffix cursor threads an integer offset
    // over the original buffer instead of re-slicing the tail (O(1) per step
    // instead of O(n)). The two compose; `count_w` needs both.
    introduce_worker_wrappers(module);
}
