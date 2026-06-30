//! Ersd → Ersd optimization. A façade over its submodules, exposing one
//! entrypoint that runs the whole pipeline (as [`crate::cont::optm`] does for
//! Cont):
//!
//! - [`prune`] — drops the items the entrypoint cannot reach, so only the
//!   program's actual slice is lowered.
//! - [`accumulate`] — re-bases `Nat`-summing self-recursion onto an inner
//!   accumulator so it becomes a tail-recursive loop downstream.
//! - [`offsets`] — re-bases buffer-walking self-recursion onto an integer
//!   cursor over the original buffer, so each step advances an offset instead
//!   of re-slicing the tail.

mod prune;

mod accumulate;

mod offsets;

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
    prune::prune_unreachable(module);

    // Re-base `Nat`-summing self-recursion (e.g. `Str/len`'s `… + 1`) onto an
    // inner accumulator so it becomes a tail-recursive loop downstream — O(1)
    // stack instead of a frame per element.
    accumulate::introduce_accumulators(module);

    // Re-base buffer-walking self-recursion (e.g. `Str/len`'s `count_w`) onto an
    // integer cursor over the original buffer, so each step advances an offset
    // instead of re-slicing the tail — O(1) per step instead of O(n).
    offsets::introduce_offsets(module);
}
