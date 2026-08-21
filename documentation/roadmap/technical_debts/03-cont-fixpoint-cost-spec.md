# The fixpoint costs more than everything it optimizes

## Status

Deliberately unrefined, and thinner than its siblings on purpose: the cost is measured and the cause is not located, because the instrumentation that would locate it does not exist yet. The figures are reproduced by `curios`' `combinator_sharing_measurements` and by the profiler command below. Nothing is started.

## Why it exists

`curios_cont::optimize` is **97.6% of an ordinary `Toml/decode` compile** — 4 719 ms of 4 836 ms — on a program with no unusual spelling anywhere in it. Every other stage is noise beside it: elaboration 6 ms, the kernel 8 ms, erasure 9 ms, the lowering into Cont 8 ms, wasm emission 39 ms, and the whole erased-arena optimizer 44 ms.

That share is not a symptom of anything else. [A reified closure is bound once, not copied per use](02-point-free-unfolding-spec.md) describes a spelling that hands this pass a larger module, and this pass is what converts that into fifteen minutes — but the 97.6% above is measured with none of that in play. Whatever a user writes, almost all of the compiler's time is here.

## Known for certain

- **The cost is super-quadratic in module size.** Over the four spellings and five sizes `combinator_sharing_measurements` walks, the emitted-function count grows 118 → 566 while the compile grows 0.54 s → 23.21 s: a 4.8× module for 43× the time, or about `size^2.4`. Within the linear-growth spellings the exponent is nearer 2.

- **It allocates enormously.** Under the profiler, a twelve-thousand-line module costs 15.2 GB across **223.8 M allocations** — roughly nineteen thousand allocations per line of IR — and the plain `Toml/decode` compile costs 2.9 GB across 42.6 M. That is the shape of whole-module analyses rebuilt per round rather than of a blowup inside any one rewrite, but the current spans cannot tell those apart.

- **It converges.** The `debug_assert!` on `ROUND_LIMIT` does not fire on any program measured here, including the ones that take twenty seconds. So this is not the backstop being reached; it is ordinary iteration being expensive.

- **The instrumentation tops out above the question.** `curios-cont` has exactly two spans — `cont_optimize` and `into_wasm` — for a fixpoint of twenty-two whole-module passes. The profiler can attribute the total and nothing inside it. This is why the cause is unlocated, and it is the first work item rather than a caveat.

## For comparison

`ROUND_LIMIT` is **1024**. GHC ships `-fmax-simplifier-iterations=4` and `-fsimplifier-phases=2`. The two designs are not the same shape — GHC's simplifier is one occurrence-analysis-guided pass where this is twenty-two independent rewrites driven to a joint fixpoint — so the numbers are not directly comparable, and the comparison is recorded because it is the obvious first question a reader will ask. The crate's own note on `ROUND_LIMIT` says 33 corpus programs stopped at the old limit of 32 and that raising it showed convergence anywhere up to 191 rounds, which says the round count is real work rather than churn, and says nothing about whether it must be.

## The shape a cure might take

Unknown, and deliberately so. Three hypotheses the measurements are consistent with and cannot separate:

- Round count scales with the program's dependency depth, so a chain of `n` definitions needs `O(n)` rounds to propagate — making the total `O(n)` rounds × `O(n)` module, which is the observed exponent.
- Per-round cost is superlinear because the analyses each round rebuilds (`known_values`, and whatever each pass derives) are not incremental.
- A pass pair does small amounts of mutual undoing that the fixpoint absorbs without reaching `ROUND_LIMIT`.

**The first work item is instrumentation, not a fix.** `curios-cont`'s module documentation already states the norm — performance there is investigated with revision worktrees and temporary instrumentation, never a permanent metrics API — so a temporary span per pass, plus a round counter, is the sanctioned route and is what turns the three hypotheses into one answer.

## Deliberately not specified

Whether the answer is incrementality, a worklist over changed nodes instead of whole-module rounds, a pass ordering that converges faster, or a lower round limit with a measured loss. Whether `into_wasm`'s own cost — 39 ms against the fixpoint's 4 719 ms on the same program — stays negligible at every size. And whether any of this is worth doing before [the point-free spelling stops handing it four times the module](02-point-free-unfolding-spec.md), which is a cheaper lever on the same product.

## How to take the figures

```sh
cargo test --release --package curios --lib -- --ignored --nocapture combinator_sharing_measurements
```

for the size-against-time series, and — writing `grammar(16, Inner::InBlock)` from that probe to a file, or using any ordinary program —

```sh
make curios/profile CURIOS_PROFILE_SOURCE=<file>
```

for the attribution and the allocation counts. Both taken **2026-08-21**, **release**, `aarch64-apple-darwin`.
