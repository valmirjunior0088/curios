# An escape query builds the escapes alone

**Not refined yet.** This specification reserves a call analysis built for what each optimizer reader asks of it, where every reader builds the whole of it today. It is not an implementation plan; what the unused facts cost is to be attributed before its boundaries are chosen.

## What is missing

`origins` (`curios-cont/src/origin.rs`), `row_facts` (`row_flow.rs`) and `return_protocols` (`optimize/protocol.rs`) call `analyze_calls` and consume only `escaping`. The analysis (`analysis.rs`) also builds incoming sites, node owners, the body-only call graph and its strongly connected components, recursion closed under nested definitions and applied function references, and every function's lexical scope. Worker splitting and returned-closure uncurrying consume sites and escapes but pay for the other facts too; contification and loop-entry separation need the body graph, sites, owners and escapes, while the inliner needs the closed recursion verdict but no lexical scopes. Known-value propagation and the invariant solving over components still need their richer facts. These unused results are allocated and traversed on every query, including through several solvers in one optimizer round.

## What is known

- **Not attributed: the removable share of a consumer's compilation cost**, measured apart from its demand and flow solvers. Neither a whole pass's profile nor the full analysis's span separates the facts its reader needs from the ones it does not. The attributed analysis readings belong to `fixpoint_pass_measurements`'s comparison protocol (`curios/src/tests/instrumented/fixpoint.rs`).
- **What a narrower builder keeps.** The two notions of recursion, the lexical scopes the verifier derives, and the treatment of nodes a round orphans: a flat scan of the live arena would count references the walk over owning bodies excludes. Dead-parameter cleanup's arena-wide escape snapshot (`optimize/dead_parameters.rs`) and its continuation-before-function phase boundary remain its own contract, not a replacement for that body-owned query.
- **What it does not reopen.** [A pass applies the independent plans its snapshot admits](../../../curios-cont/README.md#a-pass-applies-the-independent-plans-its-snapshot-admits) rejects a general retained cache.

## Refinement

- **Attribution first**: what each reader's share of the analysis costs, by the fixpoint measurement's protocol.
- **Then the builders**: phase-local builders for the facts each reader needs, beginning with an escape-only query over the same owning function bodies, with no general analysis cache retained.
- **Check**: direct escape comparisons with the existing analysis on nested and orphaned references; the current recursion, scope and snapshot-deferral tests; unchanged optimized Cont and both Wasm artifacts over the fixpoint measurement's consumer cohort; and attributed analysis counts and default-release compilation cost.
