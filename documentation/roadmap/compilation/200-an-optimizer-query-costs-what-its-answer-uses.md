# An optimizer query costs what its answer uses

**Not refined yet.** This specification reserves two places where a question the continuation optimizer asks pays for more than its answer reads: a call analysis every reader builds whole, where each consumes some of its facts, and a copy plan that enumerates a binding group's bodies before it can decline at its allowance. It is not an implementation plan. The call analysis waits on the builders under *Refinement*, what each reader pays for it being attributed under *What is known*, and the copy plan waits for a consumer: it is the documented qualification of [bounded copy admission](../../../curios-cont/README.md#a-copy-is-planned-within-its-allowance), not a violation of its copied-node ceiling.

## What is missing

**Every reader builds the whole call analysis.** `origins` (`curios-cont/src/origin.rs`), `row_facts` (`row_flow.rs`) and `return_protocols` (`optimize/protocol.rs`) call `analyze_calls` and consume only `escaping`. The analysis (`analysis.rs`) also builds incoming sites, node owners, the body-only call graph and its strongly connected components, recursion closed under nested definitions and applied function references, and every function's lexical scope. Worker splitting and returned-closure uncurrying consume sites and escapes but pay for the other facts too; contification and loop-entry separation need the body graph, sites, owners and escapes, while the inliner needs the closed recursion verdict but no lexical scopes. Known-value propagation and the invariant solving over components still need their richer facts. These unused results are allocated and traversed on every query, including through several solvers in one optimizer round.

**A bounded copy enumerates before it declines.** `CopyPlan::limited` (`curios-cont/src/optimize/clone.rs`) checks the distinct-node allowance when a pending body is popped, but a visited `LetFun` first reads every defined member, inserts its identity in `nested` and queues its body; `LetCont` likewise enumerates and queues its complete group. A request with an allowance of sixteen can therefore enumerate and queue thousands of members before its seventeenth distinct node refuses the copy, and repeated call sites repeat that work. Synthetic wide-group controls establish the shape; no current consumer is known to supply wide groups often enough for it to matter, and the copied-node allowance does not price group enumeration.

## What is known

- **What each reader pays for the analysis.** Mixed, at `f0712f8e4`, over `fixpoint_pass_measurements`' `TOML_DRIVER` (`curios/src/tests/instrumented/fixpoint.rs`) in a scratch copy built with every feature and a `profile!` span around each reader's `analyze_calls`: one warm-up and seven readings, each time a median with its span, in a profiling build whose timings include its tracing and its allocation counter. A compilation builds the analysis 143 times, counted, about 5 ms each, and the builds take 757.5 ms [755.8–767.9] of `cont_optimize`'s 1,655.4 ms [1,651.5–1,674.4], about 46%: the inliner 115.1 ms over 21 builds, `origins` 113.5 over 22, contification and loop-entry separation 110.4 over 22, the shrinker's recursion verdict 69.2 over 11, known-value propagation 68.5 over 11, `row_facts` 57.5 over 12, uncurrying 57.3, worker splitting 57.0, `return_protocols` 56.3 and component specialization 55.3 over 11 each. The three readers that consume `escaping` alone take 227.3 ms [224.9–230.3], about 14%, which is the most an escape-only query saves there, less what the query costs; each reader's demand and flow solvers stand apart from these spans.
- **What a narrower builder keeps.** The two notions of recursion, the lexical scopes the verifier derives, and the treatment of nodes a round orphans: a flat scan of the live arena would count references the walk over owning bodies excludes. Dead-parameter cleanup's arena-wide escape snapshot (`optimize/dead_parameters.rs`) and its continuation-before-function phase boundary remain its own contract, not a replacement for that body-owned query.
- **What it does not reopen.** [A pass applies the independent plans its snapshot admits](../../../curios-cont/README.md#a-pass-applies-the-independent-plans-its-snapshot-admits) rejects a general retained cache.

## Refinement

### The call analysis

- **The builders**: phase-local builders for the facts each reader needs, beginning with an escape-only query over the same owning function bodies, with no general analysis cache retained.
- **Check**: direct escape comparisons with the existing analysis on nested and orphaned references; the current recursion, scope and snapshot-deferral tests; unchanged optimized Cont and both Wasm artifacts over the fixpoint measurement's consumer cohort; and attributed analysis counts and default-release compilation cost.

### The bounded copy

- **The consumer.** It opens when attribution names a program whose compilation pays for this.
- **The direction.** Group bodies enumerated incrementally in the shared planner, keeping complete successful extents, deduplication and fresh-plan execution; no second fuel limit, and no change to which copies fit.
- **Check**: counted group-member enumeration and pending work over doubling groups, identical exact-limit plans and nested captures, and unchanged consumer artifacts and scheduling.
