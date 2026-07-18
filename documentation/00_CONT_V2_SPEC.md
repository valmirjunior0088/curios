# Continuation IR v2 — remaining work

Working implementation specification for completing and landing Continuation IR v2. This document now records only unfinished work and the final acceptance gates. Durable representation contracts already implemented belong to `curios-cont` and `curios-ersd` rustdoc rather than here.

When every item below is complete, transfer any remaining durable facts to their owning source documentation, update `AGENTS.md`, reconcile the Cont baseline assumed by `01_ERSD_V2_SPEC.md` and `05_BOOTSTRAP_SPEC.md`, replace the linked roadmap item with a checked plain-text summary, verify that nothing references this filename, and delete this specification.

## Current boundary

The production path is already:

```text
optimized Ersd
  → Ersd-owned high-CPS lowering
  → CpsModule optimization
  → delayed closure conversion
  → private MachineModule CFG
  → private structured emission model
  → raw WebAssembly
```

The legacy region representation, optimizer, direct construction API, and Ersd lowering machinery have been removed. The public continuation types use innate `Cps…` names; private backend and emission types use innate `Machine…` and `Emission…` names. `Stage::Cont` and `Stage::ContOptm` observe deterministic high CPS.

## Fixed constraints

- Cont v2 remains a hard replacement. Do not restore a legacy namespace, selectable backend, compatibility facade, retained oracle, or deprecated construction API.
- Ersd, the runtime ABI, Wasm-GC storage, Wasmtime, the uniform `anyref` language ABI, list-map helper ABI, host imports, and pipeline stage names remain unchanged.
- `CpsFunction::return_cont` remains a freshly minted, globally unique `CpsContId` with no local-continuation body. Applying the current function's return continuation is a return; every other continuation target must resolve locally.
- `CpsNode::Exit` remains exclusive to direct program termination such as `IoExit`.
- Computed-only recursive cycles continue to report `CyclicRecComputed`; unsupported live synchronous recursive initializers continue to report `UnsupportedSyncRecItem`.
- Use deterministic maps and FIFO worklists. IDs are monotonic, never reused, and stored in tombstoned arenas.
- Do not add permanent metrics APIs or input-name-specific optimizer behavior. Performance investigations use revision worktrees and temporary instrumentation that is removed after answering the question.

## 1. Complete high-CPS optimization

The current optimizer handles local rewiring, continuation forwarding and beta reduction, literal folding, known switches, effect-aware dead bindings, aggregate projection forwarding, known-callee propagation, dead parameters, bounded local inlining, contification, recursive-SCC known-argument propagation and specialization, call-pattern specialization, and reachability. The interprocedural known-argument, specialization, contification, and branch-specialization work is landed; the residual parity port and the deeper recursive-initialization cleanup remain, both gated on the §5 baseline comparison whose pre-v2 revision they also need.

### SCC-wide known arguments and specialization

Landed. Function SCCs are computed at an explicit phase boundary, each parameter is classified `Unknown | Known(CpsAtom) | Conflict`, known values propagate through complete recursive SCCs, and a complete SCC is cloned for one disagreeing call context, memoized deterministically and bounded by the enforced limits of 64 specialized clones per module and 256 live CPS nodes per clone. Recursive dead-parameter elimination runs after specialization and rewrites every direct call consistently. The under-budget, at-budget, over-budget, conflict, and deterministic-output tests exist.

### General contification and bounded inlining

Landed for the cases the CPS stage owns. Non-escaping known-call functions with one external call site are contified into a local continuation, covering single-entry recursive loops and non-recursive join points, and escaping, multi-return-context, or otherwise incompatible functions are rejected. Bounded inlining enforces the multi-site limit of 8 live nodes, and SpecConstr-style branch specialization clones a callee per known tagged-tuple call pattern under the enforced growth limit of 24 live nodes, memoizing equivalent patterns to one clone and threading the constructor's dynamic fields so the existing projection and known-switch folds collapse the deconstruction. Recursive local-continuation cloning stays structural and name-independent, with its synthetic regression retained. Contification-rejection and growth-budget tests exist. Multi-site contification by common-dominator placement is deliberately left to the machine-CFG structuring, not the CPS stage.

### Residual optimization parity

Deferred to the §5 baseline comparison, which stands up the committed pre-v2 revision this work also needs. Using that revision in a separate worktree, determine which removed passes still materially affect representative programs and port only transformations whose absence produces a demonstrated structural, compile-time, code-size, or runtime regression. The candidates are:

- common-subexpression elimination for total, non-allocating primitives;
- bounded pure-call evaluation with 10,000 steps and depth 256 (its `PURE_EVALUATION_*` limits are already defined, awaiting this pass);
- deterministic literal hoisting;
- tag and callee threading;
- loop-invariant code motion for total, non-allocating expressions;
- list-map simplification;
- packed/list slice and window forwarding.

Each retained transformation needs a high-CPS test for its semantic preconditions, trap/effect boundary, and deterministic output. Remove an item from this list only after a targeted comparison shows it is redundant in the new pipeline or after it is implemented and tested.

### Recursive initialization cleanup

The common case is landed: a `RecInit` whose members no longer capture a computed value dissolves to an ordinary `LetFun`, a surviving bidirectional mixed knot is retained, and the ordinary rewrites already reach `RecInit` bodies through the flat node arena, all with tests. The remaining generalizations — pruning dead `RecInit` members so a knot whose only capturing member died dissolves, and values-first dissolution when the computed values do not depend on the members — are deferred to the §5 baseline: no representative program currently produces a non-dissolving-but-breakable knot, so they follow the same evidence-gated rule as the residual parity port. Rejecting a function-only SCC that reaches fallback lowering remains a machine-lowering invariant, already verified there.

## 2. Finish closure and machine lowering quality

Closure materialization reuse and explicit control-flow structuring are landed. One escaping-closure materialization is reused within its lexical scope, and direct self, sibling, and external calls stay direct even when the same function also has an escaping wrapper; shells remain limited to escaping closures in a residual mixed initialization knot. The emitter recovers control structure by strongly-connected-component condensation over the emission region graph rather than emission-tree pattern recognition: acyclic control uses forward `block`/`br`/`br_table`, a reducible single-entry component becomes a `loop` (nested for nested loops), and an irreducible or multi-entry component receives exactly one localized `br_table` dispatcher while ordinary reducible functions receive none. The condensation subsumes the machine-CFG dominator, reverse-postorder, and critical-edge-splitting analyses the earlier plan listed; none of those separate passes are part of the landed design. The `structure` module carries region-CFG construction, SCC condensation, and layout, with unit fixtures for acyclic layout, single and nested loops, and an irreducible dispatcher.

### Backend-local cleanup

After CFG correctness is stable:

- coalesce compatible machine locals;
- stackify single-use total expressions;
- do not move trapping, allocating, call-like, host, cell, or termination operations across sequencing boundaries.

Treat these as required only if the revision comparison or final structural/benchmark gates show a material regression without them.

## 3. Restore complete test ownership

Backend test ownership is restored without reviving the old construction API. Private `curios-cont` fixtures build a `CpsModule`, lower it with `into_wasm`, and assert the emitted wasm shape — never generated IDs or exact bytes — covering primitive lowering (Nat, Int, Flt, packed `Bin`, lists, tuples, cells, foreign calls, and list map), rope construction/slice/read/equality/force/embed/helper-ABI, deep-chain compile-time stack safety, foreign result arities, unknown-callee closure dispatch, block-scope and closure-construction verifier failures, function-only fallback rejection, and localized dispatch. Closure reuse and residual `RecInit` retain their machine-lowering fixtures. End-to-end semantics stay in the native `.crs` corpus, kept as a gate rather than a substitute for that backend-unit coverage.

## 4. Structural acceptance fixtures

Add small, stable source fixtures and raw-Wasm inspection for the following gates.

### LCG

- The optimized kernel reaches closure conversion as a single-entry recursive continuation.
- Raw Wasm contains one natural loop for the hot kernel.
- The hot backedge has no dispatcher selector update.
- The loop contains direct scalar arithmetic, no closure allocation, and no indirect call.

### Trees

- Build and sum retain direct recursive code.
- Invariant arithmetic implementations propagate through the complete recursive SCC.
- Hot arithmetic contains no indirect calls.
- Ordinary recursive functions create no shells or mutable closure fields.

### General corpus

- Unknown higher-order calls retain the closure ABI and emit `call_ref`.
- Direct and escaping uses of the same function work together.
- Function-only recursion produces no fallback shells.
- Ordinary corpus cases use no irreducible fallback.
- The deliberate irreducible fixture uses one localized dispatcher.
- Raw Wasm validates and executes without Binaryen repairing control flow.

## 5. Baseline and performance comparison

The native comparison is complete and recorded below. The one remaining item is the single Docker correctness and benchmark run.

### Baseline revision

The committed pre-v2 baseline is `ec55850e` ("Link README to hosted documentation"), the parent of `30461f25` ("Advance continuation IR v2"): the first commit that adds `curios-cont/src/cps.rs` and `machine.rs` and removes the legacy `curios-cont/src/module.rs` and `optimize/*` region optimizer. It is the last revision with the full legacy Cont pipeline intact, and it shares the current `Stage::NAMES` and `profile` bin, so the measurement surface is identical to the compared v2 revision `c62c2fb`.

### Native comparison result

Six representative probes (LCG, trees, higher-order, mutual recursion, deep-let, BigNat/formatting), each runtime-tainted, compiled through the real prelude on both revisions in separate worktrees. All six outputs were identical on both revisions, and the LCG and trees anchors verified. Measurement used the emitted raw WAT, `curios_wasm::to_bytes`/`curios_binaryen::optimize` byte sizes, the `profile` bin, and native-executable wall-clock — no permanent metrics surface and no retained instrumentation.

v2 improves every output-quality axis on every probe:

- Raw Wasm size −16% to −42%; post-Binaryen size −34% to −63%.
- Indirect calls collapse (LCG `call_ref` 141 → 8; BigNat 65 → 0); mutual recursion, deep-let, and BigNat become fully first-order.
- Closure allocations drop sharply (BigNat 23 → 0); zero irreducible dispatchers and zero mutable closure shells on any probe, including the RecInit-heavy BigNat and mutual recursion.
- Runtime is faster: LCG −6%, trees −41%.

The one regression is compile time on recursion-heavy programs: v2 is ~3x slower to compile LCG and trees (781 ms vs 265 ms; 828 ms vs 263 ms release, min-of-4) and ~1.3–1.4x on deep-let and BigNat, with higher-order and mutual recursion at parity. The cost is concentrated entirely in the CPS optimizer (≈560 ms of LCG's 794 ms compile). It is accepted: Cont v2's objective is output quality, the compile remains sub-second, and no deferred optimization pass would reduce it. It is orthogonal to the decision below.

### Comparison decision

The residual-parity port (§1.3), the deep `RecInit` cleanup (§1.4), and the backend-local cleanup (§2) are all **not warranted** and are closed:

- **§1.3 residual parity.** The baseline ran all seven candidate passes — common-subexpression elimination, bounded pure-call evaluation, literal hoisting, tag and callee threading, loop-invariant motion, list-map simplification, and slice/window forwarding — plus function inlining and call specialization, and still produced larger, slower, more-indirect code than v2. None of the seven produces a demonstrated structural, code-size, compile-time, or runtime advantage the new pipeline lacks; they are redundant here and are removed from the plan.
- **§1.4 deep `RecInit` cleanup.** No probe, including the RecInit-heavy BigNat and mutual recursion, produced a non-dissolving-but-breakable knot, an extra shell, or a mutable closure field. There is no regression for the deferred generalizations to fix.
- **§2 backend-local cleanup.** v2's post-Binaryen code is far smaller than the baseline's, and Binaryen performs local coalescing and stackification in the native path. No size or shape regression appears without a separate CPS-stage pass.

### Remaining

- Run the full Docker correctness and benchmark suite once, only after implementation and all other landing gates are complete. Verify output anchors before accepting timings.

## 6. Documentation and repository retirement

- Remove stale references to the deleted Cont region optimizer and old names, including the `curios-base` suffix-view documentation.
- Expand owning `curios-cont` and `curios-ersd` rustdoc where a remaining invariant currently exists only here.
- Update `AGENTS.md` only with durable ownership and architecture facts.
- Reconcile `01_ERSD_V2_SPEC.md` with the actual `Cps…` input interface and remove its dependency on this filename.
- Update the bootstrap representation baseline in `05_BOOTSTRAP_SPEC.md` to name the landed CPS, machine, and emission boundaries.
- Search the complete repository for the removed region representation, preallocation names, old Cont construction types, legacy imports, obsolete adapters, and old direct construction APIs. Every remaining occurrence must be unrelated terminology or removed.
- Confirm no production or test code can invoke the old implementation.
- Replace the roadmap's linked unchecked item with a checked plain-text summary.
- Delete this specification in the same final documentation update.

## Final validation

Run the repository gates in this order after all implementation and documentation work is complete:

1. `make curios/runtime`
2. `cargo fmt --all -- --check`
3. `cargo check --workspace --all-targets --all-features`
4. `RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features`
5. `cargo test --workspace --all-targets --all-features`, deliberately in the background with output redirected to a file
6. `make curios/web` with the `wasm-bindgen-cli` version exactly matching `Cargo.lock`
7. the one final Docker correctness and benchmark run
8. the final repository legacy/API search

Do not mark Cont v2 complete because focused tests pass or because the legacy source has been deleted. Completion requires the structural fixtures, replacement backend coverage, comparison decision record, documentation retirement, and every applicable final gate.

## Out of scope

- Ersd v2 or changes to the Ersd representation beyond `into_cont`.
- A new closure ABI or shared environments.
- Perceus, reference counting, linear memory, or another allocation model.
- A full general Relooper unless ordinary Curios programs demonstrate that localized irreducible fallback is insufficient.
- Aggressive cross-block stackification.
- Profile-guided optimization.
- Primitive-alphabet cleanup.
- Changes to the runtime, Wasmtime, host ABI, Wasm-GC storage, or uniform language-value signatures.

## Done bar

Cont v2 is complete only when:

- recursive SCC known-argument propagation, specialization budgets, general contification, and every retained parity optimization are implemented and tested;
- closure reuse and the explicit CFG analysis/structuring work pass their focused fixtures;
- every deleted backend fixture has equivalent new ownership;
- LCG, trees, higher-order calls, ordinary recursion, mixed recursion, host/runtime behavior, and localized dispatch satisfy the structural and semantic gates;
- the committed baseline comparison is recorded and the single final Docker run passes;
- all repository validation gates pass with warnings denied;
- no legacy Cont implementation, adapter, compatibility API, obsolete name, or stale documentation remains;
- durable contracts live in owning rustdoc and repository documentation;
- the roadmap is checked and this working specification is deleted.
