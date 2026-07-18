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

The current optimizer handles local rewiring, continuation forwarding and beta reduction, literal folding, known switches, effect-aware dead bindings, aggregate projection forwarding, basic known-callee propagation, dead parameters, bounded local inlining, self-tail contification, and reachability. The following interprocedural and parity work remains.

### SCC-wide known arguments and specialization

- Compute function SCCs at an explicit phase boundary.
- Classify each parameter across all entries as `Unknown | Known(CpsAtom) | Conflict`.
- Propagate known values through complete recursive SCCs rather than skipping recursive functions.
- Specialize a complete SCC when one call context supplies useful known arguments; never clone only part of a recursive SCC.
- Memoize specializations deterministically and enforce both budgets:
  - at most 64 specialized clones per module;
  - at most 256 live CPS nodes per clone.
- Run recursive dead-parameter elimination after specialization and rewrite every direct call consistently.
- Add focused under-budget, at-budget, over-budget, conflict, and deterministic-output tests.

The existing SCC and pure-evaluation constants must become enforced limits rather than an inert tuple retained only to silence unused warnings.

### General contification and bounded inlining

- Generalize contification beyond the current self-recursive case to non-escaping known-call functions with one compatible return context.
- Preserve single-entry recursive loops and reject escaping, multi-return-context, or otherwise incompatible functions.
- Apply the multi-site inline limit of 8 live nodes and the branch-specialization growth limit of 24 live nodes.
- Memoize specialized inline bodies so repeated equivalent sites do not clone independently.
- Keep recursive local-continuation cloning structural and name-independent; retain the synthetic regression that exercises it.
- Add focused contification rejection and code-growth-budget tests.

### Residual optimization parity

Use a committed pre-v2 revision in a separate worktree to determine which removed passes still materially affect representative programs. Port only transformations whose absence produces a demonstrated structural, compile-time, code-size, or runtime regression. The remaining candidates are:

- common-subexpression elimination for total, non-allocating primitives;
- bounded pure-call evaluation with 10,000 steps and depth 256;
- deterministic literal hoisting;
- tag and callee threading;
- loop-invariant code motion for total, non-allocating expressions;
- list-map simplification;
- packed/list slice and window forwarding.

Each retained transformation needs a high-CPS test for its semantic preconditions, trap/effect boundary, and deterministic output. Remove an item from this list only after a targeted comparison shows it is redundant in the new pipeline or after it is implemented and tested.

### Recursive initialization cleanup

- Optimize inside `CpsNode::RecInit` using the same safe rewrites as ordinary bodies.
- Dissolve `RecInit` when dead members, aliases, specialization, or inlining break the mixed initialization knot.
- Retain fallback only for a surviving bidirectional mixed knot.
- Reject any function-only SCC that reaches fallback lowering.

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

- Identify and record the committed pre-v2 revision used as the comparison baseline.
- Reproduce representative LCG, trees, ordinary and mixed recursion, higher-order calls, host arities, cells, Nat induction, formatting, BigNat, and deep-let cases in revision worktrees.
- Compare compiler time, peak memory where practical, raw and post-Binaryen Wasm size, direct/indirect calls, closure allocation, fallback shells, loop shape, and runtime.
- Add temporary counters only for a concrete comparison question and remove them immediately afterward.
- Do not add a permanent metrics surface merely to satisfy this working specification.
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
