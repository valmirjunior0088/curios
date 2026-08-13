# You should pay for a value when you keep it, not when you name it

## Status

This specification defines the cost contract, evidence gate, design boundaries and acceptance criteria for eliminating heap objects whose fields can travel through compiler-controlled control flow without a materialized identity.

The work belongs in `curios-cont`: erasure has exposed the runtime fields, CPS still knows continuations and direct callees, and the transformation can feed projection forwarding, simplification, dead-parameter elimination, specialization and raw-scalar representation before WebAssembly emission loses those facts.

## Cost contract

An immutable aggregate does not require a heap object while every observable use can be served from its fields and every transfer stays within control flow whose representation Curios can coordinate.

Crossing a continuation edge, loop backedge or known recursive call is not by itself an escape.

A value requires materialization when an operation demands the complete runtime reference: an unknown or indirect call, a foreign or cell boundary, closure capture, storage in another heap object, a return through an unsplit interface, or any other use whose consumer is not visible to the analysis.

The optimization may rebuild the aggregate at such a boundary, so one opaque use need not force allocation along paths that remain field-only.

The contract applies to immutable products and representation-aware windows that the optimizer can describe exactly; it does not promise that arbitrary extensionally equivalent programs or encodings have equal cost.

In particular, Curios does not promise that the induction and explicit-recursion spellings of every dependently typed left fold compile identically: they may differ in closure construction, traversal order, call visibility and specialization opportunities that aggregate lifetime alone cannot erase.

## Evidence

The string-walk campaign left two heap objects in the per-character path of `/std/Str/fold`: the suffix view produced by `match b | x[h, ..t]` and the `{A, Nat}` accumulator used to carry the result and partial codepoint.

Neither value is confined to the iteration that constructs it: both are passed to the recursive `go`, so the relevant region is the visible recursive function and its backedges rather than one lexical iteration.

The measurements, commands and structural counts live in `curios/src/tests/codegen/ladder.rs` and `curios/src/tests/codegen/structural.rs`; this document records what they selected, not figures that can drift away from their probes.

The unused tier is already settled in `curios-ersd/src/into_cont.rs`: lowering declines to emit a fold suffix that the step never reads, because only lowering knows that this normally trapping slice is in range.

This specification concerns the middle tier, where the value is used but no visible consumer needs the object itself.

## Existing substrate

`forward_aggregate_projections` already replaces a projection from a visible tuple construction with the corresponding field, after which dead-binding elimination removes the construction.

The shared demand lattice already distinguishes projection-only use from opaque use, but passing a value as an argument is deliberately opaque today; propagating demand through the callee parameter is the required interprocedural strengthening.

The return protocol already coordinates multi-value results over tail-call-connected function components, and call-pattern specialization already uses worker/wrapper-shaped clones that thread dynamic tuple fields through parameters.

`represent.rs` decides raw representation for locals only, correctly refusing unilateral decisions that cross a signature; this work must coordinate every rewritten caller and callee or retain a wrapper speaking the original boxed ABI.

Curios therefore has several purpose-specific notions of demand, function escape and boundary crossing, but no general aggregate-flow fact that follows a construction through continuation and known-function parameters.

## Binaryen is a control, not the owner

The native compiler runs Binaryen at optimization level two, whose GC pipeline includes `heap2local`.

That pass replaces a heap allocation with one local per field when the allocation remains inside one WebAssembly function and flows exclusively from that allocation.

It cannot coordinate Curios function signatures, treats calls as escapes, sees a rope view only after the shared slice helper has returned it, and does not run in the browser compiler path.

The Curios pass must therefore prove its value on raw pre-Binaryen output; Binaryen remains a downstream optimizer and a comparison that reveals duplicated work, never an acceptance dependency.

## Adopted precedents

[Binaryen Heap2Local](https://github.com/WebAssembly/binaryen/blob/version_130/src/passes/Heap2Local.cpp) supplies the conservative safety baseline: require nonescape and exclusive flow, turn one object into one value per field, and decline mixed origins until the representation explicitly carries their distinction.

[GHC worker/wrapper](https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-9.15-inplace/src/GHC.Core.Opt.WorkWrap.Utils.html) supplies the signature pattern: an internal worker receives unboxed product fields, a wrapper preserves the public boxed interface, and constructed product results travel through an existing multi-result protocol.

[OCaml Flambda's unboxing of specialized arguments](https://ocaml.org/manual/4.04/flambda.html#s%3Aflambda-unboxing-specialised-args) supplies the recursive policy: propagate field arguments through a recursive group, retain wrappers at other entries, bound signature growth, and refuse a transformation that would inhibit tail calls.

[Lean 4's functional-but-in-place runtime](https://lean-lang.org/papers/lean4.pdf) solves a neighboring problem by reusing uniquely owned objects under exact reference counting; Curios does not adopt that mechanism here because tracing Wasm GC provides neither the ownership fact nor a dying object to reuse for every virtual value.

[Agda's GHC backend](https://agda.readthedocs.io/en/latest/tools/compilers.html) and [Rocq's extraction to OCaml, Haskell and Scheme](https://rocq-prover.org/doc/V8.16.0/refman/addendum/extraction.html) delegate this class of runtime representation optimization to mature functional compilers, so their precedent reinforces the GHC and OCaml techniques rather than supplying a theorem-prover-specific replacement.

## Common model: virtual values and materialization

The analysis follows an exact aggregate origin through aliases and eligible transfers, carrying the fields that downstream demand reads rather than a heap reference.

An origin and every parameter carrying it form a virtual-value region.

Within that region, projections read fields directly and transfers pass those fields to rewritten parameters.

At an ineligible boundary, materialization constructs the physical value at that use and leaves the rest of the region virtual.

The first implementation may conservatively reject an entire candidate when partial materialization would require path-sensitive state, but the analysis and tests must distinguish that implementation limit from the cost contract.

Eligibility requires all of the following:

- The origin has an exact immutable shape known in CPS.

- Every transfer is a continuation edge or known direct call whose receiving parameters can be rewritten consistently.

- Every field demanded inside the region is available on every incoming edge.

- Merged flows are exclusive: a parameter cannot sometimes receive the candidate and sometimes an unrelated physical value unless the rewrite explicitly carries and handles that distinction.

- Rewriting a function component preserves valid tail calls and keeps every shared continuation arity consistent.

- Field and signature growth remain within a measured bound.

An unknown call, foreign call, cell operation, closure capture, heap store or unsplit return is a materialization boundary rather than evidence that the original construction should necessarily allocate earlier.

Nested products may be exposed one level at a time and reconsidered by the optimizer fixpoint; recursive flattening must stop at the same growth bound and must converge without depending on the global round limit.

## Product scalar replacement

The first capability targets explicit `CpsValueExpr::Tuple` origins.

### Continuation parameters

When a tuple flows through continuation parameters and all consumers project fields or transfer it onward, replace the aggregate parameter with its demanded fields and rewrite every incoming jump.

This includes loop headers and backedges: loop-carried state is the central case, not an escape to exclude.

Only demanded fields need be threaded, so the transformation can expose newly dead fields and parameters to the existing optimizer.

### Known function parameters

When the same flow crosses known direct function calls, create or select a worker signature carrying the demanded fields.

Every member of the recursive argument-flow component that transfers the value must be rewritten consistently, and every direct call must supply the one parameter representation selected for its target.

The rewrite must preserve existing tail calls; result shapes and the tail-call-connected components that constrain them remain governed by the return protocol.

If the function escapes or retains callers that need the boxed ABI, keep a wrapper accepting the original aggregate, project its fields once, and direct eligible known calls to the worker.

Opaque recursive calls, indirect calls and host-visible functions remain on the boxed interface.

The `/std/Str/fold` accumulator is the motivating acceptance case: its `A` field may remain a reference while its `Nat` field becomes eligible for raw scalar storage, and the `{A, Nat}` object must disappear from the recursive path without relying on inlining or Binaryen.

### Results

Existing return-protocol splitting remains the owner of aggregate results that cross calls.

Product scalar replacement may expose constructions or projection-only demands that make an existing protocol eligible, but it must not introduce a competing result ABI or independently rewrite tail-call components.

## Rope-window virtualization

Rope slices share the virtual-value cost contract but are not tuple constructions and must be implemented as a separate representation-aware capability after product scalar replacement.

The emitted slice helper eagerly checks bounds, returns a fresh empty leaf for an empty window, aliases the input for a whole window, collapses a view of a view, forces and memoizes an uncached base, and otherwise allocates a physical view whose fields are tag, length, base and offset.

Only physical materialization may disappear; the bounds trap must remain at the original evaluation point, and base forcing and memoization must retain their order and effect.

The optimizer-facing form must therefore separate window preparation from physical materialization.

Preparation produces a stable virtual base, offset and length after performing every eager semantic obligation of the slice.

`len`, `get` and further `slice` consumers operate on those fields, with further slices composing offsets and performing their own bounds check against the virtual length.

An opaque consumer materializes the same representation choice that the existing helper would provide for the corresponding empty, whole or proper window case.

A backend-only peephole is rejected as the final design because it cannot expose the fields to CPS optimization, cannot coordinate recursive parameters, and would leave the browser and native optimization stories different.

## Measurement gate

No implementation mechanism or growth limit is accepted from intuition alone.

The first milestone is a corpus survey over optimized CPS that classifies candidate tuple constructions and slice results by direct projection, continuation transfer, known-function transfer, return, closure capture, heap storage, unknown call and mixed flow.

The survey must report which candidates continuation-only splitting reaches, which additionally require known-function workers, and which remain blocked after both.

The string ladder must attribute the accumulator tuple and suffix view separately using an instrument or isolated transformation for each, rather than deriving their shares from the number of operations in the loop.

Raw pre-Binaryen and optimized native artifacts must be compared so the work distinguishes an upstream optimization unlock from an allocation Binaryen already removes, and the raw browser path must remain represented in the acceptance fixtures.

Any field-count, signature-growth or clone budget must be selected from that survey and recorded beside the test or instrument that justifies it.

Evidence that would stop the campaign is a survey showing that eligible allocations are rare outside the motivating library code, or isolated measurements showing that both motivating allocations account for too little of the remaining cost to justify signature and IR complexity.

## Milestones

### M0 — Survey and instruments

- Add the aggregate-flow census and its reproducible corpus fixture.

- Add isolated attribution for the accumulator tuple and suffix view beside the string ladder.

- Record raw-versus-Binaryen structural deltas and choose the growth policy from the observed candidates.

### M1 — Aggregate-flow fact

- Strengthen demand from a syntactic scan into a fixpoint that can relate eligible arguments to receiving parameters without treating every transfer as opaque.

- Classify materialization boundaries, exclusive and mixed flows, recursive components and demanded field sets.

- Prove the fact with focused tests before it moves code.

### M2 — Continuation scalar replacement

- Rewrite projection-only tuple state through continuation parameters, including loop backedges.

- Feed the exposed fields back through the existing optimizer fixpoint and prove convergence independently of the round limit.

- Retain or decline candidates at mixed and opaque boundaries according to the first implementation's documented limit.

### M3 — Known-function workers

- Extend the same field flow through direct function calls and recursive components.

- Preserve boxed wrappers wherever the original ABI remains reachable.

- Preserve tail calls, continuation arities and the existing return-protocol ownership boundary.

- Remove the `/std/Str/fold` accumulator allocation in raw pre-Binaryen output.

### M4 — Prepared rope windows

- Introduce the optimizer-visible prepared-window form without weakening slice traps or memoization effects.

- Teach length, indexed read and further slicing to consume virtual windows.

- Materialize at opaque boundaries and remove the recursive suffix-view allocation in raw pre-Binaryen output.

### M5 — Cost contract

- Move the accepted durable cost-model decision to `documentation/DESIGN.md` once the measured implementation establishes its actual envelope.

- Retire transient measurements into their owning probes and update the roadmap with the capability and its explicit limits.

## Acceptance

The campaign is complete only when all of the following hold:

- A focused CPS fixture carries a product through a continuation loop and emits fields without the product allocation.

- A focused CPS fixture carries a product through a known recursive or mutually recursive function component and emits a worker path without the product allocation.

- Escaping and indirectly called functions retain a correct boxed wrapper, while eligible known calls use the worker.

- Mixed-origin flows either materialize correctly or are conservatively declined, with tests distinguishing the two cases.

- Tail-recursive fixtures remain tail calls after signature rewriting, and shared continuation arities remain valid.

- The `Nat` member of a split product can reach raw scalar representation when its uses permit it.

- `/std/Str/fold` emits neither the per-character accumulator tuple nor the proper suffix-view object on the raw pre-Binaryen recursive path.

- Empty, whole, nested, uncached and out-of-bounds slice fixtures preserve the existing result, eager trap and memoization behavior.

- Raw and Binaryen-optimized native execution agree, and the browser compiler accepts and executes the raw module without depending on a postpass.

- The isolated probes show the contribution of each removed allocation and keep every figure beside the command that reproduces it.

- The corpus survey demonstrates that the accepted mechanism reaches enough non-library or independently shaped candidates to justify remaining general compiler machinery rather than a `/std/Str/fold` peephole.

## Refused alternatives

**Rely on Binaryen.** It is too late to unlock Curios optimization, cannot rewrite Curios call protocols, does not cross helper calls and is absent from the browser path.

**Stack-allocate the object without exposing its fields.** Wasm GC has no stack struct representation, and retaining an opaque object would leave projections, fields and downstream simplifications hidden.

**Inline until the allocation becomes local.** Inlining may help Binaryen but duplicates code, is bounded independently, and does not provide a cost contract for recursive or escaping definitions.

**Special-case `/std/Str/fold`.** It would repair one spelling while leaving the representation problem and other loop-carried aggregates intact.

**Adopt reference-counted reset/reuse as this mechanism.** Reusing a uniquely owned dying object solves persistent reconstruction, not the absence of a materialized identity, and Curios targets tracing Wasm GC.

**Promise equal cost for all dependent fold encodings.** Aggregate lifetime cannot erase arbitrary differences in higher-order control flow, traversal and specialization, so the promise would exceed both the analysis and any testable compiler envelope.
