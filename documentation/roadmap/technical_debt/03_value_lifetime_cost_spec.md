# You should pay for a value when you keep it, not when you name it

## Status

This specification defines the cost contract, evidence gate, design boundaries and acceptance criteria for eliminating heap objects whose fields can travel through compiler-controlled control flow without a materialized identity.

The work belongs in `curios-cont`: erasure has exposed the runtime fields, CPS still knows continuations and direct callees, and the transformation can feed projection forwarding, simplification, dead-parameter elimination, specialization and raw-scalar representation before WebAssembly emission loses those facts.

M0 is complete (2026-08-17): the census, attribution and extent instruments are in `curios/src/tests/codegen/`, the multi-byte fixture and the walk-mirror family in `programs/`, and the stopping evidence was not met — the accumulator and the returned scan each measure about a fifth of the walk. The scan argument reconstruction turned out to be a source spelling rather than a compiler obligation and was cured in `/std` during M0, which retires M3.

M1a is complete (2026-08-17): the deferral is one rule under the shared solver, `split_returns` delivers `/syn/Str/step`'s result as four fields — unlocked by reworking `/std/Str/utf8/check` to thread its scan by recursion — and the relocated caller-side rebuild is measured and accepted as the intermediate M2 removes where the shape allows.

M1b and M2 are complete (2026-08-17): the forward origin analysis, the recorded fields representation with its verifier rule, and continuation scalar replacement are landed, the fold's accumulator travels as fields, the digit walk beats every figure the ladder ever recorded, and the multi-byte walk gains about thirteen percent against its pre-campaign baseline. The scan rebuild is recorded retained cost — a variant-width flow, per M2's verdict.

M4 is complete (2026-08-17): the suffix view is a virtual window — no per-character view, no helper call — carried as `(base, offset, length)` under the same recorded representation, with the eager bounds trap preserved by the `WindowExtent` guard. Against the pre-campaign baseline the digit walk is fifteen percent faster and the multi-byte walk twenty-three. The spine proceeds at M5.

## Cost contract

An immutable aggregate does not require a heap object while every observable use can be served from its fields and every transfer stays within control flow whose representation Curios can coordinate.

Crossing a continuation edge, loop backedge or known recursive call is not by itself an escape.

A value requires materialization when an operation demands the complete runtime reference: an unknown or indirect call, a foreign or cell boundary, closure capture, storage in another heap object, a return through an unsplit interface, or any other use whose consumer is not visible to the analysis.

The optimization may rebuild the aggregate at such a boundary, so one opaque use need not force allocation along paths that remain field-only.

The contract applies to immutable products and representation-aware windows that the optimizer can describe exactly; it does not promise that arbitrary extensionally equivalent programs or encodings have equal cost.

In particular, Curios does not promise that the induction and explicit-recursion spellings of every dependently typed left fold compile identically: they may differ in closure construction, traversal order, call visibility and specialization opportunities that aggregate lifetime alone cannot erase.

## Evidence

The string-walk campaign left four aggregate allocations in the per-character path of `/std/Str/fold`, spread across three source-level values and four optimizer obligations.

The suffix view is produced by `match b | x[h, ..t]`. The `{A, Nat}` accumulator carries the result and the partial codepoint. One `Scan` is constructed as the result of `step`, and another is rebuilt field by field from the loop's own parameter solely to be handed back to `step`.

The argument reconstruction was missed while the other sites were being counted, and how it was missed is worth keeping: the instrument reported the scan as *work* — a call to `step`, then `classify` — and an allocation handed **to** a call is not where a count of allocation sites looks. The existing ladder instrument counts only allocations emitted inside `/std/Str/fold`, so it also cannot by itself attribute the `Scan` construction inside `/syn/Str/step`.

The four obligations are also not uniform across inputs, and the corpus that found them cannot exercise them all. `Scan/lead()` and `Scan/bad()` are nullary and arrive as interned constants, and the argument reconstruction sits in the `cont` arm alone — so a pure-ASCII walk allocates only the suffix view and the accumulator per character, and both `Scan` obligations execute only while decoding multi-byte characters. The ladder's programs decode digit strings, which is why M0 below owes a multi-byte fixture beside them: on the existing corpus the two `Scan` shares are not small, they are unmeasured.

**M0 dissolved the fourth obligation at its source (2026-08-16).** The reconstruction was a spelling: the cont arm wrote `step(h, Scan/cont(rem, lo, hi))` with `sc` — the parameter holding exactly that value — in scope, and match refinement makes the two spellings definitionally equal, so `/std` now passes the held parameter at every such site (`fold`, `at`, `utf8/drop_width`, `utf8/count_scalars`, and their proof twins, kept in the same spelling so function and proof unfold alike). The kernel recertified the prelude over the change, the per-continuation-byte allocation left the walk, `len` and `slice` at once, and the multi-byte control measured it at about two percent of the walk (`curios/src/tests/codegen/ladder.rs`). Three obligations remain, and they are the spine's.

None of the remaining obligations is confined to the lexical expression that constructs it: each value crosses a call, continuation edge or loop backedge, so the relevant region is compiler-controlled control flow rather than one lexical iteration.

`go` is not a function by the time the optimizer sees it. It has exactly one external call site and its recursion is a tail call, so `contify_calls` turns it into a continuation, and the accumulator arrives as a continuation parameter while the scan state is an argument to a known call. That difference is not incidental to the spelling — it is what decides which capability below removes which object.

The measurements, commands and structural counts live in `curios/src/tests/codegen/ladder.rs` and `curios/src/tests/codegen/structural.rs`; this document records what they selected, not figures that can drift away from their probes.

The unused tier is already settled in `curios-ersd/src/into_cont.rs`: lowering declines to emit a fold suffix that the step never reads, because only lowering knows that this normally trapping slice is in range.

This specification concerns the middle tier, where the value is used but no visible consumer needs the object itself.

## Existing substrate

`forward_aggregate_projections` already replaces a projection from a visible tuple construction with the corresponding field, after which dead-binding elimination removes the construction.

The shared demand lattice already distinguishes projection-only use from opaque use, but passing a value as an argument is deliberately opaque today; propagating demand through the callee parameter is the required interprocedural strengthening.

**That strengthening pays before any new rewrite is written, but initially relocates an allocation rather than removing it.** `/syn/Str/step` returns a `Scan` whose every field the loop projects, but its result is passed onward as an argument and therefore reads as `Opaque`, which pins its component to the tuple protocol. Under a demand that defers to the receiving parameter the same component becomes `Fields(4)`, and `split_returns` — which already exists and already coordinates tail-call classes — becomes eligible on it. The callee can then return four fields without constructing the `Scan`, while the current caller resume reconstructs the tuple before entering its continuation; continuation scalar replacement removes that reconstruction in M2. Interprocedural demand is therefore a shipping capability and a prerequisite, not by itself the completed allocation removal.

The return protocol already coordinates multi-value results over tail-call-connected function components, and call-pattern specialization already uses worker/wrapper-shaped clones that thread dynamic tuple fields through parameters.

**Specialization already matches the scan-state reconstruction and declines it, on a budget rather than on a rule.** The rebuilt `Scan` carries a literal tag, `step` deconstructs the parameter it lands in, and every other condition holds; the callee exceeds the current `BRANCH_SPECIALIZATION_GROWTH_LIMIT`. That refusal is correct for the mechanism doing the asking — SpecConstr must duplicate the callee once per tag to thread its fields — and it is the clearest argument that a distinct mechanism is warranted, because rewriting the signature threads the same fields while cloning nothing. The exact extent and budget comparison live beside `step_specialization_extent` in `curios/src/tests/codegen/census.rs`: extent 37 against a limit of 24 at the time of taking, a refusal by less than a factor of two. A budget that declines a per-character allocation is evidence about the *mechanism*, not about the candidate.

`represent.rs` decides raw representation for locals only, correctly refusing unilateral decisions that cross a signature; this work must coordinate every rewritten caller and callee or retain a wrapper speaking the original boxed ABI.

**Raw storage for a split field is therefore two capabilities, not one.** A continuation parameter offers `Open` and can settle on a raw carrier the moment a use demands one, so splitting a tuple through continuations delivers raw fields for free. A function parameter offers `Never` — it arrives through a signature that is uniformly `anyref` — so a split field in a *worker* signature stays boxed until something decides that signature's layout, which is the decision `represent.rs` exists to refuse. Splitting a product and unboxing its fields must not be quoted as one outcome.

The demand lattice is a backward fact about how a received value is used. Deferring argument demand to the receiving parameter supplies that fact across calls, but it does not prove the forward fact that every incoming edge carries the same exact construction or that a merged flow has an exclusive origin.

Curios therefore has several purpose-specific notions of demand, function escape and boundary crossing, but no general aggregate-origin fact that follows a construction through continuation and known-function parameters. Interprocedural use demand and forward origin/exclusivity flow are separate analyses with separate focused tests, even if the eventual rewrite consumes them together.

## Binaryen is a control, not the owner

The native compiler runs Binaryen at optimization level two, whose GC pipeline includes `heap2local`.

That pass replaces a heap allocation with one local per field when the allocation remains inside one WebAssembly function and flows exclusively from that allocation.

It cannot coordinate Curios function signatures, treats calls as escapes, sees a rope view only after the shared slice helper has returned it, and does not run in the browser compiler path.

Leaning on it anyway is the road the WasmGC mainstream takes — [Binaryen's own guidance](https://github.com/WebAssembly/binaryen/wiki/GC-Optimization-Guidebook) tells toolchains to emit naive code and let `wasm-opt` do the heavy lifting, and Kotlin/Wasm and dart2wasm do exactly that — but every toolchain on that road optimizes ahead of time on a developer's machine. `curios-js` is the compiler running in the browser: a program compiled there goes straight from emission to the engine, and no engine closes the gap — as of 2026-08 [Wasmtime lists GC escape analysis as unimplemented future work](https://github.com/bytecodealliance/wasmtime/issues/9351) and V8's WebAssembly investment is speculative inlining, not scalar replacement. The refusal recorded at the end of this document is therefore a consequence of shipping an in-browser compiler, not a preference about where an optimization is nicest to write.

The Curios pass must therefore prove its value on raw pre-Binaryen output; Binaryen remains a downstream optimizer and a comparison that reveals duplicated work, never an acceptance dependency.

## Adopted precedents

[Binaryen Heap2Local](https://github.com/WebAssembly/binaryen/blob/version_130/src/passes/Heap2Local.cpp) supplies the conservative safety baseline: require nonescape and exclusive flow, turn one object into one value per field, and decline mixed origins until the representation explicitly carries their distinction.

[Graal's partial escape analysis](https://ssw.jku.at/Research/Papers/Stadler14/Stadler2014-CGO-PEA.pdf) supplies the model the common-model section below restates: virtual objects whose fields flow while control stays visible, with materialization at the escaping branch alone, so one opaque path does not force allocation on the paths that stay field-only.

[GHC worker/wrapper](https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-9.15-inplace/src/GHC.Core.Opt.WorkWrap.Utils.html) supplies the signature pattern: an internal worker receives unboxed product fields, a wrapper preserves the public boxed interface, and constructed product results travel through an existing multi-result protocol. Its [boxity analysis](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/using-optimisation.html) records the pattern's characteristic failure mode: a split whose callers keep rebuilding the box costs more than the box it removed, which is why the known-function capability here is gated on a measured reboxing balance rather than on growth alone.

[MLton's SSA2](http://mlton.org/SSA2) supplies the representation lesson: MLton changed its intermediate language so that whole-program flattening could be [expressed as data in the program](http://mlton.org/DeepFlatten) rather than coordinated between passes, and GHC's unboxed tuples in Core and Lean's explicit boxing in its IR made the same move — which is why the split below is recorded as a representation the verifier checks, not threaded as a convention the rewrites maintain.

[OCaml Flambda's unboxing of specialized arguments](https://ocaml.org/manual/4.04/flambda.html#s%3Aflambda-unboxing-specialised-args) supplies the recursive policy: propagate field arguments through a recursive group, retain wrappers at other entries, bound signature growth, and refuse a transformation that would inhibit tail calls.

[Lean's compiler IR](https://lean-lang.org/doc/api/Lean/Compiler/IR/Basic.html) supplies the closest proof-assistant precedent for small aggregate results: linearly consumed `Option`, `Prod` and `Except` values may use small unboxed struct or union results instead of heap objects, which supports pairing Curios's existing multi-result protocol with use-sensitive aggregate elimination.

[Lean's reference-counting reset/reuse](https://lean-lang.org/doc/reference/latest/Run-Time-Code/Reference-Counting/) solves a neighboring problem by reusing uniquely owned objects under exact reference counting; Curios does not adopt that mechanism here because tracing Wasm GC provides neither the ownership fact nor a dying object to reuse for every virtual value.

[Agda's GHC backend](https://agda.readthedocs.io/en/latest/tools/compilers.html) and [Rocq's extraction to OCaml, Haskell and Scheme](https://rocq-prover.org/doc/V9.1.0/refman/addendum/extraction.html) delegate this class of runtime representation optimization to mature functional compilers, so their precedent reinforces the GHC and OCaml techniques rather than supplying a theorem-prover-specific replacement.

## Common model: virtual values and materialization

The analysis follows an exact aggregate origin through aliases and eligible transfers, carrying the fields that downstream demand reads rather than a heap reference.

An origin and every parameter carrying it form a virtual-value region.

Within that region, projections read fields directly and transfers pass those fields to rewritten parameters.

At an ineligible boundary, materialization constructs the physical value at that use and leaves the rest of the region virtual.

The first implementation may conservatively reject an entire candidate when partial materialization would require path-sensitive state, but the analysis and tests must distinguish that implementation limit from the cost contract.

Eligibility requires all of the following:

- The origin has an exact immutable shape known in CPS.

- Every transfer is a continuation edge or known direct call whose receiving parameters can be rewritten consistently.

- Every field demanded inside the region is available on every incoming edge, where an edge carrying the region's own parameter or another alias of the origin is available by definition — a region is entered by constructions but circulates through aliases, and a rule that demanded a construction on every edge would decline the loops this document exists for.

- Merged flows are exclusive: a parameter cannot sometimes receive the candidate and sometimes an unrelated physical value unless the rewrite explicitly carries and handles that distinction.

- Rewriting a function component preserves valid tail calls and keeps every shared continuation arity consistent.

- Field and signature growth remain within a measured bound.

An unknown call, foreign call, cell operation, closure capture, heap store or unsplit return is a materialization boundary rather than evidence that the original construction should necessarily allocate earlier.

Nested products may be exposed one level at a time and reconsidered by the optimizer fixpoint; recursive flattening must stop at the same growth bound and must converge without depending on the global round limit.

## The split is recorded in the representation, not coordinated around it

A split parameter is a fact about the program, and the program is where it is recorded: the rewrite gives the receiving binder a fields representation in CPS itself — a parameter that *is* its fields — with the verifier holding every incoming jump and call to that shape the way it already holds arities, and the emitter reading it the way it reads the storage facts `represent.rs` decides.

The alternative was to thread bare fields and let each mechanism keep its own books — every rewritten edge, arity and tail call consistent by that rewrite's care, and idempotence maintained by rules about what may not be decided twice. The return protocol already shows what that costs: its width-two floor exists because a one-slot protocol leaves nothing on the edge recording that the class was decided, so the next round would decide it again. A recorded representation removes that class of problem rather than policing it — once a parameter is four fields, no later pass retains the freedom to disagree, and a second run of the same rewrite finds nothing left to claim.

Every mature implementation of this capability made the same move, per the precedents above: worker/wrapper is a local, checked rewrite in GHC because unboxed tuples are a type in Core, Lean records boxing in its IR rather than in a pass's private state, and MLton changed its intermediate language so flattening could be expressed. The lesson is uniform enough to state as a rule: the analyses may be as clever as they like, but the *decision* survives only where every pass can see it.

The scope of the representation is this campaign's rewrites, not the module's ABI. Signatures visible to escape or to the host stay boxed behind wrappers, the return protocol keeps its existing `Fields` decision and its ownership of results, and the raw-carrier question stays with `represent.rs` — a fields representation says how many values travel, and the carrier lattice still says how each one is held. What changes is that the later milestones inherit a checked substrate: a worker signature and a prepared window are further clients of the same recorded fact rather than two more coordination disciplines.

## Product scalar replacement

The first capability targets explicit `CpsValueExpr::Tuple` origins.

### Continuation parameters

When a tuple flows through continuation parameters and all consumers project fields or transfer it onward, replace the aggregate parameter with its demanded fields and rewrite every incoming jump.

This includes loop headers and backedges: loop-carried state is the central case, not an escape to exclude.

Only demanded fields need be threaded, so the transformation can expose newly dead fields and parameters to the existing optimizer.

The `/std/Str/fold` accumulator is this capability's acceptance case, and it is reached here rather than through a worker signature because contification has already made it a continuation parameter. Every use of it is a projection or a jump, and the continuation it exits to projects field zero and nothing else — so splitting the exit alongside the header leaves the seed construction dead and removes the `{A, Nat}` object from the path entirely, without inlining and without Binaryen. Its incoming edges are visible constructions except one: the `bad` arm passes the loop's own parameter back unchanged, and eligibility must read that alias as field-available rather than demand a construction — stated against the acceptance case deliberately, because an every-edge-constructs check reads plausibly and would decline exactly this loop. Its `A` field stays a reference; its `Nat` field is demanded raw by the multiply that reads it, and a continuation parameter is allowed to answer that.

### Known function parameters

When the same flow crosses known direct function calls, create or select a worker signature carrying the demanded fields.

Every member of the recursive argument-flow component that transfers the value must be rewritten consistently, and every direct call must supply the one parameter representation selected for its target.

The rewrite must preserve existing tail calls; result shapes and the tail-call-connected components that constrain them remain governed by the return protocol.

If the function escapes or retains callers that need the boxed ABI, keep a wrapper accepting the original aggregate, project its fields once, and direct eligible known calls to the worker.

A wrapper is also where this capability's failure mode concentrates: every call that keeps arriving boxed rebuilds at the boundary what the split removed inside, so the decision to split a signature weighs materializations introduced against constructions removed rather than assuming the worker path dominates. That balance is a measured input under the gate below, and GHC's boxity analysis is the precedent that growth alone is the wrong criterion.

Opaque recursive calls, indirect calls and host-visible functions remain on the boxed interface.

This capability's motivating acceptance case — the fold's scan argument reconstruction — was cured at source during M0 (see *Evidence*), which is what retired M3 below; and the census recorded a second disqualifier the cure exposed: the scan flow was never one exact product, because its nullary constructors lower to one-field tuples beside `cont`'s four, so a worker signature had no single shape to carry. The section stands as the design record for the reinstate condition.

Fields stay boxed at any worker boundary unless a separate decision widens what a signature may carry, per *Existing substrate* above; that limit is a property of this capability and belongs in the tests, not a defect of the rewrite.

### Results

Existing return-protocol splitting remains the owner of aggregate results that cross calls.

Product scalar replacement may expose constructions or projection-only demands that make an existing protocol eligible, but it must not introduce a competing result ABI or independently rewrite tail-call components.

## Rope-window virtualization

Rope slices share the virtual-value cost contract but are not tuple constructions and must be implemented as a separate representation-aware capability on the virtual-value framework established by product scalar replacement.

The emitted slice helper eagerly checks bounds, returns a fresh empty leaf for an empty window, aliases the input for a whole window, collapses a view of a view, forces and memoizes an uncached base, and otherwise allocates a physical view whose fields are tag, length, base and offset.

Only physical materialization may disappear; the bounds trap must remain at the original evaluation point, and base forcing and memoization must retain their order and effect.

The optimizer-facing form must therefore separate window preparation from physical materialization and retain the representation distinction the helper currently makes.

Preparation produces one of `Empty`, `Whole(original)` or `Proper { base, offset, length }` after performing every eager semantic obligation of the slice. A universal `(base, offset, length)` triple is insufficient: the empty case has no required base, the whole case must be able to materialize by returning the original reference without forcing it, and only the proper case denotes a physical view.

Represent preparation as a dedicated non-allocating CPS operation with one successor for each descriptor case, not as an ordinary tuple that recreates the allocation being removed. The operation owns the eager bounds check and any forcing or memoization required to establish a proper window; its successors receive the stable payload needed by consumers and by exact later materialization.

`len`, `get` and further `slice` consumers operate on those fields, with further slices composing offsets and performing their own bounds check against the virtual length.

**What this removes is a call as well as an allocation**, and framing it as one object per character undersells it. The window is built by the shared slice helper, so virtualizing it costs neither the `struct.new` nor the `call` that produced it, and a read through a prepared proper window reaches the base's flat payload instead of re-dispatching on a tag. The runtime already collapses a view of a view; a walk that consumes its own suffixes therefore stops creating a succession of allocated collapsed views and repeatedly paying helper-call and tag-dispatch cost, and becomes an index into one payload. That is the larger claim, and the measurement gate must be allowed to hold it to that rather than to an allocation count.

An opaque consumer materializes the same representation choice that the existing helper would provide for the corresponding empty, whole or proper window case.

A backend-only peephole is rejected as the final design because it cannot expose the fields to CPS optimization, cannot coordinate recursive parameters, and would leave the browser and native optimization stories different.

## Measurement gate

No implementation mechanism or growth limit is accepted from intuition alone.

The first milestone is a corpus survey over optimized CPS that classifies candidate tuple constructions and slice results by direct projection, continuation transfer, known-function transfer, return, closure capture, heap storage, unknown call and mixed flow.

The survey must report which candidates continuation-only splitting reaches, which additionally require known-function workers, and which remain blocked after both.

The string ladder must attribute the returned `Scan`, caller-side return reconstruction, accumulator tuple, scan argument reconstruction and suffix view using four isolated mechanisms: one probe may track the returned `Scan` as its construction moves from callee to caller, while the other three each require their own instrument or isolated transformation. Their shares must not be inferred from the number of operations in the loop or from a count scoped only to the emitted fold body — and the attribution input must include multi-byte text, because on the ladder's digit corpus the two `Scan` obligations never execute and a digit-only instrument would report their absence as yield.

M0 delivered the attribution as the walk-mirror family plus the returned-`Scan` probe, with one negative result recorded in place of a share: indexing away the suffix view costs more than the view, because `Bytes/get`'s checked path outweighs it, so the suffix share is measurable only by M4's own transformation and is read structurally — three slice calls per character — until then.

Raw pre-Binaryen and optimized native artifacts must be compared so the work distinguishes an upstream optimization unlock from an allocation Binaryen already removes. A browser execution smoke fixture was specified here and struck (2026-08-16): acceptance is judged on the native artifacts, and `curios-js`'s execution-coverage gap is its own backlog item rather than a gate of this campaign.

Any field-count, signature-growth, clone or reboxing-balance budget must be selected from that survey and recorded beside the test or instrument that justifies it.

Evidence that would stop the campaign is a survey showing that eligible allocations are rare outside the motivating library code, or isolated measurements showing that the four motivating allocation obligations and the suffix helper cost account for too little of the remaining cost to justify signature and IR complexity.

## Milestones

The campaign's spine is `M0 → (M1a + M1b) → M2 → M4`. M1a can independently enable the existing return protocol and should ship once its tests pass, but M2 requires both backward use demand and forward origin/exclusivity flow, and M4 consumes M2's recorded representation and materialization framework for a representation-aware operation. The spine is ordered by what the evidence already shows structurally: the suffix view is the one obligation that costs a helper call as well as an allocation, it is paid on every arm of the walk while the two `Scan` obligations are paid only on multi-byte characters — so the prepared window is the destination and the tuple work is its substrate, and the survey must confirm or correct that ordering before M2 lands.

M3 stands beside the spine as a conditional annex, presumed stopped until M0 argues otherwise. It is the most machinery in this document — wrapper ABI, recursive components, tail-call preservation — spent on the one motivating obligation a pure-ASCII walk never executes, so its admission gate is the survey's known-function-transfer report: it proceeds only if that report shows candidates beyond `/std/Str/fold`, its decisions are additionally gated on the reboxing balance above, and the spine neither waits for it nor depends on its verdict. M0 still chooses whether measured yield justifies proceeding at all and may stop the campaign.

### M0 — Survey and instruments

- Add the aggregate-flow census and its reproducible corpus fixture.

- Add four isolated attributions beside the string ladder: the returned `Scan` and its caller-side reconstruction as one protocol transition, the accumulator tuple, the scan argument reconstruction and the suffix view. Each falls to a distinct milestone or milestone combination, so a shared figure would attribute none of them.

- Add a multi-byte fixture to the attribution corpus. The ladder's digit programs cannot execute either `Scan` obligation, so without one the attribution would present two zero shares as measurements.

- Record raw-versus-Binaryen structural deltas, measure the specialization clone extent beside the probe that reports its current refusal, and choose the growth policy from the observed candidates.

- Report how many candidates lie outside `/std`, and report the known-function-transfer class explicitly: that report is M3's admission gate, and an empty one retires M3 without touching the spine. The stopping evidence below is a real possible outcome of this milestone, and reaching it is a result rather than a failure.

- **Done (2026-08-17).** The census (`aggregate_flow_census`), the extent probe (`step_specialization_extent`), the attribution family (`programs/walk_mirror_*.crs` under `walk_mirror_family_isolates_each_obligation` and `walk_mirror_attribution_measurements`), the returned-`Scan` probe (`returned_scan_constructions_live_in_step`) and the multi-byte fixture (`programs/parse_multibyte.crs`) are in the tree, every figure beside its command. The stopping evidence was not met — the accumulator and the returned scan each measure about a fifth of the walk — and the spine's ordering stands with one correction: the suffix view's dynamic share resisted rung isolation and is deferred to M4's own transformation.

### M1a — Interprocedural use demand

- Change the demand rule so an argument defers to the parameter that receives it. The analysis already runs under the shared solver, and its own documentation says the strengthening is a change of one rule rather than of the shape — the deferral is what turns the run from a single scan into a genuine fixpoint, not a rewrite of the substrate, and it should not be scheduled as one.

- Prove the backward fact with focused tests before it moves code, including regressions for dead-parameter elimination and `uncurry`'s returned-closure absorption, which consume the same demand lattice.

- Then let it move code on its own: with arguments no longer opaque, `split_returns` becomes eligible on components it currently pins, and the returned scan state is the first of them. Ship that before any new rewrite exists, because it is the cheapest change in this document and it measures the fact.

- Assert the intermediate shape explicitly: `/syn/Str/step` returns four fields without constructing a `Scan`, while the current caller resume reconstructs it before entering the continuation. M1a relocates this allocation; it does not claim to remove it.

- **Done (2026-08-17).** The deferral landed as one rule under the shared solver, with chain, sentinel and closure-argument tests beside it; `uncurryable` now recomputes its sole-local-application fact syntactically, because interprocedural `Applied` may be earned behind a forwarding jump the transform cannot move. Two facts the milestone surfaced: the split fired only after `/std/Str/utf8/check` was reworked to thread its scan by recursion — the curried validator captured the scan into a closure chain and held the whole component opaque — and the relocated construction costs about five percent on the digit walk until M2 removes it, the reboxing mode the adopted GHC precedent names, measured in `curios/src/tests/codegen/ladder.rs` and accepted as the intermediate state the spine's ordering already prices in.

### M1b — Aggregate origin and exclusivity

- Follow exact tuple origins forward through aliases, continuation edges and known direct calls independently of the backward demand fact.

- Classify materialization boundaries, exclusive and mixed incoming flows, recursive components and available field sets.

- Prove the forward fact with focused tests before either scalar-replacement rewrite consumes it.

- **Done (2026-08-17).** `origins` in `curios-cont/src/cps/origin.rs`: `Unreached < Exact(arity) < Opaque` under the shared solver, with boundaries stated by injection — resume parameters, escaping functions' parameters, the entry, knot-tied values — so a surviving bottom means unreached rather than assumed exact, and the loop alias resolves to the constructions that entered it exactly as the eligibility rule demands. Landed together with its first consumer, since an analysis nothing reads is dead code the gate refuses.

### M2 — Continuation scalar replacement

- Introduce the fields representation on rewritten parameters and the verifier rule that holds every incoming edge to it, before the first rewrite lands — the rewrite then changes a recorded representation rather than maintaining a convention.

- Rewrite projection-only tuple state through continuation parameters, including loop backedges.

- Feed the exposed fields back through the existing optimizer fixpoint and prove convergence independently of the round limit.

- Retain or decline candidates at mixed and opaque boundaries according to the first implementation's documented limit.

- Remove the `/std/Str/fold` accumulator allocation in raw pre-Binaryen output, and with it the seed construction the exit continuation leaves dead.

- Remove the caller-side `Scan` reconstruction exposed by M1a by splitting the continuation that receives its returned fields.

- **Done (2026-08-17), with one correction the census had already predicted.** The fields representation, its verifier rule, and the maintenance obligations landed first; the split is three local edits and the existing chain finishes the job, exactly as `split_returns` works, with the head rebuild surviving precisely where a whole-value use survives — which makes it the materialization the cost contract prescribes rather than a leak. The accumulator bullet holds: the emitted fold body carries no `{A, Nat}` construction, the walk-mirror family now compiles the idiomatic and hand-flattened spellings to identical counts, and the digit walk lands below every figure the ladder ever recorded for it. The scan-reconstruction bullet does not hold, for the shape reason recorded at M3's retirement: the loop's scan parameter mixes arity-1 interned constants with arity-4 rebuilds, and no exact product describes a variant-width flow. Its removal is a successor capability — variant-aware splitting with per-tag padding, or a uniform-width variant lowering — and each is a measured design decision this campaign does not take; the residual is one arity-4 construction per multi-byte character, priced in the ladder.

### M3 — Known-function workers, retired by the survey (2026-08-17)

- Retired. The motivating obligation — the scan argument reconstruction — was a source spelling and was cured in `/std` during M0, so no known-function rewrite has an acceptance case here. The census's needs-workers regions (`/std/Async`'s drains, `/std/Handle/write/1`, `io/bind`, program `main`s) are Io-carrier plumbing with no measured share, and the scan flow itself was never an exact product: its nullary constructors lower to one-field tuples beside `cont`'s four, the mixed-arity shape this capability's eligibility rule excludes.

- Reinstate condition: a measured hot candidate whose flow crosses known direct calls with one exact shape. If reinstated, the design above applies as written — field flow through direct calls as further clients of M2's recorded representation, each split gated on the measured reboxing balance, boxed wrappers wherever the original ABI remains reachable, and tail calls, continuation arities and the return-protocol ownership boundary preserved.

### M4 — Prepared rope windows

- Introduce the optimizer-visible prepared-window form without weakening slice traps or memoization effects.

- Preserve the `Empty`, `Whole(original)` and `Proper { base, offset, length }` distinction through preparation and exact materialization.

- Teach length, indexed read and further slicing to consume virtual windows.

- Thread prepared windows through continuation parameters and backedges as clients of M2's recorded representation — the suffix that rides the loop is the case the spine exists for, and it must not grow a second coordination discipline of its own.

- Materialize at opaque boundaries and remove the recursive suffix-view allocation in raw pre-Binaryen output — together with the helper call that produced it, which is the half an allocation count does not see.

- **Done (2026-08-17), smaller than specified, and the specification's own argument is corrected by the evidence.** The descriptor prescribed here — a dedicated preparing operation with one successor per `Empty`/`Whole`/`Proper` case — proved unnecessary: a universal `(base, offset, length)` triple suffices once the base is always a valid rope reference, because any rope opens as its own whole window with one length read, a virtual slice is offset arithmetic behind the new `WindowExtent` guard — which keeps the eager bounds trap at the original evaluation point and refuses to constant-fold a trap away — and an opaque boundary would materialize through the existing slice helper, which reproduces the empty, whole and proper representation choices exactly. The three cases are behavioral facts of the triple, not IR constructors. Forcing defers from slice time to first read, and the read helper's own memoized force makes that value-identical; the emitted fold body carries no `call $bytes/slice` and no view construction, the walk-mirror slice sites halve corpus-wide, and the first implementation is region-atomic — a region with any hostile use is declined whole, the conservative limit this document allows. The window split shares `fields.rs`, the `FieldGroup` record, and the growth ceiling with the product split, so no second coordination discipline grew.

### M5 — Cost contract

- Move the accepted durable cost-model decision to `documentation/design.md` once the measured implementation establishes its actual envelope.

- Retire transient measurements into their owning probes and update the roadmap with the capability and its explicit limits.

## Acceptance

The campaign is complete only when all of the following hold on the spine:

- A focused CPS fixture carries a product through a continuation loop and emits fields without the product allocation.

- Mixed-origin flows either materialize correctly or are conservatively declined, with tests distinguishing the two cases.

- Shared continuation arities remain valid after splitting, and the verifier — not a test's vigilance — is what holds every edge to a rewritten parameter's recorded representation.

- The `Nat` member of a product split through *continuation* parameters reaches raw scalar representation when its uses demand it.

- `/std/Str/fold` emits no accumulator tuple on the raw pre-Binaryen per-character path, `/syn/Str/step` returns fields rather than constructing the `Scan` it hands back, and the proper suffix-view object disappears together with the helper call that builds it, the half an allocation count does not see. The caller-side `Scan` rebuild is recorded retained cost rather than an acceptance subject (amended 2026-08-17): the scan is a variant-width flow no exact product describes, its residual is one construction per multi-byte character priced in the ladder, and its removal is the successor capability M2's verdict names.

- Empty, whole, nested, uncached and out-of-bounds slice fixtures preserve the existing result, eager trap and memoization behavior.

- Raw and Binaryen-optimized native execution agree.

- The isolated probes show the contribution of each removed allocation and keep every figure beside the command that reproduces it.

- The corpus survey demonstrates that the accepted mechanism reaches enough non-library or independently shaped candidates to justify remaining general compiler machinery rather than a `/std/Str/fold` peephole.

And M3 has resolved: retired (2026-08-17), with the survey report recorded beside the probes and no cost retained — the scan argument reconstruction was cured at source during M0 rather than carried. The reinstate condition is a measured hot known-function-transfer candidate of one exact shape; a reinstated M3 answers the worker criteria formerly listed here — a focused fixture emitting a worker path without the product allocation, correct boxed wrappers at escaping entries, tail calls surviving signature rewriting, the emitted callee rewritten rather than duplicated, and a `Nat` member split through a worker signature asserted to stay boxed because a function parameter offers no carrier.

## Refused alternatives

**Rely on Binaryen.** It is too late to unlock Curios optimization, cannot rewrite Curios call protocols, does not cross helper calls and is absent from the browser path.

**Stack-allocate the object without exposing its fields.** Wasm GC has no stack struct representation, and retaining an opaque object would leave projections, fields and downstream simplifications hidden.

**Inline until the allocation becomes local.** Inlining may help Binaryen but duplicates code, is bounded independently, and does not provide a cost contract for recursive or escaping definitions.

**Special-case `/std/Str/fold`.** It would repair one spelling while leaving the representation problem and other loop-carried aggregates intact.

**Adopt reference-counted reset/reuse as this mechanism.** Reusing a uniquely owned dying object solves persistent reconstruction, not the absence of a materialized identity, and Curios targets tracing Wasm GC.

**Thread bare fields by convention.** Rewriting parameters without recording the result leaves every later pass free to disagree with the split and every rewrite responsible for its own idempotence — the return protocol's width-two floor is the shape of that cost, paid once already. Recording the representation removes the choice rather than policing it.

**Adopt source-level unboxed types.** OxCaml's layouts and GHC's unboxed tuples put the representation in the surface type system; Curios's surface stays closed and representation stays an optimizer fact. Erasure already separates what a program proves from what it pays, and a surface annotation would let a spelling promise what the analysis is supposed to earn — reopening the equal-cost question below from the other side.

**Promise equal cost for all dependent fold encodings.** Aggregate lifetime cannot erase arbitrary differences in higher-order control flow, traversal and specialization, so the promise would exceed both the analysis and any testable compiler envelope.
