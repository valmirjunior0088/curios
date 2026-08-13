# A decided bound's evaluation is priced by what it builds

This is the implementation specification for making type-level evaluation of decided bounds fail predictably instead of exhausting process memory.

The motivating case is an omitted decided proof such as `Bytes/slice(built, 0, 10)`, where proving `Le(10, Bytes/len(built))` reduces a large computed `built`. Repeated binary concatenation copies and retains a growing sequence of flat buffers, so a declaration exhausts memory while remaining within its step budget.

This specification preserves the language design recorded in `documentation/DESIGN.md`: bounds remain decided propositions, and omitted proofs may be discharged by reduction. It does not change a decided bound into an inductive precondition, add a solver, or require users to hide every computed subject behind an opaque parameter.

## Status

The propagation half is done. `trivially_inhabited` no longer converts an exhausted reduction into `None`, so an omitted implicit argument whose proposition ran out of budget reports the exhaustion instead of silently falling back to a hole that later reads as the user's fault. That was a standalone diagnostic defect, correct under any pricing, and it landed ahead of this work.

Everything below — the pricing change, the audit, and the calibration — is pending.

## The defect is a price, not a missing dimension

The step counter charges one unit per reducer transition regardless of what that transition constructs. A `PackedBin::concat` that copies half a megabyte costs exactly what a `Bool` fold costs.

That is the whole defect. `curios-elab/src/reduce.rs`'s own note on the reduction loop already records the consequence — the budget bounds steps, so nothing bounds the memory a reduction allocates — and `documentation/DESIGN.md` records it again as the standing limit on decided bounds.

The motivating case makes the mispricing visible. Repeated append is a linear number of transitions performing quadratic cumulative copying, and the elaborator's closed-reduction cache retains the distinct growing intermediates. Steps and materialization are not independent axes there: they are one runaway, one of which is being counted at a flat rate of one.

The immediate failure is therefore not an unfolding-discard bug. Reducing the proof obligation really does construct the subject, and the retained constructed values can legitimately grow beyond available memory.

The runtime byte representation is not the type-level representation. The continuation backend uses a rope-like representation for efficient runtime concatenation, but changing the runtime representation cannot bound compile-time reduction.

## Decision

Reduction keeps exactly one counter. A transition costs one unit plus the logical size of whatever it constructs or newly retains.

The counter, its budget, its restoration at declaration boundaries, its command-line option, and its exhaustion error all keep their present identity and meaning. What changes is the price list.

A second budget dimension is rejected under *Refused alternatives* below. The distinction it would draw is preserved where it is actually useful — in the diagnostic — without splitting the limit that decides acceptance.

Cost is charged before allocation. If the requested charge would overflow or exceed the remaining budget, reduction returns exhaustion without attempting the allocation.

Charges are never refunded within a declaration's budget window. This makes the limit independent of allocator reuse, garbage collection, destruction order, and cache eviction, and it means the cumulative successfully constructed reducer-owned storage bounds the reducer-owned storage that can still be live.

Existing input terms and source text are not retroactively charged. Any new payload, collection slot, term node, or cache entry constructed as part of reduction is charged, including temporary values later discarded.

Units are machine-independent logical words: one unit is eight bytes of scalar payload or one reference-sized logical slot. The accounting uses fixed formulas rather than `size_of`, allocator capacities, resident-set size, or platform-dependent big-integer limb layouts.

That independence is load-bearing rather than fastidious. `curios-web` compiles to wasm32, where `usize` and `num-bigint`'s digit width both differ from the native target, and its budget constant exists to promise that a program compiling in the playground compiles at the command line. All charge arithmetic is therefore computed in `u64` regardless of host pointer width.

## Price list

The following table defines the minimum accounting categories. The allocation-site audit in M0 may add categories, but it must not weaken these charges.

| Constructed value | Charge |
| --- | --- |
| Packed bytes | Fixed value header plus `ceil(byte_length / 8)` units |
| Packed bits | Fixed value header plus `ceil(bit_length / 64)` units |
| Big natural or integer | Fixed value header plus the number of required base-2^64 logical limbs |
| List, vector, or argument storage | Fixed collection header plus one unit per retained slot, plus charges for newly constructed elements |
| Term node | Fixed charge for its variant plus one unit per retained child or scalar field |
| Reduction-cache or kernel-memo entry | Fixed entry charge plus one unit per retained slot |
| Temporary reducer buffer | Fixed buffer header plus its requested logical payload or slots |

Fixed header and term-variant charges are named constants in the shared accounting module. They are deliberately conservative, documented beside the constants, and covered by tests. They are not claims about the exact Rust heap layout.

A cache or memo entry charges its own slots and nothing more. `Term` is reference-counted, so retaining one in a cache is a pointer bump over a value that was already charged where it was constructed; charging its payload again at insertion would price one construction twice.

The same reference counting bounds the term-node row. Charging a new node for its own variant and its own slots is complete precisely because its children are shared slots that were charged at their own construction, so no walk of the child subtree is required or permitted.

For a result whose size is computable from its operands, the reducer computes and spends the complete charge before constructing the result. Size arithmetic is checked, and an overflow is reported as exhaustion rather than wrapped, truncated, or passed to an allocator.

For incremental construction whose final size is not known, each increment is charged before it is appended. Implementations may reserve or build more efficiently, but optimization must not reduce the logical charge below the specified constructed value.

Storage that is reused rather than constructed is not charged. `PackedBin`'s window and slice share their backing buffer behind a reference count and add no payload, so they charge their own value header and nothing else — the distinction the audit must make at every site is construction versus sharing, not operation category.

## Calibration

Construction pricing changes what an existing budget figure buys, so the default is recalibrated rather than retained.

This is a deliberate compatibility break in an existing observable. It is affordable here specifically: the fixed-prelude archive is explicitly not a stable interchange format, the standard library is entirely in-tree, and only a handful of fixtures state a budget explicitly. It would not be affordable in a project with published budget figures, and the specification claims no general license for the change.

The new default is chosen from the M0 measurements, not guessed in this document. Its value and evidence live beside the ignored measurement probe M0 requires, so the figure and the thing that would check it cannot drift apart.

## Configuration

`--budget <STEPS>` remains the only reduction limit and keeps its option name, its help text, and its `documentation/USAGE.md` row. Its meaning widens from transitions to priced work; its calibrated default changes.

The shared unit arithmetic and price constants belong in `curios-core`, below both checkers. The elaborator and the certificate kernel keep separate mutable counters and share no reduction state or judgments.

Embedders that supply no budget receive the product default. Test helpers set it explicitly, as `typecheck_within` already does.

## Accounting boundary

The shared `Reducer` interface used by intrinsic folds gains one fallible operation for spending several units at once — the natural extension of the single-step charge it already implies. Any shared fold that can allocate reducer-owned logical storage calls that operation before allocating.

The trait has two methods and two production implementations, `curios-elab`'s `Context` and `curios-cert`'s `Kernel`, plus test fixtures in the intrinsic fold's own tests. The seam is small; the work is its breadth, not its depth.

Checker-specific reduction code uses the same formulas and the same charge-first rule for constructions outside shared folds. A new allocation path is incomplete until its maximum logical result size is either charged or shown to reuse already charged storage without cloning or growth.

`normalize_concat` in `curios-core/src/free_monoid.rs` is the representative shape. Its fusing closure returns a `Subterm` infallibly today, so charging at the point of allocation makes the closure and the function fallible, across its binary and list callers. Expect that shape repeatedly rather than once.

The M0 audit covers at least packed bytes, packed bits, big naturals and integers, shifts, list construction and slicing, argument vectors, term reconstruction, substitutions that clone payload, elaborator reduction-cache insertion, and kernel memo and replay insertion.

The limit is not a byte-concatenation guard. A type-level shift, large integer operation, list operation, or future intrinsic must not be able to allocate an unbounded result in one charged transition. A shift is the sharpest of these: its result size is computable from its operand and shift amount without allocating, so it is charged exactly and refused before `num-bigint` is asked for the value.

An optimization such as a type-level byte rope may be added later for time complexity, but it is not the safety boundary. Every representation remains accounted, and flattening or copying it is charged.

## Retention across declarations

The budget is per declaration, restored at every item boundary, and that is deliberate: whether one declaration typechecks must not depend on how much the declarations before it had already spent.

The elaborator's reduction cache is equally deliberately *not* per declaration. It survives item boundaries so that closed reducts stay warm across the definitions reduction and erasure mint within and between items, and a fresh definition retains every entry that does not name it rather than clearing wholesale.

Those two lifetimes compose into a bound of declarations times budget, not budget. Per-declaration charging alone therefore bounds the motivating case, which is one declaration, and does not bound a module of many heavy declarations.

The cure is a compilation-scoped retention counter, distinct from the per-declaration work counter and charged only on cache and memo insertion. Its budget is a product default measured in M0. Exhausting it stops retention rather than refusing the program: the cache stops accepting new entries, and reduction continues correctly but cold.

Making retention refuse outright would be worse, because it would let a declaration's verdict turn on how much the declarations before it had cached — the property the per-declaration work budget exists to avoid.

Degrading instead does not fully escape that, and the specification does not claim it does. The elaborator's reduction loop probes its cache *before* charging, so a hit already costs nothing and a cold cache already costs re-derivation against the work budget. A declaration that would have hit a warm cache can therefore exhaust its own budget once retention has stopped. That is the elaborator's existing warmth-dependence, not a new one introduced here, and the retention default must be measured with enough headroom that ordinary compilation never reaches it. What the counter buys is a bound on pathological retention; what it does not buy is warmth-independent acceptance, which only charging cache hits their recorded cost would give, at the price of the elaborator's cache being worth having.

## Elaborator behavior

Automatic proof synthesis continues to reduce decided propositions with ordinary transparency. A computed subject is allowed to evaluate while the budget permits it.

An opaque parameter remains a useful programming idiom because reduction stops at the parameter, but this specification gives parameter opacity no special semantics and requires no opacity barrier around computed subjects.

Exhaustion is not equivalent to "not trivially inhabited". `trivially_inhabited` propagates it rather than returning `None`, which is the half already landed.

A cache hit returns an already retained result without constructing it again, and therefore charges no construction. This mirrors the counter's existing behavior exactly: the reduction loop probes the cache before it charges, so a hit already costs nothing. The elaborator's accounting is warmth-dependent, the kernel's is not, and that asymmetry predates this work rather than being introduced by it.

Ordinary definitional equality, explicit decided proofs, and every other reduction use the same counter. Protecting only omitted-proof synthesis would leave the resource hole open.

## Kernel behavior

Certificate checking enforces its own limit and never trusts accounting performed by elaboration or compilation.

The kernel's spend component already records what a remembered computation consumed and charges a memo hit exactly what a recomputation would have, across two quantities: reduction steps and minted binder identities. Its parity test holds the whole observable trajectory — refusal payloads, exhaustion points, later-minted identities — bit-identical with the evaluation memos on or off.

Construction charges ride that existing mechanism. The recorded step cost becomes the recorded priced cost, and the replay charge that already exists spends it. No new replay field is required, and the parity discipline extends unchanged.

If the replay charge is unavailable, the kernel reports exhaustion without returning the cached result. Replay arithmetic follows the same checked-overflow rule as direct reduction.

The kernel and elaborator use the same unit definitions and per-construction formulas, but tests must not assume their totals are identical, because their evaluators, their retained structures, and their memo warmth differ.

## Diagnostics

Exhaustion remains one structured error, and the existing rendering keeps its wording.

Attribution is kept without splitting the budget. The error carries the dominant charge category and the attempted charge that failed, so the message can distinguish a computation that was long from one that was large — which is the genuinely useful half of the two-dimensional design — while one number continues to decide acceptance.

Exhaustion is a normal deterministic rejection, not a panic, allocation failure, process abort, or generic internal error.

No error path attempts the refused allocation in order to produce a diagnostic. Diagnostic construction uses only bounded metadata captured before refusal: the operation category, the remaining budget, and the attempted charge.

The limit bounds reducer-created logical work and retention, not total compiler memory. Parsing a huge source file, retaining caller-owned terms, backend compilation, allocator overhead, thread stacks, and unrelated process memory remain outside this contract and must not be described as covered.

## Milestones

### M0 — Audit and measurement

- Add an ignored, explicitly bounded measurement probe beside the motivating numeric fixtures, recording the command, input sizes, observed priced cost, observed retention, and peak process memory.
- Inventory every reducer allocation site in `curios-core`, `curios-elab`, and `curios-cert`, classifying each as construction or sharing, and turn the inventory into a checklist linked from this specification.
- Measure representative prelude compilation and certificate checking to choose the recalibrated work default and the retention default with documented headroom.
- Do not run the known unbounded reproducer to completion as part of routine verification.

### M1 — Price the counter

- Introduce checked unit arithmetic and the price constants in `curios-core`, with the shared spending operation on `Reducer`.
- Charge construction in every audited shared intrinsic fold, making the fusing seams fallible where charging at the allocation point requires it.
- Charge checker-specific term reconstruction, temporary collections, and substitutions that clone payload.
- Treat any unaudited allocation discovered during implementation as part of this milestone rather than narrowing the guarantee to the motivating byte path.

### M2 — Retention and replay

- Add the compilation-scoped retention counter, charged on elaborator cache and kernel memo insertion, exhausting into a cold cache rather than a refusal.
- Carry the priced cost through the kernel's existing replay record and extend its parity test to cover it.
- Add the diagnostic attribution metadata.

### M3 — Calibrate, verify, document

- Set both defaults from the M0 measurements with enough headroom for the fixed prelude and representative certificates.
- Update `documentation/USAGE.md`, `documentation/DESIGN.md`, the CLI help, and relevant crate documentation with the widened meaning of the budget and its limits.
- Keep measurement values beside the ignored probe and normative semantics in permanent documentation rather than in this roadmap file.
- Complete the acceptance suite and the repository verification gate before checking off the roadmap item.

## Acceptance

- The motivating computed-subject fixture fails with budget exhaustion before a large allocation, process-memory spike, abort, or operating-system kill.
- The paired fixture whose subject is behind a parameter still elaborates under a low budget and materializes nothing.
- A single intrinsic operation whose result exceeds the remaining budget is refused before its allocation is attempted.
- Repeated concatenation with distinct growing cached results is bounded by cumulative charges even when every individual result would fit.
- Large packed-bit, big-integer, shift, and list-producing reductions have focused charge tests, so bytes are not the only protected payload.
- Sharing paths — window, slice, and reference-counted term retention — are shown not to charge for storage they do not construct.
- A module of many separately budgeted heavy declarations is bounded by the retention counter, and reaching that counter degrades the cache rather than refusing the program.
- The retention default is measured with enough headroom that no fixed-prelude or representative compilation reaches it, since crossing it can cost a later declaration its own budget in re-derivation.
- Automatic implicit synthesis propagates exhaustion and never disguises it as an unsolved metavariable.
- Kernel direct evaluation and memo replay produce the same acceptance or exhaustion for the same kernel budget, with the parity test extended to the priced cost.
- Checked size arithmetic rejects overflow before allocation.
- Charges are identical on the native and wasm32 targets for the same program.
- The recalibrated default compiles the fixed prelude and passes representative source and certificate tests with the measurement-documented margin.
- Diagnostics are stable enough for focused tests but expose no allocator-specific sizes or platform-dependent layout.

## Refused alternatives

**A second budget dimension.** The rejected design gives reduction independent step and materialization counters, each with its own limit, default, command-line option, and exhaustion category. It was refused because the dimensions are not independent where it matters: the motivating runaway burns both together, and no program shape was found where two budgets return a different verdict than one correctly priced budget, except the pathological shape that should be refused anyway. What it costs is a composite budget value threaded through roughly twenty signatures across the pipeline, CLI, browser, kernel, and test entry points; a second default to calibrate; a second quantity to keep in sync between two independent checkers, with a standing caveat that their totals differ; and a rule that every budget test must provision the other dimension so its failure stays the one under test. The useful half of it — telling a long computation from a large one — is a property of the diagnostic, and is kept there.

**Changing decided bounds to inductive proof obligations.** This changes the language design and gives up the intended reduction-based ergonomics.

**Adding a specialized arithmetic or bounds solver.** Such a solver could avoid some evaluations but would not protect ordinary definitional equality or allocating reductions elsewhere.

**Restricting transparency during automatic proof synthesis.** Mature systems use transparency and opacity controls to tune automation, but changing what Curios unfolds is a separate semantic and compatibility decision.

**Replacing flat type-level bytes with ropes.** It improves concatenation complexity, and would genuinely reduce the motivating case's cumulative copying, but it moves the large allocation to flattening and covers neither lists, integers, and bits nor terms and caches. It is a performance change, not a safety boundary.

**Bounding the cache by eviction instead of charging construction.** Eviction makes acceptance depend on cache state, which is exactly what the kernel's memo-parity discipline exists to prevent. The retention counter above deliberately does the opposite: it degrades the cache without touching the verdict.

**An operating-system memory limit or allocator hook.** These are platform-dependent, apply too late for useful diagnostics, include memory outside reduction, and would diverge between the native and wasm32 targets.

**Exact heap-byte accounting.** The deterministic logical-unit budget rejects pathological construction predictably across supported platforms; process-level memory remains subject to ordinary implementation overhead.

## Precedent

Lean's heartbeats count allocation work rather than recursive calls, under a single `maxHeartbeats` deadline, and its transparency modes separately control unfolding during automation. That is the direct precedent for this design: one deadline whose unit is small allocations, not a time limit paired with a memory limit.

Rocq exposes both time and allocation limits and separately supports opacity and reduction strategies. Agda likewise uses opacity to control unfolding and performance.

Curios adopts the common separation of resource limits from transparency policy, and follows Lean in pricing allocation through the single deadline it already has, using a deterministic logical measure suitable for both its elaborator and its certificate kernel.

This precedent motivates the design but does not define Curios semantics. The price list, the charge-first rule, the retention split, the propagation contract, and independent kernel enforcement in this specification are normative.

## Verification and retirement

This cross-cutting change requires the full repository done bar from `CLAUDE.md`, including formatting, linting, workspace tests, release build, documentation, invariant checks, repository hygiene, and `make curios/web` because shared reducer dependencies feed the browser build.

The implementation diff must include focused unit tests near each accounting owner and integration tests for the paired computed-versus-parameter behavior. Measurement probes remain ignored and bounded; ordinary tests must be deterministic and must not rely on observing resident-set size.

Recalibration changes what an existing budget figure buys, so fixtures that state a budget are expected to change with it. Update the assertion to what the corrected pricing says, rather than preserving an old figure's outcome.

Once all acceptance criteria pass, move the stable contract and rationale into permanent documentation, check off the roadmap item, and delete this working specification in the same landing change.
