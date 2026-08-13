# A decided bound's evaluation is priced by what it builds

This is the implementation specification for making type-level evaluation of decided bounds fail predictably instead of exhausting process memory.

The motivating case is an omitted decided proof such as `Bytes/slice(built, 0, 10)`, where proving `Le(10, Bytes/len(built))` reduces a large computed `built`. Repeated binary concatenation copies and retains a growing sequence of flat buffers, so a declaration exhausts memory while remaining within its step budget.

This specification preserves the language design recorded in `documentation/DESIGN.md`: bounds remain decided propositions, and omitted proofs may be discharged by reduction. It does not change a decided bound into an inductive precondition, add a solver, or require users to hide every computed subject behind an opaque parameter.

## Status

The propagation half is done. `trivially_inhabited` no longer converts an exhausted reduction into `None`, so an omitted implicit argument whose proposition ran out of budget reports the exhaustion instead of silently falling back to a hole that later reads as the user's fault. That was a standalone diagnostic defect, correct under any pricing, and it landed ahead of this work.

Everything below — the pricing change, the audit, and the calibration — is pending.

## The defect is the price of one transition

The step counter charges one unit per reducer transition regardless of what that transition constructs. A `PackedBin::concat` that copies half a megabyte costs exactly what a `Bool` fold costs.

That is the whole defect. `curios-elab/src/reduce.rs`'s own note on the reduction loop already records the consequence — the budget bounds steps, so nothing bounds the memory a reduction allocates — and `documentation/DESIGN.md` records it again as the standing limit on decided bounds.

The motivating case makes the mispricing visible. Repeated append is a linear number of transitions performing quadratic cumulative copying, and the elaborator's closed-reduction cache retains the distinct growing intermediates. The transition counter observes only the linear part and assigns the quadratic construction a flat price of one.

The immediate failure is therefore not an unfolding-discard bug. Reducing the proof obligation really does construct the subject, and the retained constructed values can legitimately grow beyond available memory.

The runtime byte representation is not the type-level representation. The continuation backend uses a rope-like representation for efficient runtime concatenation, but changing the runtime representation cannot bound compile-time reduction.

## Decision

Reduction keeps exactly one verdict-affecting counter. A transition costs one unit plus the logical size of whatever it constructs.

The work counter, its restoration at declaration boundaries, its command-line option, and its exhaustion error keep their present roles. The option's spelling remains `--budget`; its unit, help text, internal names, and calibrated default change from transitions to priced reduction work.

A separate compilation-scoped retention quota limits optional cache and memo storage. It is not a second acceptance budget: exhausting it refuses an insertion and leaves evaluation correct but cold. The distinction between transition-dominated and construction-dominated exhaustion is preserved in the diagnostic without splitting the limit that decides acceptance.

Cost is charged before allocation. If the requested charge would overflow or exceed the remaining budget, reduction returns exhaustion without attempting the allocation.

Charges are never refunded within a declaration's budget window. This makes the limit independent of allocator reuse, garbage collection, destruction order, and cache eviction, and it means the cumulative successfully constructed reducer-owned storage bounds the reducer-owned storage that can still be live.

Existing input terms and source text are not retroactively charged. Any new payload, collection slot, or term node constructed as part of reduction is charged, including temporary values later discarded. Optional cache-table bookkeeping is not semantic construction and is charged only to the retention quota.

Units are machine-independent logical words: one unit covers eight logical bytes of scalar payload or one abstract reference slot. An abstract slot is one unit on every target; it is not a claim that a physical pointer occupies eight bytes. The accounting uses fixed formulas rather than `size_of`, allocator capacities, resident-set size, or platform-dependent big-integer limb layouts.

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
| Temporary reducer buffer | Fixed buffer header plus its requested logical payload or slots |

Fixed header and term-variant charges are named constants in the shared accounting module. They are deliberately conservative, documented beside the constants, and covered by tests. They are not claims about the exact Rust heap layout.

Reference counting bounds the term-node row. Charging a new node for its own variant and its own slots is complete construction pricing precisely because its children are shared rather than reconstructed. Retention uses a different rule below because extending a value's lifetime is not the same resource event as constructing it.

For a result whose size is cheaply computable from its operands, the reducer computes and spends the complete charge before constructing the result. Where exact size requires performing the allocating operation, the reducer instead spends a documented conservative upper bound. A formula may overcharge but must never undercharge. Size arithmetic is checked, and an overflow is reported as exhaustion rather than wrapped, truncated, converted unsafely to `usize`, or passed to an allocator.

For incremental construction whose final size is not known, each increment is charged before it is appended. Implementations may reserve or build more efficiently, but optimization must not reduce the logical charge below the specified constructed value.

Storage that is reused rather than constructed is not charged. `PackedBin`'s window and slice share their backing buffer behind a reference count and add no payload, so they charge their own value header and nothing else — the distinction the audit must make at every site is construction versus sharing, not operation category.

## Calibration

Construction pricing changes what an existing budget figure buys, so the default is recalibrated rather than retained.

This is a deliberate compatibility break in an existing observable. It is affordable here specifically: the fixed-prelude archive is explicitly not a stable interchange format, the standard library is entirely in-tree, and only a handful of fixtures state a budget explicitly. It would not be affordable in a project with published budget figures, and the specification claims no general license for the change.

The new default is chosen from the completed measurement probe and the M0 baselines, not guessed in this document. Its value and evidence live beside the ignored probe introduced with accounting, so the figure and the thing that would check it cannot drift apart.

## Configuration

`--budget <UNITS>` remains the only verdict-affecting reduction limit and keeps its option name. Its help text, `documentation/USAGE.md` row, and internal `DEFAULT_STEP_BUDGET` terminology change from steps to reduction work. Its calibrated default changes as well.

The retention quota is a product default rather than a second command-line option. It controls an optimization, not whether a program is accepted, and ordinary users must not have to coordinate it with the work budget.

The shared unit arithmetic and price constants belong in `curios-core`, below both checkers. The elaborator and the certificate kernel keep separate mutable counters and share no reduction state or judgments.

Embedders that supply no budget receive the product default. Test helpers set it explicitly, as `typecheck_within` already does.

## Accounting boundary

The shared `Reducer` interface used by intrinsic folds gains one fallible operation for spending several units at once — the natural extension of the single-step charge it already implies. Any shared fold that can allocate reducer-owned logical storage calls that operation before allocating.

The trait has two methods and two production implementations, `curios-elab`'s `Context` and `curios-cert`'s `Kernel`, plus test fixtures in the intrinsic fold's own tests. Adding the operation is a small seam, but enforcing it is a cross-cutting API change rather than a local edit.

Checker-specific reduction code uses the same formulas and the same charge-first rule for constructions outside shared folds. A new allocation path is incomplete until its maximum logical result size is either precharged conservatively or shown to reuse already charged storage without cloning or growth.

Infallible helpers that allocate below the reducer boundary — including scope and telescope opening or releasing, term reconstruction, substitution, normalization, and packed-value operations — must gain an accounting-aware fallible path or be preceded by a conservative charge for their entire construction. The implementation must not allocate first merely to discover the charge afterward.

`normalize_concat` in `curios-core/src/free_monoid.rs` is the representative shape. Its fusing closure returns a `Subterm` infallibly today, so charging at the point of allocation makes the closure and the function fallible, across its binary and list callers. Expect that shape repeatedly rather than once.

The M0 audit covers at least packed bytes, packed bits, big naturals and integers, shifts, list construction and slicing, argument vectors, term reconstruction, scope and telescope traversal, substitutions that clone payload, elaborator reduction-cache insertion, and kernel memo and replay insertion. It also covers temporary allocation hidden inside hashing, equality, collection growth, and convenience conversions. `PackedBin` hashing must stream unaligned contents without materializing a temporary packed byte vector rather than making cache lookup fallible.

The limit is not a byte-concatenation guard. A type-level shift, large integer operation, list operation, or future intrinsic must not be able to allocate an unbounded result in one charged transition. A shift is the sharpest of these: its result-size bound is computable from its operand and shift amount without allocating the result, so it is charged before `num-bigint` is asked for the value. A shift amount too large to convert to a host index is refused by comparing it with the affordable logical bound first.

An optimization such as a type-level byte rope may be added later for time complexity, but it is not the safety boundary. Every representation remains accounted, and flattening or copying it is charged.

## Retention across declarations

The budget is per declaration, restored at every item boundary, and that is deliberate: whether one declaration typechecks must not depend on how much the declarations before it had already spent.

The elaborator's reduction cache is equally deliberately *not* per declaration. It survives item boundaries so that closed reducts stay warm across the definitions reduction and erasure mint within and between items, and a fresh definition retains every entry that does not name it rather than clearing wholesale.

Those two lifetimes compose into a bound of declarations times budget, not budget. Per-declaration charging alone therefore bounds the motivating case, which is one declaration, and does not bound a module of many heavy declarations.

The cure is a compilation-scoped retention counter, distinct from the per-declaration work counter and charged only before cache and memo insertion. Its budget is a product default measured during calibration. Exhausting it stops retention rather than refusing the program: the cache stops accepting new entries, and reduction continues correctly but cold.

An insertion's retention charge is its fixed entry and slot cost plus a conservative logical footprint of the key, result, replay record, and every referenced payload whose lifetime the insertion may extend. Payload may be omitted only when its lifetime is already at least that of the cache and the implementation can establish that fact cheaply. When ownership or sharing is ambiguous, the whole reachable logical payload is charged. Double-counting shared payload across entries is permitted because this quota controls an optional optimization and a conservative bound is safer than an unprovable exemption.

Computing that footprint must itself be allocation-free, use bounded scratch, or preflight its scratch against the retention quota. It must not walk an adversarial shared graph exponentially or allocate a graph-sized visited set before deciding whether the cache may retain the value. A cached saturating logical-footprint summary on immutable values is an acceptable implementation strategy.

Retention charges are cumulative and are not refunded when an entry is invalidated, replaced, or its table is cleared. This keeps the bound deterministic and independent of destruction order and avoids needing exact shared-ownership accounting. The retention quota uses the same target-independent `u64` unit arithmetic as work, but its charges never consume semantic work.

Making retention exhaustion reject the current computation outright would be worse, because it would let a declaration's verdict turn directly on how much the declarations before it had cached — the property the per-declaration work budget exists to avoid.

Degrading instead does not fully escape indirect warmth dependence, and the specification does not claim it does. The elaborator's reduction loop probes its cache *before* charging, so a hit already costs nothing and a cold cache already costs re-derivation against the work budget. A declaration that would have hit a warm cache can therefore exhaust its own budget once retention has stopped. That is the elaborator's existing warmth-dependence, not a new one introduced here, and the retention default must be measured with enough headroom that ordinary compilation never reaches it. What the counter buys is a conservative bound on pathological cache retention; what it does not buy is warmth-independent elaborator acceptance, which would require replay-priced elaborator cache hits.

## Elaborator behavior

Automatic proof synthesis continues to reduce decided propositions with ordinary transparency. A computed subject is allowed to evaluate while the budget permits it.

An opaque parameter remains a useful programming idiom because reduction stops at the parameter, but this specification gives parameter opacity no special semantics and requires no opacity barrier around computed subjects.

Exhaustion is not equivalent to "not trivially inhabited". `trivially_inhabited` propagates it rather than returning `None`, which is the half already landed.

A cache hit returns an already retained result without constructing it again, and therefore charges no construction. This mirrors the counter's existing behavior exactly: the reduction loop probes the cache before it charges, so a hit already costs nothing. The elaborator's accounting is warmth-dependent, the kernel's is not, and that asymmetry predates this work rather than being introduced by it.

Ordinary definitional equality, explicit decided proofs, and every other reduction use the same counter. Protecting only omitted-proof synthesis would leave the resource hole open.

## Kernel behavior

Certificate checking enforces its own limit and never trusts accounting performed by elaboration or compilation.

The kernel's spend component already records what a remembered computation consumed and charges a memo hit across two quantities: reduction steps and minted binder identities. Its intended invariant is that the whole observable trajectory — refusal payloads, exhaustion points, and later-minted identities — is bit-identical with evaluation memos on or off. The existing fixed-prelude parity test compares final verdicts at the ordinary budget; it does not yet establish low-budget failure payloads or identity trajectories, so M2 strengthens it.

Construction charges ride that existing mechanism. The recorded step cost becomes the recorded priced cost, and the replay charge that already exists spends it. Cache-table construction and insertion are excluded from that cost and consume only retention quota, so enabling memos cannot make the first evaluation spend more semantic work than disabling them. No new replay field is required for a successful replay.

If the full replay charge fits, the kernel spends it and advances the recorded binder identities exactly as it does today. If it does not fit, the kernel bypasses that memo entry for the recomputation and evaluates under the actual remaining budget; any nested replay that does not fit follows the same rule. The direct path then identifies the same first failing charge and advances exactly the identities reached before that failure. It does not return the cached result or manufacture a diagnostic from an aggregate total. Replay arithmetic follows the same checked-overflow rule as direct reduction.

The kernel and elaborator use the same unit definitions and per-construction formulas, but tests must not assume their totals are identical, because their evaluators and construction paths differ and elaborator cache hits remain free.

## Diagnostics

Exhaustion remains one structured error. Its existing headline remains recognizable while focused detail identifies the refused charge.

Attribution is kept without splitting the budget. The error carries the failing charge category, the remaining budget, and the attempted charge. Categories distinguish transition work from packed payload, bigint limbs, collection slots, term reconstruction, substitution, and other priced construction while one number continues to decide acceptance. “Dominant category” is deliberately not promised: determining dominance would require cumulative per-category accounting unrelated to refusal.

Exhaustion is a normal deterministic rejection, not a panic, allocation failure, process abort, or generic internal error.

No error path attempts the refused allocation in order to produce a diagnostic. Diagnostic construction uses only bounded metadata captured before refusal: the operation category, the remaining budget, and the attempted charge.

The limit bounds reducer-created logical work and retention, not total compiler memory. Parsing a huge source file, retaining caller-owned terms, backend compilation, allocator overhead, thread stacks, and unrelated process memory remain outside this contract and must not be described as covered.

## Milestones

### M0 — Audit and baselines

- Inventory every reducer allocation site in `curios-core`, `curios-elab`, and `curios-cert`, classifying each as construction or sharing, and turn the inventory into a checklist linked from this specification.
- Record bounded pre-change baselines for the motivating fixture, representative prelude compilation, and certificate checking: command, input size, transition count, and peak process memory. Priced work and retention do not exist yet and are not claimed at this milestone.
- Do not run the known unbounded reproducer to completion as part of routine verification.

### M1 — Price the counter

- Introduce checked unit arithmetic and the price constants in `curios-core`, with the shared spending operation on `Reducer`.
- Charge construction in every audited shared intrinsic fold, making the fusing seams fallible where charging at the allocation point requires it.
- Add accounting-aware or conservatively preflighted paths for checker-specific term reconstruction, binder traversal, temporary collections, and substitutions that clone payload.
- Add an ignored, explicitly bounded measurement probe beside the motivating numeric fixtures. It records the command, input sizes, observed priced work, and peak process memory; M2 extends it with retention.
- Treat any unaudited allocation discovered during implementation as part of this milestone rather than narrowing the guarantee to the motivating byte path.

### M2 — Retention and replay

- Add the compilation-scoped retention counter, charging conservatively reachable payload on elaborator cache and kernel memo insertion and exhausting into a cold cache rather than a refusal.
- Carry the priced cost through the kernel's existing replay record, add unaffordable-replay fallback, and extend parity tests to low-budget exhaustion, diagnostic payload, and minted identities.
- Add the failing-charge diagnostic metadata.
- Extend the ignored measurement probe with observed retention consumption.

### M3 — Calibrate, verify, document

- Use the completed probe and M0 baselines to set both defaults with enough headroom for the fixed prelude and representative certificates.
- Update `documentation/USAGE.md`, `documentation/DESIGN.md`, the CLI help, and relevant crate documentation with the widened meaning of the budget and its limits.
- Keep measurement values beside the ignored probe and normative semantics in permanent documentation rather than in this roadmap file.
- Complete the acceptance suite and the repository verification gate before checking off the roadmap item.

## Acceptance

- The motivating computed-subject fixture fails with budget exhaustion before a large allocation, process-memory spike, abort, or operating-system kill.
- The paired fixture whose subject is behind a parameter still elaborates under a low budget and materializes nothing.
- A single intrinsic operation whose result exceeds the remaining budget is refused before its allocation is attempted.
- Repeated concatenation with distinct growing cached results is bounded by cumulative charges even when every individual result would fit.
- Large packed-bit, big-integer, shift, and list-producing reductions have focused charge tests, so bytes are not the only protected payload.
- Operations whose exact output size is not cheaply available use tested conservative upper bounds and never allocate to discover their price.
- Sharing paths — window, slice, and reference-counted term retention — are shown not to charge for storage they do not construct.
- Hidden temporary-allocation paths, including unaligned packed-value hashing, are removed or precharged rather than escaping the audit through infallible helpers.
- A module of many separately budgeted heavy declarations is bounded by payload-aware retention charges, and reaching the retention quota degrades the cache rather than directly refusing the program.
- Enabling a cache or memo does not add semantic work to a first computation; table allocation and insertion consume only retention quota.
- The retention default is measured with enough headroom that no fixed-prelude or representative compilation reaches it, since crossing it can cost a later declaration its own budget in re-derivation.
- Automatic implicit synthesis propagates exhaustion and never disguises it as an unsolved metavariable.
- Kernel direct evaluation and memo replay produce the same acceptance or exhaustion for the same kernel budget. Focused parity tests include insufficient replay budgets and compare the failing category, attempted charge, remaining budget, and later-minted identities.
- Checked size arithmetic rejects overflow before allocation.
- Charges are identical on the native and wasm32 targets for the same program.
- The recalibrated default compiles the fixed prelude and passes representative source and certificate tests with the measurement-documented margin.
- Diagnostics are stable enough for focused tests but expose no allocator-specific sizes or platform-dependent layout.

## Refused alternatives

**A second verdict-affecting budget dimension.** Independent transition and materialization limits are genuinely more expressive than one weighted limit: a transition-heavy program and a construction-heavy program can receive different verdicts under the two designs. Curios deliberately accepts the weighted policy tradeoff. One user-facing limit preserves the existing configuration shape, requires one default and one acceptance threshold to calibrate, and gives the elaborator and independent kernel one deterministic quantity to reproduce. The fixed price list states the exchange rate rather than hiding it. Failing-charge attribution retains useful diagnostic distinction, but it is not claimed to make the two policies equivalent.

**Changing decided bounds to inductive proof obligations.** This changes the language design and gives up the intended reduction-based ergonomics.

**Adding a specialized arithmetic or bounds solver.** Such a solver could avoid some evaluations but would not protect ordinary definitional equality or allocating reductions elsewhere.

**Restricting transparency during automatic proof synthesis.** Mature systems use transparency and opacity controls to tune automation, but changing what Curios unfolds is a separate semantic and compatibility decision.

**Replacing flat type-level bytes with ropes.** It improves concatenation complexity, and would genuinely reduce the motivating case's cumulative copying, but it moves the large allocation to flattening and covers neither lists, integers, and bits nor terms and caches. It is a performance change, not a safety boundary.

**Bounding the cache by eviction instead of admission accounting.** Eviction requires replacement policy and shared-payload lifetime accounting and makes warmth less predictable. The monotone retention quota instead refuses new insertions after a conservative logical allowance is consumed. It never directly rejects evaluation, although the elaborator's existing free-hit behavior means becoming cold can indirectly expose an insufficient per-declaration work budget.

**An operating-system memory limit or allocator hook.** These are platform-dependent, apply too late for useful diagnostics, include memory outside reduction, and would diverge between the native and wasm32 targets.

**Exact heap-byte accounting.** The deterministic logical-unit budget rejects pathological construction predictably across supported platforms; process-level memory remains subject to ordinary implementation overhead.

## Precedent

Lean's deterministic heartbeats are useful precedent for an allocation-sensitive deadline, but not for Curios's size-weighted formulas. Lean describes heartbeats as counting “small” allocations, and its allocator increments the counter per small-object allocation rather than per logical byte. A small number of large objects can therefore be cheap in heartbeats; Curios must price logical volume to cover its motivating case. Lean also has separate recursion and subsystem limits, so this specification claims precedent for deterministic accounting and a stable primary control, not for having literally one resource limit. See the [Lean timing reference](https://lean-lang.org/doc/reference/latest/IO/Timing/) and [allocator interface](https://github.com/leanprover/lean4/blob/master/src/include/lean/lean.h).

Rocq is the closer precedent for measuring allocation volume: it exposes `Timeout` separately from `AllocLimit`, with allocation stated in machine words and dependent on memory-profiling support. That demonstrates the usefulness of allocation-volume accounting while also showing why Curios cannot adopt the mechanism directly across native and wasm32 targets. See the [Rocq vernacular command reference](https://rocq-prover.org/doc/master/refman/proof-engine/vernacular-commands.html).

Agda's opaque definitions provide precedent for controlling unfolding and performance separately from a resource counter, not for this budget model. See [Agda's opaque-definition reference](https://agda.readthedocs.io/en/latest/language/opaque-definitions.html).

Curios therefore remixes Lean's deterministic accounting and stable primary deadline with Rocq's sensitivity to allocation volume, while replacing implementation-dependent allocation units with preflighted logical units reproducible by both the elaborator and certificate kernel. Transparency remains a separate language and automation policy, as it does in these mature systems.

This precedent motivates the design but does not define Curios semantics. The price list, the charge-first rule, the retention split, the propagation contract, and independent kernel enforcement in this specification are normative.

## Verification and retirement

This cross-cutting change requires the exact repository gate from `CLAUDE.md`:

```text
make curios/runtime
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -Dwarnings
cargo test --workspace --all-targets --all-features
make curios/web
```

The web build is required because shared reducer dependencies feed the browser target. The handoff also includes the documentation, invariant, and repository-hygiene review required by `CLAUDE.md`.

The implementation diff must include focused unit tests near each accounting owner and integration tests for the paired computed-versus-parameter behavior. Measurement probes remain ignored and bounded; ordinary tests must be deterministic and must not rely on observing resident-set size.

Recalibration changes what an existing budget figure buys, so fixtures that state a budget are expected to change with it. Update the assertion to what the corrected pricing says, rather than preserving an old figure's outcome.

Once all acceptance criteria pass, move the stable contract and rationale into permanent documentation, check off the roadmap item, and delete this working specification in the same landing change.
