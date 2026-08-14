# A reduction step costs what it builds

This is the implementation specification for making type-level evaluation refuse predictably instead of exhausting process memory.

## Status

The propagation half is done. `trivially_inhabited` no longer converts an exhausted reduction into `None`, so an omitted implicit argument whose proposition ran out of budget reports the exhaustion rather than falling back to a hole that later reads as the user's fault. That was a standalone diagnostic defect, correct under any pricing, and it landed ahead of this work.

The pricing, the audit, the retention quota, and the calibration are pending.

## The defect is the price of one transition

The step counter charges one unit per reducer transition regardless of what that transition constructs. A `PackedBin::concat` that copies half a megabyte costs exactly what a `Bool` fold costs, and a `recurse` level that takes a fresh 32 MiB stack segment costs the same again.

That is the whole defect. `curios-elab/src/reduce.rs`'s note on the reduction loop already records the consequence — the budget bounds steps, so nothing bounds the memory a reduction allocates — and `documentation/DESIGN.md` records it twice more, at the decided-bound entry and at *Depth is bought with stack, not with hand-rolled frames*, where growing the stack rather than aborting was accepted on exactly this trade.

Reduction is the only stage that can be driven to arbitrary cost by a well-typed program, because it is the only stage a *type* can call. Every other stage's work is bounded by the size of what elaboration produced. That asymmetry is why the bound belongs on the counter rather than on the process.

## What the measurement decided

The reproducer is the accumulate-then-slice shape: a `rec` that appends a fixed run to an accumulator, whose result then stands under a `Bytes/slice` bound. It is parameterized by iteration count, and three arms separate the costs — the bound read off an opaque parameter, so nothing evaluates; the same iteration count with an accumulator that is replaced rather than extended, so there are steps without payload growth; and the growing accumulator, which has both.

Three conclusions follow, and the design rests on them rather than on the motivating anecdote.

**The per-transition machinery is linear and modest.** The fixed-payload arm grows linearly in iteration count at a small constant. Nothing about performing a transition is superlinear.

**The growth is constructed payload that reduction then retains.** The growing arm's excess over the fixed-payload arm converges on a quadratic in the iteration count, and its coefficient matches the cumulative size of the intermediate values within allocator overhead. Repeated binary concatenation performs a linear number of transitions and a quadratic volume of construction; the counter observes only the linear part.

**The same expression is linear when the program runs it.** `curios/src/tests/runtime.rs`'s `accumulation_loops_are_linear_by_construction` measures the identical loop end to end at a hundred thousand steps. Compile-time evaluation of a fraction of that already costs gigabytes.

So a step count cannot see either lever, and both levers are construction: what a transition builds, and how long the reducer keeps it. The limit must price both, and it must do so in units a representation change cannot move.

M0 records these figures beside the probe that reproduces them. They appear here as what the measurement *decided*, not as numbers this file could keep true.

## Decision

Reduction keeps exactly one verdict-affecting counter. A transition costs one unit plus the logical size of whatever it constructs.

The work counter, its restoration at declaration boundaries, its command-line option, and its exhaustion error keep their present roles. The option's spelling remains `--budget`; its unit, help text, internal names, and calibrated default change from transitions to priced reduction work.

A separate compilation-scoped retention quota limits optional cache and memo storage. It is not a second acceptance budget: exhausting it refuses an insertion and leaves evaluation correct but cold. The distinction between transition-dominated and construction-dominated exhaustion is preserved in the diagnostic without splitting the limit that decides acceptance.

Cost is charged before allocation. If the requested charge would overflow or exceed the remaining budget, reduction returns exhaustion without attempting the allocation.

Charges are never refunded within a declaration's budget window. This makes the limit independent of allocator reuse, garbage collection, destruction order, and cache eviction, and it means the cumulative successfully constructed reducer-owned storage bounds the reducer-owned storage that can still be live.

Existing input terms and source text are not retroactively charged. Any new payload, collection slot, term node, or native stack segment a reduction causes is charged, including temporary values later discarded. Optional cache-table bookkeeping is not semantic construction and is charged only to the retention quota.

Units are machine-independent logical words: one unit covers eight logical bytes of scalar payload or one abstract reference slot. An abstract slot is one unit on every target; it is not a claim that a physical pointer occupies eight bytes. The accounting uses fixed formulas rather than `size_of`, allocator capacities, resident-set size, or platform-dependent big-integer limb layouts.

That independence is load-bearing rather than fastidious. `curios-web` compiles to wasm32, where `usize` and `num-bigint`'s digit width both differ from the native target, and its budget constant exists to promise that a program compiling in the playground compiles at the command line. All charge arithmetic is therefore computed in `u64` regardless of host pointer width.

The same independence is what keeps this limit correct while a representation changes underneath it. A carrier that concatenates more cheaply spends fewer units for the same result; the unit itself, the price list, and the acceptance threshold are unaffected.

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
| Reducer recursion level | Fixed frame charge, at or above the deepest measured guarded frame |

Fixed header, term-variant, and frame charges are named constants in the shared accounting module. They are deliberately conservative, documented beside the constants, and covered by tests. They are not claims about the exact Rust heap layout.

The frame row is what brings depth inside the contract. `recurse` grows the native stack rather than aborting, and nothing else bounds total depth, so a data-shaped walk allocates real memory the transition counter never observed. One unit per level is already spent — every nested reduction turns its own loop — so what this row adds is a *price* for that level commensurate with the segment it can take.

Reference counting bounds the term-node row. Charging a new node for its own variant and its own slots is complete construction pricing precisely because its children are shared rather than reconstructed. Retention uses a different rule below, because extending a value's lifetime is not the same resource event as constructing it.

For a result whose size is cheaply computable from its operands, the reducer computes and spends the complete charge before constructing the result. Where exact size requires performing the allocating operation, the reducer instead spends a documented conservative upper bound. A formula may overcharge but must never undercharge. Size arithmetic is checked, and an overflow is reported as exhaustion rather than wrapped, truncated, converted unsafely to `usize`, or passed to an allocator.

For incremental construction whose final size is not known, each increment is charged before it is appended. Implementations may reserve or build more efficiently, but optimization must not reduce the logical charge below the specified constructed value.

Storage that is reused rather than constructed is not charged. `PackedBin`'s window and slice share their backing buffer behind a reference count and add no payload, so they charge their own value header and nothing else — the distinction the audit must make at every site is construction versus sharing, not operation category.

A charge covers an operation's peak, not its residue. `PackedBin::concat` fills a `Vec<u8>` and then converts it into an `Arc<[u8]>`, which allocates a second buffer of the same length; the operation costs two payloads even though one survives.

## Accounting boundary

The shared `Reducer` interface used by intrinsic folds gains one fallible operation for spending several units at once — the natural extension of the single-step charge it already implies. Any shared fold that can allocate reducer-owned logical storage calls that operation before allocating.

The trait has two methods and two production implementations, `curios-elab`'s `Context` and `curios-cert`'s `Kernel`, plus test fixtures in the intrinsic fold's own tests. Adding the operation is a small seam, but enforcing it is a cross-cutting API change rather than a local edit.

Checker-specific reduction code uses the same formulas and the same charge-first rule for constructions outside shared folds. A new allocation path is complete once its maximum logical result size is either precharged conservatively or shown to reuse already charged storage without cloning or growth.

Infallible helpers that allocate below the reducer boundary — including scope and telescope opening or releasing, term reconstruction, substitution, normalization, and packed-value operations — gain an accounting-aware fallible path, or a conservative charge for their entire construction precedes them. The implementation charges from the operands, never by allocating first to discover the price.

`normalize_concat` in `curios-core/src/free_monoid.rs` is the representative shape. Its fusing closure returns a `Subterm` infallibly today, so charging at the point of allocation makes the closure and the function fallible, across its binary and list callers. Expect that shape repeatedly rather than once.

The audit covers at least packed bytes, packed bits, big naturals and integers, shifts, list construction and slicing, argument vectors, term reconstruction, scope and telescope traversal, substitutions that clone payload, elaborator reduction-cache insertion, and kernel memo and replay insertion. It also covers temporary allocation hidden inside hashing, equality, collection growth, and convenience conversions.

Four sites are named because they were found by reading rather than by category, and they are the shape the rest of the audit should expect.

`bin_shape` classifies a `Bin` value by materializing its whole run into a `Vec` — one element per byte at `Grain::X`, one per *bit* at `Grain::B` — and `Bin/len` then reads only that vector's length. An operation whose result is a single `Nat` allocates the entire subject to compute it.

`peel_bin`'s `bin_atoms` flattens both operands into merged literal runs on every conversion between two sequence values, so a comparison that decides on the first byte can allocate both subjects in full.

`PackedBin`'s `Hash` materializes `to_packed_bytes` for an unaligned window, so a cache probe allocates. Hashing streams the unaligned contents instead, rather than making lookup fallible.

`PackedBin::concat` allocates twice per call, per the price list's last paragraph.

The limit is not a byte-concatenation guard. A type-level shift, large integer operation, list operation, or future intrinsic is covered by the same rule: no single charged transition may allocate an unbounded result. A shift is the sharpest of these — its result-size bound is computable from its operand and shift amount without allocating the result, so it is charged before `num-bigint` is asked for the value, and a shift amount too large to convert to a host index is refused by comparing it against the affordable logical bound first.

## Retention across declarations

The budget is per declaration, restored at every item boundary, and that is deliberate: whether one declaration typechecks must not depend on how much the declarations before it had already spent.

The elaborator's reduction cache is equally deliberately *not* per declaration. It survives item boundaries so that closed reducts stay warm across the definitions reduction and erasure mint within and between items, and a fresh definition retains every entry that does not name it rather than clearing wholesale.

Those two lifetimes compose into a bound of declarations times budget. Per-declaration charging alone therefore bounds the motivating case, which is one declaration, and a compilation-scoped retention counter is what bounds a module of many heavy ones. The measurement gives this counter its evidence directly: in the growing-accumulator arm, the retained intermediates account for essentially the whole excess.

The retention counter is distinct from the per-declaration work counter and is charged only before cache and memo insertion. Its budget is a product default measured during calibration. Exhausting it stops retention rather than refusing the program: the cache stops accepting new entries, and reduction continues correctly but cold.

An insertion's retention charge is its fixed entry and slot cost plus a conservative logical footprint of the key, result, replay record, and every referenced payload whose lifetime the insertion may extend. Payload may be omitted only when its lifetime is already at least that of the cache and the implementation can establish that fact cheaply. When ownership or sharing is ambiguous, the whole reachable logical payload is charged. Double-counting shared payload across entries is permitted because this quota controls an optional optimization and a conservative bound is safer than an unprovable exemption.

Computing that footprint is allocation-free, uses bounded scratch, or preflights its scratch against the retention quota. It walks no adversarial shared graph exponentially and allocates no graph-sized visited set before deciding whether the cache may retain the value. A cached saturating logical-footprint summary on immutable values is an acceptable implementation strategy.

Retention charges are cumulative and are not refunded when an entry is invalidated, replaced, or its table is cleared. This keeps the bound deterministic and independent of destruction order and avoids needing exact shared-ownership accounting. The retention quota uses the same target-independent `u64` unit arithmetic as work, and its charges never consume semantic work.

Degrading rather than refusing is what keeps a declaration's verdict off the history of the declarations before it, which is the property the per-declaration work budget exists to hold.

What remains is an *indirect* warmth dependence the specification states rather than claims to remove. The elaborator's reduction loop probes its cache before charging, so a hit already costs nothing and a cold cache already costs re-derivation against the work budget. A declaration that would have hit a warm cache can therefore exhaust its own budget once retention has stopped. That is the elaborator's existing warmth-dependence, and the retention default is measured with enough headroom that ordinary compilation never reaches it. What the counter buys is a conservative bound on pathological cache retention; warmth-independent elaborator acceptance would require replay-priced elaborator cache hits, which is a separate capability.

## Calibration

Construction pricing changes what an existing budget figure buys, so the default is recalibrated rather than retained.

This is a deliberate compatibility break in an existing observable. It is affordable here specifically: the fixed-prelude archive is explicitly not a stable interchange format, the standard library is entirely in-tree, and only a handful of fixtures state a budget explicitly. It would not be affordable in a project with published budget figures, and the specification claims no general license for the change.

The default is chosen from the completed measurement probe and the M0 baselines, and the fixed-payload arm supplies the floor the choice must respect: a construction-free program already retains a measurable amount of memory per transition, so a budget of one million transitions permits gigabytes before a single byte of payload is built. Calibration therefore selects against observed memory per unit, not only against whether the fixed prelude fits.

The value and its evidence live beside the ignored probe introduced with accounting, so the figure and the thing that would check it cannot drift apart.

## Configuration

`--budget <UNITS>` remains the only verdict-affecting reduction limit and keeps its option name. Its help text, `documentation/USAGE.md` row, and internal `DEFAULT_STEP_BUDGET` terminology change from steps to reduction work. Its calibrated default changes as well.

The retention quota is a product default rather than a second command-line option. It controls an optimization, not whether a program is accepted, and ordinary users are not asked to coordinate it with the work budget.

The shared unit arithmetic and price constants belong in `curios-core`, below both checkers. The elaborator and the certificate kernel keep separate mutable counters and share no reduction state or judgments.

Embedders that supply no budget receive the product default. Test helpers set it explicitly, as `typecheck_within` already does.

## Elaborator behavior

Automatic proof synthesis continues to reduce decided propositions with ordinary transparency. A computed subject is allowed to evaluate while the budget permits it.

An opaque parameter remains a useful programming idiom because reduction stops at the parameter; this specification gives parameter opacity no special semantics and requires no opacity barrier around computed subjects.

Exhaustion is a judgment about resources, and `trivially_inhabited` reports it as one rather than as an absent inhabitant. That half has landed.

A cache hit returns an already retained result without constructing it again, and therefore charges no construction. This mirrors the counter's existing behavior exactly: the reduction loop probes the cache before it charges, so a hit already costs nothing. The elaborator's accounting is warmth-dependent, the kernel's is not, and that asymmetry predates this work.

Ordinary definitional equality, explicit decided proofs, and every other reduction use the same counter, so the limit covers every route into unbounded computation rather than the synthesis path alone.

## Kernel behavior

Certificate checking enforces its own limit and never trusts accounting performed by elaboration or compilation.

The kernel's spend component already records what a remembered computation consumed and charges a memo hit across two quantities: reduction steps and minted binder identities. Its intended invariant is that the whole observable trajectory — refusal payloads, exhaustion points, and later-minted identities — is bit-identical with evaluation memos on or off. The existing fixed-prelude parity test compares final verdicts at the ordinary budget; M2 strengthens it to low-budget failure payloads and identity trajectories.

Construction charges ride that existing mechanism. The recorded step cost becomes the recorded priced cost, and the replay charge that already exists spends it. Cache-table construction and insertion are excluded from that cost and consume only retention quota, so enabling memos cannot make the first evaluation spend more semantic work than disabling them. A successful replay needs no new field.

If the full replay charge fits, the kernel spends it and advances the recorded binder identities exactly as it does today. If it does not fit, the kernel bypasses that memo entry for the recomputation and evaluates under the actual remaining budget; any nested replay that does not fit follows the same rule. The direct path then identifies the same first failing charge and advances exactly the identities reached before that failure, rather than returning the cached result or manufacturing a diagnostic from an aggregate total. Replay arithmetic follows the same checked-overflow rule as direct reduction.

The kernel and elaborator use the same unit definitions and per-construction formulas. Their totals differ, because their evaluators and construction paths differ and elaborator cache hits remain free, so tests compare verdicts and failure points rather than sums.

## Diagnostics

Exhaustion remains one structured error. Its existing headline stays recognizable while focused detail identifies the refused charge.

Attribution is kept without splitting the budget. The error carries the failing charge category, the remaining budget, and the attempted charge. Categories distinguish transition work from packed payload, bigint limbs, collection slots, term reconstruction, substitution, recursion depth, and other priced construction, while one number continues to decide acceptance. "Dominant category" is deliberately not promised: determining dominance would require cumulative per-category accounting unrelated to refusal.

Exhaustion is a normal deterministic rejection, reported as a diagnostic like any other refusal.

Diagnostic construction uses only bounded metadata captured before refusal — the operation category, the remaining budget, and the attempted charge — so no error path attempts the allocation that was refused.

The limit bounds reducer-created logical work, reducer-caused stack growth, and reducer retention. Parsing a huge source file, caller-owned terms, backend compilation, allocator overhead, ambient thread stacks, and unrelated process memory sit outside this contract and are bounded, where they are bounded, by other means.

## Milestones

### M0 — Audit and baselines

- Inventory every reducer allocation site in `curios-core`, `curios-elab`, and `curios-cert`, classifying each as construction or sharing, and turn the inventory into a checklist linked from this specification. The four sites named under *Accounting boundary* are its seed, not its extent.
- Add the parameterized accumulate-then-slice reproducer as an ignored, explicitly bounded probe with all three arms, and record command, input sizes, transition count, wall time, and peak process memory for each. It is cheap at sizes that already show the growth, so this milestone measures rather than extrapolates.
- Record the same baselines for representative prelude compilation and certificate checking. Priced work and retention do not exist yet and are not claimed at this milestone.

### M1 — Price the counter

- Introduce checked unit arithmetic and the price constants in `curios-core`, with the shared spending operation on `Reducer`.
- Charge construction in every audited shared intrinsic fold, making the fusing seams fallible where charging at the allocation point requires it.
- Add accounting-aware or conservatively preflighted paths for checker-specific term reconstruction, binder traversal, temporary collections, and substitutions that clone payload.
- Charge the recursion level at the `recurse` bracket, from a constant justified beside the measured worst-case frame.
- Extend the M0 probe with observed priced work beside the memory it already records.
- Treat any unaudited allocation discovered during implementation as part of this milestone rather than narrowing the guarantee to the motivating byte path.

### M2 — Retention and replay

- Add the compilation-scoped retention counter, charging conservatively reachable payload on elaborator cache and kernel memo insertion and exhausting into a cold cache rather than a refusal.
- Carry the priced cost through the kernel's existing replay record, add unaffordable-replay fallback, and extend parity tests to low-budget exhaustion, diagnostic payload, and minted identities.
- Add the failing-charge diagnostic metadata.
- Extend the probe with observed retention consumption.

### M3 — Calibrate, verify, document

- Use the completed probe and M0 baselines to set both defaults, respecting the per-transition memory floor as well as the fixed prelude's headroom.
- Update `documentation/USAGE.md`, `documentation/DESIGN.md`, the CLI help, and relevant crate documentation with the widened meaning of the budget and its scope.
- Keep measurement values beside the ignored probe and normative semantics in permanent documentation rather than in this roadmap file.
- Complete the acceptance suite and the repository verification gate before checking off the roadmap item.

## Acceptance

- The motivating computed-subject fixture fails with budget exhaustion before a large allocation, process-memory spike, abort, or operating-system kill.
- The paired fixture whose subject is behind a parameter still elaborates under a low budget and materializes nothing.
- A single intrinsic operation whose result exceeds the remaining budget is refused before its allocation is attempted.
- Repeated concatenation with distinct growing cached results is bounded by cumulative charges even when every individual result would fit.
- Large packed-bit, big-integer, shift, and list-producing reductions have focused charge tests, so bytes are not the only protected payload.
- A reduction driven to great depth is refused by the counter, and the refusal names the recursion category.
- Operations whose exact output size is not cheaply available use tested conservative upper bounds and charge from their operands.
- Sharing paths — window, slice, and reference-counted term retention — are shown to charge only for storage they construct.
- Hidden temporary-allocation paths — unaligned packed-value hashing, `bin_shape`'s run materialization, `bin_atoms`' flattening, and `PackedBin::concat`'s second buffer — are removed or precharged.
- A module of many separately budgeted heavy declarations is bounded by payload-aware retention charges, and reaching the retention quota degrades the cache rather than refusing the program.
- Enabling a cache or memo does not add semantic work to a first computation; table allocation and insertion consume only retention quota.
- The retention default is measured with enough headroom that no fixed-prelude or representative compilation reaches it, since crossing it can cost a later declaration its own budget in re-derivation.
- Automatic implicit synthesis propagates exhaustion and never disguises it as an unsolved metavariable.
- Kernel direct evaluation and memo replay produce the same acceptance or exhaustion for the same kernel budget. Focused parity tests include insufficient replay budgets and compare the failing category, attempted charge, remaining budget, and later-minted identities.
- Checked size arithmetic rejects overflow before allocation.
- Charges are identical on the native and wasm32 targets for the same program.
- The recalibrated default compiles the fixed prelude and passes representative source and certificate tests with the measurement-documented margin.
- Diagnostics are stable enough for focused tests and expose no allocator-specific sizes or platform-dependent layout.

## Refused alternatives

**A second verdict-affecting budget dimension.** Independent transition and materialization limits are genuinely more expressive than one weighted limit: a transition-heavy program and a construction-heavy program can receive different verdicts under the two designs. Curios accepts the weighted policy tradeoff. One user-facing limit preserves the existing configuration shape, requires one default and one acceptance threshold to calibrate, and gives the elaborator and independent kernel one deterministic quantity to reproduce. The fixed price list states the exchange rate rather than hiding it. Failing-charge attribution retains useful diagnostic distinction without being claimed to make the two policies equivalent.

**Bounding the cache by eviction instead of admission accounting.** Eviction requires a replacement policy and shared-payload lifetime accounting, and makes warmth less predictable. The monotone retention quota instead refuses new insertions after a conservative logical allowance is consumed, and never directly rejects evaluation.

**An operating-system memory limit or allocator hook.** These are platform-dependent, apply too late for useful diagnostics, include memory outside reduction, and would diverge between the native and wasm32 targets.

**Exact heap-byte accounting.** The deterministic logical-unit budget rejects pathological construction predictably across supported platforms; process-level memory remains subject to ordinary implementation overhead.

## Precedent

Lean's deterministic heartbeats are useful precedent for an allocation-sensitive deadline, and instructive about where such a counter stops. Lean describes heartbeats as counting "small" allocations, and its allocator increments per small-object allocation rather than per logical byte, so a small number of large objects is cheap in heartbeats — the same blind spot a transition counter has, reached by a different route. Lean also carries separate recursion and subsystem limits, so this specification claims precedent for deterministic accounting and a stable primary control rather than for having literally one resource limit. See the [Lean timing reference](https://lean-lang.org/doc/reference/latest/IO/Timing/) and [allocator interface](https://github.com/leanprover/lean4/blob/master/src/include/lean/lean.h).

Rocq is the closer precedent for measuring allocation volume: it exposes `Timeout` separately from `AllocLimit`, with allocation stated in machine words and dependent on memory-profiling support. That demonstrates the usefulness of allocation-volume accounting while also showing why Curios cannot adopt the mechanism directly across native and wasm32 targets. See the [Rocq vernacular command reference](https://rocq-prover.org/doc/master/refman/proof-engine/vernacular-commands.html).

Curios therefore remixes Lean's deterministic accounting and stable primary deadline with Rocq's sensitivity to allocation volume, replacing implementation-dependent allocation units with preflighted logical units reproducible by both the elaborator and the certificate kernel.

This precedent motivates the design without defining Curios semantics. The price list, the charge-first rule, the retention split, the propagation contract, and independent kernel enforcement in this specification are normative.

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

The implementation diff includes focused unit tests near each accounting owner and integration tests for the paired computed-versus-parameter behavior. Measurement probes remain ignored and bounded; ordinary tests are deterministic and do not observe resident-set size.

Recalibration changes what an existing budget figure buys, so fixtures that state a budget are expected to change with it. Update the assertion to what the corrected pricing says, rather than preserving an old figure's outcome.

Once all acceptance criteria pass, move the stable contract and rationale into permanent documentation, check off the roadmap item, and delete this working specification in the same landing change.
