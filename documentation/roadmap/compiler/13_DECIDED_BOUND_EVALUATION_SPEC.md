# A decided bound evaluates within deterministic resource budgets

This is the implementation specification for making type-level evaluation of decided bounds fail predictably before it can exhaust process memory.

The motivating case is an omitted decided proof such as `Bytes/slice(built, 0, 10)`, where proving `Le(10, Bytes/len(built))` reduces a large computed `built`. The current step budget limits reducer transitions, but a single transition may allocate an arbitrarily large value and repeated binary concatenation may copy and retain a growing sequence of flat buffers. A declaration can therefore exhaust memory while remaining within its step budget.

This specification preserves the language design recorded in `documentation/DESIGN.md`: bounds remain decided propositions, and omitted proofs may be discharged by reduction. It adds a second, deterministic limit to that reduction. It does not change a decided bound into an inductive precondition, add a solver, or require users to hide every computed subject behind an opaque parameter.

## Current path

`Bytes/slice` declares implicit `@ordered: Le(start, end)` and `@within: Le(end, Bytes/len(bytes))` arguments in `curios-text/src/prelude.rs`.

`curios-elab/src/elaborate/apply.rs` asks `trivially_inhabited` to synthesize an omitted implicit argument before falling back to a metavariable. `trivially_inhabited` in `curios-elab/src/elaborate/metavar.rs` fully reduces the proposed type and returns `qed` when the result is `True`.

The elaborator reducer in `curios-elab/src/reduce.rs` charges the declaration's step counter for reduction work. The current counter is initialized from `DEFAULT_STEP_BUDGET` in `curios-elab/src/context.rs`.

Binary concatenation reaches the shared intrinsic fold in `curios-core/src/reduce/intrinsic.rs`. When both operands are literals, `PackedBin::concat` in `curios-base/src/packed.rs` allocates a new flat buffer and copies both operands. Repeated append therefore performs quadratic cumulative copying for a linearly growing result.

The elaborator's closed-reduction cache may retain the distinct growing terms and results. The immediate failure is consequently not an unfolding-discard bug: reducing the proof obligation really constructs the subject, and the retained constructed values can legitimately grow beyond available memory.

The certificate kernel has an independent reducer in `curios-cert/src/kernel/whnf.rs`. It shares intrinsic folds with the elaborator and must enforce the same resource contract independently. Its memo `Replay` data already preserves step accounting across cache hits; materialization accounting must receive the same treatment.

The runtime byte representation is not the type-level representation. The continuation backend uses a rope-like representation for efficient runtime concatenation, but changing the runtime representation cannot bound compile-time reduction.

## Decision

Every bounded reduction has two counters: reduction steps and materialization units. Evaluation may proceed only while both counters have sufficient credit.

The step counter retains its current meaning. The materialization counter limits cumulative logical storage created or retained by the reducer, including storage created inside a single intrinsic fold.

Materialization is charged before allocation or cache retention. If the requested charge would overflow or exceed the remaining budget, reduction returns resource exhaustion without attempting the allocation.

Charges are never refunded during a bounded reduction. This makes the limit independent of allocator reuse, garbage collection, destruction order, and cache eviction. It also means cumulative successfully constructed reducer-owned storage bounds the reducer-owned storage that can still be live.

Existing input terms and source text are not retroactively charged. Any new payload, collection slot, term node, or cache entry constructed as part of reduction is charged, including temporary values that are later discarded.

Materialization units are machine-independent logical words, where one unit represents eight bytes of scalar payload or one reference-sized logical slot. The accounting uses fixed formulas rather than `size_of`, allocator capacities, resident-set size, or platform-dependent big-integer limb layouts.

The following table defines the minimum accounting categories. The allocation-site audit in milestone M0 may add categories, but it must not weaken these charges.

| Constructed value | Materialization charge |
| --- | --- |
| Packed bytes | Fixed value header plus `ceil(byte_length / 8)` units |
| Packed bits | Fixed value header plus `ceil(bit_length / 64)` units |
| Big natural or integer | Fixed value header plus the number of required base-2^64 logical limbs |
| List, vector, or argument storage | Fixed collection header plus one unit per retained slot, plus charges for newly constructed elements |
| Term node | Fixed charge for its variant plus one unit per retained child or scalar field |
| Reduction-cache or kernel-memo entry | Fixed entry charge plus charges for newly cloned or otherwise newly retained keys, values, and replay data |
| Temporary reducer buffer | Fixed buffer header plus its requested logical payload or slots |

Fixed header and term-variant charges are named constants in the shared accounting module. They are deliberately conservative, documented beside the constants, and covered by tests. They are not claims about the exact Rust heap layout.

For a result whose size is computable from its operands, the reducer computes and spends the complete result charge before constructing the result. Size arithmetic is checked. An arithmetic overflow is reported as materialization exhaustion rather than wrapped, truncated, or passed to an allocator.

For incremental construction whose final size is not known, each increment is precharged before it is appended. Implementations may reserve or build more efficiently, but optimization must not reduce the logical charge below the specified constructed value.

The default materialization budget is a product limit chosen from measurements, not guessed in this document. Its measurement and safety margin live beside the ignored measurement probe required by M0 so the value and the evidence cannot drift apart.

## Public configuration

A shared `ReductionBudget` value carries `steps` and `materialization` limits through pipeline entry points instead of adding unrelated scalar parameters at each layer. The shared value and materialization-unit arithmetic belong in `curios-core`; the elaborator and certificate kernel own separate mutable counters and do not share reduction state or judgments.

The existing `--budget <STEPS>` command-line option remains the step limit and retains its current compatibility. A new `--materialization-budget <UNITS>` option overrides the default materialization limit. Embedders that do not provide an override receive both product defaults.

Test helpers may set either dimension explicitly. Tests that assert step exhaustion must provide enough materialization credit to ensure the tested failure remains step exhaustion, and conversely for materialization tests.

## Accounting boundary

The shared `Reducer` interface used by intrinsic folds gains a fallible operation for spending materialization units. Any shared fold that can allocate reducer-owned logical storage calls that operation before allocating.

Checker-specific reduction code uses the same accounting formulas and precharge rule for constructions outside shared folds. A new allocation path is incomplete until its maximum logical result size is either precharged or shown to reuse already charged storage without cloning or growth.

The M0 audit covers at least packed bytes, packed bits, big naturals and integers, shifts, list construction and slicing, argument vectors, term reconstruction, substitutions that clone payload, elaborator reduction-cache insertion, and kernel memo/replay insertion.

The materialization limit is not merely a byte-concatenation guard. A type-level shift, large integer operation, list operation, or future intrinsic must not be able to allocate an unbounded result in one charged step.

An optimization such as a type-level byte rope may be added later for time complexity, but it is not the safety boundary. Every representation must remain accounted, and flattening or copying it must be precharged.

## Elaborator behavior

Automatic proof synthesis continues to reduce decided propositions with ordinary transparency. A computed subject is allowed to evaluate when both budgets permit it.

An opaque parameter remains a useful programming idiom because reduction stops at the parameter, but this specification does not give parameter opacity special semantics and does not require an opacity barrier around computed subjects.

Resource exhaustion is not equivalent to "not trivially inhabited." The path through `trivially_inhabited` must preserve and propagate step or materialization exhaustion instead of converting either error into `None` and silently falling back to an unresolved implicit argument.

When synthesis of an omitted proof exhausts a resource, the diagnostic identifies the omitted argument and distinguishes `reduction step budget exhausted` from `reduction materialization budget exhausted`. The latter reports the remaining units and the attempted charge when those values are representable.

Ordinary definitional equality, explicit decided proofs, and other reductions use the same counters. Protecting only omitted-proof synthesis would leave the resource hole open.

The elaborator charges a closed-reduction cache entry when it is first retained. A cache hit that returns an already retained result does not construct that result again and therefore does not repeat its materialization charge. This is an elaborator implementation rule, not a kernel acceptance rule.

## Kernel behavior

Certificate checking enforces its own step and materialization limits and never trusts accounting performed by elaboration or compilation.

Kernel memo replay records both the step cost and materialization cost of the original computation. A memo hit spends both recorded costs before returning the memoized result so acceptance and resource exhaustion do not depend on whether the memo happened to be warm.

If either replay charge is unavailable, the kernel reports the corresponding resource exhaustion without returning the cached result. Replay-cost arithmetic follows the same checked-overflow rule as direct reduction.

The kernel and elaborator use the same unit definitions and per-construction formulas, but tests must not assume that their total costs are identical because their evaluators and retained structures differ.

## Diagnostics and failure contract

Step exhaustion and materialization exhaustion are distinct structured reduction errors through the core, elaborator, pipeline, CLI, browser boundary, and certificate checker. Human-readable messages are rendered only at the appropriate boundary.

Materialization exhaustion is a normal deterministic rejection, not a panic, allocation failure, process abort, or generic internal error.

No error path attempts the refused allocation for the purpose of producing a diagnostic. Diagnostic construction itself uses only bounded metadata captured before refusal, such as the operation category, remaining units, and attempted units.

The limit bounds reducer-created logical materialization, not total compiler memory. Parsing a huge source file, retaining caller-owned terms, backend compilation, allocator overhead, thread stacks, and unrelated process memory remain outside this contract and must not be described as covered.

## Implementation milestones

### M0: Audit and measurement

- Add an ignored, explicitly bounded measurement probe beside the motivating numeric test. Record the command, input sizes, observed step use, observed logical materialization, and peak process memory beside the probe.
- Inventory every reducer allocation site in `curios-core`, `curios-elab`, and `curios-cert`, classify ownership and cloning, and turn the inventory into checked items in this specification or an implementation-local checklist linked from it.
- Measure representative prelude compilation and certificate checking to choose defaults with documented headroom. Do not run the known unbounded 100,000-iteration reproducer to completion as part of routine verification.

### M1: Resource representation and plumbing

- Introduce checked materialization-unit arithmetic, structured exhaustion errors, the composite `ReductionBudget`, and independent counters in the elaborator and kernel.
- Thread the composite budget through pipeline, CLI, browser, tests, and certificate entry points without merging elaborator and kernel state.
- Propagate resource errors through automatic implicit synthesis and add the two distinct diagnostic renderings.

### M2: Precharge all reducer construction

- Add the shared reducer spending seam and precharge every audited shared intrinsic allocation.
- Precharge checker-specific term reconstruction, temporary collections, cache retention, memo retention, and replay metadata.
- Record and replay the kernel's materialization cost together with its step cost.
- Treat any unaudited allocation discovered during implementation as part of this milestone rather than narrowing the guarantee to the motivating byte path.

### M3: Calibrate, verify, and document

- Set product defaults from the M0 measurements with enough headroom for the fixed prelude and representative certificates.
- Update `documentation/USAGE.md`, `documentation/DESIGN.md`, relevant crate-level docs, and CLI help with the two-budget contract and its limits.
- Keep measurement values beside the ignored probe and keep normative semantics in the permanent documentation rather than in this roadmap file.
- Complete the acceptance suite and the repository verification gate before checking off the roadmap item.

## Acceptance criteria

- The motivating computed-subject example fails with materialization exhaustion before a large allocation, process-memory spike, abort, or operating-system kill.
- The paired example whose subject is behind a parameter still elaborates under a low budget and does not materialize the hidden subject.
- A single intrinsic operation whose result exceeds the remaining budget is refused by preflight without attempting its allocation.
- Repeated concatenation with distinct growing cached results is bounded by cumulative charges even when every individual result would fit.
- Large packed-bit, big-integer or shift, and list-producing reductions have focused preflight tests so bytes are not the only protected payload.
- Raising only the materialization limit permits a moderate computed decided bound to complete when its step limit is sufficient.
- Lowering only the step limit still produces step exhaustion when its materialization limit is sufficient.
- Automatic implicit synthesis propagates both exhaustion categories and never disguises them as an unsolved metavariable.
- Elaborator cache insertion and hits obey the documented charge rule.
- Kernel direct evaluation and memo replay produce the same acceptance or exhaustion result for the same kernel budget.
- Checked size arithmetic rejects overflow before allocation.
- Default budgets compile the fixed prelude and pass representative source and certificate tests with the measurement-documented margin.
- Exhaustion diagnostics are stable enough for focused tests but do not expose allocator-specific sizes or platform-dependent layout.

## Non-goals and rejected alternatives

Changing decided bounds to inductive proof obligations is rejected because it changes the language design and gives up the intended reduction-based ergonomics.

Adding a specialized arithmetic or bounds solver is not required. Such a solver could avoid some evaluations but would not protect ordinary definitional equality or allocating reductions elsewhere.

Restricting transparency during automatic proof synthesis is not part of this change. Mature systems use transparency and opacity controls to tune automation, but changing what Curios unfolds is a separate semantic and compatibility decision.

Replacing flat type-level bytes with ropes is not sufficient. It can improve concatenation complexity but moves the large allocation to flattening and does not cover lists, integers, bits, terms, or caches.

An operating-system memory limit or allocator hook is not the language-level contract. Those mechanisms are platform-dependent, apply too late for useful diagnostics, and include memory outside reduction.

Exact heap-byte accounting is not promised. The deterministic logical-unit budget is intended to reject pathological construction predictably across supported platforms; process-level memory remains subject to ordinary implementation overhead.

## Precedent

Lean's heartbeats deliberately approximate allocation work rather than only recursive calls, while its transparency modes separately control unfolding during automation. Rocq exposes both time and allocation limits and separately supports opacity and reduction strategies. Agda likewise uses opacity to control unfolding and performance. Curios adopts the common separation of resource limits from transparency policy, while using a deterministic logical materialization measure suitable for both its elaborator and certificate kernel.

This precedent motivates the two-dimensional design but does not define Curios semantics. The accounting table, precharge rule, propagation contract, and independent kernel enforcement in this specification are normative.

## Verification and retirement

This cross-cutting change requires the full repository done bar from `CLAUDE.md`, including formatting, linting, workspace tests, release build, documentation, invariant checks, repository hygiene, and `make curios/web` because shared reducer dependencies feed the browser build.

The implementation diff must include focused unit tests near each accounting owner and integration tests for the paired computed-versus-parameter behavior. Measurement probes remain ignored and bounded; ordinary tests must be deterministic and must not rely on observing resident-set size.

Once all acceptance criteria pass, move the stable two-budget contract and rationale into permanent documentation, check off the roadmap item, and delete this working specification in the same landing change.
