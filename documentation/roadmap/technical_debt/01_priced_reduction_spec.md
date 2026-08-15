# A reduction step costs what it builds

This is the implementation specification for making type-level evaluation refuse predictably instead of exhausting process memory.

## Status

The propagation half is done. `trivially_inhabited` no longer converts an exhausted reduction into `None`, so an omitted implicit argument whose proposition ran out of budget reports the exhaustion rather than falling back to a hole that later reads as the user's fault. That was a standalone diagnostic defect, correct under any pricing, and it landed ahead of this work.

**Ma is done.** A `whnf`/`forced` memo hit spends no steps, those two tables are cleared wherever the budget is restored, and the name-keyed `unfold` memo keeps both its charge and its cross-declaration life. The normative statement moved out of this file as it landed: `curios-cert`'s `spend` and `memos` module documentation, its README's memo decision, and [the evaluation memo](../../soundness/what-the-kernel-consults/the-evaluation-memo.md), which records what the weakened assumption no longer covers. The before-and-after is beside `curios`' `kernel_memo_charge_measurements` and the certification row of `curios-prelude-archive`'s `stored_prelude_measurements`.

Two things about it are worth carrying forward. The retake reproduced the floor and divergence figures under *Kernel behavior* exactly and the certification figure only in its shape — 6.2 s to 6.1 s rather than 6.6 to 6.5, on a machine whose absolute number differs. And it left a residue this file did not predict, recorded in `spend`'s module documentation and in the soundness entry: an `unfold` record is measured over a computation that may itself have taken free term-keyed hits, so it can record less than the same body costs cold, which makes what a declaration is charged for a name depend on which declaration first unfolded it. The direction is undercharging, so it only ever accepts, and closing it needs the priced replay record M2 already carries.

**M0 is done.** The inventory is [The reducer allocation audit](01_priced_reduction_audit.md), a checklist M1 ticks. It found five sites beyond the four this file names, and the ones worth knowing before reading it are that `Natural::checked_shl` hands a converted shift amount straight to the allocator — so one well-typed term with no loop in it can ask for an arbitrary allocation — and that `Telescope::open` clones its whole chain at every beta step, on the hottest path either checker has.

**M1 is done but for two rows, which the audit records as stated residues rather than omissions.** `Cost` and the price constants live in `curios-core` below both checkers, `Reducer` carries the shared spending operation, and construction is charged in every audited shared fold, at both checkers' beta/eta/zeta/`rec` paths, and at the `recurse` bracket. `--budget` now names units of reduction work. What is left open: `release`/`capture` are charged per binder opened rather than per node rebuilt, and `bin_atoms`/`bin_segments` are unpriced temporaries — both bounded by an already-charged term rather than unbounded, which is the distinction the audit's note draws.

Three things it decided that this file did not anticipate.

**The frame row is charged per new *peak* depth, not per `recurse` call.** A level's native frame is reclaimed when the level returns, and reduction re-enters itself once per operand and once per spine link, so charging every call prices a stack the reduction is not holding — measured: the fixed prelude would not build at a hundred times the default. A high-water mark charges the peak, never refunds, and costs a wide shallow computation nothing. The two alternatives are worse and are recorded where the code is: charging when `recurse` actually grows prices the segment exactly and makes acceptance depend on the host thread's stack size, and charging nothing leaves depth bounded by the host alone.

**The frame constant is measured and there are two figures.** A guarded reduction level takes 7 264 bytes of native stack in release and 97 200 in debug, identical to the byte at every depth. The release figure is charged, because a charge must be a property of the program rather than of the build that checked it, and release is what ships.

**The default is recalibrated provisionally, from 1 000 000 to 30 000 000.** The heaviest prelude declaration measured between 2.5 and 3 million units, against about 91 000 steps before, so this keeps roughly the tenfold margin the old figure held. `curios-elab`'s `DEFAULT_STEP_BUDGET` carries the full reasoning; two things it establishes belong here.

**A single oversized construction is affordable at any default the prelude can build under**, which this file's acceptance criteria did not anticipate. `Nat/shl(1, 400000000)` prices at 6 250 004 units and builds fifty megabytes; refusing it needs a default of six million, twice the prelude's own floor with no margin. Measured at about 28 bytes of process memory per unit, this default admits roughly 780 MB in one declaration. What the charge bought is a ceiling where there was none, not a low one — the weighted single limit this file's *Refused alternatives* accepts, now with numbers.

**A `Str` literal's ceiling fell, from roughly 12 000 characters to under 6 000.** A literal's derivation nests one reduction level per byte, so the frame row charges it directly. That is not a calibration this default can fix, and it is [A string literal is checked once per use](04_string_literal_cost_spec.md)'s to fix rather than this one's — its *What spec 01 measured* section now carries the figures and what they change about its own scoping.

The retention quota and the calibration are pending.

## The defect is the price of one transition

The step counter charges one unit per reducer transition regardless of what that transition constructs. A `PackedBin::concat` that copies half a megabyte costs exactly what a `Bool` fold costs, and a `recurse` level that takes a fresh 32 MiB stack segment costs the same again.

That is the whole defect. `curios-elab/src/reduce.rs`'s note on the reduction loop already records the consequence — the budget bounds steps, so nothing bounds the memory a reduction allocates — and [the decided-bound entry](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md) and [Depth is bought with stack, not with hand-rolled frames](../../design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md) record it twice more, where growing the stack rather than aborting was accepted on exactly this trade.

Reduction is the only stage that can be driven to arbitrary cost by a well-typed program, because it is the only stage a *type* can call. Every other stage's work is bounded by the size of what elaboration produced. That asymmetry is why the bound belongs on the counter rather than on the process.

## What the measurement decided

The reproducer is the accumulate-then-slice shape: a `rec` that appends a fixed run to an accumulator, whose result then stands under a `Bytes/slice` bound. It is parameterized by iteration count, and three arms separate the costs — the bound read off an opaque parameter, so nothing evaluates; the same iteration count with an accumulator that is replaced rather than extended, so there are steps without payload growth; and the growing accumulator, which has both.

**The probe exists.** It lives in `curios/src/tests/reduction.rs`, over both the `Bin` and `List` carriers, with its figures beside it. What follows is what it decided, not numbers this file could keep true.

Three conclusions followed, and the design rests on them rather than on the motivating anecdote. **Two survive unchanged and one has been overtaken**, which is stated rather than quietly edited because the one that moved is the one the motivating anecdote came from.

**The per-transition machinery is linear and modest.** The fixed-payload arm grows linearly in iteration count at a small constant. Nothing about performing a transition is superlinear. **This is the load-bearing conclusion for calibration and it is untouched**: measured at roughly 2 KiB retained per transition while constructing nothing, so a budget of a million transitions admits about two gigabytes before a single byte of payload is built. Calibration selects against that floor.

**The growth was constructed payload that reduction then retained** — the growing arm's excess over the fixed-payload arm converged on a quadratic in the iteration count, and the counter observed only the linear part. **That arm is now linear.** Fusion no longer recopies an accumulator past a documented operand size, and the free monoid's measure reads a length off the resulting spine, so the shape that produced this conclusion no longer reproduces: peak memory for the same program fell from 321 MiB to 52 MiB above baseline and stopped growing quadratically. The conclusion was true when taken and the pre-change figures are recorded beside the probe; what it no longer supplies is a *reproducer*, so the acceptance criteria below name shapes the cap does not flatten.

**The same expression is linear when the program runs it.** `curios/src/tests/runtime.rs`'s `accumulation_loops_are_linear_by_construction` measures the identical loop end to end at a hundred thousand steps. Compile-time evaluation of that now costs about seventeen million reduction steps — sixteen times the default budget, so it refuses — where it once exhausted the machine instead. (The "already costs gigabytes" this paragraph used to carry was never checked and was wrong: at the largest iteration count the default budget admitted, the cost was 321 MiB.)

So a step count cannot see either lever, and both levers are construction: what a transition builds, and how long the reducer keeps it. That is unchanged by any of the above — a cheaper carrier spends fewer units for the same result, and the unit, the price list and the acceptance threshold are unaffected. The limit must price both, in units a representation change cannot move.

## Decision

Reduction keeps exactly one verdict-affecting counter. A transition costs one unit plus the logical size of whatever it constructs — **and a computation is charged once, not once per time its result is asked for.**

Those are two halves of one sentence, and the second is as load-bearing as the first. Pricing construction closes what the counter cannot see; charging distinct reductions closes what it sees twice. A counter that priced construction perfectly while still charging a memo hit the whole cost of the computation it replaces would refuse programs whose work it never did — measured, today, at a 262 144-step budget declared exhausted after **6 547 actual reduction steps**.

The work counter, its restoration at declaration boundaries, its command-line option, and its exhaustion error keep their present roles. The option's spelling remains `--budget`; its unit, help text, internal names, and calibrated default change from transitions to priced reduction work.

A separate compilation-scoped retention quota limits optional cache and memo storage. It is not a second acceptance budget: exhausting it refuses an insertion and leaves evaluation correct but cold. The distinction between transition-dominated and construction-dominated exhaustion is preserved in the diagnostic without splitting the limit that decides acceptance.

Cost is charged before allocation. If the requested charge would overflow or exceed the remaining budget, reduction returns exhaustion without attempting the allocation.

Charges are never refunded within a declaration's budget window. This makes the limit independent of allocator reuse, garbage collection, destruction order, and cache eviction, and it means the cumulative successfully constructed reducer-owned storage bounds the reducer-owned storage that can still be live.

Existing input terms and source text are not retroactively charged. Any new payload, collection slot, term node, or native stack segment a reduction causes is charged, including temporary values later discarded. Optional cache-table bookkeeping is not semantic construction and is charged only to the retention quota.

Units are machine-independent logical words: one unit covers eight logical bytes of scalar payload or one abstract reference slot. An abstract slot is one unit on every target; it is not a claim that a physical pointer occupies eight bytes. The accounting uses fixed formulas rather than `size_of`, allocator capacities, resident-set size, or platform-dependent big-integer limb layouts.

That independence is load-bearing rather than fastidious. `curios-js` compiles to wasm32, where `usize` and `num-bigint`'s digit width both differ from the native target, and its budget constant exists to promise that a program compiling in the playground compiles at the command line. All charge arithmetic is therefore computed in `u64` regardless of host pointer width.

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

`normalize_concat` in `curios-core/src/free_monoid.rs` is the representative shape. Its fusing closure returns a `Subterm` infallibly today, so charging at the point of allocation makes the closure and the function fallible, across its binary and list callers. Expect that shape repeatedly rather than once. It still fuses, and still copies when it does — `FUSION_CAP` only bounds *how much* it will copy at once, which changes how often this site is reached and not whether it must charge.

The audit covers at least packed bytes, packed bits, big naturals and integers, shifts, list construction and slicing, argument vectors, term reconstruction, scope and telescope traversal, substitutions that clone payload, elaborator reduction-cache insertion, and kernel memo and replay insertion. It also covers temporary allocation hidden inside hashing, equality, collection growth, and convenience conversions.

Four sites are named because they were found by reading rather than by category, and they are the shape the rest of the audit should expect.

`bin_shape` classifies a `Bin` value by materializing its whole run into a `Vec` — one element per byte at `Grain::X`, one per *bit* at `Grain::B` — and an operation whose result is a single `Nat` therefore allocates the entire subject to compute it. `Bin/len` no longer reaches it for a wholly-literal value, which now answers from the free monoid's measure, but every symbolic shape still falls through to the homomorphism and so still pays this. The site is unfixed; only its traffic changed.

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

The default is chosen from the completed measurement probe and the M0 baselines, and the fixed-payload arm supplies the floor the choice must respect: a construction-free program already retains about 2 KiB per transition, so a budget of one million transitions permits roughly two gigabytes before a single byte of payload is built. Calibration therefore selects against observed memory per unit, not only against whether the fixed prelude fits.

**Calibrate against a program that replays, not only one that evaluates once.** Per *Kernel behavior*, the kernel charges a memo hit the recorded cost of the computation it replays, and construction charges ride that record — so a construction-heavy subterm hit k times is charged k× its construction while being built once. A default set against a corpus that never replays would be set against a number no user program is charged. The fixed prelude alone is not sufficient evidence here; the corpus must include a shape that hits the same memoized construction repeatedly.

The value and its evidence live beside the ignored probe introduced with accounting, so the figure and the thing that would check it cannot drift apart.

## Configuration

`--budget <UNITS>` remains the only verdict-affecting reduction limit and keeps its option name. Its help text, `documentation/usage.md` row, and internal `DEFAULT_STEP_BUDGET` terminology change from steps to reduction work. Its calibrated default changes as well.

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

**This section is retained as the argument that was made, not as pending work: Ma has landed.** What it decided is now stated where the code is — `curios-cert`'s `spend` and `memos` modules, its README, and the soundness entry — and what follows describes the design it replaced before replacing it.

The kernel's spend component records what a remembered computation consumed and charges a memo hit across two quantities: reduction steps and minted binder identities. Its invariant *before Ma* was that the whole observable trajectory — refusal payloads, exhaustion points, and later-minted identities — is bit-identical with evaluation memos on or off. Ma narrows that to the semantic half, for the reasons below.

**What checks the invariant is weaker than the invariant, and that is worth knowing whichever way this goes.** `curios-prelude-archive`'s `kernel_memo_parity` compares verdicts, at one budget, on a corpus where nothing exhausts, and is `#[ignore]`d — so the one regime in which the charge model is *observable* is the one regime nothing checks, and no ordinary run checks any of it. That is why the strengthening promised at M2 is stated below in terms of semantic verdicts and identity trajectories rather than exhaustion payloads: after Ma, exhaustion points are permitted to differ, and a test asserting they do not would be asserting the design that Ma removes.

**That charge prices what a memo-free evaluator would have spent, not what the kernel does, and it can therefore be superlinear in the work performed.** `Spend::charge` in `curios-cert/src/kernel/spend.rs` subtracts a `Replay`'s whole recorded cost, and recorded costs *compound*: if computing `Aₙ` hits the memo for `Aₙ₋₁` twice, `S(n) = 2·S(n−1) + c`. A structure cheap to evaluate with memos can be expensive to charge. Measured while the free monoid's fusion cap was landing: the kernel declared a 262 144-step budget exhausted after **6 547 actual reduction steps**, on a program whose cold cost was genuinely quadratic while its memoized evaluation was linear.

**This specification changes it.** A `whnf`/`forced` memo hit spends no steps, and those two tables are cleared where `restore_budget` fires. The name-keyed `unfold` memo is untouched — a charged hit is history-independent, so cross-declaration sharing of definition unfolds stays exactly as safe as it is today, and it is what makes whole-module certification affordable.

**What makes free hits deterministic is aligning two lifetimes that are already mismatched.** The budget is restored at every declaration boundary, on the stated grounds that "whether one declaration typechecks must not depend on how much the declarations before it had already spent"; the memo is not, and persists across the whole module walk. Today that asymmetry is invisible only because hits cost full price. Clearing the term-keyed tables where the budget is restored makes *which entries are present* a function of the declaration under judgment, so a hit can be free without anyone trusting a cache policy. `memos.rs` already argues the entries themselves are semantically determined — a whnf entry is stored only for a local-free term, so "the reduct is a function of the definition store alone"; only their *presence* was historical, and per-declaration clearing removes that.

**The property `Kernel::uncached` protects is not the one free hits give up.** Free hits can only *reduce* spend, so they can only turn a refusal into an acceptance, and only an **exhaustion** refusal — a semantic refusal is budget-independent, and exhaustion masking a type error simply reaches the type error with more budget and refuses anyway. So the invariant weakens from "memos change nothing" to "**memos change only resource verdicts, never semantic ones**", which is still a proper invariant and still testable. Of the three things `spend.rs` claims are bit-identical with memos on or off, two survive: refusal payloads for semantic errors, and later-minted identities, because a hit still advances the entropy exactly as a recomputation would. Only exhaustion points move.

Termination is unaffected, and the reason is structural: a `Replay` is built *after* the reduct exists, so a divergent reduction never completes, never stores, and can never be hit — every step of one is charged. A memo hit only ever hands back a finished computation, and handing back a finished computation does not continue a loop.

**Verified before adoption**, on the tree that landed the free-monoid measure:

| | today | with free hits |
| --- | --- | --- |
| kernel budget floor, accumulate-then-slice at n = 800 | 131 072 | 16 384 |
| same, kernel-versus-elaborator divergence | 8–16× | 1–2× |
| whole-unit certification | 6.6 s | 6.5 s |
| `kernel_memo_parity` | passes | passes, unchanged |
| workspace suite | 1843 pass | 1843 pass |

Per-declaration clearing measured free on both axes — same budget floors as the non-clearing variant, and certification unmoved against a control taken on the same machine. The elaborator/kernel divergence closing is the practical payoff: that gap is why the elaborator twice accepted what the kernel then refused, silently, while the free-monoid measure was being developed.

**It lands before the pricing work and needs no recalibration to do so**, because free hits are monotone: they only reduce spend, so no program that compiles today can stop compiling. The default is retuned once, at M3, against both halves together.

**Why this is not left as a separate concern from construction pricing: the two multiply.** Construction charges ride the recorded cost, so under the present rule a construction-heavy memoized subterm hit k times would be charged k× its construction while being built once. Pricing construction without this change would therefore *amplify* the defect rather than sit beside it, and M3 would be calibrating a default against numbers no user program is charged.

Construction charges still ride the existing mechanism where a computation *is* performed. The recorded step cost becomes the recorded priced cost; what changes is that a `whnf`/`forced` hit no longer spends it. Cache-table construction and insertion are excluded from that cost and consume only retention quota, so enabling memos cannot make the first evaluation spend more semantic work than disabling them.

**The `unfold` replay path keeps its present shape in full.** If the charge fits, the kernel spends it and advances the recorded identities. If it does not, the kernel bypasses that entry and evaluates under the actual remaining budget, so the direct path identifies the same first failing charge and advances exactly the identities reached before it, rather than returning a cached result or manufacturing a diagnostic from an aggregate. Replay arithmetic follows the same checked-overflow rule as direct reduction.

**A second beneficiary, independent of anything above.** A `Str` literal's UTF-8 validity is discharged by running a fold over its bytes, and the kernel re-runs it at every use site — measured at 83 steps per character *per use*, against an elaborator that is flat regardless of use count because its hits are already free. Free hits remove that multiplier. The rest of that cost is not a pricing defect and is specified separately in [A string literal is checked once per use](04_string_literal_cost_spec.md); it is named here because it is the case that shows this change reaches ordinary code, not only a loop written to stress the reducer.

If the full replay charge fits, the kernel spends it and advances the recorded binder identities exactly as it does today. If it does not fit, the kernel bypasses that memo entry for the recomputation and evaluates under the actual remaining budget; any nested replay that does not fit follows the same rule. The direct path then identifies the same first failing charge and advances exactly the identities reached before that failure, rather than returning the cached result or manufacturing a diagnostic from an aggregate total. Replay arithmetic follows the same checked-overflow rule as direct reduction.

The kernel and elaborator use the same unit definitions and per-construction formulas. Their totals differ, because their evaluators and construction paths differ and elaborator cache hits remain free, so tests compare verdicts and failure points rather than sums.

## Diagnostics

Exhaustion remains one structured error. Its existing headline stays recognizable while focused detail identifies the refused charge.

Attribution is kept without splitting the budget. The error carries the failing charge category, the remaining budget, and the attempted charge. Categories distinguish transition work from packed payload, bigint limbs, collection slots, term reconstruction, substitution, recursion depth, and other priced construction, while one number continues to decide acceptance. "Dominant category" is deliberately not promised: determining dominance would require cumulative per-category accounting unrelated to refusal.

Exhaustion is a normal deterministic rejection, reported as a diagnostic like any other refusal.

Diagnostic construction uses only bounded metadata captured before refusal — the operation category, the remaining budget, and the attempted charge — so no error path attempts the allocation that was refused.

The limit bounds reducer-created logical work, reducer-caused stack growth, and reducer retention. Parsing a huge source file, caller-owned terms, backend compilation, allocator overhead, ambient thread stacks, and unrelated process memory sit outside this contract and are bounded, where they are bounded, by other means.

## Milestones

### Ma — Charge a computation once — **done**

Ordered first and independently landable. It is verified, it is two functions wide, and it is monotone — free hits only reduce spend, so nothing that compiles today can stop compiling and no fixture's stated budget has to move for it. Everything after it is the construction-pricing work, which it must precede so that M3 calibrates one default against both halves rather than two against each.

- Make a `whnf`/`forced` memo hit spend no steps while still advancing minted identities, and clear those two tables where `Spend::restore_budget` fires. Leave the name-keyed `unfold` memo charged and cross-declaration.
- Rewrite `curios-cert/src/kernel/spend.rs`'s module documentation, whose stated invariant — refusal payloads, exhaustion points and identities all bit-identical with memos on or off — is the justification for the design being replaced. The new statement is that memos change only resource verdicts.
- Add the two properties nothing asserts today: that cached spend never exceeds uncached, and that reducing the same closed term twice within one declaration charges the second time O(1).
- Strengthen `kernel_memo_parity` to the semantic half explicitly — it passes unchanged, because it compares verdicts on a corpus where nothing exhausts, and that is now the property it is *for* rather than an accident of what it happens to cover.
- Record the measured before-and-after beside a probe, per the figures under *Kernel behavior*.

### M0 — Audit and baselines — **done**

- Inventory every reducer allocation site in `curios-core`, `curios-elab`, and `curios-cert`, classifying each as construction or sharing, and turn the inventory into a checklist linked from this specification. The four sites named under *Accounting boundary* are its seed, not its extent. → **[The reducer allocation audit](01_priced_reduction_audit.md)**, which also covers `curios-utilities` and `curios-num`: the four seed sites are three-quarters in `PackedBin`, and every packed-payload and bigint-limb charge lands on a method those two crates own.
- The parameterized accumulate-then-slice reproducer is **already in the tree**: `curios/src/tests/reduction.rs`, three arms over both the `Bin` and `List` carriers, ignored and explicitly bounded, recording command, input sizes, transition count, wall time and peak process memory, with pre- and post-cap figures beside it. Read it rather than rebuilding it. What it does *not* yet carry is priced work, which M1 adds.
- Record the same baselines for representative prelude compilation and certificate checking. Priced work and retention do not exist yet and are not claimed at this milestone.

### M1 — Price the counter — **done but for two rows**

- Introduce checked unit arithmetic and the price constants in `curios-core`, with the shared spending operation on `Reducer`.
- Charge construction in every audited shared intrinsic fold, making the fusing seams fallible where charging at the allocation point requires it.
- Add accounting-aware or conservatively preflighted paths for checker-specific term reconstruction, binder traversal, temporary collections, and substitutions that clone payload.
- Charge the recursion level at the `recurse` bracket, from a constant justified beside the measured worst-case frame.
- Extend the M0 probe with observed priced work beside the memory it already records.
- Treat any unaudited allocation discovered during implementation as part of this milestone rather than narrowing the guarantee to the motivating byte path.

### M2 — Retention and replay

- Add the compilation-scoped retention counter, charging conservatively reachable payload on elaborator cache and kernel memo insertion and exhausting into a cold cache rather than a refusal.
- Carry the priced cost through the kernel's `unfold` replay record, add unaffordable-replay fallback, and extend parity tests to that path's low-budget exhaustion, diagnostic payload, and minted identities. Term-keyed hits are free after Ma, so parity there is over semantic verdicts and identities rather than exhaustion points.
- Add the failing-charge diagnostic metadata.
- Extend the probe with observed retention consumption.

### M3 — Calibrate, verify, document

- Use the completed probe and M0 baselines to set both defaults, respecting the per-transition memory floor as well as the fixed prelude's headroom.
- Update `documentation/usage.md`, `documentation/design.md`, the CLI help, and relevant crate documentation with the widened meaning of the budget and its scope.
- Keep measurement values beside the ignored probe and normative semantics in permanent documentation rather than in this roadmap file.
- Complete the acceptance suite and the repository verification gate before checking off the roadmap item.

## Acceptance

- A construction-dominated fixture fails with budget exhaustion before a large allocation, process-memory spike, abort, or operating-system kill. **It cannot be the accumulate-then-slice shape any more**: capping fusion made that program's construction linear, so it now refuses on ordinary step cost like any other long computation. Pick a shape the cap does not flatten — a single oversized construction, a large shift, or a big-integer operation, each of which the criteria below already name — and say in the fixture why that one and not the accumulator.
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
- A `whnf`/`forced` memo hit spends no steps, and reducing the same closed term twice within one declaration charges the second time O(1).
- Cached kernel spend never exceeds uncached kernel spend, on every fixture that states a budget.
- Kernel direct evaluation and memo replay produce the same *semantic* verdict — acceptance, or a refusal that is not exhaustion — for the same kernel budget, and the same later-minted identities. Exhaustion points are permitted to differ, and that is the one part of the former invariant this specification gives up.
- The `unfold` replay path still produces the same acceptance or exhaustion for the same kernel budget. Focused parity tests include insufficient replay budgets and compare the failing category, attempted charge, remaining budget, and later-minted identities.
- Whole-unit certification does not regress against a control taken on the same machine, and the elaborator's and kernel's budget floors for the same program agree within a small factor rather than the 8–16× measured today.
- Checked size arithmetic rejects overflow before allocation.
- Charges are identical on the native and wasm32 targets for the same program.
- The recalibrated default compiles the fixed prelude and passes representative source and certificate tests with the measurement-documented margin.
- Diagnostics are stable enough for focused tests and expose no allocator-specific sizes or platform-dependent layout.

## Refused alternatives

**A second verdict-affecting budget dimension.** Independent transition and materialization limits are genuinely more expressive than one weighted limit: a transition-heavy program and a construction-heavy program can receive different verdicts under the two designs. Curios accepts the weighted policy tradeoff. One user-facing limit preserves the existing configuration shape, requires one default and one acceptance threshold to calibrate, and gives the elaborator and independent kernel one deterministic quantity to reproduce. The fixed price list states the exchange rate rather than hiding it. Failing-charge attribution retains useful diagnostic distinction without being claimed to make the two policies equivalent.

**Free memo hits over a memo of unbounded lifetime.** The adopted design clears the term-keyed tables at declaration boundaries; the discarded variant leaves them to persist across the whole module walk, which measured identically on every probe. It is refused for a reason no measurement shows: with an unbounded lifetime, *which* entries are present depends on which declarations were checked before, so a hit being free would make a verdict depend on check order. Clearing costs nothing and removes the dependence, so there is no case for keeping it.

**Charging a memo hit a fixed small constant rather than nothing.** It sounds more conservative and is strictly worse: it re-introduces a dependence of the verdict on the number of hits without bounding anything the free rule does not already bound, and it needs a constant nobody can derive.

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
make curios/js
```

The web build is required because shared reducer dependencies feed the browser target. The handoff also includes the documentation, invariant, and repository-hygiene review required by `CLAUDE.md`.

The implementation diff includes focused unit tests near each accounting owner and integration tests for the paired computed-versus-parameter behavior. Measurement probes remain ignored and bounded; ordinary tests are deterministic and do not observe resident-set size.

Recalibration changes what an existing budget figure buys, so fixtures that state a budget are expected to change with it. Update the assertion to what the corrected pricing says, rather than preserving an old figure's outcome.

Once all acceptance criteria pass, move the stable contract and rationale into permanent documentation, check off the roadmap item, and delete this working specification in the same landing change.
