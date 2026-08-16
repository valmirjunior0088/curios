# A closed fold should not cost what its data is long

This is the implementation specification for making evaluation of closed terms affordable at the type level, without replacing the definitions that proofs are written against and without naming any type in the remedy.

## Status

Opened on the measurements taken while retiring *A string literal is checked once per use*, whose figures live in `curios`' `str_literal_cost_measurements` and `a_str_literal_costs_about_one_frame_per_character`. Every quantity this document reasons from is reproduced by one of those two, and none of it is restated here.

Rewritten 2026-08-16. The first version proposed Agda's dual definition selected by Idris's shape recognition: a native result shadowing an authored fold when its arguments are closed. This version replaces the mechanism and keeps everything else — the objective, the acceptance criteria, and most of the refusals. What changed is the diagnosis: the cost is not a property of folds that a rule could recognise, it is a property of the evaluator that every closed computation pays, and the remedy the field converged on operates at that layer. The shape-recognised accelerator moves to *Refused alternatives*, with the argument.

It stays filed as debt rather than as a capability for the reason the first version gave: the cost is present rather than prospective, and `curios-text/src/into_core/lowerer.rs` records the shortcut it forecloses.

## What is wrong today

A fold driven at the type level costs one guarded reduction level per element. `Cost::FRAME` prices a level at the native frame it takes, so the frame row is the overwhelming majority of what a `Str` literal costs to check, and the ceiling on a literal's length follows from dividing the budget by it. Three of `Str`'s operations pay it independently — the validity scan on every literal, the codepoint count under `len` and `at`, the drop-width under `slice` — and the same cost lands on any user type whose refinement is decided by a fold over a packed carrier.

**The depth is manufactured by the interpreter, not demanded by the term.** Both reducers are recursive functions that re-enter themselves once per operand of a nested intrinsic and once per link of a match tower, and both substitute arguments unreduced. The prelude's fold forms map onto exactly those two habits. The tail-recursive accumulator — `/syn/Str`'s `scan_from` — already runs at constant native depth in the forcing loop; its linear depth is entirely the chain of unevaluated `step` applications that unreduced substitution builds, which is also what makes retention quadratic. `curios-core`'s `retention` documents the chain, and `str_literal_cost_measurements` carries the strictness measurement that isolates its cost from the fold's own. The induction-hypothesis forms — the eliminator-driven `BigNat/trim`, the self-calling `count_scalars` and `drop_width` — nest one native reducer frame per element because forcing the hypothesis re-enters the reducer recursively. Neither the chain nor the nesting is a fact about the fold: a computation over n closed elements is n small steps, and an evaluator with an explicit stack and shared thunks performs it in constant native depth while building no chain.

**So the budget is pricing the implementation and calling it the program.** The pricing decision itself states that depth is the one row whose size is set by the reduction *strategy* rather than by the term, and the frame row charges that native frame honestly — but the frame is spent on the reducer's own recursion, not on anything the term requires. That is the whole defect: honest accounting of a dishonest expenditure. The remedy is not to reprice the row, and not to exempt recognised shapes from it, but to stop spending it.

## What the field does, read at the layer that matters

Every mature system whose kernel stayed general evaluates with a machine, not with meta-level recursion.

**Rocq**'s kernel conversion is Barras's lazy closure machine — explicit stacks and environments, [`cClosure`](https://github.com/coq/coq/blob/master/kernel/cClosure.ml) — and `vm_compute` is a second kernel machine, Grégoire and Leroy's bytecode VM. Its primitive integers, arrays and strings are the other road: [listed axioms](https://rocq-prover.org/doc/V9.2.0/refman/language/core/primitive.html) the ordinary kernel does not reduce and the machines fold natively.

**Agda**'s compile-time reduction is [a call-by-need environment machine with an implicit heap](https://agda.github.io/agda/Agda-TypeChecking-Reduce-Fast.html), running beside the reference reducer, falling back to it for what the machine does not speak, with flags to disable the machine or its sharing.

**Lean 4** is the recursive-reducer outlier, and precisely the system that solved strings by [blessing the type in the kernel](https://ammkrn.github.io/type_checking_in_lean4/whats_a_kernel.html): GMP-backed `Nat` and literal `String` support, converted to inductive form only when a definitional comparison demands it.

**Idris 2**'s [`%builtin`](https://idris2.readthedocs.io/en/latest/reference/builtins.html) recognises a shape in a user's own definition — but for the runtime representation, applied late in code generation. It is the precedent the first version of this document borrowed, and its documented fragility belongs to that mechanism at that layer.

The survey sentence the first version should have written: systems whose kernels stayed general solved closed evaluation with a machine; the system that kept a recursive reducer solved it by hardcoding the types it cared about; nobody solved it with shape recognition at the type level.

## The proposal: the closed machine

An explicit-stack, environment-based, **call-by-need evaluator for closed terms**, living in `curios-core` beside `reduce_intrinsic` and the free-monoid destructors, entered from both checkers' forcing paths whenever the redex passes the closedness gate — the test the kernel's `Memos::storable` already runs, extended by the metavariable bit on the elaborator's side, both cached per node and O(1) to probe.

**Nothing about today's semantics moves.** Call-by-need computes the same normal forms as call-by-name, raises the same errors, and diverges on the same terms; sharing changes cost alone, and cost is the target. There is no dual definition, nothing shadowed, and no second answer to reconcile: on closed terms evaluation has one result, which is what makes this representation rather than judgment — the same line `reduce_intrinsic` already sits on.

**Nothing is hardcoded and nothing is recognised.** The machine fires on closedness, a semantic property with a cached test, not on the spelling of a body. A user's refinement over a packed carrier accelerates because its check is closed, and so does every decided proposition over a closed subject and every fold form the prelude contains — the tail accumulator, the eliminator hypothesis, and the self-call alike. There is no cliff for an innocuous edit to fall off: the first version inherited Idris's warning that recognition "may be sensitive to seemingly insignificant changes", and this mechanism has no shape to lose.

**It reaches the depth, and retention follows.** Machine frames are small structures on an explicit stack, priced by the ordinary construction rows instead of `Cost::FRAME`'s native figure; shared thunks are forced once, so the accumulator chain that made memo entries quadratic in a literal's length is never built.

**It is partial by construction.** The machine speaks the fragment M0 finds hot — global unfolding, beta, `rec` unfolding, the four `Cases` families, projection, `let`, intrinsics through the shared `reduce_intrinsic` — and bails per node to the host strategy for anything else. Bailing is sound because the term is closed: the host reducer's answer is a value the machine absorbs. Agda's machine has run in production with exactly this fallback shape.

**It spends from the same counter.** A transition costs `Cost::STEP`, construction is charged through `Reducer::spend` before allocation exactly as today, and a machine frame is charged as the small structure it is. Both checkers keep reporting one `Consumption` for one program, which M2's parity gate holds.

## What this specification does not claim

**The fragment is not decided here.** M0's census decides what the machine must speak on the paths that dominate today's cost, and everything else bails; a machine that begins with the fold fragment and grows by measurement is the intended shape.

**Whether the machine is shared or duplicated is a decision to make explicitly, and this document recommends shared.** Shared widens the row the perimeter already grades weakest — both checkers running one function on the same input, as `reduce_intrinsic` does — and the perimeter entry must say so. What answers it: closed evaluation has a unique result, so the duplication doctrine's rationale — strategy differences deciding conversion — does not reach it; and with both recursive strategies retained as references, the differential gate pits three implementations against each other, so a machine defect must escape both to admit anything. Duplicating the machine per checker is the doctrine-pure alternative at twice the build and audit cost; taking it changes M1 and nothing else in this document.

**Recalibration is entailed, once.** A strategy change moves what the budget's figures buy, so `DEFAULT_STEP_BUDGET` is re-bisected against the prelude floor and the literal ceiling is restated where it is documented — that constant's own history is the precedent, recalibrated rather than retained when pricing landed. Landing the cheap strict-accumulator interim first was considered and dropped for exactly this reason: it banks part of the win at the price of recalibrating the shipped default twice.

## Milestones

### M0 — Instrument and census

- Measure what fraction of the fixed prelude's and the corpus's declaration cost is spent inside closed redexes, and which `Subterm` variants those paths reach. A read and a measurement before any build; the result fixes M1's fragment.

### M1 — The machine

- The closed machine over the M0 fragment, in `curios-core`, behind the closedness gate, bailing per node, spending from the host's counter; entered from both checkers' forcing paths — or built twice, if the sharing decision goes the other way.

### M2 — The gates

- A differential gate as an ordinary test, not an ignored probe: the machine and the recursive strategy agree on reducts over the corpus, with the prelude build exercising the machine wholesale.
- `kernel_memo_charge_measurements`, still reporting parity between the checkers.

### M3 — Account and recalibrate

- One perimeter entry for the rule — the machine agrees with the strategy on closed terms — graded, under `documentation/soundness/per-term-rules/` beside [Intrinsic fold laws and the free-monoid peel](../../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md), naming the row it widens and the differential gate as its evidence. One entry, not one per fold: nothing per-fold is trusted.
- `DEFAULT_STEP_BUDGET` re-bisected; the ceiling notes in `curios-elab`'s `context` and `curios-text`'s `lowerer` restated.

### M4 — Re-measure

- `str_literal_cost_measurements`, expecting the per-character cost to lose its frame row, the retention ladder to lose its quadratic regime, and the ceiling to move by at least an order of magnitude.
- `a_str_literal_costs_about_one_frame_per_character`, rewritten against the new shape of the cost and still an ordinary assertion.

## Acceptance

- A `Str` literal's per-character cost is bounded by a constant that does not depend on its length, measured by the probe rather than argued.
- The ceiling in characters rises by at least an order of magnitude, found by the same bisection that found the current one.
- Retention across the literal ladder is linear: the quadratic regime and its cliff are gone.
- **`/std/Str/utf8` is unchanged.** The machine did not require touching a proof.
- **A fixture that is not `Str`** — a user-defined refinement over a packed carrier, written in the test corpus — is accelerated by the same gate, because its check is closed and for no other reason.
- Both checkers still report the same cost for the same program.
- The differential gate is an ordinary test that runs with the suite.
- One graded perimeter entry names the rule, the row it widens, and its evidence.

## Refused alternatives

**A shape-recognised accelerator shadowing authored folds** — the first version of this document. Refused on three grounds the machine dissolves: recognition is syntactic and fragile where closedness is semantic and stable, so acceleration could be lost by an edit that changes no meaning; every recognised fold was a per-fold entry in the trusted base where the machine is one rule behind a differential gate; and the coverage was folds alone where the cost is paid by every closed computation, decided propositions included. Its survey also read the field one layer too shallow — Agda's dual definition is a blessed roster, Idris's recognition is a runtime representation pass — and the layer at which those systems actually meet this cost is the machine layer above.

**Capping what a memo entry may retain.** Bounds the storage and leaves the reduction as deep, so the dominant cost is untouched; aimed at a symptom.

**Declared strictness on a binder.** Against inference-over-annotation, and it fixes one fold form of three. Inferred strictness came closer — it is the accumulator measurement `str_literal_cost_measurements` records — and is absorbed rather than refused: argument handling inside the machine is where that decision now lives, with no rule of its own in either checker.

**A `force` primitive that reduces to its operand.** Mechanically inert: arguments are substituted unreduced, so the wrapper is one more unreduced link.

**Shrinking the reducer's frame.** Tried and abandoned; the first version's record stands. Annotating every step helper `#[inline(never)]`, rebuilding, and fitting peak resident size against depth showed no improvement: the frame is dominated by two large functions, and the experiment left no probe behind, which is why the outcome is stated as a direction closed rather than as a number. It also would not have changed the shape, only the constant.

**Representing `Str` as a list of characters.** The runtime price stands: `valid` erases, so a `Str` *is* its packed bytes at runtime and `to_bytes` costs nothing; a list of codepoints would allocate per element and encode on every host write.

**A native scan that replaces the definition.** The encoding leaks: connecting a natively computed result to the authored `Scan` needs a bijection nothing can prove, because nothing inducts on an intrinsic. The machine keeps the authored body as the only definition, which retires the objection rather than answering it.

**Reduction strategies offered per call site**, as Rocq's `cbv`, `lazy`, `vm_compute` and `native_compute` are. Still refused as user-facing surface: the machine is not a strategy anyone chooses, it is what evaluating a closed term is.

**Compiling closed terms through the compiler's own back end** — the `native_compute` analogue, running emitted Wasm under the runtime. Three crate boundaries forbid it: the kernel sits on `curios-core` and `curios-analysis` alone, `curios-pipeline` must not depend on the runtime, and the browser product has no Wasmtime. It would also make acceptance depend on the whole back end, which is the trusted-base trade `native_decide`-class mechanisms accept and this workspace's kernel posture exists to refuse.

**Discharging a literal's validity natively and trusting it** — an `include_str!`-as-fiat, and the general form of the native-scan remedy the lowerer defers. It is the only design that costs nothing, and it is an axiom; Rocq's discipline — countable, listed, printed — is the shape to copy if embedded data ever outgrows what honest checking affords, and that decision is not this one.

**A byte-offset `Str` API as the remedy.** Lean's shipped design — byte positions carrying boundary proofs over validated bytes — deletes two of `Str`'s three folds by construction, and it deserves its own decision on its own merits: `eq_of_bytes` already says storage decides identity, and a boundary test is one byte read where a codepoint count is a walk. It is refused *here* because no representation removes the literal's scan, and with the machine landed the scan is affordable — so the API question stops being a rescue and becomes taste, which is a different document's business.
