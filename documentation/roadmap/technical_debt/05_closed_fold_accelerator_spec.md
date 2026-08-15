# A closed fold should not cost what its data is long

This is the implementation specification for making a fold over a packed carrier affordable at the type level when its arguments are closed, without replacing the definition that proofs are written against.

## Status

Opened on the measurements taken while retiring *A string literal is checked once per use*, whose figures now live in `curios`' `str_literal_cost_measurements` and `a_str_literal_costs_about_one_frame_per_character`. Every quantity this document reasons from is reproduced by one of those two, and none of it is restated here.

It is filed as debt rather than as a capability because the cost is present rather than prospective, and because the shortcut is recorded: `curios-text/src/into_core/lowerer.rs` names a native scan as one of two deferred remedies, and `Bytes` already has natively folded operations where `Str` has authored recursion. What is new here is that the remedy has a shape the field has converged on, and that shape removes the objection the note deferred it under.

## What is wrong today

A fold driven at the type level costs one guarded reduction level per element. `Cost::FRAME` prices a level at the native frame it takes, so the frame row is the overwhelming majority of what a `Str` literal costs to check, and the ceiling on a literal's length follows directly from dividing the budget by it.

`Bytes` does not have this problem, and the reason is not that bytes are simpler: every `Bytes` operation is an intrinsic fold with a free-monoid peel law, so it folds in one step on a literal and still reduces definitionally on symbolic input. `Str`'s operations are authored `rec` definitions over `Bytes`. That is the whole of the difference, and it is what makes this a general defect rather than a fact about strings — the same cost lands on any user type whose refinement is decided by a fold over a packed carrier.

Three of `Str`'s own operations pay it independently: the validity check on every literal, the codepoint count that `len` and `at`'s bound both mention, and the drop-width that `slice` computes. `str_literal_cost_measurements` records what each costs.

## What the field does, and the one idea underneath it

Every mature system that faces this accelerates closed evaluation with a native implementation, and the interesting differences are in what they keep alongside it.

**Agda** binds a builtin to *both* an Agda definition and a primitive implementation. The primitive evaluates applications to **closed** terms; the Agda definition is used otherwise, "allowing you to prove things about the functions while still enjoying good performance of compile-time evaluation." The primitive shadows the definition rather than replacing it. [Built-ins](https://agda.readthedocs.io/en/latest/language/built-ins.html)

**Lean 4** extends its kernel with `Nat` and `String` literal support: GMP-backed arithmetic with addition, multiplication, exponentiation, subtraction, division, modulus and boolean equality overridden by primitives, and string literals converted to their inductive form only when definitional equality or a recursor's major premise demands it. [What's a kernel](https://ammkrn.github.io/type_checking_in_lean4/whats_a_kernel.html)

**Rocq** declares primitive `int63` and persistent arrays as *axioms* in the kernel, listed by `Print Assumptions`, and carries binary numeral encodings precisely so that unary representations are not unfolded — an omission of which is what makes [#13756](https://github.com/coq/coq/issues/13756) overflow on a two-factor multiplication.

**Idris 2** is the one that is not a blessed list. `%builtin Natural` optimises a *user's own* definition by **recognising a shape** in it — one argument pattern-matched, the zero case returning `0`, the successor case returning `1 + cast k`. [Builtins](https://idris2.readthedocs.io/en/latest/reference/builtins.html)

## The proposal: Agda's dual definition, selected by Idris's shape recognition

A definition keeps its authored body as the source of truth. Reduction may additionally recognise that the body *is* a fold over a packed free-monoid carrier and, **when every argument in the folded position is closed**, compute the result natively in one step instead of unrolling it.

Three properties follow, and each answers an objection that killed an earlier proposal.

**The proof corpus does not move.** The accelerator shadows the definition on closed input; on symbolic input the authored body reduces exactly as it does now, so `/std/Str/utf8`'s lemmas keep inducting on the same equations and `of_scan_eq_from`'s definitional bridge is untouched. The earlier objection — that a native scan drags a `Scan`-to-`Nat` bijection into the trusted base — applies only to a primitive that *replaces* a definition, and this one does not.

**Nothing about `Str` is hardcoded.** Recognition is by shape, so a user's own refinement over `Bytes` is accelerated on the same terms. This is the property that distinguishes the proposal from adding `Str` intrinsics, and the acceptance criteria below make it a requirement rather than an aspiration.

**It is the only remedy that reaches the depth.** The cost is a reduction level per element; a fold that does not unroll takes no levels. Retention follows for the same reason, because the entries that make it quadratic are keyed on terms the unrolling builds.

## What this specification does not claim

**The recognised shape is not yet decided, and the survey says it must cover two forms.** `/syn/Str`'s scan is tail-recursive with an accumulator; `count_scalars` and `drop_width` are non-tail, combining the recursive result. Prelude folds written with an induction hypothesis are pervasively the second form. A rule that only recognises the first fixes one of `Str`'s three costs, which is the mistake an earlier draft of this document made and the reason M0 comes before anything else.

**Shape recognition is fragile by construction.** Idris's own documentation warns the optimisation "may be sensitive to seemingly insignificant changes", and a silent loss of acceleration is a performance cliff triggered by an innocuous edit. The mitigation already exists in tree rather than being promised here: `Consumption` separates the frame row from everything else exactly, so a refusal can say that depth consumed the budget instead of naming a charge the reader cannot act on.

**Pricing a native fold is an open question, not a detail.** A fold that walks n elements and builds nothing is charged almost nothing by the current rule, which prices construction. That is the same weak bound `BinEql` and `BinSlice` already rest on — host work bounded by the size of an already-charged operand — so this inherits an existing hole rather than opening one, but inheriting it deliberately is different from inheriting it by accident.

## Milestones

### M0 — Decide the recognised shape

- State the rule, against both fold forms in the prelude: the tail-recursive accumulator (`scan_from`) and the result-combining induction hypothesis (`count_scalars`, `drop_width`, `BigNat/trim`).
- Count how many prelude folds it recognises before implementing it. A rule that recognises one of `Str`'s three costs is not worth the trusted base it takes, and counting is a read rather than a build.
- Decide whether recognition is automatic or requires the definition to opt in, and record why.

### M1 — Site the seam

- An accelerator decides what a term folds to, which is the same kind of thing `reduce_intrinsic` is — already shared between both checkers as representation rather than judgment. Siting it there keeps one implementation and one soundness argument; siting it twice doubles a rule that cannot differ without the checkers disagreeing about cost, which has reached a user twice already.
- Whichever is chosen, the closed-argument test and the fold's own law are what the perimeter entry has to state.

### M2 — Apply it

- `/syn/Str`'s scan first, since a literal drives it on every use, then the codepoint count and drop-width that `len`, `at` and `slice` rest on.
- No change to `/std/Str/utf8`. If a lemma has to move, the shadowing property has been broken and M1 is wrong.

### M3 — Account for the trusted base

- One perimeter entry per accelerated fold, graded, under `documentation/soundness/per-term-rules/` beside [Intrinsic fold laws and the free-monoid peel](../../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md), which is the row this joins and already the weakest anywhere on the perimeter.
- Rocq's discipline is the one to copy here: what was added is countable and listed, not argued about.

### M4 — Re-measure

- `str_literal_cost_measurements`, expecting the per-character cost to lose its frame row and the ceiling to move by an order of magnitude.
- The retention ladder in the same probe, expecting the quadratic to go with the unrolling that caused it.
- `kernel_memo_charge_measurements`, expecting the two checkers to still agree.

## Acceptance

- A `Str` literal's per-character cost is bounded by a constant that does not depend on its length, measured by the probe rather than argued.
- The ceiling in characters rises by at least an order of magnitude, found by the same bisection that found the current one.
- **`/std/Str/utf8` is unchanged.** The accelerator did not require touching a proof.
- **A fixture that is not `Str`** — a user-defined refinement over a packed carrier, written in the test corpus — is accelerated by the same rule. Without this the specification has produced another hardcoded type.
- Both checkers still report the same cost for the same program.
- Every accelerated fold has a graded perimeter entry naming its law.

## Refused alternatives

**Capping what a memo entry may retain.** It bounds the storage and leaves the reduction just as deep, so the dominant cost is untouched. This was the first draft of this document, and it was aimed at a symptom.

**Declared strictness on a binder.** It runs against the reason the totality checker exists — Curios infers rather than annotates — and it fixes only the tail-recursive accumulator form, which is one of `Str`'s three costs.

**Inferred strictness.** Sound strictness introduces no divergence and no extra work, and `curios-analysis` exists precisely so both checkers share such a rule, so the objections usually raised against it do not apply. It is refused on coverage alone: the survey found it reaches one fold form and not the other, and it cannot see through a higher-order fold whose combining function is a parameter.

**A `force` primitive that reduces to its operand.** Mechanically inert. Arguments are substituted unreduced, so a `force`-wrapped argument becomes another unreduced link rather than forcing anything.

**Shrinking the reducer's frame.** Tried and abandoned rather than reasoned away: annotating every step helper in both reducers `#[inline(never)]`, rebuilding, and fitting peak resident size against depth over a ladder of programs showed no improvement at all. The frame turns out to be dominated by two large functions rather than by helper locals pooled into one, and the literal's own path has a per-level cost that experiment could not isolate. Reproducing it needs the annotation, a release build, and the differential — it left no probe behind, which is why the outcome is stated here as a direction closed rather than as a number. It remains a legitimate constant-factor exercise and is not a change of shape.

**Representing `Str` as a list of characters.** Validity would be structural and `len`, `at` and `slice` would become existing intrinsics, at the price of the runtime representation: `valid` erases, so a `Str` *is* its packed bytes at runtime and `to_bytes` costs nothing. A list of codepoints would allocate per element and encode on every host write.

**A native scan that replaces the definition.** The encoding leaks: connecting a natively computed result to the authored `Scan` needs a bijection that cannot be proven, because nothing can induct on an intrinsic. Shadowing rather than replacing is exactly what avoids this, and it is the whole reason the proposal above has the shape it does.

**Offering several reduction strategies per call site**, as Rocq does with `cbv`, `lazy`, `vm_compute` and `native_compute`. Every additional strategy costs twice here, because two checkers implement reduction separately on purpose.
