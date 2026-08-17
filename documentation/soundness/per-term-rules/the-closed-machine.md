# The closed machine

**Assumes.** On a closed, metavariable-free term with no refinement in scope, `curios-core`'s `reduce_closed` computes the same weak-head value the host's recursive strategy would — the same reduct, the same errors, the same divergence-under-budget — so that substituting the machine's answer for the strategy's admits nothing the strategy would not have admitted.

**Status.** **probed**, at three layers named below, and carrying the perimeter's widest sharing asymmetry knowingly: the machine is one function in `curios-core` that both checkers run, so no disagreement between them is structurally possible over the closed fragment — the same position [Intrinsic fold laws and the free-monoid peel](intrinsic-fold-laws-and-the-free-monoid-peel.md) already holds for arithmetic, now held for beta, iota, delta and zeta on closed terms as well.

## Why the sharing is the right trade here

The two checkers duplicate their reduction strategies because strategy decides which *open* terms convert, and a strategy bug both share is a bug neither catches. A closed term has one result under any order that reaches one — confluence, plus the budget making every run finite — so closed evaluation carries no strategy content to duplicate: it is representation, like the intrinsic folds, not judgment. What the sharing buys is exact cost parity where the two price lists used to drift; what it costs is that a machine defect is a false definitional equation *both* checkers believe, which congruence carries to `False` in the admitting direction. The counterweight is that three implementations exist — the machine and both recursive strategies, which remain complete evaluators for everything the gate declines — and the differential fixture below holds the machine against the strategy directly.

## The gate, and why each half is load-bearing

`accelerable` is the representation half: no local frees and no metavariables, two bits cached per node. A local free could be a binder some in-progress judgment holds assumptions about; a metavariable's meaning is the elaborator's to resolve, not the machine's.

No-refinements-in-scope is the judgment half, and it is not an optimization. Inside a match arm, a closed scrutinee *is* the arm's assumed case value, definitionally — checking the `bad()` arm of `match classify(c)` at a literal `c` proceeds under `classify(c) = bad()` even though evaluation says otherwise, because the arm is dead code that still must type. A machine that evaluated there would answer a different question than the judgment asked. Both hosts therefore decline the machine whenever any case equation or recorded refinement is live, and take their ordinary strategy, which consults the refinement stores at every step.

## Where the machine deliberately differs, and why each difference admits nothing

**Substituted terms are evaluated before substitution.** Arguments, `let` values, and `Induct` payload binds become values first, which is what keeps an accumulator a literal instead of a chain. On closed input the reduct is unchanged by confluence. A substituend whose evaluation *errors* falls back to its unreduced spelling, restoring the strategy's deferral exactly; one whose evaluation exhausts the budget propagates, since spend is never refunded — so the one observable divergence from the strategy is that a program computing something expensive in a dead closed position can newly refuse on *budget*, never on a value. Acceptance moving at an exhaustion point is the budget's own contract, and it can move nowhere but earlier.

**Values may be deeper than the strategy's.** A more-reduced weak-head form is still a weak-head form, and conversion is closed under further reduction. Eta, by contrast, is *not* left to conversion: the machine runs the hosts' own contraction probe, because the elaborator's witness keying reads rigid heads off weak-head forms and a skipped contraction failed the prelude — the record of a difference that looked benign and was not.

**`Induct` arms bind payload values directly on both sides**, the kernel's rule; the elaborator's projection binding exists to guard annotation holes a metavariable-free payload cannot carry.

## Evidence

- `curios-core`'s `machine::tests` — the behavioral battery: both recursion encodings and the eliminator form compute the right values under linear budgets, folded spellings survive plain reduction, a dead erroring argument defers where a demanded one surfaces, a non-productive group exhausts, and a closed *neutral* accumulator stays linear through the run-scoped value memo.
- `curios-cert`'s `the_closed_machine_agrees_with_the_strategy` — the differential fixture: the same closed terms put to a machine-bearing kernel and to one with the machine disabled, asserting identical reducts.
- The fixed prelude — the bulk fixture: every `/std` and `/syn` declaration elaborates and certifies with the machine live in both checkers, and `curios-prelude-archive`'s `kernel_disagreements` walk still reports zero.
- `curios`' `str_literal_cost_measurements` and `kernel_memo_charge_measurements` — the cost-parity fixtures: the two checkers report the same figure for the same closed program, which the sharing makes structural rather than coincidental.
