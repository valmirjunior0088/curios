# A guard's equation reaches a form beneath a binder of its own

`curios_analysis::could_answer` (`curios-analysis/src/case_equation.rs`) reads a part beneath a binder of the stuck form's, a lambda's body or a match's arm, as it stands, and reduces nothing below it. So under `match u((x) => x) < 10`, the goal `u((x) => x + 0 * c) < 10` counts `c` and is refused `Holds` in the `true` arm, where conversion compares the two lambdas' bodies reduced and holds the forms equal at the top of an item. A proof beneath such a binder is read like any term too, since the walk asks which parts are proofs (`ReadsProofs::proof_parts`) only outside every binder of the form's: `u((x) => Nat/div(x, b, @nonzero(b, c, q)))` beside `u((x) => Nat/div(x, b, @q))`, seen by reading the walk and not yet probed. Such a refusal prints the fact with its hidden proof left out, as [A diagnostic spells what its reader can write](../../design/tools/a-diagnostic-spells-what-its-reader-can-write.md) leaves out a proof the procedure proves, so it reads as the guard itself: seen as `Holds(a / b < 5)` refused under the guard `a / b < 5`, before the filter passed over a proof at its position. No program of `/std` is known to meet the shape, and no workaround of it was found there on 2026-11-20.

The next step is stage 1.

## Decided

Settled by the user on 2026-11-20.

- **A refusal the theory does not make is a defect whether or not a program meets it**, so this waits on a design and not on a consumer.
- **Beneath a binder, the two forms are compared in step.** Reducing one side there has no bound the walk can see: a body naming a recursive group, the group forced, unfolds to a body naming it again. Conversion, reducing both sides in step, stops at the first difference, and that is the reading taken beneath a binder, the walk outside every binder unchanged.

## Open

- **What a proof beneath a binder is read as**: left out as the walk leaves one out outside every binder, which needs its sort beneath binders the walk opens, or compared as any part is.

## Stages

1. **The refusals as tests.** The two programs above, the second probed, refused today and held to their acceptance.
2. **In step beneath a binder.** `could_answer` compares the parts beneath a binder of the form's two at a time, as conversion does.
3. **Proofs beneath a binder**, as *Open* settles.

## Verification

- Stage 1's programs accepted, and a near miss beside each, a body that differs past every reduction, still refused; each mutation-checked.
- `curios`'s `tests::unfolding` compared door (`Consumed::Compared`) counted at 8, 14 and 20 definitions, in user-space instructions retired, against the walk as it stands.

## Retirement

When stage 3 lands, the reading is stated in `curios-analysis`'s README, and this file is deleted.
