# A stuck term is handed back as written

[A solution is the rigid side as written](../../../curios-elab/README.md#a-solution-is-the-rigid-side-as-written) solves a metavariable to the term the problem writes, and a call of a definition is now that term wherever reduction meets it: both reducers keep a call whose unfolding is a stuck `match` as its own weak-head form, as a recursive member's folded call is kept, so `pick(h)` with `h: Bool/Holds(f(k) != 3)` elaborates to `pick(@f(k), h)` and `/std/Map/internal/Node/wedge_answers` reads back. What is left is the retirement: the rule and the decisions below moved to the documents that own them, and the one figure the work may have moved read.

## Decided

- **The rule is the recursive member's, for every definition.** The call kept is the first one a reduction unfolded since it last took an arm, a field or an answer: kept where what it unfolds to is a stuck `match`, and unfolded where that is a head — a constructor, a literal, a type former, a function, an intrinsic a fold reads through — a projection, or a call reduction leaves standing, a recursive member's folded call. Lean keeps the call of the innermost definition whose `match` is stuck; here that call is `at_place(k, c / 4, c % 4)` for `digit(k, c)`, a helper `/std/Map/digits` keeps private, so the call kept is the outermost one unfolded without progress, the one the problem wrote.
- **A definition whose unfolding names it again is unfolded as before.** Its recursion is no group's, so the unfolding of a pair against itself, which `restuck` ends for a recursive member, has nothing to end it by.
- **A kept call is its own forced form**, neutral to `force_rec` and the certifier's `force` as the stuck `match` it unfolds to is.
- **A reader of what a kept call unfolds to unfolds it**, each at its own door: conversion where the call meets another shape, every rule that reads it as an atom having had the pair; entailment, the bound's reduct where it enters; the readers of what a stuck form waits on; the refinements' reduced spellings in both checkers; `Driver::force`, the form every shared analysis reads a type in; and a `Bool` formula's leaf, through `Reducer::unfold_kept`, so the leaf set is the value's.
- **Layers keep their one spelling.** The folds' merging and cancelling and the spellings [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) lists stay as they are: the rule changes what an atom is spelled as and nothing a layer is, and a respelled layer names nothing its own atoms do not.
- **Rejected: a stuck node handed back whole as written; every operand that reaches a stuck form kept as written; every call kept.** Each refused `/std` in a prototype at `a7e7e558f` (142, 107 and 12 refusals), the first because the folds read their layers off the terms, the other two because the readers know an atom by its spelling.
- **Rejected: a definition's call keyed as a stuck form in the classification**, so two definitions' calls are classed by conversion: it met `Set/has` against `Option/is_some`, and put so many pairs to the certifier that a derived `Eql` for an indexed `Vec` ran out of budget, where the `Bool` leaf read through the call meets them with nothing classed.
- **Rejected: every carrier's laws decided at comparison and never written back, as `Bool`'s are.** The folds read their layers off the terms, so it needs readers that know an atom and a layer through a reducer, and no read-back turns on a layer.
- **Rejected: a written subterm looked up by its reduct when a solution is committed.** It reads a reduct back to a spelling, which the solution decision's own **Rejected** paragraph turns down.

## Open questions

- What the rule costs: `/std`'s build time against the record it moves. The runaway fixtures hold within the budgets they state.

## Stages

None left.

## Verification

The suites, the laws grid and its audit, the respelling grid, `/std` and every ticket of the soundness board, under both checkers; the runaway fixtures within the budgets they state, any moved one restated with its reason.

## Retirement

[A solution is the rigid side as written](../../../curios-elab/README.md#a-solution-is-the-rigid-side-as-written) restated where its fallback to a reduct no longer arises; [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)'s rationale that a fold may not respell restated to say what a fold respells is a layer and never an atom; the rule and the decisions above stated in [`conversion-recurrence.md`](../../design/soundness/conversion-recurrence.md), a folded call being a recursive member's or a definition's kept call, with the rustdoc of `Subterm::inward`, `force_rec` and `force` following it; then this specification is deleted.

## Next step

The retirement, its first act `/std`'s build time read on a quiet machine.
