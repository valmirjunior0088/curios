# A stuck term is handed back as written

[A solution is the rigid side as written](../../../curios-elab/README.md#a-solution-is-the-rigid-side-as-written) solves a metavariable to the term the problem writes, and `the_seventh_part_of_the_library_reads_back` fails because one is solved to a reduct: reduction rebuilds a term it cannot reduce out of what it reduced inside it, so a pair derived from that term has no written side left to solve to. [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) already keeps one carrier's decisions out of the terms: two `Bool` terms are compared by a decision diagram "built for one comparison … and never written back". The other carriers write theirs back. This specification realigns every carrier to the `Bool` rule: reduction answers a head; what it cannot reduce it hands back as written; an equation between stuck terms is decided where two terms are compared, through `curios-algebra`'s views over atoms; and nothing decided is written back.

## What is wrong today

- **A stuck intrinsic keeps its reduced operands.** The elaborator's intrinsic arm (`curios-elab/src/reduce.rs`) rebuilds the node from `reduce_intrinsic` (`curios-core/src/reduce/intrinsic.rs`), which reduces each operand to see whether the operation folds and keeps the reducts where it does not; the certifier's reducer runs the same function. Over `pub let pick(@a: Nat, h: Bool/Holds(a != 3)) -> Nat = a;`, `pick(h)` with `h: Bool/Holds(f(k) != 3)` elaborates to `pick(@match k | 0 => 7 | n + 1 => n end, h)`, where the problem writes `f(k)` (probed at `c3e70483f`). `Holds(…)` against `Holds(…)` with a metavariable open is decided by a run over the two reducts (`compare_same_global_apply`'s `Spun::Open`, then the reduced run in `Convert::drain`, `curios-elab/src/convert.rs`), whose sides hold `f(k)` reduced before any pair reaches `solve_at_birth`; pairing an intrinsic's operands with their written spellings inside `convert_intrinsic` reaches nothing for that reason.
- **The read-back it breaks.** `bcd72c6c7` left two `Eq/subst` endpoints of `/std/Map/internal/Node/wedge_answers` to inference, solved so to `digit(k, c)`'s reduct, which names `/std/Map/digits`'s private `high` and `low`, so the read-back declines that `let`, rightly, since a reader at `Node` writes neither.
- **The laws written back.** The folds the law decision lists under "A stuck term is spelled one way" — a sum merged and a difference cancelled on every demand, a `min` or `max` tree, an `abs` and a `sign` over a scale, a power's one spelling, a stuck `Flt` difference — and the `Bool` identities `bool_laws` applies, `Xor(x, true)` to `Not(x)` among them, write the algebra's result into the term. `probe_spellings`, the four ways one order fact is written, exists so a guard recorded under one spelling is met under another.
- **A stuck `match` or projection** may be rebuilt over its reduced scrutinee or subject the same way; stage 1 settles which arms do.

## Context

The conversion chain both checkers run (`curios-analysis`'s `convert_intrinsics`) pairs atoms by identity and hands the rest to the checker to class by conversion (`Pass::Atoms`, then `classes` in each checker), so a written operand that is not identical to its counterpart's reduct is still found one there, at the cost of a conversion. `curios-algebra` depends on `curios-num` alone and names no term ([The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md)): a law decided there is decided over atoms, a smaller surface than a fold rebuilding Curios/Core terms, and it sits in the trusted base both checkers share.

## Goal

Every term the compiler writes from a problem — a solution, a report's side, a refinement's key — is spelled as the program wrote it. Reduction rebuilds no term it did not reduce to a value. The carriers' laws are decided at comparison through `curios-algebra` and never written back, and the respelling folds and the probe-side alignments only they needed are retired.

## Decided

- **The rule is the `Bool` rule, for every carrier.** A fold that builds a value outright stays, the law decision's first place; a fold that rewrites a stuck term to another spelling of it moves to the comparison.
- **Reduction still decides.** Where the algebra decides a stuck operation to a value, a comparison to `true` among them, the fold builds that value, so a scrutinee inside reduction is decided through the views and not by its spelling, which [A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) holds in rejecting a fold completed only where conversion compares. What moves is the respelling alone.
- **Both reducers change together**, each stage with a case put to both checkers.
- **Rejected: a written subterm looked up by its reduct when a solution is committed.** Each run would carry the written sides of the problem it descends from, and a solution met against a reduct would be respelled as the first written subterm reducing to it. It reads a reduct back to a spelling, which the solution decision's own **Rejected** paragraph turns down, and it is a second mechanism the first half of stage 2 makes unneeded.

## Open questions

- Whether stage 2 keeps conversion complete on its own: written operands route pairs through classing where identity paired them, which `/std`, the suites and the laws grid answer.
- What a comparison's views must memoize per node so that deciding at comparison costs no more than merging on every demand, read as `/std`'s build time and the runaway fixtures' budgets (`curios`'s `tests::runaways`).
- Which of the folds stage 1 lists build a value outright, and which respell.
- What the refinement store keys a guard under once no spelling is rewritten, and which of `probe_spellings`' alignments that leaves.

## Stages

1. **The inventory.** Every arm of both reducers, and every fold of `reduce_intrinsic`, that rebuilds a term it did not reduce to a value, listed here against the law decision's three places. Check: the list, each item at the code it names.
2. **A stuck term handed back as written**: intrinsics first, then `match` and projections, in both reducers. Check: `pick(h)` elaborates to `pick(@f(k), h)`, in `curios`'s `tests::inference::implicit` beside the solution rule's own tests and mutation-checked; the seventh part reads back; `/std` builds and the suites pass; `/std`'s build time is read against the record it moves.
3. **Each written-back law decided at comparison**, a law family at a time, through `curios-algebra`'s views and never written back. Check: `tests::laws::generated` and `tests::laws::audit` hold each family at both checkers.
4. **The respelling folds retired**, with what only they needed: the alignments of `probe_spellings` stage 3 leaves without a use, and the `gt`/`ge` mirroring at the `/sys` rows where nothing reads it. Check: the suites and the soundness board's tickets under both checkers.

## Verification

The suites, the laws grid and its audit, `/std` and every ticket of the soundness board, under both checkers; the runaway fixtures within the budgets they state, any moved one restated with its reason.

## Retirement

[A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) restated to the one rule, its "A stuck term is spelled one way" retired; [A solution is the rigid side as written](../../../curios-elab/README.md#a-solution-is-the-rigid-side-as-written) restated where its fallback to a reduct no longer arises; then this specification is deleted.

## Next step

An investigation: stage 2's intrinsic half in a scratch worktree. Does `/std` build and do the suites pass with a stuck intrinsic handed back with its operands as written, and does `pick(h)` elaborate to `pick(@f(k), h)`? It ends with that answer, which settles the first open question for intrinsics.
