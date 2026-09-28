# Algebra, part 0: what the campaign's baseline assumes

Working specification for two fixes and one correction the rest of the campaign takes as landed: conversion reads `x + 1 <= y` and `x < y` alike at `Nat` as it already does at `Int`; an implicit bound whose proposition is solved only after its insertion is filled on retry; and the bound decision's record says what is true. [Part 1](01-one-owner-spec.md) takes its baseline on the tree this part leaves, and [part 2](02-bounds-from-facts-spec.md)'s fill builds on the retry. Each was met while writing the standard library's proofs, where each cost a lemma or an explicit argument that conversion or the fill should have supplied.

It precedes part 1, and is independently implementable and retirable. Each stage lands alone.

## What this builds on

- **Comparison alignment.** `align_comparisons` (`curios-core/src/reduce/intrinsic.rs`) reads a negated comparison as its dual and a `<=` meeting a `<` as `<` of the successor, in both converters and before the peels ([Intrinsic fold laws and the free-monoid peel](../../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md)). `successor_comparison` beside it spells a comparison through its successor, and `Nat::cancel_common` (`curios-core/src/nat.rs`) cancels the floor two sums share.
- **The law grid.** `curios/src/tests/laws.rs` states each law conversion decides as a held row and each near miss as a refused one, at every carrier, and `every_held_law_closes_by_refl` holds the held rows.
- **The fill.** `insert_auto_argument` (`curios-elab/src/elaborate/apply.rs`) asks `is_prop` whether an implicit parameter's type is a proposition; a proposition is a bound, filled by its trivial inhabitant when it reduces to `Bool/True` and parked for retry otherwise. `Sort::of_in` (`curios-elab/src/convert/sort.rs`) answers the question, conservatively: a shape it cannot classify is `Type`, which is the sound direction.

## The gap

**The seam is read one way at `Int` and another at `Nat`.** `pub let seam(i: Nat, x: Nat, p: Nat/Le(i + 1, x)) -> Nat/Lt(i, x) = p;` is refused, inferred `Nat/Le(i + 1, x)` against expected `Nat/Lt(i, x)`, and `Eq(Nat/le(i + 1, x), Nat/lt(i, x))` does not close by `Eq/refl()`. `align_comparisons` rewrites the `<=` side as `<` of its successor and never cancels the `1` both sides then share, so the standard library crosses the seam with `Nat/Lt/le_succ_of_lt` and its neighbours at every place a proof meets it.

**A bound whose proposition is pinned later is never filled.** With `let proved(@P: Prop, @p: P) -> P = p;`, the definition `pub let t(n: Nat) -> Nat/Le(n, n + 1) = proved();` is refused: "implicit argument 'p' of 'proved' was not inferred". At insertion `P` is an unsolved metavariable, and `Sort::of_in`'s catch-all answers `Type(0)` for it, so `p` is inserted as an ordinary implicit rather than as a bound. Nothing asks again once `P` is solved to `Nat/Le(n, n + 1)`, which reduces to `Bool/True`.

**The bound decision's record has four errors.** In [A bound is stated in a decided proposition and discharged by reduction](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md):

- its counts ("thirty-three times across `/std`, against a hundred and fourteen `Eq/trans` and seventy-five `Eq/sym`") are stale and restate what a search answers;
- "an arm's is reachable by construction" is false: contradictory guards nest, and it is harmless only because the refinement store is a lookup that composes nothing and is retracted with its arm;
- hypotheses entering conversion are attributed to CoqMT alone, where they are the Calculus of Congruent Inductive Constructions' (Blanqui, Jouannaud and Strub, 2007), and CoqMT (Strub, 2010; Jouannaud and Strub, 2017) is what keeps a theory in conversion without them;
- "a body checked under an inconsistent context carry a false definitional equation" should say an equation escaping the context that licensed it.

## Stages

1. **The seam.** In `align_comparisons`, spell the `<=` side through `successor_comparison`, which cancels the shared floor, in both arms, and at `Int` likewise unless one of its law rows moves. The grid's "Nat comparisons" gains held `Eq(x + 1 <= y, x < y)`, `Eq(x < y, x + 1 <= y)` and `Eq(x + y + 1 <= y + y, x < y)`, and refused `Eq(x + 2 <= y, x < y)` and `Eq(x + 1 <= y, x <= y)`; a unit test in `curios-core/src/reduce/intrinsic/compare_tests.rs` pins the spelling. The call sites the seam makes redundant are reported, not removed: part 1's baseline counts them.
2. **The pinned-later bound.** `Sort::of_in` gains an arm for a metavariable, answering `Prop` when its type reduces to `Prop`. Every `is_prop` caller is swept for what the answer changes there: `elaborate/apply.rs`'s two, `elaborate/struct_.rs`'s two, `resolve.rs`, `elaborate/module.rs`, `elaborate/match_.rs`'s two and `totality.rs`. Fixtures in `curios/src/tests/numeric/bound_tests.rs`: `proved()` filled; refused against `Nat/Le(n + 1, n)`; refused under a guard, since a retry withholds the arm's refinements; and, under the same guard, `proved(@Nat/Lt(a, b))`, whose proposition is known at insertion, filled.
3. **The record.** The four corrections above.

## Verification

- The seam's rows, held and refused, with `every_held_law_closes_by_refl`. Mutation: restoring the old construction fails the grid.
- The fill's fixtures. Mutation: removing the metavariable arm refuses the rows it fills.
- `cargo x clippy` elaborates and certifies `/std` over both changes, and the two-checker fixtures stay clean.
- Both changes are equivalences read on the probe side — the successor reading and a cancellation, and a sort answered exactly where it was answered conservatively — so neither moves a verdict that was sound; each is still held to [A law is decided where it neither respells nor invents](../../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md)'s bar.

## Documentation

- The seam: `align_comparisons`' and `successor_comparison`'s rustdoc, the fold-laws entry's paragraph on alignment, and the clause of [A comparison is spelled one way when it is stuck](../../design/toolchain/a-comparison-is-spelled-one-way-when-it-is-stuck.md) that weighs reading `<=` as `<` of the successor.
- The retry: `Sort::of`'s "Conservative" paragraph, and the bound decision's sentence on retrying a parked bound.

## Rejected

- **Keeping the lemma at each call site.** It is what the library does today, and it leaves `Nat` and `Int` deciding one relation two ways, which part 1's published contract exists to end.
- **Fixing the one caller.** Having `insert_auto_argument` alone ask again once the proposition is solved would leave every other `is_prop` caller answering `Type` for the same metavariable; the answer at its root serves them all.

## Completion criteria

- `x + 1 <= y` and `x < y` meet in conversion at `Nat` as at `Int`, held in the grid with its controls.
- An implicit bound whose proposition is solved after insertion is filled on retry, and refused under a guard its retry does not see.
- The bound decision states what is true.

## Retirement

The contracts are the rustdoc and the decisions the stages touch. Point part 1's and part 2's "What this builds on" at those owners, replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
