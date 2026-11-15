# Index inversion solves through a constructor's index

The rule this breaks is [An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md): an arm is specialized by "the most-general solution of `actual indices ~ case targets`", the Goguen–McBride–McKinna step, and `curios-analysis`'s `solve_indices` states the same of itself. Where a variable stands inside an index written as a constructor's term, the solution the unifier finds is less general than that. Its next step is stage 1, a ready delivery.

## What is wrong today, and where

**A binder forced twice to two constructor terms is dropped.** `consolidate` (`curios-analysis/src/invert.rs`) keeps a binder forced in two positions only where the two values convert, refuses the case where they clash, and otherwise drops the binder's solutions. Under `e: Eq()(Option/some(x), Option/some(y))`, the arm `refl()` forces its binder at `some(x)` and at `some(y)`, which neither convert nor clash, so nothing is solved and `Eq/refl()` against `Eq()(x, y)` is refused, `inferred: Eq(@Nat)(x, x)`. The most-general solution substitutes the first forcing into the second position and takes `some(x) ~ some(y)` apart by injectivity, refining `y` to `x`.

**A sequence's cons is not taken apart.** The positions are unified each alone, and two sequences meet through the word strip (`peel_intrinsic`), which reads two leading elements that differ as terms as undecided. Under `m: Mem()(x, [h, ..l])` and `here(@x, @l): (x, [x, ..l])`, the second position is `[h, ..l] ~ [x, ..l']`, refused, so `h` is not refined to `x`, `inferred: Eq(@T)(x, x)`, `expected: Eq(@T)(x, h)`. A free monoid cancels a prefix of one element: `[a, ..u] = [b, ..v]` holds exactly where `a = b` and `u = v`, for a `List` and a packed `Bytes` alike. Where neither side starts with a known element, `[..a, ..b] ~ [..c, ..d]`, nothing follows, as now.

**An outer variable is never refined to another.** The second direction, which refines the variables the scrutinee's indices name, keeps a solution only where the value names no flexible variable (`unify_node`, "the forced value must be *rigid*"), so once the two positions above meet, `y ~ x` between two outer variables is refused there too, where the solution rule asks only that a variable not occur in its own value.

**What `/std` writes around it.** Each of these restates a family over variable indices so that a match substitutes them, and each goes once an arm is specialized through the constructor, its caller matching the family directly: `Member/unconsed` and `Unconsed` (`curios-prelude/std/List/Member.crs`), `Pairwise/leading` and `trailing` with `Leading` and `Trailing` (`List/Pairwise.crs`), `sort_by`'s `topmost` and `beneath` with `Topmost` and `Beneath` behind `layered_over` and `layered_below` (`List/sort_by.crs`), and `Option/same_value`, `SameValue` and `same_value_refl` (`Option.crs`).

## Settled

- **One unifier.** The change is to `solve_indices` and what it calls, which both checkers run ([Conversion is one relation in both checkers](../../design/soundness/conversion-is-one-relation-in-both-checkers.md)); no checker gains a rule of its own.
- **Positions in sequence.** A solution found in one position is applied to the positions after it before they are unified, which is how a forcing met twice becomes one equation between two values, as the deletion and injectivity rules state it.
- **Which variable a solution names.** Where both sides of an equation are outer variables, `y ~ x`, the scrutinee's variable is refined to the case's term, the orientation the second direction refines in today, and the other way round where that refinement does not fit its variable (`fits_its_variable`, a sort below a sort); where neither fits, neither is solved. So the variable met later is refined to the one met earlier: `y` to `x` in the first program above, `h` to `x` in the second. Solving the variable bound later in context instead serves a unifier that rewrites its context; an arm binds each solution and re-assumes the locals that name it within itself, so either direction is well-scoped.
- **Injectivity where it holds.** Two applications of one constructor, a tuple and a cons of a known element are taken apart; a concatenation of unknown words is not; a clash stays a clash only where it is one today (a relevant family's tags, a literal the peel answers `Impossible`).

## Open

- **What the rigidity refusal guards.** `solve_indices`'s rustdoc names the parameter cycle `b := b + 1`, which an occurs check refuses as well; whether the elaborator's refinement store, which keeps a refined variable spelled, needs more than an occurs check and solutions applied in sequence against a cycle through a refinement already in force is read at stage 1.
- **What a refined parameter does to grading.** Whether refining an outer variable that is a recursion's parameter moves how the recursion's calls grade against it (`Shape::against`, `curios-analysis/src/totality/shape.rs`) is read off the totality suites and `/std`'s certification at stage 1.

## Stages

1. **The unifier.** Positions in sequence, a re-forcing unified against the forcing before it with the outer variables flexible, a cons of a known element taken apart, and an outer variable's solution held to the occurs check in place of the rigidity refusal, as far as *Open* finds that enough. Check: the two programs above, as fixtures put to both checkers, refining `y` and `h`; a refusal for `[..a, ..b] ~ [..c, ..d]`; `curios-analysis`'s `invert/tests.rs` and `curios`'s `tests::rules::index_tests`; the workspace lint and tests.
2. **`/std` matches the families directly.** The helpers above go, their callers rewritten to match the family. Check: `curios test` in `curios-prelude/std` over the subjects touched, and the workspace lint.

## Verification

The workspace lint and tests at each stage; `cargo xboard report`, since inversion is in the trusted base; the prelude's image compared before and after stage 1, every declaration that moves read.

## Retirement

The decision's rationale gains the sequence and the free monoid's cancellation as the unifier's statement of the step; `invert.rs`'s module documentation states the rules; nothing cites this file, and it is deleted.
