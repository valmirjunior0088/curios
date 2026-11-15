# An arm's index equations are solved as one problem

The rule is [An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md)'s, and `curios-analysis`'s `solve_indices` now holds it. An arm's index equations are solved as one problem, in order, over its binders and the outer variables, by deletion, solution, injectivity and conflict. What remains is `/std`, which still writes around the unifier that solved one kind of variable a pass. Its next step is stage 2, a ready delivery.

## What remains

**What `/std` writes around the old unifier.** Each of these restates a family over variable indices so that a match substitutes them, where a match on the family itself now specializes its arm through the constructor:

- `Member/unconsed` and `Unconsed` (`curios-prelude/std/List/Member.crs`), behind `uncons`;
- `Pairwise/leading` and `trailing`, with `Leading` and `Trailing` (`List/Pairwise.crs`), behind `head` and `tail`;
- `sort_by`'s `topmost` and `beneath`, with `Topmost` and `Beneath` (`List/sort_by.crs`), behind `layered_over` and `layered_below`;
- `Option/same_value`, `SameValue` and `same_value_refl` (`Option.crs`), behind `some_injective`.

## Stages

2. **`/std` matches the families directly.** Each function the helpers stand behind matches its family, and the helpers go. Check: `curios test` in `curios-prelude/std` over the subjects touched, and the workspace lint.

## Retirement

Nothing cites this file, and it is deleted with stage 2.
