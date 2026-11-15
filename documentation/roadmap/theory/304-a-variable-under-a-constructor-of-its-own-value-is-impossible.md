# A variable under a constructor of its own value is impossible

Its next step is a ready delivery in `curios-analysis/src/invert.rs`'s occurs check (`Unifier::reaches`). It waits for a consumer: no `/std` site writes an arm for a cyclic case, or a lemma to refute one.

## What is missing

The unifier has no cycle rule, so an equation that sets a variable to a constructor term holding it is undecided rather than impossible. `match e end` over `e: Eq()(l, [1, ..l])` is refused as "missing arm: its indices are not provably impossible at the scrutinee's", and so is one over a tree that holds itself, `Eq()(t, node(t, t))`. No finite value is its own proper subterm. Agda's unifier states this as its *Cycle* step, which fires on a variable that occurs "strongly rigid" in the other side.

A successor cycle is the same case. `match e end` over `e: Eq()(n, n + 1)` is refused too: `n` is refined to the case's binder `z`, and the second equation, `n + 1 ~ z`, is a variable against a successor of itself, which the occurs check leaves undecided. The peel never meets two sums there.

## Goal

Where the occurs check meets a flexible variable inside the other side, and every layer above the occurrence is one injectivity would take apart, the case is impossible. A layer qualifies where it is a constructor of a relevant family, a tuple, a successor, or a known element of a word. Under anything else, such as an application, a projection or a proposition's constructor, the occurrence is no cycle, and the equation stays undecided, as now.

## Settled

- **The same walk as injectivity.** A layer counts toward a cycle exactly where injectivity would take it apart, so the rule licenses no conclusion injectivity does not, and a proposition's constructor, which injectivity leaves alone, breaks the chain.

## Stages

1. **The rule**, in `curios-analysis/src/invert.rs`'s occurs check. Check: the three programs above accepted with no arm in both checkers; `Eq()(l, [..m, ..l])` and `Eq()(t, f(t))`, an opaque `f`, still refused as missing arms; a cycle through a `Prop` family's constructor refused; `cargo xboard report`; the workspace lint and tests.

## Retirement

The rule goes to `invert.rs`'s module documentation and to [An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md)'s rationale beside the conflict rule; nothing cites this file, and it is deleted.
