# A universe level settled before its evidence is in

**Not refined yet.** This specification reserves two places where [a universe level that settles by where it came from](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md) settles before what would decide it is known, beside what [irrelevant universe levels](01-irrelevant-universe-levels.md) take. It is not an implementation plan.

## The findings

- **A witness goal deferred past its declaration.** The scheduler orders an item after the witnesses it reaches by operator or method name, and not after those its `!` or a `use` premise reaches. A goal whose witness is declared later in the unit therefore resolves only after its declaration's scheme has closed, so finalization makes the goal ground, and a generic declaration dispatching through it is settled at its least levels (`tests::universes::a_goal_deferred_past_its_declaration_settles_at_its_least_levels`). Two things ride with it:
  - a late resolution that would constrain a level its declaration already closed is left to the kernel rather than reported at that declaration;
  - the refusal a program meets names neither the level nor the late witness — `rewrap` at `Box(Type)` reads `inferred: Type, expected: ?`.
- **`identify_universe_levels` commits two instances' levels equal** where unfolding alone would decide their convertibility. Rocq answers the same situation with weak `ULub` constraints, which conversion may drop rather than commit.

## Refinement

The first finding states the edges the scheduler gains (`curios-text`'s `order.rs`, which already adds soft edges for operators and method wrappers), what still defers once they are in (witness cycles), the check a late resolution runs, and the diagnostic; its fixture's first count turns to one. The second states which comparisons commit levels today, what a weak constraint would let conversion do instead, and what it changes in the census.
