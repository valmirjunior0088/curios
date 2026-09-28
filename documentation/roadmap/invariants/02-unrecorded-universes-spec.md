# Standard-library invariants, part 2: universes the elaborator does not record

**Not refined yet.** Working specification for the universe constraints a written type implies and the elaborator never records, which the kernel does not see because it types a type's reduct rather than the type as written. Whether that is a soundness gap is not known, and finding out is this part's first stage; the rest is refined by its answer. It stays a specification of its own even if the answer is that nothing is exploitable, because the kernel's reading of written types is then still owed.

Its investigation is the campaign's first wave and needs nothing. Its fix waits for [part 1](01-checkers-agree-spec.md)'s third stage, the change to solving, since both change what solving and finalization emit.

## What this builds on

- **How the kernel types a type.** `infer_type` (`curios-cert/src/kernel/infer.rs`) reduces the type with `reduce_forced` and infers the reduct. A closed type's arguments are evaluated on the way, which is why the kernel's `infer` is memoized per node: a type-level claim such as `Eq(Str/trim(" x "), "x")` became a value whose fields are an exponential tree, typed in its own size only because a remembered type is handed back without replaying its mints.
- **How the elaborator settles levels.** `finalize_definition` (`curios-elab/src/elaborate/module.rs`) settles a signature's universe context with the constraints its body raised, through `finalize_universe_metas` (`curios-elab/src/context.rs`); the surface has no syntax for a level, so every level is inferred.
- **How solving spells a solution.** A flexible side is solved by the rigid side's reduct, and by its written spelling only when the reduct is a `stalled_unfolding` (`curios-elab/src/convert.rs`). `a_type_named_through_a_higher_universe_solves_at_its_own` (`curios/src/tests/universes.rs`) holds the case the written spelling once broke.

## The gap

**A written type can violate levels its reduct hides.** Inferring the written type instead of its reduct refused 44 `/std` declarations on universe levels, when it was tried during the invariants work. `/std/Async/poll_ready` returns `Io.{0,0}({List.{u2,v2}(Job), List.{w2,x2}(Parked)})`: `Io` at level 0 is applied to a type at `max(u2, w2)`, which nothing constrains, because the elaborator never recorded `max(u2, w2) ≤ 0`; the reduct, the intrinsic `Io(…)`, carries no instance to violate. `Parse`'s combinators, before their rewrite over `Parse/Of`, put `A: Type.{u}` at `Type.{y}` inside `Result.{0,u}(Parse/Error, {Nat, List.{y,z}(A)})`, and in isolation their reduct fails too — yet the kernel certified them, by a path not yet explained.

**The stalled-unfolding exception can still raise a universe.** A type-valued alias `F(x) = G(x)` with `G` recursive is a stalled unfolding, so solving commits its written spelling, which is typed by `F`'s codomain — possibly above `G`'s. Re-validation cannot refuse it, because the metavariable's level is still open when the solution commits. No program reaching it has been written.

**`!` in a match arm is refused at a large success type.** `pub let walk(x: Result(Str, Type), n: List(Str)) -> Result(Str, Type) = match n | [] => x | [_, .._] => let t = x!; Result/success(t) end;` is refused — "this Type would need to be strictly below itself", `?u1+1 ≤ ?u2`, pointing at the signature's `Type` — where the same `!` in a flat body passes and `Result/bind` in the arm passes. `/std/Cli` met it: a walk over a specification-indexed family is large, because its constructors bind an argument that holds a `Type`, and binds through `Result/bind` instead.

## Stages

1. **The investigation.** Timeboxed, with its exit criterion stated before it starts: a closed term of `/std/False` that the kernel certifies because a written type's levels went unrecorded, or an argument why none exists — the kernel checks every term it certifies, and the question is whether a term it certifies can inhabit a type whose written spelling is ill-levelled in a way that matters. The 44 refusals are re-taken on the tree the stage starts from, each classified by the constraint the elaborator failed to record; the path by which `Parse`'s old combinators certified is explained; the stalled-unfolding case gets its program; and the `!` refusal is traced to the constraint that raises it. A found term goes to the soundness perimeter's regression discipline at once.
2. **Refined from the answer.** The elaborator records the constraints a written type implies — where, is what stage 1 says — the kernel then infers the written type, and the per-node `infer` memo is kept or retired by what the kernel's cost does without the reduct's arguments evaluated. The `!` refusal is fixed or recorded as the rule, with its reason.

## Verification

- Stage 1's classification of the 44, and a fixture for each class that is fixed.
- The two-checker fixtures and `kernel_disagreements` stay clean, and `/std` certifies with the kernel typing written types.
- The type-level claims [part 3](03-shared-term-costs-spec.md) benchmarks keep their budgets.

## Retirement

Record the level contract in `curios-elab`'s and `curios-cert`'s documentation and the universe entries of [the soundness perimeter](../../design/language/the-soundness-perimeter.md), replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
