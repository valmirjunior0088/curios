# Standard-library invariants, part 2: universes the elaborator does not record

Working specification for the universe constraints a written type implies and the elaborator never records, which the kernel does not see because it types a type's reduct rather than the type as written. The leading hypothesis is that one declaration shape produces them — a type former whose result level floats free of its body — and the first stage tests it before anything is built on it. Whether any of it is a soundness gap is not known, and the same stage answers that; the part stands even if nothing is exploitable, because the kernel's reading of written types is still owed.

Its first stage needs nothing. Its fix waits for [part 1](01-checkers-agree-spec.md)'s fourth stage, the change to solving, since both change what solving and finalization emit.

## What this builds on

- **How a type former is levelled.** `/sys`'s formers are definitions — `List(T: Type) -> Type = ListType(T)`, built by `pub_fn` in `curios-text/src/sys_module.rs` — and the two `Type`s in the signature are levelled independently, so `List.{u,v}` takes a `T` at `Type u` and promises its result at `Type v` for any `v ≥ u`. A type alias written in a module, `let F(A: Type) -> Type = …`, is levelled the same way; the surface has no syntax for a level, so every level is inferred.
- **How the kernel types a type.** `infer_type` (`curios-cert/src/kernel/infer.rs`) reduces the type with `reduce_forced` and infers the reduct. Its documentation gives the reason: `List.{v,w}(Waker)` types as `Type v`, its former's promised codomain, while its reduct `ListType(Waker)` is the minimal `Type 0` the constructor size condition needs, and typing the written spelling cost the standard library twelve items to that condition and one to a projection through `/std/Fmt`. A closed type's arguments are evaluated on the way, which is why the kernel's `infer` is memoized per node.
- **How the elaborator settles levels.** `finalize_definition` (`curios-elab/src/elaborate/module.rs`) settles a signature's universe context with the constraints its body raised, through `finalize_universe_metas` (`curios-elab/src/context.rs`), generalizing whatever stays unconstrained.
- **How solving spells a solution.** A flexible side is solved by the rigid side's reduct, and by its written spelling only when the reduct is a `stalled_unfolding` (`curios-elab/src/convert.rs`). `a_type_named_through_a_higher_universe_solves_at_its_own` (`curios/src/tests/universes.rs`) holds the case the written spelling once broke.

## The gap

**A written type can violate levels its reduct hides.**

```
use /std/{List, Nat, Io};

pub let pair_of(x: List(Nat)) -> Io({List(Nat), List(Nat)}) =
    Io/pure((x, x));
```

elaborates to `pair_of : (x: List.{u,v}(Nat)) -> Io.{0,0}({List.{u,v}(Nat), List.{u,v}(Nat)})`: two universe parameters nothing constrains, and `Io` at level 0 applied to a type that, as written, sits at `Type v`, with `v ≤ 0` never recorded. The reduct, `IoType` over two `ListType(Nat)`, is well levelled, which is why the kernel certifies it. `/std/Async/poll_ready`'s `Io({List(Job), List(Parked)})` is the same shape. Inferring written types instead of reducts refused 44 `/std` declarations when it was tried during the invariants work; whether they are all this shape is the first thing stage 1 answers. `Parse`'s combinators, before their rewrite over `Parse/Of`, failed in isolation even at their reduct and were certified anyway, by a path not yet explained.

**The stalled-unfolding exception can still raise a universe.** A type-valued alias `F(x) = G(x)` with `G` recursive is a stalled unfolding, so solving commits its written spelling, which is typed by `F`'s codomain — possibly above `G`'s. Re-validation cannot refuse it, because the metavariable's level is still open when the solution commits. No program reaching it has been written.

**`!` is refused at a large payload type.** In an arm,

```
pub let walk(x: Result(Str, Type), n: List(Str)) -> Result(Str, Type) =
    match n
    | [] => x
    | [_, .._] =>
        let t = x!;
        Result/success(t)
    end;
```

is refused — "this Type would need to be strictly below itself", `?u1+1 ≤ 0`, pointing at the `Type` in `x`'s type — and in a flat body, `let t = x!; Result/success(t)`, it is refused too, as `inferred Result(Str, ?)`, expected `Result(Str, Type)`. `Result/bind(x, (t) => Result/success(t))` passes in both places. `/std/Cli` met it: a walk over a specification-indexed family is large, because its constructors bind an argument that holds a `Type`, and it binds through `Result/bind` instead.

## Stages

1. **The investigation.** Timeboxed, with its exit criterion stated before it starts: a closed term of `/std/False` that the kernel certifies because a written type's levels went unrecorded, or an argument why none exists — the kernel checks every term it certifies, and the question is whether a term it certifies can inhabit a type whose written spelling is ill-levelled in a way that matters. The 44 refusals are re-taken on the tree the stage starts from, with the kernel inferring written types in a scratch build, and each is classified by the constraint the elaborator failed to record, the free result level tested first. The path by which `Parse`'s old combinators certified is explained, the stalled-unfolding case gets its program or an argument that none exists, and both `!` refusals are traced to the constraint that raises them. A found term goes to the soundness perimeter's regression discipline at once.
2. **A type former's result level is its body's.** Where stage 1 confirms the hypothesis, the elaborator solves a type-valued definition's result level to the level its body is typed at instead of generalizing it apart: `List` takes `T: Type u` to `Type u`, and every alias the same. A written type then sits where its reduct does, a use at a higher level still passes by cumulativity, and a declaration like `pair_of` loses the parameters it never needed. The formers' universe arity changes, and every instance with it; the prelude archive is rebuilt and measured. Where stage 1 finds another class, it gets its own constraint, recorded where elaboration first meets it.
3. **The kernel types the written type.** `infer_type` infers the type as written, which after stage 2 lands in the sort its reduct does, so the size condition loses nothing; its documentation is rewritten to say so. The per-node `infer` memo is kept or retired by what the kernel's cost does without the reduct's arguments evaluated.
4. **`!` at a large type.** Fixed from stage 1's trace, or recorded as the rule with its reason; `/std/Cli`'s walks may then use `!`.

## Verification

- Stage 1's classification of the 44, and a fixture for each class that is fixed; `pair_of`'s signature carries no universe parameter.
- The two-checker fixtures and `kernel_disagreements` stay clean, and `/std` certifies with the kernel typing written types.
- The type-level claims [part 3](03-shared-term-costs-spec.md) benchmarks keep their budgets.
- The prelude build is measured before and after stages 2 and 3, naming the stage.

## Retirement

Record the level contract in `curios-elab`'s and `curios-cert`'s documentation and the universe entries of [the soundness perimeter](../../design/language/the-soundness-perimeter.md), replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
