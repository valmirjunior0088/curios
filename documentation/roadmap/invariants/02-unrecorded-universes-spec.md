# Standard-library invariants, part 2: universes the elaborator does not record

Working specification for the universe constraints a written type implies and the elaborator never records, which the kernel does not see because it types a type's reduct rather than the type as written. Stage 1 has run, and its findings below replace the hypothesis it tested: the refusals come from how finalization settles a level, not from one declaration shape, and none of them is a soundness gap. The part stands because the kernel's reading of written types is still owed.

## What this builds on

- **How a type former is levelled.** `/sys`'s formers are definitions — `List(T: Type) -> Type = ListType(T)`, built by `pub_fn` in `curios-text/src/sys_module.rs` — archived as `List.{u,v} : (T: Type u) -> Type u = (T: Type u) => ListType(T)` with the constraint `v ≤ u`. The result is the argument's level; `v` occurs in neither the type nor the body. `Io`, `Cell` and `Channel` are the same. The surface has no syntax for a level, so every level is inferred.
- **How the kernel types a type.** `infer_type` (`curios-cert/src/kernel/infer.rs`) reduces the type with `reduce_forced` and infers the reduct. Its documentation gives the reason: an application of a former types at the former's promised codomain, while its reduct is the minimal level the constructor size condition needs, and typing the written spelling cost the standard library twelve items to that condition and one to a projection through `/std/Fmt`. A closed type's arguments are evaluated on the way, which is why the kernel's `infer` is memoized per node.
- **How the elaborator sizes a type.** Its sort of a tuple type (`elaborate_tuple_type`), a Π (`elaborate_func_type`) and a declaration's domains (`telescope_sizing`) is `Sort::of` (`curios-elab/src/convert/sort.rs`), which reduces first. Both checkers therefore size a type from its reduct.
- **How the elaborator settles levels.** `finalize_definition` (`curios-elab/src/elaborate/module.rs`) splits a declaration's levels into its interface — the metas of its type — and its internal levels — the metas of its body, plus `result_sort_only_metas`, every meta of the type's terminal that no binder domain mentions. `UniverseSolver::finalize` (`curios-elab/src/universe_solver.rs`) minimizes the internal ones, a level with no lower bound going to zero, and generalizes every other unsolved level in the connected component. An occurrence of a polymorphic declaration mints its instance levels with the same role a written `Type` in a signature gets (`Context::instantiate_assumption`).
- **How conversion treats instance levels.** Two spellings that differ only in levels are decided by committing the levels equal, before either side unfolds (`identify_universe_levels`, `curios-elab/src/convert/neutral.rs`); only two unequal ground levels fall through to unfolding.
- **How solving spells a solution.** A flexible side is solved by the rigid side's reduct, by its written spelling when the reduct is a `stalled_unfolding`, and by its written spelling when the reduct does not re-check (`solve_at_birth`, `curios-elab/src/convert.rs`).

## The gap

**A written type can violate levels its reduct hides.**

```
use /std/{List, Nat, Io};

pub let pair_of(x: List(Nat)) -> Io({List(Nat), List(Nat)}) =
    Io/pure((x, x));
```

elaborates to `pair_of : (x: List.{u,v}(Nat)) -> Io.{0,0}({List.{u,v}(Nat), List.{u,v}(Nat)})`: `Io` at level 0 applied to a type that, as written, sits at `Type u`, with `u ≤ 0` never recorded, and `v` a parameter nothing uses. The reduct, `IoType` over two `ListType(Nat)`, is well levelled, which is why the kernel certifies it. Inferring written types instead of reducts refuses 46 `/std` declarations.

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

## Findings

Taken on `bb28989d`. **To retake them**, patch `infer_type` to infer `type_` itself instead of `reduce_forced(type_)`, gated on an environment variable so `curios-prelude`'s build still certifies, and widen `kernel_disagreements`' per-verdict line to the full error; then `cargo test -p curios-prelude-archive --all-features --lib -- --ignored kernel_disagreements --nocapture` with the variable set walks `/std` typing written types, and `curios wonder diagnostics` certifies a probe the same way. A declaration's archived scheme — type, universe context and raw terms — is read off `with_prelude`'s items. Neither driver is checked in.

**The 46.** 39 are `Oversized`: a family's constructor or field domain written at a level above the family's own — `Json`, `Html`, `Toml`, `Test`, `Vec`, `Command`, `http/Request`, `http/Response`, most of `Async`, `Cli` and `Tui/Layout`. 7 are `Mismatch`: `Async/poll_ready`, `Async/expire_sleepers`, `Tui/drive`, `Parse/many0`, `Parse/sep_by0`, `Toml/build/inline_fold`, `Toml/values/array_parse`. All are one class.

**An occurrence's level floats above its lower bound.** `List(Nat)` checks `Nat : Type 0` against a fresh `Type ?u`, which records `0 ≤ ?u`. Where the occurrence stands in a signature or a family's domain, `?u` counts as interface and is generalized, so the written type sits at a parameter while both checkers size the enclosing type from the reduct, where the occurrence is gone. `Json.{u,v,w,x} : Type 0` holds two `List` occurrences in its constructors, each at a generalized `u` above the family's 0; `poll_ready` carries sixteen parameters and `Tui/drive` twenty-eight. Rocq and Agda settle the same level at its lower bound (see stage 2).

**A level occurring nowhere is kept.** `List`'s `v` is one: joined to `u` by one constraint, it is generalized as a parameter, and every occurrence of `List` mints it. This inflates counts and causes no refusal.

**The hypothesis.** "A former's result level floats free of its body" is the second finding; it causes none of the refusals. Solving `List`'s `v` away leaves `pair_of` at `(x: List.{u}(Nat)) -> Io.{0}({List.{u}(Nat), …})`, still written at `Type u` under an `Io` at 0.

**`!`.** The refusal is not `!`'s: `Monad/bind(x, (t) => Result/success(t))` at `Result(Str, Type)` is refused with the same `?u1+1 ≤ 0`. `/std/Monad/Monad` is `.{u,v,w,x,y} : (M: (Type u) -> Type v) -> Type max(u,v,w+1,x+1,y+1)` with `w, x, y ≤ u`, the levels of `pure`'s `@A` and `bind`'s `@A` and `@B`. `Result`'s witness is `(@E: Type u) -> Monad.{v, max(u,v), 0, 0, 0}((A) => Result.{u,v}(E, A))`: the three method levels occur only in its goal, so `result_sort_only_metas` counts them determined, and having no lower bound they go to zero. `bind` through that witness takes `A, B : Type 0` only. The flat body meets the same refusal through its parked region.

**`Parse`'s old combinators.** Taken from `d5d24a63`, with the since-removed `Nat/Lt/le_succ_of_lt(…, ok)` written `ok`, they elaborate, certify and run as a standalone module on this tree; `many0` is `Parse.{u,0,u}(List.{u,0}(A))`, `A`'s level carried into the occurrence. The divergence recorded earlier no longer reproduces; the current `many0` and `sep_by0` are in the class above.

**The two written-spelling commits.** A stalled unfolding and `solve_at_birth`'s fallback both commit a written spelling. An argument check bounds an occurrence's level from below only, so a written spelling's level is at least its reduct's: committing it can raise a metavariable's level and never lower it — a possible spurious refusal, never an admission. Once an occurrence sits at its lower bound, the two are equal and neither path raises one. No program was written.

**No closed term of `False`.** By the same monotonicity a written type overstates its level and never understates it, and the kernel reads a written level only where it infers an application's type, where an overstatement can only refuse. A struct over `List(Type)` is placed at `Type u+1`; the retraction `wrap(A) = S { [A] }`, `el(s) = match s.l …` that Hurkens' paradox needs certifies only there; the kernel typing written types refuses the one probe declaration with a floating occurrence, `Io` over two `List(Type)`, as `expected Type.{u+1}, found Type.{v}` — an overstatement, refused.

## Stages

1. **The investigation.** Landed: the findings above.
2. **Finalization settles a level by where it came from.** Three rules in `UniverseSolver::finalize`, fed by `finalize_definition`:
   - An occurrence's instance level takes its lower bound — the `max` of its lower bounds, which Curios's levels can state — wherever it stands, signature included. A level with no lower bound stays a parameter.
   - A level with no lower bound goes to zero only when it is the declaration's own result sort, the terminal `Type` itself; `result_sort_only_metas` narrows to that, so a level nested in the terminal, such as a witness's goal's, is settled by the first rule.
   - A level occurring in neither the type nor the body is dropped, its constraints carried through to the levels it related.

   This is Rocq's minimization — "applied only to fresh universe variables. It simply adds an equation between the variable and its lower bound" — and Agda's, which under `--cumulativity` instantiates a level metavariable bounded only from below to the join of its bounds; Lean has no cumulativity and reaches the same levels by unification. Finalization tells an occurrence's level from a written `Type` by a provenance the meta carries from its minting. The occurrence levels `/std` archives lose their floating parameters — `pair_of` and `Json` none, `List` one — `Monad` keeps its five, and `Result`'s witness keeps its method levels. Every Rust site that spells a former's instance arity is updated, the prelude archive is rebuilt and measured.

   Rejected: minimizing only the occurrences of definitions that unfolding erases, since conversion observes those levels (`identify_universe_levels` commits them equal before unfolding) and the rule needs no such classifier; identifying a level bounded only from above with its bound, which would rewrite `Monad`'s interface to two levels by fiat — whether `Monad` ties its methods to `M`'s domain, as Lean's does, is a separate decision; equating an occurrence's level with its argument's at application, which is incomplete under cumulativity when one level bounds several arguments; sizing by the written spelling in both checkers, which keeps every floating level and pays the size condition; resolving the `/sys` formers to their intrinsics, which leaves aliases and nominal occurrences; and declaring the reduct the truth, which leaves the kernel accepting a declared type it never typed.
3. **The kernel types the written type.** `infer_type` infers the type as written, which after stage 2 lands in the sort its reduct does, so the size condition loses nothing and the 46 go to zero; its documentation is rewritten to say so. The per-node `infer` memo is measured with and without it, and kept unless certification without it stays within a fifth of its budget, since [part 3](03-shared-term-costs-spec.md)'s last stage extends it.
4. **`!` at a large type.** Stage 2's second rule fixes it. The arm program, the flat body and `Monad/bind` at `Result(Str, Type)` become fixtures, `Result/bind` their control, and `/std/Cli`'s walks use `!`.

## Verification

- `pair_of`'s signature carries no universe parameter, `Json` none, and `List` one; `Monad/bind` at a large payload elaborates.
- The two-checker fixtures and `kernel_disagreements` stay clean, and `/std` certifies with the kernel typing written types.
- The type-level claims [part 3](03-shared-term-costs-spec.md) benchmarks keep their budgets.
- The prelude build is measured before and after stages 2 and 3, naming the stage.

## Retirement

Record the level contract in `curios-elab`'s and `curios-cert`'s documentation and the universe entries of [the soundness perimeter](../../design/language/the-soundness-perimeter.md), replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it. `identify_universe_levels` committing levels equal where unfolding alone would decide, and Rocq-style variance for a nominal type's universe parameters, are recorded as findings rather than taken here.
