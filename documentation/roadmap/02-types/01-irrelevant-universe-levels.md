# A universe level only a parameter's type mentions is irrelevant

Working specification for comparing a nominal type's universe levels by variance rather than by equality. `!` holds its region at the level of the action it binds, because both checkers equate the levels of two instances of an `induct` or a `struct`; Rocq, and MetaCoq's verified specification of it, compare them by variance instead, and a level that only types a parameter is irrelevant. [A universe level is implicit, cumulative, and settles by where it came from](../../design/types/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md) states irrelevance as the rule; this brings both checkers to it at the narrowest point that closes the gap: irrelevance, and nothing covariant.

It is independent of every other spec. Its kernel stage lands before its elaborator stage, so the kernel never refuses what the elaborator starts producing.

## What this builds on

- **Levels settled by where they came from.** An occurrence's level settles at its recorded floor, the kernel types a type as written, and `!` sequences a large payload through `Result`'s `Monad` witness; `/std/Cli`'s `fill` binds through `Result/bind` for this gap alone.
- **How the checkers compare a nominal node today.** Every nominal node carries its universe instance, and both checkers equate it level by level:
  - `curios-elab/src/convert.rs`: `compare_levels`, which adds one equation per level, in `compare_induct_type`, `compare_variant`, `compare_struct_type` and `compare_struct`;
  - `curios-cert/src/kernel/convert.rs`: `levels_eq` in the `InductType`, `StructType`, `Variant` and `Struct` arms, and the level-only rule over `Term::level_differences`.
- **Subsumption** relates sorts and function types only, and is conversion everywhere else ([Subsumption is a relation, not a traversal order](../../design/types/subsumption-is-a-relation-not-a-traversal-order.md)).
- **Full application is structural.** `InductType`, `StructType`, `Variant` and `Struct` are built only inside a fully applied former or constructor wrapper, and the kernel checks their counts at the boundary; a partial application is the wrapper, not the node.
- **Positivity is the precedent for where a derived fact about a declaration lives** ([Strict positivity modulo polarity](../../design/types/strict-positivity-modulo-polarity.md)): `polarities` rides `InductDecl` and `StructDecl`, one analysis in `curios-analysis` computes it for both checkers, the prelude's are archived once per compiler build, and the kernel recomputes a carried vector rather than believing it (`curios-analysis`'s `tests/driven.rs::a_carried_polarity_vector_is_recomputed_rather_than_believed`).

## The gap

```
use /std/{Str, Nat, Result};
let small(n: Nat) -> Result(Str, Nat) = Result/success(n);
pub let big(n: Nat) -> Result(Str, Type) =
    let _ = small(n)!;
    Result/success(Nat);
```

is refused, "this Type would need to be strictly below itself", `1 ≤ 0`, while `Result/bind(small(n), (_) => Result/success(Nat))` passes. The region's monad is `(A) => Result.{0,1}(Str, A)`, the action is `Result.{0,0}(Str, Nat)`, and conversion equates the second level. `Option` fails the same way; `Io` does not, since a `/sys` former unfolds and its level goes with it. It does not come from settling levels by their provenance: `Nat/of_str : (Str) -> Option(Nat)` carries no universe parameter under any settlement. `monad_tests`' `a_bang_holds_its_region_at_a_lower_nominal_actions_level` pins it, and `/std/Cli`'s `fill` — a `Carrier` payload a level below its `Values` region — is the library's instance.

## Prior art

- **Rocq** ([reference manual, *Universe polymorphism*](https://rocq-prover.org/doc/master/refman/addendum/universe-polymorphism.html)). Each universe of a cumulative inductive is irrelevant (`*`), covariant (`+`) or invariant (`=`), inferred and optionally annotated. `list@{u} (A : Type@{u})` is `*u`: "any two instances of `list` are convertible … whenever [their arguments] are convertible". A record whose field is `Type@{u}` is covariant (`Monoid`), and a universe to the left of an arrow is invariant (`monad@{i}`).
- **Timany and Sozeau, pCuIC** ([*Cumulative Inductive Types In Coq*, FSCD 2018](https://cs.au.dk/~timany/publications/files/2018_FSCD_cumind.pdf)). `C-Ind` relates fully applied instances by their indices and constructor arguments, and "we do not consider parameters … Not considering parameters allows our cumulativity relation for universe-polymorphic inductive types to mimic the behavior of template-polymorphic inductive types". `Ind-Eq` makes mutual cumulativity judgmental equality, and `Constr-Eq-L/R` make constructors of related instances equal (`nil@{i} A ≃ nil@{j} A`). Consistency is a ZFC model with inaccessibles in which `A ≼ B` gives `⟦A⟧ ⊆ ⟦B⟧`; it leaves out inductive types in `Prop`. Coq's inference by checking two fresh instances against each other once blew up on the HoTT library, which is why cumulativity became opt-in there.
- **MetaCoq** ([`Universes.v`](https://github.com/MetaCoq/metacoq/blob/main/common/theories/Universes.v), [`PCUICEquality.v`](https://github.com/MetaCoq/metacoq/blob/main/pcuic/theories/PCUICEquality.v)). PCUIC's `Variance.t` is `Irrelevant | Covariant | Invariant`; `cmp_universe_variance` compares nothing for `Irrelevant`, by the problem's relation for `Covariant` and by equality for `Invariant`; an inductive's variance applies only fully applied, and a fully applied constructor compares no level at all, since "fully applied constructors are always compared at the same supertype".
- **Lean 4** has no cumulativity and lifts by hand with `ULift` ([language reference, *Universes*](https://lean-lang.org/doc/reference/4.24.0/The-Type-System/Universes/)); **Agda**'s `--cumulativity` relates `Set i ≤ Set j` only — "`List {lzero} Nat` is not a subtype of `List {lsuc lzero} Nat`" ([documentation, *Cumulativity*](https://agda.readthedocs.io/en/latest/language/cumulativity.html)).

## Objections, answered

- *No demand, no lifting workaround in `/std`.* `Cli`'s `fill` binds through `Result/bind` for exactly this, and any helper answering `Option(Nat)` or `Result(E, Nat)` meets it in a larger region.
- *Retrofit cost.* Low: the archive is not an interchange format and the library is in-tree.
- *Cumulativity in parameters widens the large-elimination guard's input.* The objection's example, `Vec(Type u, n)` against `Vec(Type v, n)`, relates different parameter arguments, which Rocq does not relate either. An irrelevant level leaves the two instances' constructor telescopes identical, and the guard reads telescopes.
- *The kernel's disagreement count.* It reads zero.

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **Irrelevance only.** Conversion alone changes and subsumption stays as it is. Covariance — `Value.{u} ≤ Value.{v}`, a level typing a field — waits for a program that asks, since it needs both checkers' subsumption to reach nominal types.
2. **A value compares at its family's variance**, rather than MetaCoq's "fully applied constructors compare nothing": the kernel's `ground` compares unfoldings untyped, so "compared at the same supertype" is not guaranteed here.
3. **The variance lives beside the polarities.** A `variances` vector on `InductDecl` and `StructDecl`, computed by one analysis in `curios-analysis` both checkers run, archived with the prelude, and recomputed by the kernel rather than believed. It changes the archived layout, so it takes the stored-unit format's routing obligations. One difference from positivity: positivity reads the zonked registries at the end of a module, while conversion needs a family's variance as soon as a later item compares two of its instances, so the elaborator computes it when the family's group finalizes.
4. **Inference by occurrence, compositional.** A level is irrelevant when no constructor payload, field or index type mentions it except through a position that is itself irrelevant — another nominal's irrelevant level, or a `/sys` former's, which reduction removes — so a `Tree(A)` holding `List(Tree(A))` or `Option(Tree(A))` stays irrelevant; every other level is invariant. Rocq's inference by fresh-instance subtyping is more precise and is what blew up.

## Stages

1. **Investigation.** It ends when each of these is answered:
   - A variance census over `/sys` and `/std`: which universe parameters are irrelevant, per family.
   - The `Prop` argument. pCuIC's model leaves `Prop` families out, and Curios's `Prop` is strict, proof-irrelevant and definitionally K. An irrelevant level of a `Prop` family — `Eq`'s, which types its parameter `A` — leaves its constructors and the large-elimination guard's verdict unchanged; say so from [Index inversion and K](../../design/soundness/elimination/index-inversion-and-k.md) and [Large-elimination guard](../../design/soundness/elimination/large-elimination-guard.md), and probe it.
   - Attack shapes, each a `.crs` probe and a term-level fixture: a level a payload mentions treated as irrelevant — a `Type`-carrying family equated across levels, toward a retraction of `Type 0` — which is the mutation the fixtures must catch; an irrelevant level on a `Prop` family eliminated large; values compared untyped through `ground`; a concept's parameter-typing level against its method levels in witness resolution. A find follows `.claude/commands/hunt-unsoundness.md`'s regression discipline.
   - What conversion stops pinning: which elaborator levels were solved only by a nominal equation, and where finalization now settles them.
2. **The analysis** in `curios-analysis`, with its unit tests and `curios-analysis/tests/driven.rs` coverage, and the `variances` vector carried on the registry entries.
3. **The kernel compares by variance**: the four arms and the level-only rule; a board entry under `documentation/design/soundness/conversion/`; fixtures mutation-checked against treating an invariant level as irrelevant.
4. **The elaborator compares by variance**: `compare_levels` skips irrelevant positions; the census is retaken and every change explained; `a_bang_holds_its_region_at_a_lower_nominal_actions_level` flips, and `/std/Cli`'s `fill` sequences with `!`.

## Verification

- The fixture flips, and its `Result/bind` control still passes.
- `/std` certifies, `kernel_disagreements` reports none, and `curios-analysis/tests/driven.rs` is clean.
- Every attack shape is refused, each by a mutation-checked fixture.
- The parameter census and the prelude build's rows are retaken and every change explained; [A shared term costs its size](../08-architecture/01-shared-term-costs.md)'s type-level claims keep their budgets.

**To retake the measurements.** The census is `cargo test -p curios-prelude-archive --all-features --lib -- --ignored --nocapture universe_parameter_census`, one line per `/std` definition with its parameter count; the kernel walk is `kernel_disagreements` in the same crate, run the same way. The prelude build's rows are read from `curios-prelude-archive/.artifacts/profile.tsv` (elaboration) and `curios-prelude/.artifacts/profile.tsv` (certification) after `cargo xtask clippy`, folded by `target/debug/curios profile <file>`, whose first data row is the total; take them on an otherwise idle machine. Those claims are timed as [its budget table](../08-architecture/01-shared-term-costs.md#budgets) says.

## Rejected

- **Template polymorphism**, dropping parameter-only levels from a nominal's context and computing its sort from its arguments: Rocq is retiring it, and pCuIC shows irrelevance subsumes it.
- **Parameter cumulativity** (`Vec(Type u, n) ≤ Vec(Type v, n)`): neither Rocq nor pCuIC relates different parameter arguments, and it is what the guard's concern was about.
- **Subsumption without conversion**, relating instances one way only: `Ind-Eq` makes mutually related instances equal, and a one-way rule still refuses the region's `M(A)` against the action's type wherever conversion rather than subsumption compares them.
- **Explicit lifting**, as Lean's `ULift`: with no level syntax a program cannot write one.
- **Covariance now**: no program asks, and it needs both checkers' subsumption to reach nominal types.
- **Comparing no level on a value**, MetaCoq's rule for a fully applied constructor: see decision 2.

## Retirement

Check [the universe decision](../../design/types/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md), which states irrelevance as the rule with its prior art and what was rejected, against what landed; extend `syntax.md`'s cumulativity sentence ("A type accepted at one level is also accepted where a higher level is required") to a nominal type whose level only types its parameters; record the new board entry's status; replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
