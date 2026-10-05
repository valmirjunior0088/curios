# `!` sequences actions whatever level their payloads sit at

Working specification for a region whose actions' payloads sit at different universe levels. A level has no syntax, so the only universe refusal a program is owed is a type that would have to contain itself; `!` refuses more than that in every monad, for two reasons that do not depend on each other. Both checkers equate the levels of two instances of an `induct` or a `struct`, where Rocq, and MetaCoq's verified specification of it, compare them by variance and a level that only types a parameter is irrelevant. And a witness's levels are closed where it resolves, before the rest of its region is read. [A universe level is implicit, cumulative, and settles by where it came from](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md) states irrelevance as the rule; this brings both checkers to it at the narrowest point that makes the heading true, irrelevance and nothing covariant, and closes a witness's levels with the declaration that resolves it.

Its kernel stage lands before its elaborator stage, so the kernel never refuses what the elaborator starts producing. It meets two specs and waits on neither: [A universe level settled before its evidence is in](02-levels-settled-before-their-evidence.md) keeps a goal resolved after its consumer has closed and what identification commits at a definition's instance, and [A subsumption blocked on a metavariable waits as a subsumption](03-blocked-subsumption.md) decides at which relation the level shortcuts run; here the levels those shortcuts pair stop including an irrelevant one.

## What this builds on

- **Levels settled by where they came from.** An occurrence's level settles at its recorded floor, the kernel types a type as written, and a concept's method level is its family's domain: `Monad(M: (Type) -> Type)`'s `bind(@A: Type, @B: Type, …)` takes both payloads at one level, and cumulativity admits a small type there, so sequencing a `Nat` beside a `Type` needs no lift. `!` sequences a large payload through `Result`'s `Monad` witness (`monad_tests`' `a_bang_sequences_a_large_payload`).
- **The monad's own function accepts every mix.** `Result/bind` takes the action's payload and the result's at levels of their own, and `Io/bind` accepts the same programs; `/std/Cli`'s `fill` binds through `Result/bind` for this gap alone.
- **How the kernel compares a nominal node today.** `levels_eq` in the `InductType`, `StructType`, `Variant` and `Struct` arms of `structural` (`curios-cert/src/kernel/convert.rs`). A former applied in full reaches them as its node, since `compare` forces both sides first; `rec_instances`, the level-only rule over `Term::level_differences`, answers for a bare projection and a stuck application.
- **How the elaborator compares one today.** An `induct`'s former is a `rec` projection and a `struct`'s a definition, and two applications of one are decided before `compare_induct_type` or `compare_struct_type` is reached, at three sites of `curios-elab/src/convert.rs` that pair levels through `Term::level_differences` and commit each pair equal (`identify_universe_levels`): `drain`'s levels-only shortcut ahead of reduction, `compare_same_global_apply` once the spines agree, and `drain`'s arm for two applied projections of one group, through `level_question`, which a recurrence asks too. Past them, `compare_levels` adds one equation per level in `compare_induct_type`, `compare_variant`, `compare_struct_type` and `compare_struct`.
- **How a witness's levels close today.** `instantiate` (`curios-elab/src/resolve.rs`) matches the witness's terminal against the goal and then calls `UniverseSolver::close_instance`, which pins the instance to the levels the goal already fixes and minimizes every other level the witness's scheme minted, whether the goal resolves inside its consumer or, deferred, after it (`attempt_witness_goal`, `retry_witness`). Its documentation gives the reason for the second alone: a goal resolved after its consumer has finalized has no finalization left to close it.
- **`!` names the region's monad before it checks the action.** `elaborate_bang` (`curios-elab/src/elaborate/binding.rs`) hands the wrapper the monad `region_monad` reads off the expected type, so the `Monad` goal is rigid at once and its witness closes at the region's payload. `Monad/bind` written out infers the monad from its action, and its witness closes at the action's.
- **Subsumption** relates sorts and function types only, and is conversion everywhere else ([Subsumption is a relation, not a traversal order](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md)).
- **Full application is structural.** `InductType`, `StructType`, `Variant` and `Struct` are built only inside a fully applied former or constructor wrapper, and the kernel checks their counts at the boundary; a partial application is the wrapper, not the node.
- **Positivity is the precedent for where a derived fact about a declaration lives** ([Strict positivity modulo polarity](../../design/theory/strict-positivity-modulo-polarity.md)): `polarities` rides `InductDecl` and `StructDecl`, one analysis in `curios-analysis` computes it for both checkers, the prelude's are archived once per compiler build, and the kernel recomputes a carried vector rather than believing it (`curios-analysis`'s `tests/driven.rs::a_carried_polarity_vector_is_recomputed_rather_than_believed`). The kernel reads a registry entry while it walks the items and accepts it after: positivity and declaration acceptance run once every item is defined (`curios-cert/src/recheck.rs`).
- **What the prelude's families hold**, counted at `b900e392f` without reduction: 159 families, 100 with no universe parameter. The other 59 carry 102 parameters. 59 are irrelevant, `Result`'s, `Option`'s, `Eq`'s and `Accessible`'s among them. 41 are invariant: 37 in `/std/Cli`, every one reaching `Value`'s `Type`-typed field, and 4 in `Monad` and `Lift`, whose methods bind at them. 2 are mentioned only on an instance the count did not force, `Vec`'s through `Counted` and `Map/Key`'s through `Hash/hash`. None is mentioned by an index target alone.

## The gap

A payload that is a type sits a level above one that is a value. With `small(n: Nat) -> M(Nat)` and `large(n: Nat) -> M(Type)`:

```crs
use /std/{Str, Nat, Result};
let small(n: Nat) -> Result(Str, Nat) = Result/success(n);
let large(n: Nat) -> Result(Str, Type) = Result/success(Nat);
pub let big(n: Nat) -> Result(Str, Type) =
    let _ = small(n)!;
    Result/success(Nat);
pub let one(n: Nat) -> Result(Str, Nat) =
    let _ = large(n)!;
    Result/success(n);
```

| | A large region, a small action (`big`) | A small region, a large action (`one`) |
| --- | --- | --- |
| `!` over `Result` | refused | refused |
| `!` over `Io` | accepted | refused |
| `Monad/bind` written out, over `Result` | refused | accepted |
| `Monad/bind` written out, over `Io` | refused | accepted |
| `Result/bind`, `Io/bind` | accepted | accepted |

The refusal is "this Type would need to be strictly below itself", `1 ≤ 0`, and over `Io` with `!` it reads `type mismatch`, `inferred: Io(Type)`, `expected: Io(?)`. `Option` fails the first column as `Result` does, a function that reads `Nat/of_str(text)!` and answers `Option/some(Vec(Nat, n))` being its plain instance, and a region holding both actions is refused over `State` and `Try`.

**A nominal type's levels are part of its identity.** `big`'s monad is `(A) => Result.{0,1}(Str, A)`, the action is `Result.{0,0}(Str, Nat)`, and conversion equates the second level. `Io` passes that cell, since a `/sys` former unfolds and its level goes with it. It does not come from settling levels by their provenance: `Nat/of_str : (Str) -> Option(Nat)` carries no universe parameter under any settlement. The same refusal needs no `!`:

```crs
let through(F: (Type) -> Type, x: F(Nat), y: F(Type)) -> Nat = 0;
let probe(n: Nat) -> Nat = through((A) => Result(Str, A), small(n), Result/success(Nat));
```

where every level is committed by `identify_universe_levels` and `compare_levels` is never asked, while `big`'s refusal is raised by `compare_levels` in `compare_induct_type`.

**A witness's levels are closed where it resolves.** With `!` the witness resolves before the action is read, at the region's payload, so a larger action no longer fits its method. Written out, `Monad/bind` infers its monad from the action and the witness closes at the action's payload, so a larger region does not fit. `Io` fails both ways though its former carries no level.

`monad_tests`' `a_bang_holds_its_region_at_a_lower_nominal_actions_level` pins the first cell, and `/std/Cli`'s `fill`, a `Carrier` payload a level below its `Values` region, is the library's instance.

**Both changes, tried.** A copy of the tree at `b900e392f` reads two switches from the environment while a program elaborates, `/std` built with neither. One leaves a witness's levels open. The other treats every level of `Result`, `Option`, `State` and `Try` as irrelevant at each site above and in the kernel's four arms. Counted over eleven programs:

| | As today | Witness left open | Irrelevance | Both |
| --- | --- | --- | --- | --- |
| `!` over `Result`, `Option`, `State` and `Try`, six programs | refused | one accepted | accepted | accepted |
| `through` | refused | refused | accepted | accepted |
| `!` over `Io`, two programs | refused | accepted | refused | accepted |
| `Monad/bind` written out over `Io`, a large region | refused | accepted | refused | accepted |
| `Monad/bind` written out over `Result`, a large region | refused | refused | refused | refused |

Every `!` program is accepted with both, and by the kernel. It does not show that `/std` elaborates under either change, what the parameter census becomes, or what either costs, and its second switch is cruder than decision 5.

**What stays refused.** `Monad/bind(small(n), (_) => Result/success(Nat))` at `Result(Str, Type)`, the table's last row. Its cause is not traced; it reads as the wrapper's monad solved from the action and carrying the action's levels, `Result.{0,0}(Str, ·)`, where no `Type` fits. `!` reads the region and does not meet it. [A subsumption blocked on a metavariable](03-blocked-subsumption.md)'s refreshed copy, reaching an irrelevant level as it reaches a codomain's sort, is the rule that would; it waits for a program that writes the method out.

## Prior art

- **Harper and Pollack** ([*Type checking with universes*](https://doi.org/10.1016/0304-3975(90)90108-T), TCS 89, 1991). Typical ambiguity: levels are left unwritten, and a term is accepted when an assignment of levels makes it correct. A refusal no assignment explains is a refusal of the checker's making.
- **Rocq** ([reference manual, *Universe polymorphism*](https://rocq-prover.org/doc/master/refman/addendum/universe-polymorphism.html)). Each universe of a cumulative inductive is irrelevant (`*`), covariant (`+`) or invariant (`=`), inferred and optionally annotated. `list@{u} (A : Type@{u})` is `*u`: "any two instances of `list` are convertible … whenever [their arguments] are convertible". A record whose field is `Type@{u}` is covariant (`Monoid`), and a universe to the left of an arrow is invariant (`monad@{i}`). Its inference walks occurrences ([`kernel/inferCumulativity.ml`](https://github.com/rocq-prover/rocq/blob/master/kernel/inferCumulativity.ml)): the parameters' types are not walked, the arity's sort is not, and a constructor's conclusion is — "If we have Inductive foo@{i j} : ... -> Type@{i} := C : ... -> foo Type@{j} i is irrelevant, j is invariant." Its minimization "can be applied at the end of a proof" and touches "only the fresh universes generated for each global application" ([`dev/doc/universes.md`](https://github.com/rocq-prover/rocq/blob/master/dev/doc/universes.md)): a level is chosen once the definition has been read. Its account of template polymorphism shows it lost through a `state` monad over `prod` ([*Template vs. "full" universe polymorphism*](https://rocq-prover.github.io/platform-docs/rocq_theory/explanation_template_polymorphism.html)).
- **Timany and Sozeau, pCuIC** ([*Cumulative Inductive Types In Coq*, FSCD 2018](https://cs.au.dk/~timany/publications/files/2018_FSCD_cumind.pdf)). `C-Ind` relates fully applied instances by their indices and constructor arguments, and "we do not consider parameters … Not considering parameters allows our cumulativity relation for universe-polymorphic inductive types to mimic the behavior of template-polymorphic inductive types". `Ind-leq` asks more than arguments: "corresponding constructors need to construct judgementally equal results". `Ind-Eq` makes mutual cumulativity judgmental equality, and `Constr-Eq-L/R` make constructors of related instances equal (`nil@{i} A ≃ nil@{j} A`). Consistency is a ZFC model with inaccessibles in which `A ≼ B` gives `⟦A⟧ ⊆ ⟦B⟧`; it leaves out inductive types in `Prop`, "because they add extra complexity to the construction of set theoretic models". Coq 8.7 computed the relation by checking two fresh instances against each other, which blew up on a case from the HoTT library, and cumulativity became opt-in there.
- **MetaCoq** ([`Universes.v`](https://github.com/MetaCoq/metacoq/blob/main/common/theories/Universes.v), [`PCUICEquality.v`](https://github.com/MetaCoq/metacoq/blob/main/pcuic/theories/PCUICEquality.v)). PCUIC's `Variance.t` is `Irrelevant | Covariant | Invariant`; `cmp_universe_variance` compares nothing for `Irrelevant`, by the problem's relation for `Covariant` and by equality for `Invariant`; an inductive's variance applies only fully applied, and a fully applied constructor compares no level at all, since "fully applied constructors are always compared at the same supertype".
- **Lean 4** has no cumulativity — "a type in `Type u` is not automatically also in `Type (u + 1)`" ([language reference, *Universes*](https://lean-lang.org/doc/reference/latest/The-Type-System/Universes/)) — and its `bind : {α β : Type u} → m α → (α → m β) → m β` holds both payloads to one universe ([*Functors, Monads and `do`-Notation*](https://lean-lang.org/doc/reference/latest/Functors___-Monads-and--do--Notation/)). mathlib's [`ULiftable`](https://leanprover-community.github.io/mathlib4_docs/Mathlib/Control/ULiftable.html) carries a monad across universes by hand: "this class convert between instantiations, from `M.{u} : Type u₁ → Type u₂` to `M.{v} : Type v₁ → Type v₂` and back".
- **Agda**'s `--cumulativity` relates `Set i ≤ Set j` only — "`List {lzero} Nat` is not a subtype of `List {lsuc lzero} Nat`" ([documentation, *Cumulativity*](https://agda.readthedocs.io/en/latest/language/cumulativity.html)) — and its standard library's `IO` binds at one level, `bind : {B : Set a} …`, with `lift!` to raise an action by hand ([`IO/Base.agda`](https://github.com/agda/agda-stdlib/blob/master/src/IO/Base.agda)).

## Objections, answered

- *No demand, no lifting workaround in `/std`.* `Cli`'s `fill` binds through `Result/bind` for exactly this, any helper answering `Option(Nat)` or `Result(E, Nat)` meets it in a larger region, and a function that parses and answers a type meets it at its first `!`.
- *A lift would do.* Lean and Agda lift by hand; with no level syntax a program cannot write one, and cumulativity already admits the small payload where the large one is taken, so nothing is left to lift.
- *Retrofit cost.* Low: the archive is not an interchange format and the library is in-tree.
- *Cumulativity in parameters widens the large-elimination guard's input.* The objection's example, `Vec(Type u, n)` against `Vec(Type v, n)`, relates different parameter arguments, which Rocq does not relate either. An irrelevant level leaves the two instances' constructor telescopes identical, targets included, and the guard reads telescopes.
- *pCuIC's model leaves `Prop` families out.* For the model's complexity, by its own account, and not for a counterexample. The two `Prop` families of the prelude that take a universe parameter, `Eq` and `Accessible`, are irrelevant in it; what the guard and inversion read of a family — its literal result sort, its constructors' telescopes and their targets ([Large-elimination guard](../../design/soundness/elimination/large-elimination-guard.md), [Index inversion and K](../../design/soundness/elimination/index-inversion-and-k.md)) — is what the analysis reads, so an irrelevant level changes neither verdict. The kernel stage holds it by a fixture.
- *The kernel's disagreement count.* It reads zero.

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **Irrelevance only.** Conversion alone changes and subsumption stays as it is. Covariance — `Value.{u} ≤ Value.{v}`, a level typing a field — waits for a program that asks, since it needs both checkers' subsumption to reach nominal types.
2. **A value compares at its family's variance**, rather than MetaCoq's "fully applied constructors compare nothing": the kernel's `ground` compares unfoldings untyped, so "compared at the same supertype" is not guaranteed here. With nothing covariant it refuses nothing more: two values at different invariant levels share no type.
3. **The variance lives beside the polarities.** A `variances` vector on `InductDecl` and `StructDecl`, computed by one analysis in `curios-analysis` both checkers run, archived with the prelude, and recomputed by the kernel rather than believed. It changes the archived layout, so it takes the stored-unit format's routing obligations. One difference from positivity: positivity reads the zonked registries at the end of a module, while conversion needs a family's variance as soon as a later item compares two of its instances. So the elaborator computes it when the family's group finalizes, and the kernel reads the carried vector while it walks the items, as it reads every registry entry, and reconciles it after the walk, beside positivity: a carried irrelevance the recomputation denies refuses the module.
4. **Inference by occurrence, compositional.** A level is irrelevant when no constructor payload, index target, field or index type mentions it except through a position that is itself irrelevant — another nominal's irrelevant level, or a `/sys` former's, which reduction removes — so a `Tree(A)` holding `List(Tree(A))` or `Option(Tree(A))` stays irrelevant; every other level is invariant. Only a parameter's type and the result sort go unread, as in Rocq's walk. The targets are read because a level may reach nothing else:

   ```crs
   induct N(F: (Type) -> Type): pub Type | n(T: Type, w: F(T)) end
   induct Fam(F: (Type) -> Type): (D: Type) -> pub Type | mk(): (N(F)) end
   ```

   elaborates to `Fam.{u,v,w}` with `mk : Fam.{u,v,w}(F)(N.{u,v}(F))`. No payload, field or index type of `Fam` mentions `u`, `u` is invariant in `N`, and at one index two instances of `Fam` apart in `u` have different inhabitants.
5. **Every site that pairs levels reads the vector.** The walk that aligns two terms' levels skips an irrelevant position of a nominal node and of a former applied in full, so the levels-only shortcut, the same-global rule, the applied-projection arm and the recurrence question commit nothing there, and `compare_levels` adds no equation there. A bare or partly applied former keeps equality, as an inductive's variance applies only fully applied in Rocq and MetaCoq.
6. **A witness's levels close with the declaration that resolves it.** A goal resolved while its consumer is open is pinned to what the goal fixes and no further; the levels left are the consumer's, and its `finalize` settles them with everything the declaration said. A goal resolved after its consumer has closed is closed where it resolves, as today, until [the first decision of the spec beside this one](02-levels-settled-before-their-evidence.md) removes deferral.

## Open questions

Each is answered by the stage named, before its code.

- **How the alignment recognizes a former applied in full** (stage 5). A nominal type reaches it in three spellings: its node; an instance of its family's name at the head of a full application; and, for an `induct`, a `rec` projection applied in full, whose two instantiated groups differ in their own binder types as well as in the node's instance. The experiment compared the last as the two forced nodes.
- **A telescope still holding a metavariable when its group finalizes** (stage 3). The solution may mention any level, so every level of such a family is invariant unless the elaborator can show the telescope closed.
- **Whether a level left open reaches a signature** (stage 2). A level a witness's scheme minted is the body's unless a type mentions it; one that a signature does mention becomes a parameter, and each is named by the census.
- **What conversion stops pinning** (stages 2 and 5). An irrelevant level types a parameter, so its argument bounds it from below and `finalize` settles it there; which declarations' parameter counts move is the census's answer.

## Stages

1. **The matrix and the count.** `monad_tests` holds the table's cells as fixtures with today's verdicts, over `Result`, `Option`, a `struct` monad and `Io`, with a region holding both actions and `through`; the existing fixture is the first. `curios-prelude-archive` gains an inventory test printing each family's vector with the part that fixes each invariant level, by the protocol under *Verification*.
2. **A witness's levels close with its declaration**, decision 6. Elaborator alone. `one` is accepted over `Result` and `Io`, and `Monad/bind` written out is accepted over `Io` in a large region; `tests::universes::a_goal_deferred_past_its_declaration_settles_at_its_least_levels` and the two `/std` declarations `finalize` names for its settlement loop do not move.
3. **The analysis** in `curios-analysis`, decisions 3 and 4, with its unit tests and `curios-analysis/tests/driven.rs` coverage, the `variances` vector carried on the registry entries, and the inventory test reading it.
4. **The kernel compares by variance**: the four arms, with `rec_instances` as it is, and the reconciliation after the walk; a board entry under `documentation/design/soundness/conversion/`. Fixtures, each mutation-checked: an invariant level treated as irrelevant, a `Type`-carrying family equated across levels toward a retraction of `Type 0`; a level only an index target mentions, the `Fam` of decision 4; an irrelevant level on a `Prop` family eliminated large; values compared untyped through `ground`; a concept's parameter-typing level against its method levels; a carried vector that claims an irrelevance the recomputation denies. A find follows `.claude/commands/hunt-unsoundness.md`'s regression discipline.
5. **The elaborator pairs levels by variance**, decision 5. Every nominal cell of the matrix is accepted with `!`; `a_bang_holds_its_region_at_a_lower_nominal_actions_level` takes the name of what then holds; `/std/Cli`'s `fill` sequences with `!`.

## Verification

- Every `!` fixture of the matrix is accepted, `through` is, and the last row of the experiment's table is held as refused, with the monad's own function as each cell's control.
- `/std` certifies, `kernel_disagreements` reports none, and `curios-analysis/tests/driven.rs` is clean.
- Every attack shape is refused, each by a mutation-checked fixture.
- After stages 2 and 5 the parameter census and the prelude build's rows are retaken and every change explained; the rows of `type_level_claim_measurements` (`curios/src/tests/reduction.rs`) are retaken with them.

**To retake the measurements.** The census is `cargo test -p curios-prelude-archive --all-features --lib -- --ignored --nocapture universe_parameter_census`, one line per `/std` definition with its parameter count; the kernel walk is `kernel_disagreements` in the same crate, run the same way. The prelude build's rows are read from `curios-prelude-archive/.artifacts/profile.tsv` (elaboration) and `curios-prelude/.artifacts/profile.tsv` (certification) after `cargo xtask clippy`, folded by `target/debug/curios profile <file>`, whose first data row is the total; take them on an otherwise idle machine. Those claims are counted: `cargo test --package curios --all-features -- --ignored --nocapture type_level_claim_measurements`.

The count of the prelude's families is `counted`. Restore the prelude (`curios_prelude_archive::with_prelude`) and, for every entry of each root's `induct_decls` and `struct_decls`, walk each constructor's payload types and index targets, each field type and each index type, leaving out the parameters' types and the result sort. A `Type` marks the parameters of its level invariant; a nominal node, or an instance of a family's name applied in full, marks those at the family's invariant positions; an instance of `/sys/List/List`, `/sys/Io/Io`, `/sys/Cell/Cell` or `/sys/Channel/Channel` marks none; any other instance marks all of its own. Iterate from every level irrelevant until no vector changes. Stage 3's analysis replaces the walk and forces what this one does not.

The experiment is `counted`. Its first switch returns from `UniverseSolver::close_instance` once `pin_instance` has run. Its second, given a list of families, skips `compare_levels` and the kernel's `levels_eq` for them, withholds `drain`'s levels-only shortcut and `compare_same_global_apply`'s polymorphic case, and, where `drain` has reduced a pair to two applied projections that force to nodes of one listed family, compares the nodes. Its programs are the table's cells over `Result` and `Io`, a region holding both actions over `Result`, `State`, `Try` and `Io`, `Option` answering a type from `Nat/of_str` and a second function using it, and `through`.

## Rejected

- **Template polymorphism**, dropping parameter-only levels from a nominal's context and computing its sort from its arguments: pCuIC shows irrelevance subsumes it, and Rocq's own account shows it lost as soon as a definition wraps the type.
- **Parameter cumulativity** (`Vec(Type u, n) ≤ Vec(Type v, n)`): neither Rocq nor pCuIC relates different parameter arguments, and it is what the guard's concern was about.
- **Subsumption without conversion**, relating instances one way only: `Ind-Eq` makes mutually related instances equal, and a one-way rule still refuses the region's `M(A)` against the action's type wherever conversion rather than subsumption compares them.
- **Explicit lifting**, as Lean's `ULift`: with no level syntax a program cannot write one.
- **Covariance now**: no program asks, and it needs both checkers' subsumption to reach nominal types.
- **Comparing no level on a value**, MetaCoq's rule for a fully applied constructor: see decision 2.
- **Inferring variance by checking two fresh instances against each other**, as Coq 8.7 did: it is what blew up, and Rocq walks occurrences now.
- **Declining the level shortcuts on any nominal instance**, leaving the pair to the structural arms: the shortcuts exist so that two spellings of one computation are identified without running it, and a nominal type inside such a spelling would cost the reduction they avoid.
- **Weak constraints for an irrelevant level**, stored and forgotten at minimization as Rocq's `UWeak`: an irrelevant level is compared at nothing, so there is no constraint to keep.
- **Resolving the witness after the action**: `Monad/bind` written out does, and refuses the other direction. A method's level is the larger of its action's and its region's, which neither order has read when it resolves.
- **Closing every witness with a declaration, the deferred ones included**: a goal resolved after its consumer has closed has no declaration left, which is what the spec beside this one removes.

## Retirement

Check [the universe decision](../../design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md), which states irrelevance as the rule with its prior art and what was rejected, against what landed, and state in it that a witness's levels close with the declaration that resolves it; extend `syntax.md`'s cumulativity sentence ("A type accepted at one level is also accepted where a higher level is required") to a value of a nominal type whose level only types its parameters; record the new board entry's status; replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
