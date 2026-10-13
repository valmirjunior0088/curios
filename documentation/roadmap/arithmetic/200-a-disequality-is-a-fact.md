# A disequality is a fact

Working specification for reading a guard that rules one value out — an `==` that came back `false`, a `!=` that came back `true` — as a fact a bound is proved from. [A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) reads both arms of a comparison and the true arm of an equality, and leaves an equality's false arm unread, so the zero check a newcomer writes first does not license the division it guards.

It is independent of every other spec.

## What this builds on

- **A guard is read by the case it took.** `Reader::guard` (`curios-elab/src/entailment/facts.rs`) admits a `<` or a `<=` in its true arm as it stands and in its false arm as its dual, through `le/of_not_lt` and `lt/of_not_le`, and an `==` in its true arm as an equation, through `eq_of_eql`. Every other pair is pushed to `unread` under the comment `An equality's false arm, and an inequality's true one, is a disjunction`. `sides` reads `NatLt`, `NatLe`, `NatEql` and their `Int` twins and nothing else, so a `!=` is no comparison to it in either arm.
- **A hypothesis is read the same way.** `Reader::comparison` admits a bound and an equation, and a hypothesis that states `Holds(Bool/not(a == b))` is left unread.
- **A disjunction already has a form.** A `Split` is a comparison the proof splits on, with the facts each case adds; `Reader::open` opens one for a truncated subtraction, a `min`, a `max`, an `abs` and a `sign`, and joins it to a split already open on a comparison conversion holds to be the same one or its negation (`answered_guard`). A case the scope already decides is read as facts, the cases are otherwise kept as a split the search opens where it needs one, and the search counts the rows it derives against one cap.
- **The proofs are written from a fixed vocabulary.** `OrderSyntax` (`curios-utilities/src/syntax.rs`) names one carrier's order lemmas, `curios-text/src/registry.rs` binds them to `/std/Nat/le`, `/std/Nat/lt`, `/std/Int/le` and `/std/Int/lt`, and a proof form is written only where every name it applies is in scope (`in_scope`).
- **A natural is at least zero**, which the reader states of every monomial over naturals and conversion decides.
- **The two carriers state their division's bound apart.** `Div(Nat)` and `Rem(Nat)` state `Ok(b) = Holds(0 < b)` (`curios-std/src/Nat.crs`); `Div(Int)` and `Rem(Int)` state `Ok(b) = NonZero(b)` (`curios-std/src/Int.crs`), which the false arm of `b == 0` states outright.
- **An arm that states no case learns nothing.** `Reader::guards` reads the scrutinee entries whose value is a `Bool`, and a literal dispatch's `| _ =>` records none: [`documentation/syntax.md`](../../syntax.md) has it that a default "binds nothing and refines no index".

## The gap

Over `let average(total: Nat, count: Nat) -> Nat`, probed at `d5046c5cc`:

| The guard around `total / count` | The bound |
| --- | --- |
| `match 0 < count \| true => …`, `count > 0`, `count >= 1` | discharged |
| `match count < 1 \| true => 0 \| false => …` | discharged |
| `match count \| 0 => 0 \| k + 1 => …` | discharged |
| `match count == 0 \| true => 0 \| false => …` | refused: `not read: a guard's false arm, count == 0` |
| `match count != 0 \| true => … \| false => 0` | refused: `not read: a guard, count != 0` |
| `match Bool/not(count == 0) \| true => …` | refused |
| `choose \| count == 0 => 0 \| _ => … end` | refused |
| `match count \| 0 => 0 \| _ => …` | refused, with nothing listed as unread |

The same holds of `List/len(xs) == 0` before a division by the length. `let ratio(a: Int, b: Int) -> Int = match b == 0 | true => 0 | false => a / b end;` is accepted, so one spelling of the check is a fact at `Int` and none at `Nat`. Each refusal ends `decide the bound with a guard before the operation`, which is what the program did, and names no guard that would be read.

`README.md` offers the feature in these words: "`/` asks for a positive divisor, `List/get` for an index below the length, each discharged by a guard's own decision with no proof named".

## Prior art

- **Mathlib's `linarith` leaves a disequality out by default and splits on request.** Its frontend states that "Disequality hypotheses `t ≠ 0` do not fit in this pattern", that it "does not split disequality hypotheses, since this would lead to a number of runs exponential" in their number, and that with `splitNe` it "will case split on disequality hypotheses", running once with `x < y` and once with `x > y` ([`Mathlib.Tactic.Linarith.Frontend`](https://florisvandoorn.com/carleson/docs/Mathlib/Tactic/Linarith/Frontend.html)). The procedure here is modelled on `linarith` and already holds what `linarith` lacks for this: a split opened only where the search needs one, under a cap that counts derived rows.
- **Lean lists the check itself among what its users get wrong.** `n / 0 = 0` there, so a statement over a quotient is false unless a hypothesis rules zero out ([Lean 4 pitfalls](https://leanprover-community.github.io/extras/pitfalls.html)). Curios demands the bound instead, which moves the cost to the guard: the guard a reader writes has to be one the procedure reads.

## Decisions

None is settled. What the tree's own rules already fix:

- **The proof is a term both checkers recheck**, as every proof the procedure writes is, so reading a new fact adds nothing to the trusted base.
- **A new lemma is `/std`'s, reached through a registry slot**, since no stage spells a prelude name.

## Open questions

- **How a disequality enters.** Recommended, and not settled: as a `Split` on `a <= b`, whose case that holds adds `a < b` and whose case that fails adds `b < a` by `lt/of_not_le`. The first case needs one lemma a carrier, `a <= b` and `a == b` failing give `a < b`, stated over its operands as `of_not_le` is and proved by `qed` where the guard's arm answers `a == b` as `false`. It joins a split already open on the same comparison, as a subtraction's does. At `Nat` against a side conversion holds at zero, the case `a < 0` is refuted by the fact that a natural is at least zero, so the zero check costs one case and no search past it. Not read: whether a guard's `qed` proves `Holds(Bool/not(a == b))` where the guard is written through `Eql` and where it is written as `!=`, the two spellings a guard is recorded under.
- **What bounds the cost.** `linarith` declines because the runs double with each disequality. Here a split is one the search opens where it needs one, and the cap counts what is derived. Whether that holds the library's cost where it stands is the second stage's reading; a scope that holds many disequalities and a bound none of them bears on is its fixture.
- **Whether an arm that states no case learns what the arms before it took.** `match count | 0 => 0 | _ => total / count end` is the other spelling a reader reaches for. Giving the default arm the disequalities its siblings' literals state is a change to what an arm is typed under, which [`documentation/syntax.md`](../../syntax.md) states as "typed once, under the cases its own pattern states and nothing else" and [Patterns are compiled where their types are known](../../design/surface/patterns-are-compiled-where-their-types-are-known.md) decides. Recommended: this spec leaves the arm as it is, and the refusal there names the spelling that is read, `| k + 1 =>`; the arm is a decision of its own if a consumer asks.
- **What the refusal says once the guards are read.** Its last line tells a reader to decide the bound with a guard. Recommended: where the unread list holds a decision over the bound's own atoms, the line names it and what was not read of it; otherwise it stays.

## Stages

1. **An `==` guard's false arm and a `!=` guard's true arm are read.** The lemma at `Nat` and at `Int`, its slot in `OrderSyntax` and its binding; `sides` reads a `!=`; `Reader::guard` opens the split. Check: the eight rows of the table as fixtures in `curios`'s `tests::bounds`, each discharged or refused as the table's last column is rewritten to say; the `Int` program still accepted; `List/len(xs) == 0` before a division by the length discharged.
2. **A hypothesis that states a disequality is read**, by the same split with the hypothesis as its proof. Check: `let per(total: Nat, n: Nat, p: Holds(Bool/not(n == 0))) -> Nat = total / n;` accepted, and its twin over `Holds(n != 0)`; and the cost fixture of the second open question, answered inside the cap.
3. **The refusal names what was not read.** Check: the pinned reports of `tests::bounds` that list a decision unread, each rewritten once.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format` after each stage, with that stage's fixtures by name. The lint elaborates, erases and certifies `/std`, so the library's own bounds are held by it. Two figures may move and are read once the second stage lands, counted, from the profile streams `cargo xtask workspace lint` files: the rows a search derives over `/std` (the `entailment::derived` sample) and the nodes of the proofs it writes (`entailment::written`).

## Rejected

- **`Div(Nat)` stating `NonZero`, as `Int` does.** The false arm of `count == 0` would then state the bound outright, and every bound `/std` proves from `0 < n` would need the step from one to the other, which is this spec's lemma read the other way at every site instead of once in the reader.
- **Splitting on every disequality before the search.** It is the doubling `linarith` declines.

## Completion and retirement

The decision takes the new fact into its fragment and the lemma into its vocabulary; the reader's `//!` lists a disequality among where a fact comes from and drops it from what is recorded unread; `tests::bounds` holds the grid. The line in `documentation/roadmap.md` becomes part of the checked line that cites the decision, and this file is deleted.
