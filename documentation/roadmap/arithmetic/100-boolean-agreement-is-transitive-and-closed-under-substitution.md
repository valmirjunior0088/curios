# Boolean agreement is transitive and closed under substitution

[Conversion is one relation in both checkers](../../design/soundness/conversion-is-one-relation-in-both-checkers.md) holds conversion to transitivity, and [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) to symmetry, transitivity and closure under substitution with the theory in it. Two `Bool` terms are decided by a truth table over their atoms only while the pair holds at most `curios-algebra`'s `BOOL_ATOM_CAP` of eight, so a chain or a substitution that carries a pair past eight leaves undecided what its parts decide.

**Not refined yet.** It waits on the design of what bounds the comparison in the cap's place, under *Refinement*.

## What is wrong

`Formula::new_atom` (`curios-algebra/src/boolean.rs`) declines a ninth atom of the pair, and the refusal says the two terms were `not compared` (`curios-elab/src/error/display.rs`). The cap bounds the pair and not either side:

- **Transitivity.** With `A` the conjunction of `a0 || a1 || a2 || a3 || a4` and the negation of each of its five atoms, and `B` the same over `a5` to `a9`, `Eq/refl()` closes `Eq()(A, false)` and `Eq()(false, B)` and is refused at `Eq()(A, B)`, whose pair holds ten atoms (`curios`'s `tests::laws::written`, `a_mismatch_nothing_compared_says_so`).
- **Substitution.** Absorption, `Eq()(b || (b && c), b)`, closes by `Eq/refl()`. At `c := a1 && a2 && a3 && a4 && a5 && a6 && a7 && a8` it closes as a call of that lemma and is refused by `Eq/refl()`, nine atoms being past the table (the row `Bool, at the table's cap` in `tests::laws::written`).

A cap on the pair breaks both laws at any value it is given: two contradictions over more than half the cap each convert with `false` and not with each other. Raising it moves the break and does not remove it.

`tests::laws::audit` does not meet it. Its substitution rows put a comparison for a `Bool` variable, `b := x < y`, one atom for one, and its chains join laws of a few atoms each, so no row carries a pair past eight.

Probed at `1e23ec62c` with the release compiler, the checkers refuse alike: a local `let` annotated at `Eq()(false, false)` carries a proof of `Eq()(A, A)` to `Eq()(B, B)` through both, since the kernel checks the annotation as a step too, and no program was found that one checker accepts and the other refuses.

## What is known

- **Some bound has to stand.** Deciding two formulas equivalent is co-NP-complete. What can move is what a refusal at the bound is.
- **A spent budget is outside the laws.** The audit does not count a refusal by a spent budget as a refusal (the fourth point of [Conversion is one relation in both checkers](../../design/soundness/conversion-is-one-relation-in-both-checkers.md)), and [the soundness board](../../design/soundness/the-soundness-board.md) states acceptance relative to a deterministic step budget and upward closed in it. A comparison priced in the reduction budget, declining by exhausting it, leaves both laws whole where the cap breaks them, and a larger budget decides what a smaller one declined, which no cap does.
- **The same two formulas are decided on every target.** That is why the cap is a constant of the language (`curios-algebra`'s `boolean` module). The reduction budget is deterministic and counts no host time, so a price keeps it where the price is a function of the two formulas alone.
- **What could decide past the table.** A reduced ordered decision diagram built for one comparison and never written back, or a satisfiability search over the two formulas' difference, decides what the table decides and in practice far past eight atoms. [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) rejects a diagram as a normal form, which respells, and notes that at the comparison it is the table under the same cap. A diagram's size follows the order of its variables, and atoms are ranked by a structural hash, so a price on a diagram's nodes would move a verdict with the order binders are declared in: either the order is one no declaration order moves, or the price counts what no order moves, as the `2ⁿ` evaluations `Formula::evaluation_size` states for the table do.
- **The answer stays equality or nothing.** Whatever decides past the table answers as `Formula::agree` does, since a disagreement over independent opaque atoms is no counterexample.

## Refinement

The next step is the bounded choice of a comparison procedure and its deterministic price, checked against the transitivity and substitution examples above. This violation of conversion's contract precedes atom-ranking performance work. It waits on neither the broader Boolean and bitwise normal-form capability nor the relational layer; neither is part of its completion check.

- **The design.** Which procedure decides a pair past eight atoms, what it is priced by and whether the table stays the procedure below some size; the price stated as a function of the two formulas and charged before the work, as `evaluation_size` is.
- **The decisions it moves.** [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)'s agreement "up to eight atoms" and the `boolean` module's "The cap is the theory's" restated for what replaces the cap, and the `not compared` line restated or retired with it.
- **Check**: both examples above close by `Eq/refl()` in both checkers; the audit gains substitution rows that carry a law's atoms past eight and chains whose pair passes eight; and `tests::runaways` states what a comparison over many atoms spends.
