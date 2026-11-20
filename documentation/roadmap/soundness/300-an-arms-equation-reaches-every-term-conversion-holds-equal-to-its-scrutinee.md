# An arm's equation reaches every term conversion holds equal to its scrutinee

**Not refined yet.** This specification reserves four positions where an arm's equation answers less than conversion holds equal to its scrutinee, each in the refusing direction. It is not an implementation plan: each waits for a program that needs it, and is refined as a delivery of its own when one does.

## What is missing

[Case equations and their key](../../design/soundness/case-equations-and-their-key.md) has an arm's equation answer the terms conversion holds equal to its scrutinee ([A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md)). Four positions stop short of that.

**A question inside a question.** A question a judgment's reduction puts to conversion is answered by plain reduction, which asks nothing ([A question reduction asks is answered by plain reduction](../../design/soundness/a-question-reduction-asks-is-answered-by-plain-reduction.md)). Under `match f(a + b) < 10` and, in its `true` arm, `match h(f(a + b) < 10) < 5`, the term `h(f(b + a) < 10) < 5` is not `true`: it is the inner guard's scrutinee only to a conversion that asks the outer guard about the respelled argument. A fold is the same: `h(f(a + b) == f(b + a)) == h(true)` does not reduce, which both checkers' `a_fold_inside_a_question_is_not_taken_again` hold. The decision rejects a third reduction until a program needs one.

**A form naming a binder the scrutinee does not.** Which equations a stuck form is put to is a cost filter, `curios-analysis`'s `could_reduce_to`: it keeps a scrutinee's reduced spelling from being settled for every stuck form in its arm. It passes over a binder that is itself a proof and counts every other, one that stands where reduction would erase it included. Under `match f(b) < 10`, `f(b + 0 * c) < 10` converts with the guard outside the arm and is not `true` inside it, the row `curios`'s `tests::respelling` states refused; so is a form whose other binder stands inside a proof term that is no variable. `/std/Map`'s `Node/mapped_find` is a program that meets it: a leaf's lookup is a comparison reached through `lookup(leaf(k0, v0), k).0`, which names the value `v0` reduction would erase, so the arm on `Hash/hash(k0) == k` answers nothing there, and the proof states its leaf case over the comparison's answer instead.

**An index read by plain reduction.** Index inversion forces an index through `curios-analysis`'s `Driver::force`, which is plain reduction, so an index that is its arm's scrutinee only up to conversion is not inverted. No cell of the grid reaches it.

**A guard under an inner arm on an expression.** A guard's reduced spelling is settled with the equations inside it withheld, and an inner arm restates the guard only where its case solves a variable the guard reads ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)). Under `match match h(p) == h(q) | true => k(0) | false => k(1) end`, with `p, q : F(g(c))` and `F(b) = match b | true => {} | false => Nat end`, the inner arm `match g(c) | true => …` makes `p` and `q` units and the guard's scrutinee `k(0)`, and `Holds(k(0))` is not `True` there: the guard's spelling, settled without `g(c) = true`, is the stuck match, and the scrutinee as written is answered `true` before it is reduced, so conversion never meets `k(0)`. Split on `c` over `p, q : F(c)`, the same arm restates the guard and settles it under the solution, and the certifier holds `Holds(k(0))` `True` there. Both checkers refuse the arm on the expression alike at `910ab8f34`, and no program was found that needs it.

## Refinement

Each position is refined alone, by the program that needs it: what the equation would answer there, the rule both checkers read for it in the shared layer, so the two agree, and what it costs every stuck form an arm meets, the reason each stops where it does. A position no program needs stays reserved here.

- **Check**, for each: the program that asked elaborates and certifies; the row that states the position refused is restated as accepted; and the refusals around it, which conversion does not hold equal to the scrutinee, stay refused in both checkers.

## Retirement

As a position lands, its rule goes to [Case equations and their key](../../design/soundness/case-equations-and-their-key.md) and it is removed here; one declined for good goes to that decision's **Rejected** paragraph with its reason. Once no position remains and nothing cites this file, it is deleted.
