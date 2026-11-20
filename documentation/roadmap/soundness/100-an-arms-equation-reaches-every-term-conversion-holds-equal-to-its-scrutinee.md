# An arm's equation reaches every term conversion holds equal to its scrutinee

[Case equations and their key](../../design/soundness/case-equations-and-their-key.md) has an arm's equation answer the terms conversion holds equal to its scrutinee ([A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md)). Three positions stop short of that, each in the refusing direction, so a program the rule licenses is refused there or proved around it, as `/std/Map`'s `Node/mapped_find` is today. A question inside a question is no fourth: [A question reduction asks is answered by plain reduction](../../design/soundness/a-question-reduction-asks-is-answered-by-plain-reduction.md) rejects the third reduction it would take, for a program nothing has yet written.

## What is missing

**A form naming a binder the scrutinee does not.** Which equations a stuck form is put to is a cost filter, `curios-analysis`'s `could_reduce_to`: it keeps a scrutinee's reduced spelling from being settled for every stuck form in its arm. It passes over a binder that is itself a proof and counts every other, one that stands where reduction would erase it included. Under `match f(b) < 10`, `f(b + 0 * c) < 10` converts with the guard outside the arm and is not `true` inside it, the row `curios`'s `tests::respelling` states refused; so is a form whose other binder stands inside a proof term that is no variable. `/std/Map`'s `Node/mapped_find` is a program that meets it: a leaf's lookup is a comparison reached through `lookup(leaf(k0, v0), k).0`, which names the value `v0` reduction would erase, so the arm on `Hash/hash(k0) == k` answers nothing there, and the proof states its leaf case over the comparison's answer instead.

**An index read by plain reduction.** Index inversion forces an index through `curios-analysis`'s `Driver::force`, which is plain reduction, so an index that is its arm's scrutinee only up to conversion is not inverted. No cell of the grid reaches it.

**A guard under an inner arm on an expression.** A guard's reduced spelling is settled with the equations inside it withheld, and an inner arm restates the guard only where its case solves a variable the guard reads ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)). Under `match match h(p) == h(q) | true => k(0) | false => k(1) end`, with `p, q : F(g(c))` and `F(b) = match b | true => {} | false => Nat end`, the inner arm `match g(c) | true => …` makes `p` and `q` units and the guard's scrutinee `k(0)`, and `Holds(k(0))` is not `True` there: the guard's spelling, settled without `g(c) = true`, is the stuck match, and the scrutinee as written is answered `true` before it is reduced, so conversion never meets `k(0)`. Split on `c` over `p, q : F(c)`, the same arm restates the guard and settles it under the solution, and the certifier holds `Holds(k(0))` `True` there. Both checkers refuse the arm on the expression alike at `910ab8f34`, and no program was found that needs it.

## Stages

Each position is taken alone, as a stage of its own, refined when it is taken: what the equation answers there, the rule both checkers read for it in the shared layer, so the two agree, and what it costs every stuck form an arm meets, the reason each stops where it does.

1. **A form naming a binder the scrutinee does not**, first, since `/std` meets it. Check: `Node/mapped_find`'s leaf case written over the arm's equation rather than over the comparison's answer, elaborated and certified; the row `curios`'s `tests::respelling` states refused restated as accepted; and the refusals around it, which conversion does not hold equal to the scrutinee, still refused in both checkers.
2. **An index read by plain reduction.** Check: a family whose index is its arm's scrutinee only up to conversion inverted in both checkers, and the refusals around it unchanged.
3. **A guard under an inner arm on an expression.** Check: `Holds(k(0))` `True` under the inner arm on `g(c)` in both checkers, and the refusals around it unchanged.

## Next step

Stage 1, refined against `Node/mapped_find`.

## Retirement

As a position lands, its rule goes to [Case equations and their key](../../design/soundness/case-equations-and-their-key.md) and it is removed here; one declined for good goes to that decision's **Rejected** paragraph with its reason. Once no position remains and nothing cites this file, it is deleted.
