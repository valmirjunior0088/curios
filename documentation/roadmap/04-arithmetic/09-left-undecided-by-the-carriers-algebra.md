# What the carriers' algebra leaves undecided

**Not refined yet.** This specification preserves what the carriers' algebra does not decide and no consumer has asked for, with the approaches already discussed, and the rows the law grid still refuses, as [the relational layer](08-relational-layer.md) reserves its own. Each opens when a consumer needs what it decides, and refinement establishes its fragment, algorithm contract and acceptance criteria before any implementation. It is not an implementation plan. What a verdict owes to how a term is spelled is decided, in [A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md); the positions that decision leaves unanswered are the last section here.

## Boolean and bitwise normal forms

**Capability wanted.** Reason beyond today's local identities and the truth table's bounded agreement — two `Bool` terms with a connective between them are put to a truth table over their atoms, a rung of `curios-analysis`'s conversion chain, which declines past `curios-algebra`'s `BOOL_ATOM_CAP` of eight atoms — including relationships among comparison atoms and further natural bitwise laws.

The cap is also a place conversion is not transitive, by design: with `A` the conjunction of `a0 || a1 || a2 || a3 || a4` and the negation of each of its five atoms, and `C` the same over `a5` to `a9`, `A` converts with `false` and `false` with `C`, and `A` does not convert with `C`, their pair holding ten atoms. A reader can count the atoms, and the refusal says the two were not compared.

**Previously discussed.** Reduced ordered decision diagrams for Boolean reasoning, algebraic normal form for natural bitwise operations, and replacing the truth table's fixed cap with a budget the reduction budget prices.

**Still to refine.** Which representations serve which operations, how arithmetic relationships enter the Boolean procedure, and which complexity limits keep work predictable. The cap is a constant of the language on purpose, so the same two formulas are decided on every target (`curios-algebra`'s `boolean` module), and a priced budget has to keep that. Agreement, impossibility and failure to decide need separate contracts; disagreement over independent opaque atoms is not automatically a realizable counterexample.

**To consider, for moving the cap.** Some bound has to stand, since deciding two formulas equivalent is co-NP-complete; what can move is how far a procedure reaches before it stops. A reduced ordered decision diagram built for one comparison and never written back, or a satisfiability search over the two formulas' difference, decides what the table decides and in practice far past eight atoms. [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) rejects a diagram as a normal form, which respells, and notes that at the comparison it is the table under the same cap; the question here is that use under a larger bound. Whatever bounds it has to decide the same two formulas on every target, and a diagram adds one condition the table does not have: its size follows the order of its variables, and atoms are ranked by a structural hash, so a bound on a diagram's nodes would move a verdict with the order binders are declared in. Either the order is one no declaration order moves, or the bound stays on the number of atoms and the diagram is only how the comparison is run.

## Polynomial unification proposals

**Capability wanted.** Broader polynomial and sign reasoning, and solving proposals beyond today's, so unification can propose a solution where a metavariable sits inside a polynomial equation. Today the elaborator solves the one unsolved metavariable an equation over `Nat` or `Int` is linear in, by exact division of the equation's linear view, and puts the quotient to conversion (`curios-elab`'s `convert::linear`): `?w * (y + z)` against `x * z + x * y` gives `x`. It declines, and the equation stays undecided, where two metavariables are unsolved, where one stands at a power — `?w * ?w + ?w * y` against `x * x + x * y`, which `x` solves — and where one stands inside an atom, as an argument of a call.

**Previously discussed.** A ring procedure sharing the appropriate mathematics of `Nat` and `Int`, with its solver on the elaborator's side of the certifier's dependency closure; the elaborator chooses solutions and the certifier checks what reaches it.

**Still to refine.** Whether a metavariable at a power is solved at `Nat`, once a consumer needs it — a square matrix stored flat, `Vec(T, n * n)`, is the likely one. With the unknown on one side only, that side is strictly increasing in it, so the equation has at most one natural solution; the solution is an exact root, which the elaborator would propose and conversion check as it checks a quotient. At `Int` a square has two roots, so the equation stays refused there. Whether several equations are solved together as the linear system they are in their metavariables, which the worklist approximates by retrying a parked equation once another has solved one of its unknowns. The fragment, the treatment of natural coefficients and integer signs, opaque atoms, and where a proposal ends and a search belongs to [the elaborator's search over the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) instead.

## A reflection law for `Bytes/eql`

`a == b` giving `Eq(a, b)` holds as a lemma for `Nat`, `Byte` and `Int` (`Int/eq_of_eql`). `Bytes/eql` does not reduce over a list, so its reflection would be a new intrinsic law, declared as [the declared operations](02-declared-operations.md) are. Stepping confirms a match without one, so it waits for a consumer that needs the equation rather than the step.

## One internal sequence carrier

**Capability wanted.** One internal sequence carrier and common operations, preserving `List`, `Bits` and `Bytes` for the guest.

**Previously discussed.** A carrier and kind pair, with representation changes through the erased and continuation IRs, emission and stored archives. [The carriers' algebra](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) shares one word algebra, which deliberately works over today's three representations.

**Still to refine.** The representation, carrier and element typing, packed storage, proof operands, intrinsic signatures, stage boundaries, archive compatibility and performance, and whether a reasoning capability needs it or it can proceed on its own.

## Rows the law grid refuses

`tests::laws::written` holds rows no rule decides yet, each a candidate for a family of its own. Parity read as a clash: the gcd test decides `x * 2 + 1 == y * 2` false as a fold, and read by the `Nat` peel as `Impossible` it would let `match h end` close `Eq()(x * 2 + 1, y * 2)`, so it needs its own row and [Coverage](../../design/soundness/elimination/coverage.md)'s evidence before inversion may rely on it. Map fusion, `map(map(xs, f), g) = map(xs, (x) => g(f(x)))`. A shift by a symbolic count's exponent law, and a literal-count right shift's quotient. Boolean agreement past the truth table's cap, above.

## Retirement

Each section leaves for a working specification of its own when its consumer arrives; this file is deleted with its roadmap entry once none is left.

## What an arm's equation leaves unanswered

An arm's equation answers the terms conversion holds equal to its scrutinee ([A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md)). Three positions stop short of that, each in the refusing direction, and each waits for a program that needs it.

**A question inside a question.** A question a judgment's reduction puts to conversion is answered by plain reduction, which asks nothing. Under `match f(a + b) < 10` and, in its `true` arm, `match h(f(a + b) < 10) < 5`, the term `h(f(b + a) < 10) < 5` is not `true`: it is the inner guard's scrutinee only to a conversion that asks the outer guard about the respelled argument. A fold is the same: `h(f(a + b) == f(b + a)) == h(true)` does not reduce, which both checkers' `a_fold_inside_a_question_is_not_taken_again` hold. The decision rejects a third reduction until a program needs one.

**A form naming a binder the scrutinee does not.** Which equations a stuck form is put to is a cost filter, `curios-analysis`'s `could_reduce_to`: it keeps a scrutinee's reduced spelling from being settled for every stuck form in its arm. It passes over a binder that is itself a proof and counts every other, one that stands where reduction would erase it included. Under `match f(b) < 10`, `f(b + 0 * c) < 10` converts with the guard outside the arm and is not `true` inside it, the row `curios`'s `tests::respelling` states refused; so is a form whose other binder stands inside a proof term that is no variable.

**An index read by plain reduction.** Index inversion forces an index through `Env::force`, which is plain reduction, so an index that is its arm's scrutinee only up to conversion is not inverted. No cell of the grid reaches it.
