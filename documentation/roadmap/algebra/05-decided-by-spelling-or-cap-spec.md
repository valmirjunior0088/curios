# Algebra, part 5: what conversion still decides by spelling or by cap

**Not refined yet.** This specification preserves five capabilities of the carriers' algebra that no consumer has asked for, with the approaches already discussed. They were topics of the placeholder this campaign replaced when it was sequenced into parts 0–4, and [part 4](04-relational-layer-spec.md) reserves the relational layer the same way. Each opens when a consumer needs what it decides, and refinement establishes its fragment, algorithm contract and acceptance criteria before any implementation. It is not an implementation plan.

## Canonical forms and refinement keys

**Capability wanted.** Algebraically equivalent expressions have canonical forms in the chosen fragments, and refinement lookup uses keys that do not depend on incidental term spelling. Today a case equation is recorded under a few spellings of its scrutinee and found by matching them ([Case equations inside an arm](../../soundness/what-the-kernel-consults/case-equations-inside-an-arm.md)).

Conversion's readers key atoms on spelling too — cancellation, a monomial's factors, a connective's leaves, the truth table, the linear views. A pair they decide nothing about is read once more with every atom's arguments forced and the sums and products inside them ordered ([Intrinsic fold laws and the free-monoid peel](../../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md)), which brings two atoms equal up to reducing their arguments and commuting inside them to one spelling. Past that, two convertible atoms are still two: an atom that is no application keeps its insides as written, so two stuck `match`es whose branches commute a sum never pair, and what the chain falls back to is the positional congruence, whose operand order is a structural hash — such a verdict still turns on the order the binders were declared in. `tests::laws`'s `written` grid records it as a refused row, stated at two binder orders.

**Previously discussed.** A total atom order, complete normalization within each supported fragment, canonical key construction and caching, and retirement of the spelling probes the keys replace. Identity stays separate from presentation order; hashes alone cannot establish equality.

For the atoms conversion's readers pair, three directions past forcing. Atoms identified up to definitional equality, as Mathlib's `ring_nf` keeps them through `AtomM`, testing each new atom against the table with `isDefEq` — which its own documentation warns can become very expensive, and which the shared chain cannot do without calling a judgment it leaves to each checker. Congruence closure over alien subterms, as Nelson–Oppen purification does and as CoqMT builds a decidable theory into conversion with confluence, normalization and decidability proved. And an order-independent fallback: pairing the atoms left over by rigid head where the pairing is unique, before the positional congruence, which removes the hash from the verdict for distinct heads but changes what the fallback accepts, since two heads may still convert once unfolded and a metavariable has none.

**Still to refine.** The exact fragments, atom relations, placement of normalization, cache lifetime and invalidation, and the consequences for substitution, universes, proof irrelevance, sharing and cost — and, for the atoms, whether identity moves past a normal form at all, what an order-independent fallback may commit a metavariable to, and how either is charged. Canonical comparison views do not by themselves justify changing reduction's spelling; the existing spelling decisions remain in force until a measured replacement lands.

## Boolean and bitwise normal forms

**Capability wanted.** Reason beyond today's local identities and the truth table's bounded agreement — two `Bool` terms with a connective between them are put to a truth table over their atoms, a rung of `curios-analysis`'s conversion chain, which declines past `curios-algebra`'s `BOOL_ATOM_CAP` of eight atoms — including relationships among comparison atoms and further natural bitwise laws.

**Previously discussed.** Reduced ordered decision diagrams for Boolean reasoning, algebraic normal form for natural bitwise operations, and replacing the truth table's fixed cap with a budget the reduction budget prices.

**Still to refine.** Which representations serve which operations, how arithmetic relationships enter the Boolean procedure, and which complexity limits keep work predictable. The cap is a constant of the language on purpose, so the same two formulas are decided on every target (`curios-algebra`'s `boolean` module), and a priced budget has to keep that. Agreement, impossibility and failure to decide need separate contracts; disagreement over independent opaque atoms is not automatically a realizable counterexample.

## Polynomial unification proposals

**Capability wanted.** Broader polynomial and sign reasoning, and solving proposals beyond today's, so unification can propose a solution where a metavariable sits inside a polynomial equation.

**Previously discussed.** A ring procedure sharing the appropriate mathematics of `Nat` and `Int`, with its solver on the elaborator's side of the certifier's dependency closure; the elaborator chooses solutions and the certifier checks what reaches it.

**Still to refine.** The fragment, the treatment of natural coefficients and integer signs, opaque atoms, and where a proposal ends and a search belongs to [part 2](02-bounds-from-facts-spec.md)'s engine instead.

## A reflection law for `Bytes/eql`

`a == b` giving `Eq(a, b)` holds as a lemma for `Nat`, `Byte` and `Int` (`Int/eq_of_eql`). `Bytes/eql` does not reduce over a list, so its reflection would be a new intrinsic law, declared as [part 3](03-declared-operations-spec.md) declares its operations. Stepping confirms a match without one, so it waits for a consumer that needs the equation rather than the step.

## One internal sequence carrier

**Capability wanted.** One internal sequence carrier and common operations, preserving `List`, `Bits` and `Bytes` for the guest.

**Previously discussed.** A carrier and kind pair, with representation changes through the erased and continuation IRs, emission and stored archives. [Part 1](../../design/toolchain/one-crate-owns-the-carriers-algebra-and-the-checkers-share-its-strategy.md)'s shared word algebra deliberately works over today's three representations.

**Still to refine.** The representation, carrier and element typing, packed storage, proof operands, intrinsic signatures, stage boundaries, archive compatibility and performance, and whether a reasoning capability needs it or it can proceed on its own.
