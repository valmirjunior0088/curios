# What conversion still decides by spelling or by cap

**Not refined yet.** This specification preserves five capabilities of the carriers' algebra that no consumer has asked for, with the approaches already discussed, and the rows the law grid still refuses, as [the relational layer](08-relational-layer.md) reserves its own. Each opens when a consumer needs what it decides, and refinement establishes its fragment, algorithm contract and acceptance criteria before any implementation. It is not an implementation plan.

## Canonical forms and refinement keys

**Capability wanted.** Algebraically equivalent expressions have canonical forms in the chosen fragments, and refinement lookup uses keys that do not depend on incidental term spelling. Today a case equation is recorded under a few spellings of its scrutinee and found by matching them ([Case equations and their key](../../design/soundness/elimination/case-equations-and-their-key.md)).

The two checkers do not look for an equation in the same places, and which way they part turns on how a guard is spelled. The elaborator escalates a key it missed to its arguments reduced, which the kernel does not, so under `let n = m + 0; match Nat/in_range(n, 240, 244)` the elaborator accepts `Nat/le/of_in_range(m, 240, 244, True/qed())` and the kernel refuses it; [Case equations and their key](../../design/soundness/elimination/case-equations-and-their-key.md) records that direction as admitting and closed at certification. The other refuses: the kernel records a guard over local definitions with them substituted, while the elaborator settles only the spelling that names them, so under `let n = Byte/to_nat(c); match Nat/in_range(n, 0, 0x7F)` the kernel's reduct answers `Byte/to_nat(c) <= 0x7F` and the elaborator's does not. The procedure that proves a bound from the facts in scope met both, and writes its proofs over the spellings the keys hold ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)); an author writing the same terms still meets them. Settling reduced spellings is also a measured cost of elaboration, and [A shared term costs its size](../05-compilation/01-shared-term-costs.md)'s settlement stage keeps a settled spelling for as long as the frames it rests on stand.

Conversion's readers key atoms on spelling too — cancellation, a monomial's factors, a connective's leaves, the truth table, the linear views. A pair they decide nothing about is read once more with every atom's arguments forced and the sums and products inside them ordered ([Intrinsic fold laws and the free-monoid peel](../../design/soundness/conversion/intrinsic-fold-laws-and-the-free-monoid-peel.md)), which brings two atoms equal up to reducing their arguments and commuting inside them to one spelling. Past that, two convertible atoms are still two: an atom that is no application keeps its insides as written, so two stuck `match`es whose branches commute a sum never pair, and what the chain falls back to is the positional congruence, whose operand order is a structural hash — such a verdict still turns on the order the binders were declared in. `tests::laws`'s `written` grid records it as a refused row, stated at two binder orders.

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

**Still to refine.** The fragment, the treatment of natural coefficients and integer signs, opaque atoms, and where a proposal ends and a search belongs to [the elaborator's search over the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) instead.

## A reflection law for `Bytes/eql`

`a == b` giving `Eq(a, b)` holds as a lemma for `Nat`, `Byte` and `Int` (`Int/eq_of_eql`). `Bytes/eql` does not reduce over a list, so its reflection would be a new intrinsic law, declared as [the declared operations](02-declared-operations.md) are. Stepping confirms a match without one, so it waits for a consumer that needs the equation rather than the step.

## One internal sequence carrier

**Capability wanted.** One internal sequence carrier and common operations, preserving `List`, `Bits` and `Bytes` for the guest.

**Previously discussed.** A carrier and kind pair, with representation changes through the erased and continuation IRs, emission and stored archives. [The carriers' algebra](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) shares one word algebra, which deliberately works over today's three representations.

**Still to refine.** The representation, carrier and element typing, packed storage, proof operands, intrinsic signatures, stage boundaries, archive compatibility and performance, and whether a reasoning capability needs it or it can proceed on its own.

## Rows the law grid refuses

`tests::laws::written` holds rows no rule decides yet, each a candidate for a family of its own. Parity read as a clash: the gcd test decides `x * 2 + 1 == y * 2` false as a fold, and read by the `Nat` peel as `Impossible` it would let `match h end` close `Eq()(x * 2 + 1, y * 2)`, so it needs its own row and [Coverage](../../design/soundness/elimination/coverage.md)'s evidence before inversion may rely on it. Map fusion, `map(map(xs, f), g) = map(xs, (x) => g(f(x)))`. A shift by a symbolic count's exponent law, and a literal-count right shift's quotient. Boolean agreement past the truth table's cap, above.
