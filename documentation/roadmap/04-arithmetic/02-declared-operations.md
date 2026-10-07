# Declared operations for `min` and `max`, `abs` and `sign`, powers, and the float identities

Working specification for what conversion decides about the operations the numeric library reasons over, each declared with the theory it is decided in and held by the grid generated from `curios-algebra`'s law table. Its lemma counterpart is [the numeric laws](03-numeric-laws.md), their consumer beside [`/std/Rat`](04-exact-rationals.md) and the proofs of [the elementary functions](06-flt-elementary-functions.md).

It builds on the algebra's law families and generated grid, and is independently implementable and retirable. Each stage lands alone.

## What this builds on

- **The algebra's declarations and grid.** `Intrinsic::algebra` names every operation's algebraic role; the grid instantiates each declared family's laws at every carrier it covers and holds each instance through both checkers, through the kernel asked alone, and against the carrier's semantics — for `Flt`, the binary64 model over the grid's bit patterns; the audit holds each law reversed, chained, at compound terms and through a solved metavariable.
- **The kinds already implemented**:
  - the semilattice on a leaf set, `peel_bool`, deciding `&&` and `||` up to commutation, association and repetition, and `peel_lattice`, deciding `min` and `max` the same way over leaves that carry a literal offset;
  - the lattice of a total order, `Lattice`, a `min` at most each leaf and at least what is at most all of them and a `max` the mirror, read over the plain comparison's verdicts;
  - domination, `Operation::dominators`, deciding `x - y <= x` by the operand a result never exceeds, and `Operation::upper_bound`, a literal bound;
  - symmetric operands, `peel_symmetric`, and the pairing of a commutative operation's operands, `peel_commutative`, both keyed on an operation and its carrier;
  - a semiring embedding, `Nat/to_int`, pushed through a sum and a product by `int_of_nat` and read back by `int_preimage`;
  - inversion pairs, a conversion reducing back through the one that inverts it (`round_trip`);
  - a fold into another operation: `Int`'s difference into the sum's normal form, a shift by a literal count into a coefficient.
- **A shift by a symbolic count is a product by powers.** `then_power` reads `shl(v, k)` as `v` times one atom `shl(1, m)` for each monomial `m` of the count, repeated by the monomial's literal coefficient, the count's literal floor peeled into the coefficient, at `Nat` and at `Int`, whose atom is `Nat`'s widened. A power is at least one (`Operation::lower_bound`), which the comparison reads beside a side's floor.
- **[A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md).** A pair's atoms are classed by each checker's own conversion and a stuck fold is taken again over them (`curios-core`'s `classed`), so a fold that pairs atoms by identity registers its nodes there.
- **The level algebra.** `Level` (`curios-core`'s `universe`) is a maximum of a constant and atoms with offsets: duplicates coalesce at the greatest offset, a constant an offset reaches is dropped, and a successor distributes. It is the normal form `max` takes here, and `min` its mirror.
- **The binary64 model.** Every one of the 2⁶⁴ bit patterns is a distinct value under one symmetric NaN rule ([`Flt` is specified by a model](../../design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md)), which is what makes commutativity hold of the carrier rather than of numbers alone.
- **The routing for new arithmetic.** `.claude/rules/checking.md`'s routing for a numeric carrier's arithmetic, which every promotion below takes whole.
- **[The procedure that proves a bound from the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)**, which reads a truncated subtraction through the case split its definition makes and reads each promoted operation the same way.

## The gap

`abs` and `sign` at `Int` are library functions, so conversion sees their unfolded matches: `abs(+0 - a)` against `abs(a)` and `sign(+0 - a)` against `+0 - sign(a)` are refused. `pow` is a library recursion by squaring, so `pow(x, y + 1)` against `pow(x, y) * x` is refused, and `shl(1, k)` against `pow(2, k)`.

What the library definitions decide today is kept: `abs(Nat/to_int(n))` is `n`, `abs(+0 - Nat/to_int(n) - 1)` is `n + 1`, `sign(Nat/to_int(n) + 1)` is `+1`, and `pow(x + 1, 2)` is `x * x + 2 * x + 1`.

## An operation is declared with the theory it is decided in

Conversion decides a theory or it does not decide it: a law is held where its decision is a normal form or a pairing that depends on its operands' values alone, so that two spellings of one operand give one verdict, a law holds at every term put for its variables, and two laws chain. A law that reads an operand's shape fails that wherever a fold has already changed the shape: the product fold distributes past a single monomial, so a rule matching a product inside `abs` meets a sum. Each operation below is therefore declared with the largest theory that has such a decision, and a true law outside it is a lemma in [the numeric laws](03-numeric-laws.md), with a refused row recording why conversion does not take it.

**A promoted operation computes from its operands alone.** Its fold asks the plain comparison of its operands (`compare_nat`, `compare_int`) and reads no arm's equation, so what it reduces to is the same in every arm, and commutation, association and repetition hold there as they do outside. Under a guard, `min(a, b)` stays `min(a, b)`; a `/std` lemma takes a hypothesis to the equation, and the procedure that proves a bound reads the operation through its case split.

**The comparison's order is transitive.** A `min` keeps the operands no other is at most, so two associations of one `min` agree only where `a <= b` and `b <= c` decided give `a <= c` decided. `curios-core`'s `the_order_nat_decides_is_transitive` and its `Int` twin hold the plain comparison to that over a roster of shapes, which each promoted operation joins.

## `min` and `max`

`/sys` operations at `Nat` and at `Int`, with nothing of this section left to land. Declared a semilattice with successor, dominated by its operands, the theory Agda decides of its universe levels ([universe levels](https://agda.readthedocs.io/en/latest/language/universe-levels.html)):

- commutative, associative and idempotent, decided as a set of leaves, as `&&` is;
- a literal added to every operand leaves, `min(a + 1, b + 1) = min(a, b) + 1`, so the least floor stands outside as a sum's does;
- an operand the comparison holds at most another answers for both: `min(x, x + y) = x`, `max(x, 0) = x`, `min(x % 8, 8) = x % 8`;
- `min(a, b)` is at most each operand and `max(a, b)` at least each, the domination `compare_nat` reads and its mirror, which is new; and a term is at most a `min` where it is at most both operands, at least a `max` where it is at least both.

The fold folds literals, hoists the least floor, flattens the operands that are the same operation, drops each leaf the comparison holds another at most, and rebuilds the node only where it dropped one. Two stuck nodes of one operation are one value where their leaf sets are, read by a peel that respells nothing.

Lemmas: `min(a, b) + max(a, b) = a + b`; `(a - b) + b = max(a, b)`; and a symbolic summand shared by every operand, `min(a + c, b + c) = min(a, b) + c`, since deciding the equations of idempotent semifields, of which the integers under `max` and `+` are one, is co-NP-complete ([Equational theories of idempotent semifields](https://arxiv.org/abs/2402.09876)).

## `abs` and `sign`

Declared at `Int`, `abs` answering a `Nat` and `sign` an `Int` in `{-1, 0, +1}`:

- a negation leaves `abs` and flips `sign`: `abs(+0 - n) = abs(n)`, `sign(+0 - n) = +0 - sign(n)`;
- a literal factor leaves, by its magnitude and its sign, and so does a power, which no substitution turns into a sum: `abs(8 * n) = 8 * abs(n)`, `sign(shl(n, k)) = sign(n)`, `abs(shl(n, k)) = shl(abs(n), k)`;
- `abs` inverts the embedding: `abs(Nat/to_int(m)) = m`, and the `abs` of a non-negative combination of widened naturals, or of one negated, is its preimage; the `sign` of one the comparison holds positive is `+1`;
- `Nat/to_int(abs(n))` is `n` where the comparison decides `0 <= n`, and `+0 - n` where it decides `n <= 0`;
- two `abs` are one value where their operands are equal or opposite, `abs(a - b)` against `abs(b - a)`.

The operand is normalized before it is read, so one value has one reading.

Lemmas: `abs(a * b) = abs(a) * abs(b)` and `sign(a * b) = sign(a) * sign(b)`, which read a product a fold has distributed, `abs((l + 1) * j)` reaching `abs` as `l * j + j`, so deciding them takes factoring a polynomial; `NonNeg(n)` giving `Eq()(Nat/to_int(abs(n)), n)`, and its twin below zero, which `/std/Map`'s `Key(Int)` consumes; and the order facts, the triangle inequality and the decomposition [the numeric laws](03-numeric-laws.md) list.

## Powers

`pow` is promoted at `Nat` and declared a morphism from `(ℕ, +)` into `(ℕ, ·)` at a fixed base:

- `pow(x, 0) = 1` and `pow(x, 1) = x`;
- a sum in the exponent is a product, `pow(x, a + b) = pow(x, a) * pow(x, b)`, so a literal floor is so many factors, `pow(x, a + 2) = x * x * pow(x, a)`, and a literal coefficient repeats a factor, `pow(x, 2 * a) = pow(x, a) * pow(x, a)`;
- a closed power folds, `pow(2, 10) = 1024`, and `pow(1, a) = 1`;
- a power of a positive base is positive: `0 < pow(2, k)`, and `0 < shl(x + 1, k)`.

The normal form is the one Mathlib's `ring` keeps for a variable exponent ([`Mathlib.Tactic.Ring`](https://florisvandoorn.com/LeanCourse25/docs/Mathlib/Tactic/Ring/Basic.html)): the exponent is normalized, and a monomial of it under a base is an atom of the product, the base an atom up to conversion. Each factor is charged before it is built, so a literal exponent no budget covers is refused.

The left shift is read through it, `shl(x, k) = x * pow(2, k)`, so `shl(1, k)` and `pow(2, k)` are one atom and `shl(shl(x, a), b) = shl(x, a + b)` holds at a symbolic count. At `Int` the atom is the widened power, so a comparison over it pulls back to `Nat` through `int_preimage`. The exponent law at base two and its positivity are held already, over the atom `shl(1, k)`, which the promotion respells as `pow(2, k)`.

Lemmas: `pow(pow(x, a), b) = pow(x, a * b)` and `pow(x * y, a) = pow(x, a) * pow(y, a)`. Beside `pow(x, 2) = x * x`, the first gives the second by one chain, and the second at two literals is `pow(6, a)` against `pow(2, a) * pow(3, a)`, a base to factor; no finite set of identities is complete for exponentiation over the naturals ([Tarski's high school algebra problem](https://en.wikipedia.org/wiki/Tarski%27s_high_school_algebra_problem), Wilkie 1980 and Gurevič 1988). `shr(shl(x, k), k) = x` stays a lemma.

## The families `Int` shares with `Nat`

`Int` divides truncating toward zero and shifts right arithmetically, so some of what `Nat` declares is no law at `Int`. Declared at `Int`, each held by its declaration or its written row, with nothing of this section left to land:

- the bitwise identities: `and`, `or` and `xor` commute; `and` and `or` are idempotent and `xor` cancels itself; `0` absorbs `and` and is the unit of `or` and `xor`; `-1` is the unit of `and` and absorbs `or`;
- a shift by zero is the value and a shifted zero is zero, at both shifts;
- `i / +1 = i`, `i % +1 = +0`, a zero dividend, a dividend that is its divisor, and an exact multiple whatever the signs of its coefficients and its divisor, `(i * -6) / 2 = i * -3` and `(i * 2) % 2 = +0`, the quotient of an exact multiple being the same whichever way a division rounds;
- the product's associativity.

Controls, refused with their counterexamples, each at `i = -1`: `(i * 2 + 1) % 2 = +1`, where the remainder is `-1`; the floor law, `(i + 2) / 2` against `1 + i / 2`, which are `0` and `1`; and the Euclidean split, `(2 * i + 1) / 2` against `i`, which are `0` and `-1`. And every domination `Nat` reads — `i - j <= i`, `and(i, j) <= i`, `shr(i, n) <= i`, `i % d < d`, `i / d <= i` — each false at an operand below zero.

## `Flt`

Held of every bit pattern under the model, NaNs included, each through its declaration and held to the model over the grid's patterns ([`Flt` is specified by a model](../../design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md)): an operation's rounding direction is part of what it is; `add`, `mul`, `min` and `max` commute, and `fma`'s two factors; a difference is the sum with its subtrahend negated; a rounding to an integral value is fixed by every direction; the byte conversions undo each other both ways; and a composition of `neg`, `abs` and `copysign` is written one way, a magnitude under a sign bit that is clear, set, another value's, or that flipped, so `neg(neg(x))` is `x`, `abs(neg(x))` is `abs(x)` and `neg(copysign(x, y))` is `copysign(x, neg(y))`. No `Flt` declaration inherits a ring or order law, and nothing of the float half is left to land.

What is not a law stays a control, refused with its counterexample: associativity of `add` and `mul`, and distributivity, by rounding; `f + 0.0 = f`, false at `-0.0`; `f * 1.0 = f`, since a signaling NaN is quieted; `f - f = 0.0`, at an infinity or a NaN; `f * 0.0 = 0.0`, at an infinity, a NaN, or a negative `f`; `f == f`, at a NaN; `lt(a, b) = not(ge(a, b))`, with a NaN on either side; a sum in one direction against the swapped sum in another; `fma(a, b, c)` against `fma(a, c, b)`; and `min(a, a) = a`, since a signaling NaN is quieted. The associativity of `min` and `max` holds of the model and is a refused candidate, a law over a multiset of leaves that nothing consumes.

## Promotion

`min` and `max` at both carriers are `/sys` operations, and `abs`, `sign` and `pow` become ones. Each is new arithmetic on a numeric carrier and takes everything `.claude/rules/checking.md`'s routing names for one: its `/sys` row and `Intrinsic::signature` entry, which the prelude build checks against each other; its literal fold in every constant folder sharing `scalar` — `curios-core`'s, `curios-ersd`'s and `curios-cont`'s; the emitter's lowering with its i31 fast path and its `big_emitter` library; its declaration; its nodes in `classed`; and the procedure's reading of its definition. Each folds as it executes, at both signs and past the i31, since `Nat` and `Int` are unbounded at every layer ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)). The library function a promotion replaces becomes a re-export under its existing name, and the rows it held by reduction are stated before it lands and hold after.

## Stages

Every law above, every row the library definitions hold today and every control stands in the grid (`curios`'s `tests::laws::written`), each refused row commented with the stage that moves it, and the audit puts each declared law to a product of two sums.

1. **`abs` and `sign`**, with the two lemmas `/std/Map` consumes.
2. **`pow`**, the power atom becoming `pow(2, k)`.

Each stage moves its rows from refused to held, extends the grid's audit to the kind it declares, and adds its operation's rows to `curios`'s `tests::bounds`.

## Verification

- Each declared law is a generated row, or a written one where no family states it, held at every carrier and direction its declaration covers through both checkers and the kernel asked alone; each control is refused; one mutation per new kind is run and caught.
- Each promoted operation folds as it executes and agrees with `curios-num` over a generated grid, at both signs, past the i31 and past 64 bits, in all three constant folders and in the running program.
- Every row the library definitions hold before a promotion holds after it.
- The plain comparison is transitive over a roster of shapes: literals, a symbol, a sum, a difference, a quotient, a remainder, a bitwise `and`, a byte's value, and each promoted operation as it lands.
- `Flt` declarations are held to the model over the pattern grid.
- `tests::bounds` fills a bound over each promoted operation through its definition, and a fixture states that `min(a, b)` under `match a <= b` is `a` by the lemma and not by reflexivity.

## Documentation and design record

- [A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) gains the numeric declarations, the domination mirror and the rows moved from refused to held; [`Flt` is specified by a model](../../design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md) gains the float declarations.
- [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) gains that an operation is declared with the theory it is decided in, and that a declared operation reads no arm.
- `/std/Nat`, `/std/Int` and `/std/Flt` document the promoted operations and what conversion decides about them.

## Rejected

- **The high-school identities whole.** Mathlib's `ring` takes a power through a product of bases and GHC's `ghc-typelits-natnormalise` collapses a power of a power, a tactic and a solver whose miss is a goal left open; in conversion a miss is a law that fails at a term put for its variable.
- **`abs` and `sign` as homomorphisms of the product.** Decided where the product still stands and refused where a fold has distributed it, so a verdict would follow a spelling.
- **Dropping a `min`'s operand on whatever the comparison decides, with no transitivity held.** Two associations of one `min` then differ, the inner fold having dropped the operand that ordered the other two.
- **A fold that reads its arm.** `min(a, b)` would be `a` under `match a <= b`, as the library definition is, and two associations of one `min` would differ inside an arm, an equation answering no consequence of itself.
- **A sum with `min` and `max` decided whole**, for the cost the lemma above cites.
- **Float subtraction built as the sum at its `/sys` row.** The difference would leave every stage below Core for a peephole to restore, and a report would print a written `a - b` as a sum.
- **Presburger definitions read by a relational layer.** It is [the relational layer](08-relational-layer.md)'s, conditional on a consumer.
- **Ring or order laws for `Flt`.** Each non-law above is refused for a counterexample under the model.
- **Promoting `gcd`.** Its certified form recurses on well-founded `<` in the library, which is [the numeric laws](03-numeric-laws.md)'; nothing in conversion needs it.

## Completion criteria

- `min`, `max`, `abs`, `sign` and `pow` are `/sys` operations whose declarations hold the laws above by conversion, folding as they execute and reading no arm.
- Every decided `Flt` law above is held by its declaration, with its controls refused.
- Every family above that is a law at `Int` is declared there, with the non-laws refused as controls.
- The new kinds — the exponent morphism, the semilattice with successor and its domination mirror, the magnitude and the sign, and the float's sign and magnitude — have their algorithms in `curios-algebra`, their mutations caught and their audit recorded.

## Retirement

Move the declarations' contracts to `curios-algebra`'s and Core's documentation and to the design decisions above, and the operations' contracts to the `/std` modules. Replace the roadmap entry with a checked summary, update [the numeric laws](03-numeric-laws.md) and [exact rationals](04-exact-rationals.md) to the permanent record, verify that nothing references this filename, and delete it.
