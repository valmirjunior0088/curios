# Algebra, part 3: declared operations for `pow`, `min` and `max`, `abs` and `sign`, and the float identities

Working specification for the operations and laws the numeric library needs conversion to decide, each a declaration of a kind [part 1](../../design/toolchain/one-crate-owns-the-carriers-algebra-and-the-checkers-share-its-strategy.md) implements or this part adds, held by the grid generated from `curios-algebra`'s law table. It takes the declaration halves of what were the `Nat`, `Int` and `Flt` laws specifications; their lemma halves are [the numeric laws](../numeric-laws-spec.md)', which is this part's consumer beside [Rat part 1](../rat/01-exact-rationals-spec.md) and the proofs of [the elementary functions](../flt/02-elementary-functions-spec.md).

It builds on the law families and the generated grid part 1 delivered, and is independently implementable and retirable. Each operation's stage lands alone.

## What this builds on

- **Part 1's declarations and grid.** `Intrinsic::algebra` names every operation's algebraic role; the grid instantiates each declaration kind's laws at every carrier it covers and holds each instance through both checkers and against the carrier's semantics — for `Flt`, the binary64 model over part 1's pattern grid; the audit covers the kinds that exist, and each kind added here extends it.
- **The kinds already implemented**, each with an operation to carry here:
  - the semilattice on a leaf set, `peel_bool`, deciding `&&` and `||` up to commutation, association and repetition;
  - domination, `nat_dominators`, deciding `x - y <= x` by the operand a result never exceeds ([The bounds oracle and the division family](../../soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md));
  - symmetric operands, `peel_symmetric`, deciding a swapped `==` or bitwise operation equal;
  - homomorphisms, `len` over concatenation and `Nat/to_int` as an ordered-semiring embedding ([Open fold laws and the sum normal form](../../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md));
  - inversion pairs, a conversion reducing back through the one that inverts it;
  - idempotence, among the bitwise laws;
  - a derived operation, which is its definition.
- **The binary64 model.** Every one of the 2⁶⁴ bit patterns is a distinct value under one symmetric NaN rule ([The binary64 model and its NaN rule](../../soundness/per-term-rules/the-binary64-model-and-its-nan-rule.md)), which is what makes commutativity hold of the carrier rather than of numbers alone.
- **The routing for new arithmetic.** `CLAUDE.md`'s change routing for a numeric carrier's arithmetic, which every promotion below takes whole.
- **[Part 2](02-bounds-from-facts-spec.md)**, which reads each promoted operation's definition by a case split, so a bound over one is proved from the facts in scope.

## The gap

`min` and `max` at both carriers, and `abs` and `sign` at `Int`, are library functions, so conversion sees only their unfolded matches: `min(a, b)` against `min(b, a)`, `abs(+0 - a)` against `abs(a)`, and `abs(a * b)` against `abs(a) * abs(b)` are all refused. `pow` is no operation, so the binary scales' laws — `shl(x, k) = x · pow(2, k)` and the exponent laws — cannot be declared. `Flt`'s laws that hold for every bit pattern under the model — commutativity, the sign operations, subtraction as negated addition, idempotent roundings, and the byte round trip in the direction not yet held — are refused, each a refused row pointing at the specification that was to declare it.

## Declared or proved

Every law a consumer needs is either a declaration here — an operation's definition, kind or morphism, which conversion then decides and the generated grid holds — or a lemma in [the numeric laws](../numeric-laws-spec.md), whose linear steps part 2 fills. A fact inside a declaration that conversion misses is a gap here, recorded here. A fact outside every declaration is a lemma, and a refused row records why conversion does not take it.

The specifications this part replaces asked for `min`, `max`, `abs` and `sign` to be declared by their Presburger definitions and read by a relational layer, so that their order facts would hold by conversion. Their order facts are consequences a case split and linear arithmetic reach, which part 2 proves wherever a bound needs one; the equations a type could need are the semilattice and homomorphism laws, which the kinds below decide. What only a relational layer adds — a Boolean combination of comparisons that reduces, a case-split equation that holds by conversion — waits for a consumer in [part 4](04-relational-layer-spec.md).

## `Nat`

- **`pow`**, promoted to `/sys` and declared a morphism from `(ℕ, +)` into `(ℕ, ·)` at a fixed base: `pow(x, a + b) = pow(x, a) · pow(x, b)`, `pow(x, 0) = 1` and `pow(x, 1) = x`, with `pow(pow(x, a), b) = pow(x, a · b)` beside them. This is the one new kind in the part's numeric half. The left shift is read through it: `shl(x, k) = x · pow(2, k)`, so `shl(shl(x, a), b) = shl(x, a + b)` holds by conversion. `shr(shl(x, k), k) = x` stays a lemma.
- **`min` and `max`**, promoted and each declared a semilattice — commutative, associative and idempotent, decided as a leaf set as `&&` is — and dominated. `min(a, b)` is at most each operand, through the domination `compare_nat` already reads; `max(a, b)` is at least each, through its mirror, which is new. Then `min(a, b) <= a`, `min(a, b) <= b`, `a <= max(a, b)` and `b <= max(a, b)` hold by conversion, beside the semilattice equations. `min(a, b) + max(a, b) = a + b` and `(a - b) + b = max(a, b)` are lemmas.

## `Int`

- **`abs` and `sign`**, promoted — `abs` answering a `Nat`, `sign` an `Int` in `{-1, 0, +1}` — and declared homomorphisms of `(ℤ, ·)` into `(ℕ, ·)` and `(ℤ, ·)`, pushed through a product as `Nat/to_int` is pushed through a sum. Then `abs(a · b) = abs(a) · abs(b)` and `sign(a · b) = sign(a) · sign(b)` hold by conversion, and with negation read as multiplication by `-1`, so do `abs(-n) = abs(n)` and `sign(-n) = -sign(n)`. `abs(Nat/to_int(m)) = m` holds through the embedding's inversion pair.
- **`min` and `max`**, as `Nat`'s.
- **The signed scale.** `shl(n, k)` is `n` times the widened `pow(2, k)`, so with the homomorphisms `sign(shl(n, k)) = sign(n)` and `abs(shl(n, k)) = shl(abs(n), k)` hold by conversion.

What stays a lemma at `Int` is [the numeric laws](../numeric-laws-spec.md)': the order facts of `abs` and `sign`, the triangle inequality, the decomposition `n = sign(n) · Nat/to_int(abs(n))`, and the `Prop` half of `abs(a - b)` being zero exactly when `Eq(a, b)`.

## `Flt`

These hold for every bit pattern under the model, NaNs included, and each is validated against the model over part 1's pattern grid. No `Flt` declaration inherits a ring or order law.

- **Commutativity** of `add`, `mul`, `min` and `max` in every rounding direction, and of `fma`'s two factors, declared symmetric and decided by the symmetric kind's operand order. `eql` and `neq` are held already.
- **The sign operations.** `neg(neg(x)) = x`, `abs(abs(x)) = abs(x)`, `abs(neg(x)) = abs(x)`, `copysign(copysign(x, y), z) = copysign(x, z)` and `neg(copysign(x, y)) = copysign(x, neg(y))`, each a bit operation holding of every pattern. Declared as operations on the sign of a float read as its sign and its magnitude — `neg` negates the sign, `abs` clears it, `copysign` takes another float's — so every composition of the three follows. This is the one new kind in the part's float half.
- **Subtraction** is addition of the negation, `sub(r, a, b) = add(r, a, neg(b))`, in every direction; declared a derived operation, as the model defines it.
- **The roundings to an integral value are idempotent**, `round_integral(r, round_integral(s, x)) = round_integral(s, x)`; declared projections onto the integral values, which every direction fixes.
- **The byte round trip, both ways.** `of_le_bytes(to_le_bytes(f)) = f` is held today; `to_le_bytes(of_le_bytes(b, @e)) = b` becomes held, since every pattern is a value and no NaN is canonicalized. Declared an isomorphism between `Flt` and eight-byte `Bytes`, whose two inversion pairs the existing kind reads.

What is not a law stays a control, refused at every direction it applies to, each with its counterexample: associativity of `add` and `mul`, and distributivity, by rounding; `f + 0.0 = f`, false at `-0.0`; `f * 1.0 = f`, since a signaling NaN is quieted; `f - f = 0.0`, at an infinity or a NaN; `f * 0.0 = 0.0`, at an infinity, a NaN, or a negative `f`; `f == f`, at a NaN; and `lt(a, b) = not(ge(a, b))`, with a NaN on either side.

## Promotion

Each promoted operation is new arithmetic on a numeric carrier and takes everything `CLAUDE.md`'s change routing names for one: its `/sys` row and `Intrinsic::signature` entry, which the prelude build checks against each other; its literal fold in every constant folder sharing `scalar` — `curios-core`'s, `curios-ersd`'s and `curios-cont`'s; the emitter's lowering with its i31 fast path and its `big_emitter` library; its declaration; and part 2's reading of its definition. Each folds as it executes, at both signs and past the i31, since `Nat` and `Int` are unbounded at every layer ([Nat and Int are an i31 until they outgrow it](../../design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)). The library function a promotion replaces becomes a re-export under its existing name.

## Stages

1. **Rows first.** State every law above and every control in the grid at every carrier and direction it applies to, refused, each commented with the stage that moves it.
2. **`Flt`'s identities.** No promotion, since the operations are intrinsics already: the symmetric, derived, projection and isomorphism declarations first, then the sign-and-magnitude kind.
3. **`min` and `max`**, at both carriers, with the domination mirror.
4. **`abs` and `sign`**, with their homomorphisms.
5. **`pow` and the scales**, with both shifts read through it.

Each stage moves its rows from refused to held, extends part 1's audit to the kind it declares, and adds its operation's rows to part 2's grid where part 2 has landed.

## Verification

- Each declared law is a generated row held at every carrier and direction its declaration covers; each control is refused; one mutation per new kind is run and caught, and the perimeter entry names it.
- Each promoted operation folds as it executes and agrees with `curios-num` over a generated grid, at both signs, past the i31 and past 64 bits.
- `Flt` declarations are held to the model over the pattern grid.
- Part 2's grid fills a bound over each promoted operation through its definition.

## Documentation and design record

- [Open fold laws and the sum normal form](../../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md) and [The bounds oracle and the division family](../../soundness/per-term-rules/the-bounds-oracle-and-the-division-family.md) gain the numeric declarations and the domination mirror; [The binary64 model and its NaN rule](../../soundness/per-term-rules/the-binary64-model-and-its-nan-rule.md) gains the float declarations.
- [A law is decided where it neither respells nor invents](../../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md): the rows moved from refused to held.
- `/std/Nat`, `/std/Int` and `/std/Flt` document the promoted operations and what conversion decides about them.

## Rejected

- **Presburger definitions read by a relational layer.** It is [part 4](04-relational-layer-spec.md)'s, conditional on a consumer, because the demand the replaced specifications named is met by part 2 and the kinds above.
- **Ring or order laws for `Flt`.** Each non-law above is refused for a counterexample under the model.
- **Promoting `gcd`.** Its certified form recurses on well-founded `<` in the library, which is [the numeric laws](../numeric-laws-spec.md)'; nothing in conversion needs it.

## Completion criteria

- `pow`, `min`, `max`, `abs` and `sign` are `/sys` operations whose declarations hold the laws above by conversion, folding as they execute.
- Every decided `Flt` law above is held by its declaration, with its controls refused.
- The new kinds — the exponent morphism, the domination mirror and the sign-and-magnitude operations — have their algorithms in Algebra, their mutations caught and their audit recorded.

## Retirement

Move the declarations' contracts to `curios-algebra`'s and Core's documentation and to the soundness entries above, and the operations' contracts to the `/std` modules. Replace the roadmap entry with a checked summary, update [the numeric laws](../numeric-laws-spec.md) and [Rat part 1](../rat/01-exact-rationals-spec.md) to the permanent record, verify that nothing references this filename, and delete it.
