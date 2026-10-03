# Numeric laws: the Euclidean layer, the binary scales, the integer order, and the float bits

Working specification for the theorems and certified functions `/std/Nat`, `/std/Int` and `/std/Flt` still lack, proved in the library over what conversion decides. [`/std/Rat`](04-exact-rationals.md) is the consumer that asks for the `Nat` and `Int` layers and names which layer each of its stages needs; each layer is also an independent capability of its module that stands without any consumer.

The operations these laws are about — `pow`, `min`, `max`, `abs` and `sign` — are [declared to conversion](02-declared-operations.md) with every law it decides about them. The linear steps inside the proofs below are the elaborator's to fill ([A bound that follows from the facts in scope is proved by the elaborator](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)), so a proof here states its nonlinear or inductive step and leaves the arithmetic around it to the elaborator. A layer depends on those declarations only for the operations it uses, and lands when they have.

## What this builds on

`Nat` and `Int` are unbounded at every layer, the running program included ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)), so a law stated over them holds of the compiled program, and nothing below needs a second, library-defined number. Their arithmetic is reasoned about by the kernel rather than beside it: what conversion decides — the commutative-semiring and ring laws through the sum normal form, Euclid's identity and the remainder's bound, a comparison through its difference split by sign, and `Nat/to_int` as an ordered-semiring embedding — is recorded in [Open fold laws and the sum normal form](../../design/soundness/conversion/open-fold-laws-and-the-sum-normal-form.md), and [the declared operations](02-declared-operations.md) add theirs.

The library carries what reduction does not state:

- `/std/Nat/div_mod` hands back the quotient and remainder `/` and `%` compute, with Euclid's identity (`joined`) and the bound (`bounded`) as its proofs;
- `/std/Nat/Divides(d, n)` is a multiple witness, with `refl`, `trans`, `zero`, `one`, `add`, `mul`, and `of_rem`, which turns a zero remainder into divisibility with the quotient as the witness;
- `/std/Nat/lt` and `/std/Nat/le` carry the order laws, and `/std/WellFounded/recurse` over `WellFounded/lt` is strong induction along `<` — the measure a Euclidean recursion recurses on;
- `/std/Int`'s indexed `Sign` view — every `Int` is `nonneg(n)`, the embedding of `n`, or `neg(n)`, which is `-1 - Nat/to_int(n)` — with `view`, `trichotomy` and `eq_of_eql`;
- `/std/Int/lt` and `/std/Int/le`, each law proved by carrying the `Nat` law along the embedding through the sign view, which the `Int` lemmas below follow too: a statement about `Int` is split by the view of its operands, and each case is a `Nat` law read through `Nat/to_int`.

**Declared or proved.** Every law below is a lemma. A law conversion decides is [declared](02-declared-operations.md), and a fact inside the fragment conversion decides, or [the elaborator proves from the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), that neither reaches is a gap there, recorded there — in the law grid or in `curios`'s `tests::bounds`. A fact outside it is proved here, and a refused row in the law grid records why conversion does not take it.

## `Nat`: the Euclidean remainder

**The converse bridge.** `Divides(d, n)` implies `n % d == 0` for a positive `d`. `of_rem` is the other direction. The converse needs `(k · d) % d = 0` for a symbolic `k`, which it takes from `div_mod`'s uniqueness below. With a literal divisor the remainder is a defined operation, and the equation holds by conversion.

**Uniqueness of division.** Two pairs `(q, r)` and `(q′, r′)` with `n = q · d + r`, `r < d` and the same for the primed pair are equal. This is the fact every Euclidean argument ends in, and it is what lets `div_mod(k · d, d)` be read as `(k, 0)`.

**Exact division.** `exact_div(n, d, @ok: Lt(0, d), p: Divides(d, n)) -> Nat` returns the quotient, with `Eq()(n, exact_div(n, d, p) · d)` beside it. It is `n / d` in its computation; the law is what the divisibility evidence buys.

**A certified greatest common divisor.** `/std/Nat/gcd` exists as general recursion and a type may not mention it. The certified one recurses on `Lt(b % a, a)` through `WellFounded/recurse` over `WellFounded/lt`, so it is total and may appear in a type; whether it replaces the existing `gcd` or stands beside it is decided when it lands, preferring one `gcd`. The other way to a total `gcd` is a size-change rule for `NatRem` in the totality analysis, which would accept the existing definition as written; its divisor is the pattern `bp + 1`, so the rule would have to read a remainder as below its divisor's shape rather than below a nonzero variable. The certified `gcd`'s laws:

- it divides both operands;
- every common divisor divides it;
- symmetry, `gcd(a, 0) = a`, `gcd(a, 1) = 1`.

**Coprimality.** `Coprime(a, b)` is `Eq()(gcd(a, b), 1)`, with `is_coprime` as its decision and the bridge to common-divisor reasoning: two numbers are coprime exactly when every common divisor is one. Then:

- the quotients of two numbers by their gcd are coprime;
- coprime cancellation — `Coprime(a, b)` and `Divides(a, b · c)` give `Divides(a, c)` — which is Euclid's lemma;
- the reduced-fraction prerequisites: two fractions in lowest terms with equal cross products have equal numerators and denominators.

## `Nat`: the unsigned binary scale

The dyadic `Rat` normalizes by stripping powers of two and compares values held at different binary exponents; both are unsigned facts about a magnitude and belong here. `shl(x, k) = x · pow(2, k)` and the exponent laws are [declared](02-declared-operations.md).

- **The shift undone.** `shr(shl(x, k), k) = x`.
- **Trailing zeros.** `trailing_zeros(n)` counts the factors of two in a positive `n`, and `odd_part(n)` is what is left, with the reconstruction `n = shl(odd_part(n), trailing_zeros(n))`, the oddness of `odd_part(n)`, and uniqueness: an odd `m` and a count `k` with `n = shl(m, k)` are `odd_part(n)` and `trailing_zeros(n)`. Zero is excluded by a `Lt(0, n)` premise rather than given a conventional count.
- **Comparing differently shifted magnitudes.** `shl(a, i)` against `shl(b, j)` is decided by aligning the smaller count, `shl(a, i - j)` against `b` when `j ≤ i`, with the order and equality it answers proved to be the unshifted comparison's.

## `Nat`: `min` and `max`

Their order facts and semilattice equations hold by conversion through their [declarations](02-declared-operations.md). The two equations a case split proves are lemmas here:

- `min(a, b) + max(a, b) = a + b`;
- `(a - b) + b = max(a, b)`, truncated subtraction read through the same definitions.

## `Int`: order

Two laws a proof might expect to state are already conversion's and are not lemmas: order reversal under negation, `a <= b` being `+0 - b <= +0 - a`, and additive cancellation in its order form, `a + n <= b + n` being `a <= b`.

- **Totality**, as a disjunction: `Le(a, b)` or `Le(b, a)`, from `trichotomy`. Its `Bool` form holding by conversion would take [the relational layer](08-relational-layer.md), reserved until a consumer needs it.
- **The executable relations agree with the propositions**: `ord`, `==`, `<`, `<=`, `>` and `>=` each decide the proposition their spelling names, and `ord(a, b)` is `eq` exactly when `Eq()(a, b)`.
- **Flip symmetry**: `ord(b, a)` is `ord(a, b)` reversed.
- **Multiplication monotonicity under a non-positive factor**: `Le(a, b)` and `Le(k, +0)` give `Le(b · k, a · k)`, the twin of `le/mul_mono_r`.

## `Int`: cancellation

- **Multiplicative cancellation under a nonzero premise**: `Eq()(a · c, b · c)` and `NonZero(c)` give `Eq()(a, b)`, on both sides of the product. This is the integral-domain fact both `Rat` normalizations use in place of an inverse. Conversion never takes it, since without the premise `a · c = b · c` does not give `a = b`.

## `Int`: `abs`, `sign`, `min` and `max`

The homomorphism and reflection laws hold by conversion through their [declarations](02-declared-operations.md). These are lemmas, each a case split on the operand's sign with linear arithmetic in each case:

- **the order facts**: `abs(n)` is zero exactly when `n` is, `Le(+0, n)` exactly when `sign(n)` is not `-1`, and `Le(-Nat/to_int(abs(n)), n)` and `Le(n, Nat/to_int(abs(n)))`;
- **the absolute difference**: `abs(a - b)` is symmetric and satisfies the triangle inequality `Le(abs(a - c), abs(a - b) + abs(b - c))` in `Nat` — the form a rounding error is compared in;
- **decomposition**, `Eq()(n, sign(n) · Nat/to_int(abs(n)))`: a product of two defined operations, which linear arithmetic reads as one opaque atom;
- **`abs(a - b)` is zero exactly when `Eq()(a, b)`**: its `Bool` half holds by conversion, and its `Prop` half is a lemma through `eq_of_eql`;
- `min` and `max`'s equations, as `Nat`'s.

## `Int`: the signed scale

The dyadic `Rat` holds a signed mantissa at a binary exponent, so the unsigned binary-scale layer has to reach through a sign. That a shift keeps the sign is [declared](02-declared-operations.md).

- **A right shift is a floor**: `shr(n, k)` is `⌊n / 2ᵏ⌋`, so for a negative `n` it is `-shr(abs(n) - 1, k) - 1`, and it agrees with the `Nat` shift on a non-negative `n`.
- **Parity through the sign**: `n` is odd exactly when `abs(n)` is, so the odd-mantissa invariant is stated once on the magnitude. It holds by conversion in its `Bool` form, the remainder by a literal being a defined operation.
- **Odd-mantissa uniqueness**: two odd mantissas at two exponents denoting one value are equal, and so are their exponents — the unsigned uniqueness of `odd_part` and `trailing_zeros`, carried through the sign.

## `Flt`: theorems over the bits

Provable in `/std` without further trust, over the model the [float declarations](02-declared-operations.md) are validated against:

- `ord` is reflexive, transitive and total, antisymmetric to `Eq`, and `ord(a, b) = eq` exactly when `Eq()(a, b)` — so `Ord(Flt)` is IEEE's `totalOrder` in fact as well as in intent.
- A `Key(Flt)` over `to_le_bytes` is a congruence, since the byte round trip is injective. Whether to give one is a separate decision: `/std/Map` withholds it because `/std/ops/Eql`'s witness is IEEE `==`, which calls `+0.0` and `-0.0` equal and a NaN equal to nothing, and a map keyed on propositional equality would disagree with it at both ends. The theorem lands whichever way that goes.

## Verification

- Each law elaborates over symbolic operands and is exercised at the boundaries it states: a zero dividend, divisor one, a dividend below its divisor, exact multiples and their neighbours, `gcd` with a zero or a one, zero and both signs, equal magnitudes of opposite sign, a cancellation premise's boundary, and shifts across the i31 and past 64 bits.
- The executable `exact_div`, `gcd`, `trailing_zeros` and `odd_part` agree with `curios-num` over a generated grid, since the running program computes them over the same unbounded `Nat`.
- No `Int` proof opens a case analysis the sign view already performs; a missing shared step is added to the view's neighbourhood once rather than re-derived per law.
- No lemma writes a linear step [the elaborator proves from the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), and no lemma leans on a fact conversion does not decide without a refused row recording why.

## Completion criteria

- `Divides` has both bridges to the remainder, and `exact_div`, the certified `gcd` and `Coprime` exist with the laws above.
- The binary-scale helpers exist with reconstruction, uniqueness and aligned comparison, beside `shr(shl(x, k), k) = x`.
- The `Int` order bridges, multiplicative cancellation, the `abs` and `sign` lemmas and the signed-scale lemmas exist under `/std/Int` with operation-first names.
- `Ord(Flt)` is proved a total order agreeing with `Eq`, and the `Key(Flt)` congruence is proved.
- The `Rat` specifications need no private division, gcd, bit-stripping or sign reasoning of their own.
- Before this specification is deleted, the contracts are recorded in `/std/Nat`'s, `/std/Int`'s and `/std/Flt`'s documentation and their modules' signatures, the roadmap entry is a checked summary, and no reference to this filename remains.
