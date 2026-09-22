# `Int` laws: cancellation, sign, absolute value and the signed scale

Working specification for the `Int` theorems the standard library still lacks past its order laws. They are an independent capability of `/std/Int`, and the two `Rat` specifications ([dyadic](rat-dyadic-spec.md), [general](rat-general-spec.md)) are the consumers that ask for them.

## What this builds on

`Int` is unbounded at every layer, the running program included ([Nat and Int are an i31 until they outgrow it](../design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)). The kernel reasons about its arithmetic:

- sums and products reduce to a sum normal form, so the commutative-ring laws — identities, inverses, associativity, commutativity, distributivity, subtraction as the sum of a negation — hold by `Eq/refl()`;
- a comparison of two sides is decided through their difference split by sign, so `i < j`, `+0 < j - i` and `i + 1 <= j` are one proposition;
- `Nat/to_int` is an ordered-semiring embedding: it widens through sums and products, and a comparison between embedded sides narrows to the `Nat` one, so a non-negative side's floor is definitional.

The library carries what reduction does not state:

- `/std/Int`'s indexed `Sign` view — every `Int` is `nonneg(n)`, the embedding of `n`, or `neg(n)`, which is `-1 - Nat/to_int(n)` — with `view`, `trichotomy` and `eq_of_eql`;
- `/std/Int/Lt`: `try`, `le_of_lt`, `trans`, `lt_of_lt_le`, `lt_of_le_lt` and `le_of_not_lt`;
- `/std/Int/Le`: `refl`, `try`, `trans`, `add_mono_l`, `mul_mono_r` under a non-negative factor, and `antisym`.

Each is proved by carrying the `Nat` law along the embedding through the sign view, and the laws below follow the same route: a statement about `Int` is split by the view of its operands, and each case is a `Nat` law read through `Nat/to_int`.

## Order

- **Totality**, as a disjunction: `Le(a, b)` or `Le(b, a)`, from `trichotomy`.
- **The executable relations agree with the propositions**: `ord`, `==`, `<`, `<=`, `>` and `>=` each decide the proposition their spelling names, and `ord(a, b)` is `eq` exactly when `Eq(a, b)`.
- **Flip symmetry**: `ord(b, a)` is `ord(a, b)` reversed.
- **Order reversal under negation**: `Le(a, b)` gives `Le(-b, -a)`, and the strict twin.
- **Multiplication monotonicity under a non-positive factor**: `Le(a, b)` and `Le(k, +0)` give `Le(b · k, a · k)`, the twin of `Le/mul_mono_r`.

## Cancellation

- **Additive cancellation** is definitional through the difference and needs no lemma; the order form, `Le(a + n, b + n)` giving `Le(a, b)`, is stated for the proofs that meet it spelled that way.
- **Multiplicative cancellation under a nonzero premise**: `Eq(a · c, b · c)` and `NonZero(c)` give `Eq(a, b)`, on both sides of the product. This is the integral-domain fact both `Rat` normalizations use in place of an inverse.

## Sign and absolute value

`Int/abs` answers a `Nat` and `Int/sign` an `Int` in `{-1, 0, +1}`; their laws are what the `Rat` specifications read a value's magnitude and sign through.

- **Decomposition**: `Eq(n, sign(n) · Nat/to_int(abs(n)))`, and `abs(Nat/to_int(m))` is `m`.
- **Reflection**: `abs(-n)` is `abs(n)`, `sign(-n)` is `-sign(n)`, and `sign(a · b)` is `sign(a) · sign(b)`, `abs(a · b)` is `abs(a) · abs(b)`.
- **Order facts**: `abs(n)` is zero exactly when `n` is, `Le(+0, n)` exactly when `sign(n)` is not `-1`, and `Le(-Nat/to_int(abs(n)), n)` and `Le(n, Nat/to_int(abs(n)))`.
- **Absolute difference**: `abs(a - b)` is symmetric, zero exactly when `Eq(a, b)`, and satisfies the triangle inequality `Le(abs(a - c), abs(a - b) + abs(b - c))` in `Nat` — the form a rounding error is compared in.

## The signed scale

The dyadic `Rat` holds a signed mantissa at a binary exponent, so the unsigned binary-scale layer of [`nat-laws-spec.md`](nat-laws-spec.md) has to reach through a sign.

- **Shifts keep the sign**: `shl(n, k)` is `n · 2ᵏ`, so its sign is `n`'s and its absolute value is `shl(abs(n), k)`.
- **A right shift is a floor**: `shr(n, k)` is `⌊n / 2ᵏ⌋`, so for a negative `n` it is `-shr(abs(n) - 1, k) - 1`, and it agrees with the `Nat` shift on a non-negative `n`.
- **Parity through the sign**: `n` is odd exactly when `abs(n)` is, so the odd-mantissa invariant is stated once on the magnitude.
- **Odd-mantissa uniqueness**: two odd mantissas at two exponents denoting one value are equal, and so are their exponents — the unsigned uniqueness of `odd_part` and `trailing_zeros`, carried through the sign.

## Verification

- Each law elaborates over symbolic operands and is exercised at zero, at both signs, at equal magnitudes of opposite sign, at a cancellation premise's boundary, and past the i31 and 64 bits.
- No proof opens a case analysis the sign view already performs; a missing shared step is added to the view's neighbourhood once rather than re-derived per law.

## Completion criteria

- The order, cancellation, sign, absolute-value and signed-scale laws above exist under `/std/Int` with operation-first names.
- The `Rat` specifications need no private sign reasoning of their own.
- Before this specification is deleted, the contracts are recorded in `/std/Int`'s documentation and its modules' signatures, the roadmap entry is a checked summary, and no reference to this filename remains.
