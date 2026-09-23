# `Nat` laws: the Euclidean remainder and the binary scale

Working specification for the `Nat` theorems and helpers the standard library still lacks: the Euclidean layer past certified division, and the unsigned binary-scale layer. Both are independent capabilities of `/std/Nat` that stand without any consumer; [the `Rat` specification](rat-laws-spec.md) is the consumer that asks for them, and names which layer each stage needs.

## What this builds on

`Nat` is unbounded at every layer, the running program included ([Nat and Int are an i31 until they outgrow it](../design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)), so a law stated over `Nat` holds of the compiled program, and nothing below needs a second, library-defined natural. Its arithmetic is reasoned about by the kernel rather than beside it:

- sums and products reduce to a sum normal form, so the commutative-semiring laws hold by `Eq/refl()`;
- Euclid's identity is definitional — `d · (x / d) + x % d` is `x` for the same dividend and divisor — and so is the remainder's bound, `x % d < d`;
- a left shift by a symbolic count is read through its power, `shl(x, k + 1) = 2 · shl(x, k)`, and a right shift through its floor, so a shift recurses on its count;
- `/std/Nat/div_mod` hands back the quotient and remainder `/` and `%` compute, with Euclid's identity (`joined`) and the bound (`bounded`) as its proofs;
- `/std/Nat/Divides(d, n)` is a multiple witness, with `refl`, `trans`, `zero`, `one`, `add`, `mul`, and `of_rem`, which turns a zero remainder into divisibility with the quotient as the witness;
- `/std/Nat/Lt` and `/std/Nat/Le` carry the order laws, and `/std/WellFounded/recurse` over `WellFounded/lt` is strong induction along `<` — the measure a Euclidean recursion recurses on.

Every law below is proved over those facts. None is a new kernel rule; if a proof needs a fact the reducer does not decide, the finding goes to the law grid in `curios/src/tests/laws.rs` before a lemma is written around it.

## The Euclidean remainder

**The converse bridge.** `Divides(d, n)` implies `n % d == 0` for a positive `d`. `of_rem` is the other direction. The converse needs `(k · d) % d = 0` for a symbolic `k`, which the reducer does not decide today; either that becomes a law beside Euclid's identity — a multiple of the divisor leaves no remainder — or it is proved from `div_mod`'s uniqueness below. The first is preferred if the law grid admits it as a held row with a control.

**Uniqueness of division.** Two pairs `(q, r)` and `(q′, r′)` with `n = q · d + r`, `r < d` and the same for the primed pair are equal. This is the fact every Euclidean argument ends in, and it is what lets `div_mod(k · d, d)` be read as `(k, 0)`.

**Exact division.** `exact_div(n, d, @ok: Lt(0, d), p: Divides(d, n)) -> Nat` returns the quotient, with `Eq(n, exact_div(n, d, p) · d)` beside it. It is `n / d` in its computation; the law is what the divisibility evidence buys.

**A certified greatest common divisor.** `/std/Nat/gcd` exists as general recursion and a type may not mention it. The certified one recurses on `Lt(b % a, a)` through `WellFounded/recurse` over `WellFounded/lt`, so it is total and may appear in a type; whether it replaces the existing `gcd` or stands beside it is decided when it lands, preferring one `gcd`. Its laws:

- it divides both operands;
- every common divisor divides it;
- symmetry, `gcd(a, 0) = a`, `gcd(a, 1) = 1`.

**Coprimality.** `Coprime(a, b)` is `Eq(gcd(a, b), 1)`, with `is_coprime` as its decision and the bridge to common-divisor reasoning: two numbers are coprime exactly when every common divisor is one. Then:

- the quotients of two numbers by their gcd are coprime;
- coprime cancellation — `Coprime(a, b)` and `Divides(a, b · c)` give `Divides(a, c)` — which is Euclid's lemma;
- the reduced-fraction prerequisites: two fractions in lowest terms with equal cross products have equal numerators and denominators.

## The unsigned binary scale

The dyadic `Rat` normalizes by stripping powers of two and compares values held at different binary exponents; both are unsigned facts about a magnitude and belong here.

- **Power-of-two scaling.** `shl(x, k)` is `x · 2ᵏ` for every `k`, `shl(shl(x, a), b)` is `shl(x, a + b)`, and `shr(shl(x, k), k)` is `x`. With the shift-count law in the reducer, each is an induction on the count.
- **Trailing zeros.** `trailing_zeros(n)` counts the factors of two in a positive `n`, and `odd_part(n)` is what is left, with the reconstruction `n = shl(odd_part(n), trailing_zeros(n))`, the oddness of `odd_part(n)`, and uniqueness: an odd `m` and a count `k` with `n = shl(m, k)` are `odd_part(n)` and `trailing_zeros(n)`. Zero is excluded by a `Lt(0, n)` premise rather than given a conventional count.
- **Comparing differently shifted magnitudes.** `shl(a, i)` against `shl(b, j)` is decided by aligning the smaller count, `shl(a, i - j)` against `b` when `j ≤ i`, with the order and equality it answers proved to be the unshifted comparison's.

## Verification

- Each law elaborates over symbolic operands and is exercised at the boundaries it states: a zero dividend, divisor one, a dividend below its divisor, exact multiples and their neighbours, `gcd` with a zero or a one, and shifts across the i31 and past 64 bits.
- The executable `exact_div`, `gcd`, `trailing_zeros` and `odd_part` agree with `curios-num` over a generated grid, since the running program computes them over the same unbounded `Nat`.
- No lemma leans on a fact the reducer does not decide without a law-grid row recording why.

## Completion criteria

- `Divides` has both bridges to the remainder, and `exact_div`, the certified `gcd` and `Coprime` exist with the laws above.
- The binary-scale helpers exist with reconstruction, uniqueness and aligned comparison.
- The `Rat` specifications need no private division, gcd or bit-stripping of their own.
- Before this specification is deleted, the contracts are recorded in `/std/Nat`'s documentation and its modules' signatures, the roadmap entry is a checked summary, and no reference to this filename remains.
