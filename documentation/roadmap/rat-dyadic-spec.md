# Dyadic `Rat`: exact binary rationals and correctly rounded `Flt` boundaries

Working specification for the first phase of `/std/Rat`, a certified exact rational: the dyadic numbers ℤ[1/2] with exact arithmetic, executable comparison, binary64 conversion in both directions, and the proofs that the conversions round correctly. [The general phase](rat-general-spec.md) extends the same type to every rational without changing its public contract, which is what the privacy rules below protect.

Five stages, each a checkbox on the roadmap and each landable alone: the core, the binary64 conversions, the law corpus, quotient narrowing, and the boundary proofs. The integer facts they consume are `Nat`'s and `Int`'s own, specified in [`nat-laws-spec.md`](nat-laws-spec.md) (the unsigned binary scale) and [`int-laws-spec.md`](int-laws-spec.md) (sign, absolute value, cancellation and the signed scale); nothing integer-level is written privately here.

## Permanent decisions

**Canonical dyadic representation.** A value is `mantissa · 2^exponent`, with an odd mantissa for every nonzero value and one canonical zero, so equality is structural.

**Integers are the language's.** Mantissa and exponent are `Int`, which is unbounded at run time ([Nat and Int are an i31 until they outgrow it](../design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)); no exact computation reintroduces a width, and no library integer stands beside `Int`.

**Certificates only where fields interact.** `Rat` carries one joint canonicity certificate, because mantissa and exponent together decide whether a pair is reduced. It erases, and no equality proof may depend on distinguishing its inhabitants.

**Representation privacy keeps the general phase open.** The representation is private and the exported laws mention only `Rat` and its operations. No public theorem exposes a complete dyadic case analysis, so the general phase can add an odd denominator without invalidating a client.

**Rounding stays at the `Flt` boundary.** The dyadic phase has no interior division and no `Div(Rat)` witness; the one division-shaped operation rounds a quotient straight to binary64.

**Names are the repository's.** `Rat/of_dyadic` is the normalizing constructor and `Rat/of_int` its exponent-zero case; conversions are `of_*` and `to_*`; no `mk`, `widen`, `narrow` or suffixed variants.

## Stage 1 — the core

```crs
pub struct Rat: Type {
    mantissa: Int,
    exponent: Int,
    canonical: Canonical(mantissa, exponent),
}
```

A canonical nonzero value has an odd mantissa; canonical zero is `(+0, +0)`. `of_dyadic(m, e)` strips the trailing zeros of `m`'s magnitude through `Nat`'s `odd_part` and `trailing_zeros`, adds the count to `e`, and forces canonical zero; `of_int(n)` is `of_dyadic(n, +0)`. The canonicity kernel proves that normalization preserves the value and yields an odd-or-zero mantissa, that equal nonzero values with odd mantissas have equal fields — `int-laws-spec.md`'s odd-mantissa uniqueness — that every raw zero normalizes to one representation, and that equal raw pairs give structurally equal results.

Addition and subtraction align exponents, shift the mantissa at the larger exponent by the difference, add once, and normalize. Multiplication multiplies mantissas, adds exponents, and normalizes. Negation and absolute value act on the mantissa. Comparison aligns the two values and compares mantissas through `nat-laws-spec.md`'s shifted comparison, reaching through the sign by `int-laws-spec.md`'s.

```text
zero, one : Rat
of_int : Int -> Rat
of_dyadic : Int -> Int -> Rat
add, sub, mul : Rat -> Rat -> Rat
neg, abs : Rat -> Rat
eql : Rat -> Rat -> Bool
cmp : Rat -> Rat -> Ordering
lt, le, gt, ge : Rat -> Rat -> Bool
Le(x, y) := Bool/Holds(le(x, y))
Lt(x, y) := Bool/Holds(lt(x, y))
NonZero(x) := Bool/Holds(Bool/not(eql(x, zero)))
```

`curios-prelude-archive/std/Rat.crs` is registered after `Int` in `lib.crs` as `pub mod Rat; pub use Rat/{let Rat};`, with `Add`, `Sub`, `Mul`, `Eql` and `Cmp` witnesses, and `Show` and `Ord` where useful. No compiler lowering emits `Rat`, so the syntax registry is unchanged.

Verified by normalizing zero, both signs and long powers of two; by the structural equality of distinct raw pairs after `of_dyadic`; by arithmetic and comparison against an exact rational reference; and by the erased layout, which is exactly two `Int` fields.

## Stage 2 — binary64 conversion

`Flt` is IEEE 754 binary64, every bit pattern a distinct value, computed exactly by `curios-num`'s model; `Flt/to_le_bytes` and `Flt/of_le_bytes` (the latter under `EightBytes`) fold through that model, so their round trip is a theorem of the model rather than a postulate. Do not add a conversion rule asserting it.

**Exactly, from binary64.** `Rat/of_flt_bytes : Bytes -> Option(Rat)` answers `none` unless the input is eight bytes, then reads the sign, the 11-bit exponent field `e` and the 52-bit fraction `f`:

- `e` in `1..2046`: `of_dyadic(±(f + 2⁵²), e - 1075)`;
- `e = 0`, `f ≠ 0`: `of_dyadic(±f, -1074)`;
- either zero: canonical zero — collapsing `-0.0` is deliberate;
- `e = 2047`: `none`, for both infinities and the NaN.

Nothing rounds, and `of_flt(x)` is `of_flt_bytes(Flt/to_le_bytes(x))`.

**Correctly rounded, to binary64.** `Rat/to_flt_bytes : Rat -> Bytes` computes the unbiased exponent from the mantissa's bit length and the exponent, then:

- a normal-range value keeps its leading 53 bits, forms a guard bit and a sticky bit from the rest, rounds to nearest with ties to even, carries into the exponent, and rechecks overflow;
- a subnormal-range value rounds on the `2⁻¹⁰⁷⁴` grid instead of keeping 53 bits;
- a magnitude at or past `2¹⁰²⁴ - 2⁹⁷⁰`, halfway from the largest finite value `2¹⁰²⁴ - 2⁹⁷¹` to `2¹⁰²⁴`, rounds to the signed infinity;
- a nonzero value that rounds to zero keeps its sign in the emitted pattern.

`to_flt(x)` is `Flt/of_le_bytes(to_flt_bytes(x))`, with `EightBytes` discharged by the length `to_flt_bytes` states of its result. The helpers — bit length, leading bits, guard and sticky — are `Nat` computations; the ones with value beyond this boundary go to `nat-laws-spec.md`, and the rounding policy stays here. `Nat`'s `/` and `%` are available to them as certified operations through `div_mod`, where the plan this replaces had to avoid them.

Verified against a correctly rounded IEEE 754 reference over a generated corpus, with every format boundary pinned: normals, subnormals, both zeros, the normal/subnormal edge, the overflow boundary, exact halves and significand carry.

## Stage 3 — the law corpus

Exactly the algebra and order a client and the boundary proofs need, with no field theory for a type without interior division:

- additive and multiplicative identities, commutativity and associativity, distributivity, negation and subtraction, additive cancellation, and multiplicative cancellation under `NonZero` — the integral-domain substitute for an inverse;
- comparison reflection, reflexivity, antisymmetry concluding `Eq`, transitivity, totality, the strict/non-strict connections, addition monotonicity, multiplication monotonicity under a sign premise, order reversal under negation, absolute value's non-negativity, and the subtraction and absolute-difference transformations a rounding error is compared through.

Each operation's law is proved in three steps: the value equation on raw aligned pairs from `Int`'s laws and the scale facts, the operation's denotation of that value, and transport through `of_dyadic` by canonical uniqueness. Sign cases are `int-laws-spec.md`'s and are not repeated per theorem.

## Stage 4 — quotient narrowing

`Rat/ratio_to_flt_bytes : Rat -> Rat -> Bytes` rounds the exact quotient of two values once, straight to binary64, without building an interior rational. Scale the numerator's magnitude by the power of two that leaves the quotient at least 54 significant bits, and take one certified `div_mod` against the denominator's: the quotient's leading 53 bits are the significand and the next is the guard bit, any bits past it and a nonzero remainder make the sticky bit, and the exponents and the scale fix the binary exponent. The packing and rounding policy is stage 2's, shared rather than restated. The sign and zero table is fixed first: `0/0` is the NaN, a nonzero value over zero the signed infinity, zero over a nonzero value the signed zero the sign rule gives, and everything else rounds to nearest, ties to even.

Verified against an exact rational reference over generated numerators, denominators, exponents and signs, with the zero, infinity, subnormal, overflow, underflow, halfway and carry cases pinned and very unequal exponents exercised.

## Stage 5 — the boundary proofs

Stated over `Bytes` and `Rat`; the `Flt` model itself is trusted code, not a Curios proof, and is where these stop.

- **Round trip**: `of_flt_bytes(b) = some(x)` with `b` not the negative-zero pattern gives `to_flt_bytes(x) = b`; `-0.0` widens to canonical zero and canonical zero narrows to `+0.0`.
- **Nearest value**: for `x` narrowing to a finite `r`, no finite binary64 `y` is nearer — `abs(r - x) ≤ abs(y - x)` — under the overflow premise `abs(x) < 2¹⁰²⁴ - 2⁹⁷⁰`; ties choose the even significand; the half-ulp corollary in the normal range, its absolute-grid form for subnormals, carry into the next exponent and the signed zero follow.
- **Quotients**: `ratio_to_flt_bytes` is the correctly rounded quotient for a nonzero denominator, stated with cleared denominators so no interior quotient is built, and the `div_mod` step's quotient and remainder imply stage 2's rounding decision.

The proof library isolates the field interpretation, the grid spacing and adjacent representable values, the guard and sticky characterization of below-, at- and above-half remainders, carry, the overflow threshold, and the cleared-denominator comparison. A proof that needs to open the representation beyond the executable-correctness lemmas strengthens those lemmas instead.

## Non-goals

Laws about native `Flt` arithmetic; interior division or a `Div(Rat)` witness; decimal parsing or formatting; exact roots or constructive reals; replacing `Int` or `Flt` as the pragmatic runtime defaults.

## Completion criteria

- Every `Rat` is canonical by construction, and the exact operations, equality and comparison agree with the dyadic values they denote.
- Every finite binary64 converts exactly, and every `Rat` converts to binary64 by round-to-nearest-even, both proved.
- The public surface leaves no dyadic-only theorem that would block the general phase.
- Before this specification is deleted, the representation, the canonical invariant, the contracts, the rounding policy and the theorem surface are recorded in `/std/Rat`'s documentation, signatures and tests, the roadmap entry is a checked summary, [the general phase](rat-general-spec.md) refers to the landed API, and no reference to this filename remains.
