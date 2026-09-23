# `Rat`: exact rationals and their laws

Working specification for `/std/Rat`, a certified exact rational: every finite rational with one canonical representation, exact arithmetic, division under a nonzero bound, binary64 conversion in both directions, exact decimal text, and the laws that make each of those a theorem. The integer facts it consumes are `Nat`'s and `Int`'s own, specified in [`nat-laws-spec.md`](nat-laws-spec.md) — the certified `gcd`, `exact_div`, `Coprime`, Euclid's lemma and the unsigned binary scale — and [`int-laws-spec.md`](int-laws-spec.md) — sign, absolute value, cancellation and the signed scale; nothing integer-level is written privately here.

Five stages, each landable alone: the executable core, the binary64 boundary, exact decimals, the laws, and the boundary proofs.

## What this builds on

- **`/std/Dyadic`**, public: `{mantissa: Int, exponent: Int}`, unnormalized, whose `+`, `-` and `·` never round and whose `Eql`, `Cmp`, `Ord` and `Hash` read the value, with `normalized` answering the odd-mantissa representation. Every dyadic is a rational, so `Rat` takes one in whole.
- **`Dyadic/of(f, @ok: Flt/Finite(f))`**: a finite float's exact value, through `/sys/Flt/mantissa` and `/sys/Flt/exponent`, which take the same bound. `Dyadic/try_of` is the `Option` form.
- **`Flt/rounded/of_dyadic(r, d)`**: the one rounding in the library, `curios_num::Floating`'s own, line for line, in every direction — with the subnormal grid, the carry into the exponent, and the overflow each direction sends where it sends it.
- **`Nat` and `Int`**: unbounded at run time ([Nat and Int are an i31 until they outgrow it](../design/toolchain/nat-and-int-are-an-i31-until-they-outgrow-it.md)), so no exact computation here reintroduces a width.

## Permanent decisions

**One canonical representation.** A value is `numerator · 2^exponent / odd_denominator`, zero as `(+0, +0, 1)`, and a nonzero value with an odd numerator magnitude, an odd denominator and the two coprime. Equality is then structural, so `Eql(Rat)` may be derived, `Key(Rat)` is a congruence, and `Eq(x, y)` in a type means the same number. The binary part is kept apart from the odd denominator so a dyadic value — every finite float's — is the denominator-one case with no gcd to take.

**Certificates only where fields interact.** `Rat` carries one joint canonicity certificate, because the three fields together decide whether a triple is reduced. It erases, and no equality proof may depend on distinguishing its inhabitants.

**The representation is private.** The exported laws mention only `Rat` and its operations, so the representation can change — a faster normalization, a different split of the binary part — without invalidating a client.

**A bound where a domain is excluded, `Option` where no answer is a real outcome.** Division takes `@ok: NonZero(y)`; the conversion from a float takes `@ok: Flt/Finite(f)`, the bound `Dyadic/of` consumes; each has a `try_` form answering `Option`. Text reading answers `Option`, because text that spells no number is an outcome, not an excluded domain.

**Rounding stays at the `Flt` boundary.** Nothing inside `Rat` rounds; every conversion to `Flt` rounds once, through `Flt/rounded/of_dyadic`, and no second rounding is written.

**Names are the repository's.** Conversions are `of_*` and `to_*`; the plain form takes the bound and the `try_` form answers `Option`; no `mk`, `widen`, `narrow` or suffixed variants. When `Rat` lands, `Dyadic`'s float conversions take the `_flt` suffix — `Dyadic/of` becomes `of_flt` and `try_of` becomes `try_of_flt` — since `Dyadic` then converts from more than one type.

## Stage 1 — the executable core

```crs
pub struct Rat: Type {
    numerator: Int,
    exponent: Int,
    odd_denominator: Nat,
    canonical: Canonical(numerator, exponent, odd_denominator),
}
```

**Construction.** `of_scaled_ratio(n: Int, e: Int, d: Nat, @ok: Nat/Lt(0, d))` is the normalizing constructor: it collapses a zero numerator, strips the powers of two from the numerator into the exponent and from the denominator out of it through `Nat`'s `odd_part` and `trailing_zeros`, divides both by their certified `gcd` through `exact_div`, and builds the certificate. The rest are its cases:

```text
of_int : Int -> Rat
of_nat : Nat -> Rat
of_dyadic : Dyadic -> Rat
of_ratio : (n : Int, d : Nat, @ok : Nat/Lt(0, d)) -> Rat
of_flt : (f : Flt, @ok : Flt/Finite(f)) -> Rat
try_of_flt : Flt -> Option(Rat)
```

`of_dyadic` strips the mantissa's trailing zeros — `Dyadic/normalized` — and needs no gcd; `of_flt(f, @ok)` is `of_dyadic(Dyadic/of(f, @ok))`, so nothing decodes a float's bits here, and a negative zero is canonical zero, deliberately.

**Operations.** Negation and absolute value act on the numerator. Addition and subtraction align the exponents, cross-multiply the odd denominators, add once, and normalize once; multiplication multiplies numerators and denominators, cross-cancelling first where that is proved equivalent. Comparison aligns and cross-multiplies without normalizing.

```text
zero, one : Rat
add, sub, mul : Rat -> Rat -> Rat
neg, abs : Rat -> Rat
eql : Rat -> Rat -> Bool
cmp : Rat -> Rat -> Ordering
lt, le, gt, ge : Rat -> Rat -> Bool
min, max : Rat -> Rat -> Rat
Le(x, y) := Bool/Holds(le(x, y))
Lt(x, y) := Bool/Holds(lt(x, y))
NonZero(x) := Bool/Holds(Bool/not(eql(x, zero)))
non_zero : (x : Rat) -> Option(NonZero(x))
reciprocal : (x : Rat, @ok : NonZero(x)) -> Rat
div : (x : Rat, y : Rat, @ok : NonZero(y)) -> Rat
floor, ceil, trunc, round : Rat -> Int
```

A reciprocal exchanges the numerator's magnitude with the denominator, carries the sign to the new numerator, negates the exponent, and normalizes. The zero case is excluded by the bound rather than answered, as every `/sys` division states its domain, and `Div(Rat)` states it the same way — `Ok(b) = NonZero(b)`, as `Div(Nat)` states `Lt(0, b)` — so `/` on `Rat` is ordinary. No infinity, NaN or signed zero enters the type.

`curios-prelude-archive/std/Rat.crs` is registered after `Dyadic` in `lib.crs` as `pub mod Rat; pub use Rat/{let Rat};`, with `Eql`, `Cmp`, `Ord`, `Add`, `Sub`, `Mul`, `Div`, `Show`, `Spell` and `Hash` witnesses. No compiler lowering emits `Rat`, so the syntax registry is unchanged.

Verified by normalizing equivalent raw fractions, powers of two on either side, shared odd factors, denominator one and zero; by arithmetic, comparison and division against an exact rational reference over `curios-num`, folded and executed; by type-level `Eq/refl()` facts on closed values; and by the erased layout, which is exactly the three numeric fields.

## Stage 2 — the binary64 boundary

**Exactly, from binary64.** `of_flt` and `try_of_flt` above: every finite float is dyadic, so nothing rounds.

**Correctly rounded, to binary64.** `Rat/to_flt(r: Flt/Rounding, x: Rat) -> Flt` rounds `numerator · 2^exponent / odd_denominator` once. A denominator-one value is `Flt/rounded/of_dyadic(r, Dyadic { mantissa = numerator, exponent = exponent })`. Otherwise, scale the numerator's magnitude by the power of two that leaves the quotient at least 54 significant bits and take one certified `div_mod` against the denominator: a nonzero remainder becomes one more low bit, `of_dyadic(r, Dyadic { mantissa = 2q + 1, exponent = e - 1 })`, which with at least 54 quotient bits lies below every rounding point and so decides every direction exactly as a sticky bit would. No approximation through `Flt` and no unbounded expansion.

**A quotient straight to binary64.** `Rat/ratio_to_flt(r, x, y)` rounds `x / y` once without building the interior rational: it cross-multiplies the stored components, fixes the sign and zero table first — `0/0` the NaN, a nonzero value over zero the signed infinity, zero over a nonzero value the signed zero the sign rule gives — and rounds everything else through the same `div_mod` route. It equals `to_flt(r, div(x, y, @ok))` for a nonzero `y`.

Verified against a correctly rounded reference in every direction over a generated corpus, with every format boundary pinned — normals, subnormals, both zeros, the normal/subnormal edge, the overflow boundary, exact halves and significand carry — and very unequal exponents exercised.

## Stage 3 — exact decimals

`Rat/of_str : Str -> Option(Rat)` parses an optional sign, digits, an optional fraction and an optional signed exponent exactly: the coefficient is the `Int` its digits spell with the point removed, a fraction of `f` digits lowers the written exponent by `f`, and the resulting decimal exponent `k` contributes `5ᵏ` to the numerator and `k` to the binary exponent when `k ≥ 0`, or the odd denominator `5⁻ᵏ` and `k` to the binary exponent when it is not, with one `of_scaled_ratio` to finish. No `Flt` takes part.

Presentation distinguishes the exact from the general: `to_decimal : Rat -> Option(Str)` succeeds exactly when the reduced odd denominator is a power of five and never rounds, and `to_str : Rat -> Str` is total, spelling `n/d` for `Show`. A rounded formatter takes an explicit precision and direction and is a separate specification.

`/std/Toml` keeps storing `Int` and `Flt` and depends on none of this; a profile storing exact values is a separate decision.

## Stage 4 — the laws

**Canonical uniqueness.** Equivalence is denominator-cleared: align the exponents, cross-multiply the positive denominators. Normalization preserves it; zero is unique; nonzero equivalent values have equal exponents once the powers of two are separated; and coprime reduced odd numerators and denominators are unique — the reduced-fraction step, from `nat-laws-spec.md`'s `Divides`, `Coprime`, Euclid's lemma and `exact_div`, never a second divisibility theory. So equal values have equal fields, `eql` is structural, and `Key(Rat)` is a congruence through the fields.

**Ring and order.** Identities, commutativity, associativity, distributivity, negation, subtraction, additive cancellation and multiplicative cancellation under `NonZero`; comparison reflection, reflexivity, antisymmetry concluding `Eq`, transitivity, totality, the strict/non-strict connections, monotonicity under addition and under multiplication by a sign, order reversal under negation, absolute value's non-negativity, and the subtraction and absolute-difference transformations a rounding error is compared through. Each is proved in three steps: the value equation on raw aligned triples from `Int`'s laws and the scale facts, the operation's denotation of that value, and transport through the constructor by canonical uniqueness. Positive denominators cross-multiply without reversing an order.

**The field.** Under `NonZero` premises: the reciprocal is nonzero and an involution; left and right inverses; quotient reconstruction; division by one and by itself; the reciprocal of a product; division cancellation; sign and absolute value; and the order under a positive or a negative divisor. Equalities conclude through canonical uniqueness, never by opening a certificate.

**Decimals.** Parsing denotes the written integer times the power of ten; `of_str(to_decimal(x))` is `some(x)` whenever `to_decimal` succeeds; termination is exactly the power-of-five denominator; zero has one spelling.

## Stage 5 — the boundary proofs

Stated over `Rat` and `/std/Flt`'s exact layer. The rounding the conversions go through, `Flt/rounded/of_dyadic`, is Curios code, so its correctness is proved here in every direction; the `Flt` primitives beside it are the trusted model, and what ties the two together is `flt-laws-spec.md`'s reflected model.

- **Round trip.** `to_flt(r, of_flt(f, @ok))` is `f` for every finite `f` but `-0.0`, in every direction; `-0.0` widens to canonical zero and canonical zero narrows to `+0.0`.
- **The directions.** For `x` narrowing to a finite `f` toward negative, `f ≤ x` and no finite binary64 lies in `(f, x]`; toward positive and toward zero by symmetry.
- **Nearest value.** For `x` narrowing to a finite `f` to nearest, no finite binary64 `y` is nearer — `abs(f - x) ≤ abs(y - x)` — under the overflow premise `abs(x) < 2¹⁰²⁴ - 2⁹⁷⁰`; ties to even choose the even significand and ties to away the larger magnitude; the half-ulp corollary in the normal range, its absolute-grid form for subnormals, carry into the next exponent and the signed zero follow.
- **Quotients.** `ratio_to_flt` is the correctly rounded quotient for a nonzero denominator, stated with cleared denominators so no interior quotient is built, and the `div_mod` step's quotient and remainder imply the rounding decision.

The proof library isolates the field interpretation, the grid spacing and adjacent representable values, the guard and sticky characterization of below-, at- and above-half remainders, carry, the overflow threshold, and the cleared-denominator comparison. A proof that needs to open the representation beyond the executable-correctness lemmas strengthens those lemmas instead.

## Non-goals

Laws about native `Flt` arithmetic, which are `flt-laws-spec.md`'s; infinities, NaNs or signed zero inside `Rat`; irrational values, roots, exponentiation or transcendental functions; hexadecimal or locale formatting; replacing `Int` or `Flt` as the pragmatic runtime defaults; migrating JSON, TOML, format strings or `Flt` APIs.

## Completion criteria

- Every finite rational has one canonical `Rat`, the operations agree with the values they denote, and division is total under `NonZero`.
- Every finite binary64 converts exactly, and every `Rat` converts to binary64 through `Flt/rounded/of_dyadic` in every direction, both proved.
- The ring, order and field laws hold, and every accepted decimal parses to its exact value.
- Before this specification is deleted, the representation, the canonical invariant, the normalization, the division contract, the rounding and decimal policies and the theorem surface are recorded in `/std/Rat`'s documentation, signatures and tests, the roadmap entry is a checked summary, and no reference to this filename remains.
