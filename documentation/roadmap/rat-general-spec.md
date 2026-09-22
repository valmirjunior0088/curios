# General `Rat`: every rational, division, and exact decimals

Working specification for the second phase of `/std/Rat`: extending [the dyadic phase](rat-dyadic-spec.md) privately to every finite rational, adding division and its field laws, generalizing the binary64 boundaries, and parsing and presenting decimals exactly. It consumes the dyadic phase's landed API and theorems, and from [`nat-laws-spec.md`](nat-laws-spec.md) the certified `gcd`, `exact_div`, `Coprime` and Euclid's lemma — none of which is reimplemented here.

Five stages, each a checkbox on the roadmap: the representation, the ring and order laws, division and the field laws, the binary64 boundaries, and decimals.

## Stage 1 — the representation

```crs
pub struct Rat: Type {
    numerator: Int,
    exponent: Int,
    odd_denominator: Nat,
    canonical: Canonical(numerator, exponent, odd_denominator),
}
```

A value denotes `numerator · 2^exponent / odd_denominator`. Canonical zero is `(+0, +0, 1)`; a nonzero value has an odd numerator magnitude, an odd denominator and the two coprime. The denominator's positivity follows from its oddness and is stated once inside `Canonical`, where `/std/BigPos` would once have carried it as a type of its own. A dyadic value is the denominator-one case of the one runtime structure; the implementation may take that as a fast path, but no second public representation exists, and `denominator == 1` is a derived fact rather than an invariant.

**Construction.** `of_int` and `of_dyadic` keep their meaning. `of_ratio(n: Int, d: Nat, @ok: Lt(0, d))` takes exponent zero, and `of_scaled_ratio(n, e, d, @ok)` is the full normalizing constructor: it collapses a zero numerator, strips the powers of two from the numerator into the exponent and from the denominator out of it through `odd_part` and `trailing_zeros`, divides both by their `gcd` through `exact_div`, and builds the erased certificate.

**Operations.** Negation and absolute value act on the numerator. Addition and subtraction align the exponents, cross-multiply the odd denominators, add once, and normalize once; multiplication multiplies numerators and denominators, cross-cancelling first where that is proved equivalent. Comparison aligns and cross-multiplies without normalizing. Every dyadic signature stays source-compatible.

**Migration.** Every dyadic value keeps its meaning; `of_flt_bytes` still produces denominator one; no Core, Ersd, Cont, Wasm, ABI or syntax representation changes, and the prelude archive is rebuilt for the layout rather than migrated.

Verified by normalizing equivalent raw fractions, powers of two on either side, shared odd factors, denominator one and zero; by arithmetic and comparison against an exact rational reference; by the whole dyadic corpus unchanged; and by `of_dyadic(m, e)` being `of_scaled_ratio(m, e, 1)` structurally.

## Stage 2 — uniqueness, ring and order

**Canonical uniqueness.** Equivalence is denominator-cleared: align the exponents, cross-multiply the positive denominators. Normalization preserves it; zero is unique; nonzero equivalent values have equal exponents once the powers of two are separated; and coprime reduced odd numerators and denominators are unique — the reduced-fraction step, from `nat-laws-spec.md`'s `Divides`, `Coprime`, Euclid's lemma and `exact_div`, never a second divisibility theory. So equal values have equal fields, and `eql` stays structural.

**The laws, re-established.** Every ring and order law the dyadic phase exports keeps its statement and gets a denominator-cleared proof: identities, commutativity, associativity, distributivity, negation, subtraction, both cancellations, sign and absolute value; comparison reflection, reflexivity, antisymmetry, transitivity, totality, the strict/non-strict connections, monotonicity under addition and under multiplication by a sign, order reversal under negation, and the absolute-difference transformations. Positive denominators cross-multiply without reversing an order, and exponent alignment uses the scale facts rather than native shifts. A theorem is never dropped because its old proof opened the representation; one that truly carried a dyadic-only premise is restated abstractly first, and the compatibility decision recorded.

## Stage 3 — division and the field laws

```text
NonZero(x) := Bool/Holds(Bool/not(eql(x, zero)))
non_zero : (x : Rat) -> Option(NonZero(x))
reciprocal : (x : Rat, @ok : NonZero(x)) -> Rat
div : (x : Rat, y : Rat, @ok : NonZero(y)) -> Rat
```

A reciprocal exchanges the numerator's magnitude with the denominator, carries the sign to the new numerator, negates the exponent, and normalizes. The zero case is excluded by the precondition rather than answered, which is how every `/sys` division states its domain, and the `Div(Rat)` witness states it the same way — `Ok(b) = NonZero(b)`, as `Div(Nat)` states `Lt(0, b)` — so the witness is honest and `/` on `Rat` is ordinary. Under `NonZero` premises: the reciprocal is nonzero and an involution; left and right inverses; quotient reconstruction; division by one and by itself; the reciprocal of a product; division cancellation; sign and absolute value; and the order under a positive or a negative divisor. Equalities conclude through canonical uniqueness, never by opening a certificate. No infinity, NaN or signed zero enters the type.

## Stage 4 — the binary64 boundaries

`of_flt_bytes` is unchanged: every finite binary64 is dyadic. `to_flt_bytes` rounds `numerator · 2^exponent / odd_denominator` once, through the dyadic phase's quotient route — a scaled numerator, one `div_mod` against the denominator, the significand and guard from the quotient and the sticky bit from the rest — so no approximation through `Flt` and no unbounded expansion. `ratio_to_flt_bytes(x, y)` cross-multiplies the stored components, `(nx · dy) · 2^(ex - ey) / (dx · ny)`, handles sign and zero first, and is proved equal to converting `div(x, y)` when `y` is nonzero. The nearest-value, ties-to-even, normal, subnormal, overflow, underflow, carry and signed-zero theorems generalize with the dyadic ones as their denominator-one case, and the quotient theorem stays stated with cleared denominators.

## Stage 5 — exact decimals

`Rat/of_str : Str -> Option(Rat)` parses an optional sign, digits, an optional fraction and an optional signed exponent exactly: the coefficient is the `Int` its digits spell with the point removed, a fraction of `f` digits lowers the written exponent by `f`, and the resulting decimal exponent `k` contributes `5ᵏ` to the numerator and `k` to the binary exponent when `k ≥ 0`, or the odd denominator `5⁻ᵏ` and `k` to the binary exponent when it is not, with one `of_scaled_ratio` to finish. No `Flt` and no approximate decimal takes part. Separators, if any, are specified rather than inherited from a codec.

Presentation distinguishes the exact from the general: `to_decimal : Rat -> Option(Str)` succeeds exactly when the reduced denominator is a power of five and never rounds, and `to_ratio_str : Rat -> Str` is total, a deterministic exact spelling for diagnostics and `Show` that `of_str` need not accept. A rounded formatter takes an explicit precision and mode and is a separate specification. Proved: parsing denotes the written integer times the power of ten; `of_str(to_decimal(x))` is `some(x)` whenever `to_decimal` succeeds; termination is exactly the power-of-five denominator; zero has one spelling.

`/std/Toml` keeps storing `Int` and `Flt` and depends on none of this; a TOML profile storing exact values is a separate decision, not a side effect of this stage.

## Non-goals

Infinities, NaNs or signed zero inside `Rat`; irrational values, roots, exponentiation or transcendental functions; hexadecimal floats or locale formatting; a shortest-decimal algorithm for binary64; migrating JSON, TOML, format strings or `Flt` APIs.

## Completion criteria

- Every finite rational has one canonical `Rat`, and the dyadic API and behaviour are unchanged.
- The ring, order and field laws hold for every denominator, the latter under `NonZero`, and `/` on `Rat` is the witness's.
- Every `Rat` converts to binary64 with one proved rounding decision, and every accepted decimal parses to its exact value.
- Before this specification is deleted, the representation, the canonical invariant, the normalization, the division contract, the rounding and decimal policies and the theorem surface are recorded in `/std/Rat`'s documentation, signatures and tests, the roadmap entry is a checked summary, and no reference to this filename remains.
