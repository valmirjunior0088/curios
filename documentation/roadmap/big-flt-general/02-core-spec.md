# General `BigFlt` core representation and operations

Post-program-analysis implementation specification for extending private dyadic `BigFlt` into a canonical reduced rational type. This work consumes the landed `/std/BigNat` Euclidean arithmetic layer and preserves the existing abstract API established by the dyadic core.

## Objective

Represent every finite rational number exactly as a signed numerator, a power-of-two exponent, and a positive odd denominator. Preserve canonical structural equality, exact arithmetic, executable comparison, proof erasure, and the existing behavior of every dyadic value.

The mathematical interpretation is:

```text
numerator · 2^exponent / odd_denominator
```

## Representation

```crs
pub struct BigFlt : Type {
    numerator : BigInt,
    exponent : BigInt,
    odd_denominator : BigPos,
    canonical : Canonical(numerator, exponent, odd_denominator)
}
```

Use one uniform runtime structure. Dyadic values carry denominator one; the implementation may recognize that value as a fast path, but a second public nominal representation is not introduced.

`odd_denominator` is positive through `BigPos`. The `Canonical` certificate additionally establishes oddness and coprimality where required.

## Canonical invariant

Canonical zero is exactly `(0, 0, 1)`. For a nonzero numerator:

- the numerator magnitude is odd;
- the denominator is odd;
- numerator magnitude and denominator are coprime.

The `denominator == 1` dyadic classification is a derived Boolean or lemma, not a separate semantic invariant. Merely proving `denominator != 1` does not make a general value canonical.

The canonicity kernel proves that two canonical triples with equal denominator-cleared values have equal numerator, exponent, and denominator. Structural `eql` therefore remains valid.

## Construction and normalization

Use repository-native `of_*` names:

```text
of_big_int : BigInt -> BigFlt
of_dyadic : BigInt -> BigInt -> BigFlt
of_ratio : BigInt -> BigPos -> BigFlt
of_scaled_ratio : BigInt -> BigInt -> BigPos -> BigFlt
```

`of_big_int` and `of_dyadic` preserve their existing meaning. `of_ratio(numerator, denominator)` uses exponent zero. `of_scaled_ratio` is the full normalizing constructor.

Normalization:

1. collapses a zero numerator to `(0, 0, 1)`;
2. strips powers of two from the numerator and adds their count to the exponent;
3. strips powers of two from the denominator and subtracts their count from the exponent;
4. computes the GCD of the remaining numerator magnitude and denominator;
5. divides both by that GCD through certified exact division;
6. constructs the erased `Canonical` evidence.

No public function is named `mk` or `mk_ratio`.

## Exact operations

Negation and absolute value act on the numerator. Addition and subtraction align binary exponents, cross-multiply odd denominators, add signed numerators, and normalize once. Multiplication multiplies numerators and denominators, preferably cross-cancelling before large products when the optimization is proven equivalent.

Comparison aligns exponents and cross-multiplies positive denominators without constructing a normalized result. Existing `zero`, `one`, `add`, `sub`, `mul`, `neg`, `abs`, `eql`, `cmp`, `lt`, `le`, `gt`, and `ge` signatures remain source-compatible.

Add division-shaped operations:

```text
reciprocal : BigFlt -> Option(BigFlt)
div : BigFlt -> BigFlt -> Option(BigFlt)
```

Zero produces `none`. Nonzero inputs swap signed numerator magnitude with the positive denominator, transfer sign to the new numerator, negate the binary exponent, and normalize.

## Migration contract

- Every existing dyadic value retains its exact mathematical meaning.
- `of_dyadic`, arithmetic, equality, comparison, and native conversion signatures remain available.
- Existing abstract theorem statements remain valid; their internal proofs may be replaced.
- `of_flt_bytes` continues to produce denominator-one values.
- No Core, Ersd, Cont, Wasm, host ABI, or syntax representation changes are introduced.
- The prelude archive is rebuilt for the private runtime-layout change rather than migrated.
- `/syn` remains unchanged because compiler lowering does not emit `BigFlt`.

## Soundness discipline

- GCD and exact division are imported from `/std/BigNat`, never duplicated privately.
- Every normalization branch produces checked oddness and coprimality evidence.
- Runtime fast paths do not bypass the canonical constructor.
- No theorem exposes a complete representation eliminator.
- Proof erasure leaves exactly the three intended integer fields.
- All exact operations remain independent of native `Flt` arithmetic.

## Verification

- Normalize equivalent raw fractions, signed values, powers of two in either side, shared odd factors, denominator one, and zero.
- Compare arithmetic and comparison with an arbitrary-precision rational reference.
- Run the complete dyadic behavioral corpus unchanged.
- Confirm `of_dyadic(m, e) = of_scaled_ratio(m, e, one)` extensionally and structurally.
- Confirm proof erasure and the intended three-field runtime representation.
- Benchmark denominator-one fast paths and large cross-products.
- Run the repository done bar.

## Non-goals

- Infinities, NaNs, signed zero, irrational values, exact roots, or transcendental functions inside `BigFlt`.
- Decimal parsing or formatting, which belongs to the later exact decimal interop effort.
- Binary32 conversion changes, which belong to the later general binary32 boundary effort.
- Full field and order theorem publication, staged in the following law specifications.
- Exposing numerator, exponent, or denominator as a stable public representation contract.

## Completion criteria

- Every finite rational has a constructible canonical `BigFlt` representation.
- Canonical representation is unique and executable equality remains structural.
- Existing dyadic APIs and behavior remain compatible.
- Exact arithmetic and comparison agree with the rational reference.
- `reciprocal` and `div` have explicit zero behavior and exact executable semantics.
- The following law and boundary specifications require no representation redesign.
- Before this specification is deleted, the general representation, canonical invariant, normalization algorithm, public construction contracts, and dyadic migration guarantees are recorded in the owning `/std/BigFlt` documentation and tests; remaining plans refer to the landed API and invariant lemmas rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
