# General `BigFlt` binary32 boundaries

Post-Wonder implementation specification for extending the established binary32 conversion API and proofs from dyadic inputs to every canonical rational `BigFlt`.

## Objective

Preserve `of_flt_bytes`, `to_flt_bytes`, `of_flt`, and `to_flt` while changing conversion to binary32 into a correctly rounded exact-ratio operation. Preserve `ratio_to_flt_bytes` as an allocation-avoiding quotient boundary over already-general operands.

## Conversion from binary32

`of_flt_bytes` is unchanged semantically. Every finite binary32 value is dyadic, so it constructs denominator one through `of_dyadic`. Signed zero still collapses to canonical mathematical zero; infinity and NaN still return `none`.

The existing byte round-trip theorem remains valid with its documented negative-zero exception.

## Conversion to binary32

`to_flt_bytes` interprets a value as:

```text
numerator · 2^exponent / odd_denominator
```

and rounds the exact quotient once to binary32 using round-to-nearest-even. Reuse the landed compare-subtract-double digit engine behind `ratio_to_flt_bytes`, generalized to consume the stored denominator directly.

The algorithm computes only the leading significand, guard bit, sticky information, exponent, and carry required for one rounding decision. It must not first approximate the rational through native `Flt` or allocate an unbounded binary expansion.

## Quotient boundary

For `ratio_to_flt_bytes(x, y)`, cross-multiply the stored rational components conceptually:

```text
(nx · 2^ex / dx) / (ny · 2^ey / dy)
    = (nx · dy) · 2^(ex-ey) / (dx · ny)
```

Handle sign and zero before magnitude scheduling. The implementation may avoid constructing or GCD-normalizing the intermediate quotient. For nonzero `y`, prove equivalence with converting the successful exact `div(x, y)` result.

## Correctness proofs

Generalize the landed dyadic nearest-value theorem:

- the emitted finite value minimizes exact absolute error among binary32 values;
- exact ties choose an even significand;
- normal, subnormal, overflow, underflow, carry, and signed-zero boundaries follow one policy;
- quotient correctness is stated through denominator-cleared error comparisons;
- the dyadic theorem corpus remains a denominator-one specialization.

## Verification

- Compare generated rational conversions against a trusted exact IEEE-754 reference.
- Pin odd denominators around every normal/subnormal and halfway boundary.
- Exercise huge numerator/denominator and exponent disparities.
- Re-run every dyadic conversion and proof fixture unchanged.
- Benchmark stored-denominator and quotient paths independently.
- Confirm the native wrappers remain thin byte reinterpretations.
- Run the repository done bar.

## Non-goals

- Binary64 or configurable target formats.
- Decimal parsing or formatting.
- Approximate interior arithmetic.
- Nonfinite or signed-zero values inside exact `BigFlt`.

## Completion criteria

- Existing binary32 API names and dyadic behavior are preserved.
- Every general rational converts with one correctly proved rounding decision.
- Direct quotient conversion agrees with exact division followed by conversion when division succeeds.
- No native floating arithmetic participates in exact conversion logic or proofs.
- Before this specification is deleted, the stored-denominator rounding algorithm, quotient equivalence, and generalized boundary theorem contracts are recorded in the owning `/std/BigFlt` documentation, signatures, and tests; remaining plans refer to landed functions and theorems rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
