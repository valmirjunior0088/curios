# Correctly rounded `BigFlt` ratio narrowing

Working implementation specification for rounding an exact quotient of two BigFlt values directly to binary32 without adding exact interior division to the BigFlt API.

This work depends on [`04_BIG_FLT_CORE_SPEC.md`](04_BIG_FLT_CORE_SPEC.md), shares binary32 packing rules with [`05_BIG_FLT_BINARY32_SPEC.md`](05_BIG_FLT_BINARY32_SPEC.md), and supplies the executable basis for the ratio theorem in [`09_BIG_FLT_BOUNDARY_PROOFS_SPEC.md`](09_BIG_FLT_BOUNDARY_PROOFS_SPEC.md).

## Objective

Provide the only division-shaped operation in stage 1:

```crs
BigFlt/narrow_ratio_b : BigFlt -> BigFlt -> Bytes
```

The function rounds the exact mathematical quotient once, directly to a binary32 byte pattern using round-to-nearest-even. It does not construct an interior rational value and does not justify a `Div(BigFlt)` witness.

## Algorithm

Normalize numerator and denominator signs and magnitudes, account for their exact powers-of-two exponents, and use a base-2 compare-subtract-double digit loop to produce:

- the leading significand bits required by the target range;
- a guard bit;
- sticky information for every unconsumed remainder bit;
- the exponent and carry information required by the shared binary32 packer.

The loop must compute only the precision needed for one final rounding decision. It must not materialize an unbounded quotient or route through native floating arithmetic.

General magnitude comparison and shift helpers belong beside BigNat or BigInt when reusable. The quotient-digit state machine and its termination measure belong to this specification.

## Edge semantics

Specify and test one explicit sign and zero table before implementing the loop:

- `0/0` produces a chosen quiet NaN byte pattern;
- nonzero divided by zero produces signed infinity;
- zero divided by a finite nonzero value produces signed zero according to the sign rule;
- finite nonzero operands use round-to-nearest-even;
- overflow, subnormal results, underflow, halfway cases, and carry use the same binary32 policy as `narrow_b`.

The native wrapper is:

```text
narrow_ratio = Flt/of_le_bytes ∘ narrow_ratio_b
```

## Structural obligations

- Each loop step preserves the exact numerator/denominator relationship.
- Compare-subtract updates keep the remainder bounded by the active denominator scale.
- Guard and sticky results summarize all discarded quotient information needed by the rounding decision.
- The loop has an evident structural or bounded termination argument compatible with future checking.
- Sign handling is separated from magnitude extraction so proof cases do not multiply unnecessarily.

These obligations expose facts for the later formal proof; they do not by themselves establish global nearest-value correctness.

## Verification

- Compare results against an exact rational reference over generated numerator, denominator, exponent, and sign combinations.
- Pin zero, infinity, signed-zero, normal/subnormal boundary, overflow, underflow, exact halfway, and significand-carry cases.
- Exercise very unequal exponents and large packed magnitudes.
- Confirm no exact rational allocation or native division appears in the implementation.
- Benchmark the digit loop independently from existing Dragon4 conversion work.

## Non-goals

- Exact interior division or a quotient-valued result.
- General BigNat `divmod`, `gcd`, or rational reduction.
- A `Div(BigFlt)` witness.
- The formal denominator-cleared nearest-value theorem, which belongs to the boundary proof specification.

## Completion criteria

- `narrow_ratio_b` has fully specified behavior for every numerator and denominator pair.
- Generated reference tests agree with exact rational rounding.
- The algorithm exports or proves the structural invariants required by the formal ratio-correctness proof.
- Its binary32 packing behavior is shared with or demonstrably identical to `narrow_b`.
