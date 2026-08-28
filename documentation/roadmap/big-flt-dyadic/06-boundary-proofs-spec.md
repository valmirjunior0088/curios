# Dyadic `BigFlt` binary32 boundary proofs

Post-bootstrap implementation specification for the formal round-trip and correct-rounding results over the landed `of_flt_bytes`, `to_flt_bytes`, and `ratio_to_flt_bytes` boundaries.

This is the final dyadic `BigFlt` layer. It depends on the exported abstract algebra and order corpus and on structural correctness lemmas exposed by both executable conversion paths.

## Boundary discipline

All quantified theorems are stated over `Bytes` and `BigFlt`. Native `Flt` reinterpretation is no longer opaque — it folds through the binary32 model like every other `Flt` operation — but it remains *unproved* here, since the model is trusted code rather than a Curios proof; that is the boundary these theorems stop at.

Statements avoid postfix `!` in types by carrying successful decode hypotheses as equations.

Reference tests validate executable behavior but do not replace Curios proofs. Conversely, the proofs do not assert algebraic properties of native Wasm floating operations.

## Widening and narrowing round-trip

For `b : Bytes`, `Eq(of_flt_bytes(b), Option/some(x))`, and `b` not equal to the negative-zero pattern, prove:

```text
Eq(to_flt_bytes(x), b)
```

Prove separately that negative zero widens to canonical zero and canonical zero narrows to positive zero. No stronger bitwise round-trip statement is true.

The proof must cover normal values, subnormals, and exponent/significand boundary transitions through the actual executable decoder and encoder.

## Correct rounding of exact values

For `x` whose narrowed output is finite, let `r` be that output widened back. For every finite four-byte pattern `b` widening to `y`, prove:

```text
abs(r - x) <= abs(y - x)
```

The range premise must account for the binary32 overflow rounding boundary `2^128 - 2^103`, not merely the largest finite decoded value. Overflow after rounding must be excluded when `r` is required to widen successfully.

Refine the nearest-value result with:

- exact ties choose the representable value with an even significand;
- the usual half-ulp error corollary in the normal range;
- the corresponding absolute-grid statement for subnormals;
- explicit handling of carry into a larger exponent and signed zero.

## Correct rounding of exact ratios

For a nonzero denominator, prove that `ratio_to_flt_bytes` is the correctly rounded exact quotient.

State comparisons denominator-cleared: cross-multiply inequalities and use absolute value to account for denominator sign. The theorem must not require constructing an interior quotient value.

Prove that the digit loop's leading, guard, sticky, remainder, and exponent invariants imply the same nearest-value and tie-to-even decision used by direct narrowing.

## Proof structure

The proof library should isolate:

- exact interpretation of binary32 normal and subnormal fields;
- power-of-two grid spacing and adjacent representable values;
- guard/sticky characterization of below-half, exact-half, and above-half remainders;
- carry and exponent-boundary behavior;
- overflow threshold behavior;
- denominator-cleared absolute-error comparison for ratios.

Use the abstract BigFlt law layer for error manipulation. If a proof requires opening the private representation beyond the dedicated executable-correctness lemmas, strengthen those lemmas instead of leaking representation details into public theorem statements.

## Soundness discipline

- Every Prop definition uses checked structural elimination, checked inductive case analysis, or recursion on an evident structural subterm.
- Computation of a boundary result is not itself proof of its rounding specification.
- Prefer congruence and explicit `Eq/trans` chains to elaboration-order-sensitive rewrites.
- All proof definitions must remain compatible with future termination and positivity checking.

## Verification

- Exercise theorem statements over representative normal, subnormal, zero, halfway, carry, and exponent-extreme witnesses.
- Confirm every proof and certificate erases.
- Keep generated IEEE-754 reference tests as an independent executable oracle.
- Run the complete repository done bar.

## Completion criteria

- Byte round-trip is proved with the documented negative-zero exception.
- Direct narrowing is proved nearest with the stated overflow premise and ties-to-even refinement.
- Ratio narrowing is proved nearest through denominator-cleared comparisons.
- The formal statements depend only on public abstract laws and byte-level boundary functions.
- Before this specification is deleted, the round-trip, nearest-value, ties-to-even, overflow, and denominator-cleared theorem contracts are recorded in the owning `/std/BigFlt` documentation, theorem signatures, and tests; remaining plans refer to landed theorems rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
