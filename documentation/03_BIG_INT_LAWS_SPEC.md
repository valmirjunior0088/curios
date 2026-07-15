# `BigInt` laws required by `BigFlt`

Working implementation specification for the signed algebra, order, and power-of-two facts required by the [`BigFlt` specification](02_BIG_FLT_SPEC.md).

PT2 delivered canonical packed `BigNat` and `BigInt` operations, but deliberately deferred the complete signed law corpus while no consumer required it. BigFlt is the first consumer that needs those facts. This specification makes that prerequisite explicit instead of silently assuming PT2 already supplied it.

## Objective

Provide the reusable `BigInt` theorem layer needed to prove BigFlt canonicity, exact arithmetic, ordering, and boundary correctness. The layer lifts existing `BigNat` results through the signed representation and proves only the additional structural bit facts required by exact power-of-two scaling.

## Scope

The required signed algebra includes:

- commutativity and associativity of addition and multiplication;
- left and right distributivity;
- additive identity, negation, subtraction, and cancellation;
- multiplication cancellation under the appropriate nonzero premise;
- sign and absolute-value decomposition;
- comparison reflection, reflexivity, antisymmetry, transitivity, and totality;
- addition monotonicity and multiplication monotonicity under sign premises;
- lemmas connecting absolute difference, subtraction, and order.

The required power-of-two layer includes:

- exact multiplication by powers of two;
- structural left and right shift views over packed magnitudes;
- stripping and counting trailing zero bits;
- exact comparison of differently shifted magnitudes;
- interaction between sign, absolute value, shifts, and comparison.

## Ownership boundary

General signed algebra belongs in the `BigInt` module because it is reusable independently of floats. Float-specific leading-bit extraction, guard/sticky accumulation, and quotient-digit scheduling belong to the executable boundary specifications that consume them.

If a helper is naturally a general BigNat or BigInt operation, place its implementation and theorem beside that type even when BigFlt is its first consumer. Do not duplicate sign-case reasoning inside BigFlt proofs merely to keep this work local to the float module.

## Proof strategy

Lift each law from the validated BigNat corpus through the canonical BigInt sign cases. Share sign and magnitude lemmas so commutativity, associativity, distributivity, cancellation, and order do not each reproduce the same case product.

The provable fragment remains structural arithmetic. Native `Nat` subtraction, division, and remainder on symbolic operands are opaque and cannot justify these theorems.

Prefer small reusable lemmas and explicit `Eq/trans` chains to large elaboration-order-sensitive match rewrites. Every proof definition must use checked structural elimination or recursion on an evident structural subterm.

## Deliverables

- A documented signed-law module or focused theorem sections beside `BigInt`.
- The complete algebra and order facts imported by `04_BIG_FLT_CORE_SPEC.md` and `07_BIG_FLT_LAWS_SPEC.md`.
- Power-of-two and shifted-comparison facts required by representation normalization and binary32 conversion.
- Focused positive and boundary tests proving the theorem statements elaborate and erase.

## Non-goals

- Float encodings, guard bits, sticky bits, or binary32 rounding.
- General Euclidean division, `gcd`, coprimality, or reduced rational forms.
- Native `Int` arithmetic laws.
- Moving float-only quotient loops into the general integer API.

## Verification

- Exercise each exported theorem through Curios elaboration.
- Confirm proof fields and theorem arguments erase.
- Test sign boundaries, zero, equal magnitudes with opposite signs, and shifted comparisons.
- Run the repository done bar after implementation.

## Completion criteria

- No BigFlt specification assumes an unstated BigInt ring or order theorem.
- The exported law set is sufficient for BigFlt canonicity, arithmetic transport, ordering, and boundary error comparisons.
- No float-specific algorithm has leaked into the reusable signed algebra layer.
