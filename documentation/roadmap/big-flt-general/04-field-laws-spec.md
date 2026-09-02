# General `BigFlt` division and field laws

Post-program-analysis implementation specification for the division-specific API and theorem corpus over canonical nonzero rational `BigFlt` values. This work follows the core and general ring/order laws rather than blocking their delivery.

## Objective

Specify `reciprocal` and `div` completely and prove that general `BigFlt` forms a field when division by zero is excluded. Keep zero handling explicit through `Option` and reflected nonzero propositions.

## Executable contracts

```text
reciprocal : BigFlt -> Option(BigFlt)
div : BigFlt -> BigFlt -> Option(BigFlt)
```

Required behavior:

- `reciprocal(zero) = none`;
- `div(x, zero) = none`;
- a nonzero reciprocal exchanges numerator magnitude and denominator, transfers sign, negates the power-of-two exponent, and normalizes;
- successful division equals multiplication by the successful reciprocal;
- no special infinity, NaN, or signed-zero value enters the exact type.

Whether a global `Divide(BigFlt)` witness is added is a deliberate API decision: the concept requires a total `BigFlt -> BigFlt -> BigFlt` operation, while exact division is partial at zero. The default is no witness unless the concept or a nonzero-denominator wrapper supplies an honest total domain.

## Laws

Under explicit nonzero premises, prove:

- reciprocal is nonzero;
- reciprocal involution;
- left and right multiplicative inverse;
- quotient reconstruction;
- division by one and self-division;
- reciprocal of a product;
- division cancellation;
- sign and absolute-value behavior;
- order interaction for positive and negative divisors.

State theorems over successful `Option/some` equations or a named `NonZero` proposition; do not use postfix `!` in types.

## Proof strategy

Reduce reciprocal identities to canonical rational normalization, then use the ring, cancellation, order, and coprimality layers. Shared denominator-cleared lemmas belong in the general law specification if clients beyond division use them.

Avoid proving field identities by opening proof certificates. Equality concludes through canonical uniqueness.

## Verification

- Pin zero, one, negative one, dyadic, odd-denominator, and cross-cancelling quotients.
- Compare successful division with an arbitrary-precision rational reference.
- Exercise every law over symbolic nonzero evidence.
- Confirm no `Divide(BigFlt)` witness exists unless its totality contract is separately approved.
- Confirm proof erasure and run the repository done bar.

## Non-goals

- Division by zero conventions involving infinity or NaN.
- Native floating division laws.
- Rational exponentiation, roots, transcendental operations, or algebraic closures.
- Decimal or binary formatting.

## Completion criteria

- Partial executable division is fully specified and reference-tested.
- The field laws hold under explicit nonzero premises.
- Zero is never hidden behind a default quotient or runtime trap.
- Public witness behavior remains mathematically honest.
- Before this specification is deleted, partial division semantics, the nonzero-premised field theorem surface, and the `Divide(BigFlt)` witness decision are recorded in the owning `/std/BigFlt` documentation, signatures, and tests; remaining plans refer to landed operations and laws rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
