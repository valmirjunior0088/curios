# Dyadic `BigFlt` algebra and order laws

Post-bootstrap implementation specification for the abstract theorem corpus over the landed canonical dyadic `BigFlt` type.

The proofs consume the exported `/std/BigInt` law layer and the `BigFlt` canonicity kernel. They do not reason about native `Flt` instructions or byte encodings.

## Objective

Provide exactly the algebraic and order facts needed by ordinary exact clients and by the binary32 boundary proofs, without attempting a field theory for a type that deliberately lacks interior division.

## Algebraic laws

Prove and export:

- left and right additive identity;
- addition commutativity and associativity;
- negation and subtraction laws;
- left and right additive cancellation;
- left and right multiplicative identity;
- multiplication commutativity and associativity;
- left and right distributivity;
- left and right multiplication cancellation under the appropriate nonzero premise;
- zero and sign interaction for multiplication and absolute value.

Cancellation is the integral-domain substitute for inverse reasoning. No inverse or field law belongs in this layer.

## Proof staging

For each operation:

1. establish the value equation on raw aligned pairs using `BigInt` laws and power-of-two interaction lemmas;
2. prove the executable operation denotes that raw value;
3. transport the equality through `of_dyadic` using canonical uniqueness.

Do not repeat sign cases inside every `BigFlt` theorem; missing reusable sign reasoning belongs in `/std/BigInt`.

## Order laws

Prove and export:

- comparison reflection;
- reflexivity, antisymmetry, transitivity, and totality;
- strict versus non-strict relation connections;
- addition monotonicity;
- multiplication monotonicity under nonnegativity or the corresponding sign premise;
- order reversal under negation;
- absolute-value nonnegativity and sign elimination;
- subtraction and absolute-difference transformations needed to compare rounding errors.

Antisymmetry concludes structural `Eq` because the certified representation is unique.

## Abstraction constraints

Theorems mention the abstract `BigFlt` type and exported operations. Do not expose a theorem claiming every value is definitionally a dyadic constructor form; clients should survive a future private representation extension.

Every proof uses checked structural elimination, checked inductive case analysis, or recursion on an evident structural subterm. Prefer congruence and explicit `Eq/trans` chains to elaboration-order-sensitive match rewrites.

## Non-goals

- Exact division, inverses, or field laws.
- Binary32 encoding or correct-rounding proofs.
- General rational normalization.
- Laws about native `/sys/Flt` arithmetic.

## Verification

- Exercise every exported theorem through Curios elaboration.
- Confirm all proof values erase.
- Pin zero, sign, cancellation-premise, and order-boundary cases.
- Confirm the boundary proof specification can state its error comparisons using only this public law layer.
- Run the repository done bar.

## Completion criteria

- The exported theorem corpus is sufficient for the byte-boundary proofs without opening the private representation.
- No proof relies on opaque native arithmetic or an unstated BigInt fact.
- All definitions remain compatible with future termination and positivity checking.
- Before this specification is deleted, the public algebra and order theorem surface and its abstraction constraints are recorded in the owning `/std/BigFlt` documentation, theorem signatures, and tests; remaining plans refer to landed laws rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
