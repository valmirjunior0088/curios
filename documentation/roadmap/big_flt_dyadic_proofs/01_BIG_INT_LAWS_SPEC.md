# `BigInt` algebra, order, and binary-scale laws

Post-bootstrap implementation specification for completing the reusable theorem layer over the existing canonical packed `/std/BigInt` representation. This is the first standard-library theorem effort immediately after bootstrap and is complete independently of any floating-point consumer.

## Objective

Establish `BigInt` as a certified ordered commutative ring with executable comparison and reusable power-of-two operations. Every theorem belongs beside the operation it describes in `curios-prelude-archive/std/BigInt.crs`, uses the established snake_case naming convention, and erases completely.

The existing `BigInt` representation and executable operations remain authoritative. This project adds the missing laws, structural helpers, and focused tests; it does not redesign the sign representation.

## Algebraic laws

Prove and export:

- left and right additive identity;
- addition commutativity and associativity;
- left and right additive inverse;
- subtraction expressed through addition and negation;
- left and right additive cancellation;
- left and right multiplicative identity;
- multiplication commutativity and associativity;
- left and right distributivity;
- multiplication by zero and sign interaction;
- left and right multiplication cancellation under the appropriate nonzero premise;
- absolute-value and sign decomposition.

Public theorem names follow the existing `/std/BigNat` vocabulary where the statements agree: `add_comm`, `add_assoc`, `mul_comm`, `mul_assoc`, `distrib_l`, `distrib_r`, and operation-first names such as `add_zero_l`, `add_cancel_r`, and `neg_involutive` for the remaining facts.

## Order laws

Prove and export:

- comparison reflection: `cmp(a, b) = eq` implies `Eq(a, b)`;
- comparison reflexivity and flip symmetry;
- strict-order transitivity and totality;
- equivalence between `cmp`, `eql`, `lt`, `lte`, `gt`, and `gte`;
- antisymmetry of non-strict order;
- addition monotonicity;
- multiplication monotonicity under nonnegative or nonpositive sign premises;
- order reversal under negation;
- sign and absolute-value order facts;
- subtraction and absolute-difference transformations.

Reuse `/std/Order` and `/std/Eq` rather than introducing a parallel relation hierarchy. Boolean operations remain lowercase; proposition-valued relations introduced for public theorem statements use PascalCase only when a named proposition materially improves the API.

## Binary-scale laws

Add the executable helpers and structural facts needed to treat powers of two without native `Int` overflow:

- exact multiplication by powers of two;
- structural left- and right-shift views over packed magnitudes;
- stripping and counting trailing zero bits;
- reconstruction after stripping;
- exact comparison of differently shifted magnitudes;
- interaction between sign, absolute value, shifts, parity, and comparison.

General helpers belong in `/std/BigNat` when they operate only on unsigned magnitudes and in `/std/BigInt` when sign is semantically relevant. Low-level helpers use the established `_raw` suffix; public constructors and conversions use `of_*` and `to_*` names.

## Proof strategy

Lift reusable unsigned facts from `/std/BigNat` through the three `BigInt` sign cases. Establish shared sign, magnitude, and congruence lemmas once instead of reproducing the same case product in each public theorem.

Every proof uses checked structural elimination, checked inductive case analysis, or recursion on an evident structural subterm. Native `Nat` or `Int` division, remainder, shifts, and subtraction on symbolic values are executable operations, not proof oracles.

## Non-goals

- General division, remainder, Euclidean division, greatest common divisors, divisibility, or coprimality over packed integers.
- Rational normalization or reduced-fraction uniqueness.
- Floating-point representations, encodings, rounding, guard bits, or sticky bits.
- Field laws, multiplicative inverses, or exact quotients.
- Redesigning `BigNat`, `NonZero`, or `BigInt` runtime representations.

Those Euclidean capabilities are specified separately as a prerequisite of the later general rational `BigFlt` extension.

## Implementation order

1. Inventory the existing `/std/BigNat` theorem corpus and add only missing unsigned facts with independent value.
2. Add shared `BigInt` sign, magnitude, equality, and congruence lemmas.
3. Complete the additive and multiplicative laws.
4. Complete comparison reflection, order, and monotonicity.
5. Add binary-scale helpers and their reconstruction and comparison laws.
6. Exercise every exported theorem, confirm proof erasure, and run the repository done bar.

## Completion criteria

- The documented algebraic, order, and binary-scale laws are exported under `/std/BigInt` with repository-consistent names.
- Every public theorem elaborates over symbolic inputs without relying on opaque native arithmetic.
- Proof arguments and results erase and do not change the runtime layout of `BigInt` values.
- Focused tests cover zero, opposite signs, equal magnitudes, cancellation premises, order boundaries, and long packed power-of-two factors.
- The specification is complete without requiring `BigFlt`, TOML, or any other downstream consumer to exist.
- Before this specification is deleted, the exported theorem contracts, binary-scale invariants, and proof strategy needed by clients are recorded in the owning `/std/BigInt` documentation, theorem signatures, and tests; remaining plans refer to landed laws rather than this file; the roadmap entry is a checked unlinked summary; and no reference to this filename remains.
