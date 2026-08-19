# General `BigFlt` canonical, ring, and order laws

Post-program-analysis implementation specification for lifting the dyadic theorem corpus across the landed reduced rational `BigFlt` representation.

## Objective

Prove canonical reduced-fraction uniqueness and re-establish the abstract ring and order laws over every rational `BigFlt`. Existing public dyadic theorem statements should survive unchanged; general proofs replace their representation-specific internals.

## Canonical uniqueness

Define denominator-cleared equivalence by aligning binary exponents and cross-multiplying positive odd denominators. Prove:

- normalization preserves that equivalence;
- canonical zero is unique;
- nonzero canonical equivalence implies equal exponents after separating powers of two;
- coprime reduced odd numerators and denominators are unique;
- equal canonical values have structurally equal fields and therefore `Eq` values.

The reduced-fraction step consumes the landed `/std/BigNat` `Divides`, `Coprime`, Euclid's lemma, and exact-division facts. Do not reproduce a second divisibility theory inside `BigFlt`.

## Ring laws

Re-establish and export:

- additive and multiplicative identities;
- addition and multiplication commutativity and associativity;
- negation and subtraction laws;
- left and right distributivity;
- additive cancellation;
- multiplication cancellation under nonzero premises;
- zero, sign, and absolute-value interaction.

Theorem names remain operation-first snake_case and preserve the existing `/std/BigFlt` API wherever statements agree with the dyadic phase.

## Order laws

Prove:

- comparison reflection and executable-predicate agreement;
- reflexivity, antisymmetry, transitivity, and totality;
- strict and non-strict relation connections;
- addition monotonicity;
- multiplication monotonicity under sign premises;
- order reversal under negation;
- absolute-value nonnegativity and sign elimination;
- subtraction and absolute-difference transformations used by rounding proofs.

Positive denominators permit cross multiplication without reversing order. Binary-exponent alignment uses the standalone `BigInt` binary-scale facts rather than native shifts.

## Migration verification

For each theorem exported by the dyadic `/std/BigFlt` law layer:

1. retain the public statement when it remains mathematically valid;
2. replace dyadic representation proofs with denominator-cleared rational proofs;
3. add a compatibility fixture showing existing downstream source still elaborates;
4. remove no theorem merely because its old proof opened the private representation.

If a theorem truly exposed a dyadic-only premise, replace it with an abstract statement before completing the migration and document the compatibility decision.

## Non-goals

- Reciprocal, division, inverse, or field laws, which belong to the following field-law effort.
- Binary32 encodings or correct-rounding proofs.
- Decimal parsing or presentation.
- Public representation projections or a rational eliminator.

## Completion criteria

- Canonical reduced rational representation is provably unique.
- Every retained ring and order theorem holds for general denominators.
- Existing abstract dyadic clients continue to elaborate without source changes.
- No proof relies on opaque native arithmetic or an unstated coprimality fact.
- All theorem values erase and the repository done bar passes.
- Before this specification is deleted, canonical uniqueness, the retained ring and order theorem surface, and compatibility guarantees are recorded in the owning `/std/BigFlt` documentation, theorem signatures, and tests; remaining plans refer to landed laws rather than this file; the roadmap subitem is a checked unlinked summary; and no reference to this filename remains.
