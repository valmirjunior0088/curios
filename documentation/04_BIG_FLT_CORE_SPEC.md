# `BigFlt` core representation and operations

Working implementation specification for the canonical dyadic representation, exact executable operations, comparison, and standard-library integration required by [`02_BIG_FLT_SPEC.md`](02_BIG_FLT_SPEC.md).

This work begins after [`03_BIG_INT_LAWS_SPEC.md`](03_BIG_INT_LAWS_SPEC.md) supplies the signed facts needed by the canonicity kernel and exact operations.

## Objective

Introduce a representation-private `BigFlt` whose inhabitants denote unique exact dyadic values and whose executable operations preserve that representation.

## Representation

```crs
pub struct BigFlt : Type {
    mantissa : BigInt,
    exponent : BigInt,
    canonical : Canonical(mantissa, exponent)
}
```

The mathematical value is `mantissa · 2^exponent`. A canonical value has an odd nonzero mantissa, or is the single zero representation with both mantissa and exponent zero. `BigInt` already excludes a signed zero mantissa.

The representation remains private. External code constructs values through exported operations and consumes only abstract laws.

## Smart construction and canonicity

The total smart constructor `mk(mantissa, exponent)` strips trailing zero bits from the magnitude, adds the stripped count to the exponent, and forces canonical zero.

The canonicity kernel must establish:

- **strip correctness:** normalization preserves the represented value and returns an odd-or-zero mantissa;
- **uniqueness:** equal nonzero values with odd mantissas have equal mantissas and exponents;
- **canonical zero:** every raw zero normalizes to one representation;
- **construction congruence:** equal raw aligned-pair values produce structurally equal certified results.

Certificates are proof-irrelevant and never inspected as meaningful data. If an equality proof depends on distinguishing certificate inhabitants, the statement or reduction route is wrong.

## Exact operations

Addition and subtraction align exponents, shift the appropriate mantissa, perform one exact `BigInt` addition or subtraction, and call `mk`. Multiplication multiplies mantissas, adds exponents, and calls `mk`. Negation and absolute value act on the mantissa while preserving or rebuilding the certificate as appropriate.

Stage 1 exports:

```text
zero, one : BigFlt
mk : BigInt -> BigInt -> BigFlt
add, sub, mul : BigFlt -> BigFlt -> BigFlt
neg, abs : BigFlt -> BigFlt
eql : BigFlt -> BigFlt -> Bln
cmp : BigFlt -> BigFlt -> Order
lt, lte, gt, gte : BigFlt -> BigFlt -> Bln
```

Equality is structural because certification gives a unique representation. Comparison aligns exact values and delegates signed magnitude comparison to `BigInt`.

## Proposition-level relations

Define relations by reflection from executable booleans rather than by introducing unrelated inductives:

```text
Lte(x, y) := Eq(BigFlt/lte(x, y), true)
Lt(x, y) := Eq(BigFlt/lt(x, y), true)
NonZero(x) := Eq(BigFlt/eql(x, zero), false)
```

The deeper algebraic and order theorem corpus belongs to [`07_BIG_FLT_LAWS_SPEC.md`](07_BIG_FLT_LAWS_SPEC.md).

## Module and witness placement

`curios-prelude/std/BigFlt.crs` is registered after BigNat and BigInt in `curios-prelude/std.crs`; the prelude build script discovers and fingerprints the registered source automatically.

`Add`, `Sub`, `Mul`, `Eql`, and `Cmp` witnesses belong in the `/std` operator facade modules, following project convention. `Show` and `Ord` may be supplied where useful. There is no `Div(BigFlt)` witness.

Presentation functions may consume `Str` and `Char` but must not depend on their representation.

## Soundness discipline

- `mk` stripping recurses structurally over packed low bits.
- Every constructor result carries checked canonicity evidence.
- The certificate erases and does not affect runtime equality or layout.
- Export only abstract operations and laws; do not export a complete dyadic representation eliminator.
- Keep exponents as `BigInt`; do not reintroduce native `Int` overflow into exact computations.

## Verification

- Test normalization of zero, positive and negative odd mantissas, and values with long powers-of-two factors.
- Test structural equality of distinct raw representations after `mk`.
- Test exact arithmetic and comparison against an arbitrary-precision reference.
- Confirm certificate erasure and the intended runtime field layout.
- Confirm all witnesses dispatch to the exact operations.
- Run the repository done bar.

## Completion criteria

- Every public `BigFlt` value is canonical by construction.
- Exact operations preserve mathematical value and canonicity.
- Equality and comparison agree with the represented dyadic values.
- No exact interior division or native floating operation appears in the core API.
