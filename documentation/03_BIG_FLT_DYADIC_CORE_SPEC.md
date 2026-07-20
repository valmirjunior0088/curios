# Dyadic `BigFlt` core representation and operations

Entry-point implementation specification for the first `BigFlt` phase: a certified, representation-private canonical dyadic rational with exact arithmetic and executable comparison. In the chosen project order this work follows the independent native-width `Toml` codec; the complete standalone `BigInt` law project is deliberately deferred until immediately after bootstrap.

This specification and executable binary32 conversion complete the `BigFlt` representation work required before bootstrap. The larger theorem and quotient-boundary projects are deliberately deferred until afterward.

## Objective

Introduce a private `BigFlt` whose inhabitants denote unique exact elements of ℤ[1/2]. Exact closed arithmetic lives over `BigInt`; native `Flt` remains an opaque pragmatic runtime facility used only at explicit conversion boundaries.

## Actual `BigInt` dependency

The complete ordered-ring corpus is not a hard prerequisite of the executable dyadic core. This effort adds or imports only:

- sign and magnitude reflection;
- trailing-two stripping and reconstruction;
- exact power-of-two scaling;
- comparison of differently shifted magnitudes;
- odd-mantissa cancellation and uniqueness facts.

These focused facts remain owned by `/std/BigInt` and become foundations reused by the later standalone corpus. The remaining additive, multiplicative, monotonicity, and absolute-difference laws are completed immediately after bootstrap and consumed by the dyadic theorem and boundary-proof projects.

## Permanent design decisions

**Canonical dyadic representation.** A value is represented as `mantissa · 2^exponent`, with a unique odd-mantissa form for every nonzero value and one canonical zero.

**Repository-native construction names.** `BigFlt/of_dyadic` is the normalizing smart constructor and `BigFlt/of_big_int` is its exponent-zero convenience. Public conversions use `of_*` and `to_*`; this API does not introduce `mk`, `widen`, `narrow`, or opaque `_b` suffixes.

**Certificates only where fields interact.** `BigFlt` carries a joint canonicity certificate because mantissa and exponent together determine whether a representation is reduced. Proof irrelevance and erasure keep the certificate out of runtime data.

**Representation privacy preserves future mobility.** Exported laws mention only the abstract type and its operations. No public theorem exposes a complete dyadic case analysis, so the later general rational phase may add an odd denominator without invalidating clients.

**Rounding stays at native boundaries.** The dyadic core has no exact interior division and no `Div(BigFlt)` witness. Exact quotient rounding is a separate post-bootstrap boundary operation.

## Representation

```crs
pub struct BigFlt : Type {
    mantissa : BigInt,
    exponent : BigInt,
    canonical : Canonical(mantissa, exponent)
}
```

The representation is private. A canonical nonzero value has an odd mantissa. Canonical zero has both mantissa and exponent equal to zero. `BigInt` already excludes a signed-zero magnitude.

## Smart construction and canonicity

```text
of_big_int : BigInt -> BigFlt
of_dyadic : BigInt -> BigInt -> BigFlt
```

`of_dyadic(mantissa, exponent)` strips trailing zero bits from the magnitude, adds the stripped count to the exponent, and forces canonical zero. `of_big_int(value)` calls `of_dyadic(value, BigInt/zero)`.

The canonicity kernel establishes:

- **strip correctness:** normalization preserves the represented value and returns an odd-or-zero mantissa;
- **uniqueness:** equal nonzero values with odd mantissas have equal mantissas and exponents;
- **canonical zero:** every raw zero normalizes to one representation;
- **construction congruence:** equal raw aligned-pair values produce structurally equal certified results.

Certificates are proof-irrelevant and never inspected as meaningful data. If an equality proof depends on distinguishing certificate inhabitants, the statement or reduction route is wrong.

## Exact operations

Addition and subtraction align exponents, shift the appropriate mantissa, perform one exact `BigInt` addition or subtraction, and call `of_dyadic`. Multiplication multiplies mantissas, adds exponents, and calls `of_dyadic`. Negation and absolute value act on the mantissa while preserving or rebuilding the certificate.

The core exports:

```text
zero, one : BigFlt
of_big_int : BigInt -> BigFlt
of_dyadic : BigInt -> BigInt -> BigFlt
add, sub, mul : BigFlt -> BigFlt -> BigFlt
neg, abs : BigFlt -> BigFlt
eql : BigFlt -> BigFlt -> Bool
cmp : BigFlt -> BigFlt -> Order
lt, lte, gt, gte : BigFlt -> BigFlt -> Bool
```

Equality is structural because certification gives a unique representation. Comparison aligns exact values and delegates signed magnitude comparison to `BigInt`.

## Proposition-level relations

Reflect executable booleans rather than introducing unrelated inductives:

```text
Lte(x, y) := Eq(BigFlt/lte(x, y), true)
Lt(x, y) := Eq(BigFlt/lt(x, y), true)
NonZero(x) := Eq(BigFlt/eql(x, zero), false)
```

PascalCase is reserved for these proposition-valued interfaces; executable predicates remain lowercase.

## Module and witness placement

`curios-prelude/std/BigFlt.crs` is registered after `BigNat` and `BigInt` in `curios-prelude/std.crs`:

```crs
pub mod BigFlt;
pub use BigFlt/{let BigFlt};
```

`Add`, `Sub`, `Mul`, `Eql`, and `Cmp` witnesses belong in the existing `/std` operator façade modules. `Show` and `Ord` may be supplied where useful. There is no dyadic `Div(BigFlt)` witness.

`BigFlt` belongs entirely to `/std`; no compiler lowering emits it, so `/syn` and the canonical syntax registry remain unchanged.

## Soundness discipline

- `of_dyadic` stripping recurses structurally over packed low bits.
- Every constructor result carries checked canonicity evidence.
- The certificate erases and does not affect runtime equality or layout.
- Public operations and laws do not expose a complete representation eliminator.
- Exponents remain `BigInt`; exact computations do not reintroduce native `Int` overflow.
- Open native floating operations do not participate in exact core proofs.

## Non-goals

- Laws about native `/sys/Flt` arithmetic.
- The complete dyadic ring and order theorem corpus, which is a post-bootstrap effort.
- Exact interior division, field laws, or a `Div(BigFlt)` witness.
- General Euclidean division, GCD, coprimality, or reduced rational normalization.
- Exact square roots, constructive reals, or exact decimal arithmetic.
- `BigFlt/of_str` or decimal formatting.
- Replacing native `Int` or `Flt` as pragmatic runtime defaults.

## Verification

- Test normalization of zero, positive and negative odd mantissas, and values with long powers-of-two factors.
- Test structural equality of distinct raw representations after `of_dyadic`.
- Test exact arithmetic and comparison against an arbitrary-precision reference.
- Confirm certificate erasure and the intended two-integer runtime layout.
- Confirm every witness dispatches to the exact operation owned by `/std/BigFlt`.
- Run the repository done bar.

## Completion criteria

- Every public dyadic `BigFlt` value is canonical by construction.
- Exact operations preserve mathematical value and canonicity.
- Equality and comparison agree with represented dyadic values.
- No exact interior division or native floating operation appears in the core API.
- The public contract leaves no dyadic-only theorem that would block the later private odd-denominator extension.
- Before this specification is deleted, representation privacy, the canonical invariant, construction and operation contracts, the focused `BigInt` dependencies, and the absence of interior division are recorded in the owning `/std/BigFlt` documentation and tests; remaining plans refer to the landed API rather than this file; the roadmap entry is a checked unlinked summary; and no reference to this filename remains.
