# `BigFlt` specification

Umbrella specification for exact dyadic arithmetic, conversion to and from native binary32 values, and the proof obligations shared by those capabilities. The work is divided into focused implementation specifications so each prerequisite, executable layer, and theorem layer can be reviewed and completed independently.

The representation series supplies primitive `Byte`, `Bits`, and `Bytes`; packed `BigNat` and `BigInt`; and the final [`Char`/`Str` presentation layer](SYNTAX.md#character-and-string-literals). `BigFlt` completes the series without changing those representations.

This is a working reference rather than permanent architecture documentation. Fold durable conclusions into `AGENTS.md`, `ROADMAP.md`, relevant rustdoc, and standard-library documentation as they land, then delete the working specifications when the arc is complete.

## Objective

Stage 1 ships `BigFlt` as a certified, representation-private canonical dyadic rational: an arbitrary-precision element of ℤ[1/2]. It supports exact closed arithmetic and explicit, correctly rounded conversion at the native `Flt` boundary.

Native `Flt` arithmetic remains an opaque pragmatic runtime facility. `BigFlt` supplies the mathematical layer; it does not assert algebraic laws about Wasm floating-point instructions.

## Permanent design decisions

**Exact mathematical value, opaque native wrapper.** All algebra and proofs live over `BigInt` and `BigFlt`. Native `Flt` appears only at explicit byte reinterpretation boundaries.

**Canonical dyadic representation.** A value is represented as `mantissa · 2^exponent`, with a unique odd-mantissa form for every nonzero value and one canonical zero.

**Certificates only where fields interact.** `BigFlt` carries a joint canonicity certificate because mantissa and exponent together determine whether a representation is reduced. Proof irrelevance and erasure keep the certificate out of runtime data.

**Representation privacy preserves future mobility.** `BigFlt` remains a representation-private `struct`. Exported laws mention only the abstract type and its operations; no public theorem exposes a complete dyadic case analysis.

**Correct rounding happens at the boundary.** Stage 1 has no exact interior division operation and no `Div(BigFlt)` witness. `narrow_b` and `narrow_ratio_b` round exact values directly to binary32 using round-to-nearest-even.

**Byte-level theorem boundary.** Open native `Flt` primitives are opaque to reduction and conversion. Formal theorems therefore relate `Bytes` and `BigFlt`; native wrappers remain thin functions over `Flt/to_le_bytes` and `Flt/of_le_bytes`.

## Sub-specifications

| Specification | Responsibility |
| --- | --- |
| [`03_BIG_INT_LAWS_SPEC.md`](03_BIG_INT_LAWS_SPEC.md) | Signed integer algebra, order, and power-of-two facts required by BigFlt |
| [`04_BIG_FLT_CORE_SPEC.md`](04_BIG_FLT_CORE_SPEC.md) | Canonical representation, smart construction, exact operations, executable comparison, and module integration |
| [`05_BIG_FLT_BINARY32_SPEC.md`](05_BIG_FLT_BINARY32_SPEC.md) | Native byte reinterpretation, exact widening, correctly rounded narrowing, and behavioral tests |
| [`07_BIG_FLT_LAWS_SPEC.md`](07_BIG_FLT_LAWS_SPEC.md) | BigFlt ring, cancellation, order, monotonicity, and absolute-value theorem corpus |
| [`08_BIG_FLT_RATIO_NARROWING_SPEC.md`](08_BIG_FLT_RATIO_NARROWING_SPEC.md) | Correctly rounded quotient extraction without adding exact interior division |
| [`09_BIG_FLT_BOUNDARY_PROOFS_SPEC.md`](09_BIG_FLT_BOUNDARY_PROOFS_SPEC.md) | Round-trip, nearest-value, ties-to-even, half-ulp, and ratio-correctness proofs |

The dependency order is:

```text
BIG_INT_LAWS
      │
      ├──> BIG_FLT_CORE ──> BIG_FLT_LAWS
      │          │                  │
      │          ├──> BIG_FLT_BINARY32 ──┐
      │          │                       ├──> BIG_FLT_BOUNDARY_PROOFS
      │          └──> BIG_FLT_RATIO ─────┘
```

The executable binary32 and ratio work may proceed alongside parts of the law corpus once the core representation exists. The final boundary proofs require all preceding layers.

## Goals

- A certified, representation-private canonical dyadic `BigFlt`.
- Exact `add`, `sub`, `mul`, `neg`, and `abs` with the algebraic and order laws needed by clients and boundary proofs.
- Exact widening of every finite binary32 byte pattern.
- Correctly rounded narrowing of exact dyadics and exact quotients using round-to-nearest-even.
- Byte-level round-trip, nearest-value, tie-to-even, half-ulp, and denominator-cleared quotient theorems.
- Thin `widen`, `narrow`, and `narrow_ratio` wrappers over native `Flt` reinterpretation.
- Zero postulates and proof definitions compatible with future termination and positivity checking.

## Non-goals

- Laws about native `/sys/Flt` arithmetic.
- Exact interior division, field laws, or a `Div(BigFlt)` witness.
- A rational-denominator extension without a demonstrated workload.
- Exact square roots, constructive reals, or exact decimal arithmetic.
- Replacing native `Int` or `Flt` as pragmatic runtime defaults.
- A postulate mechanism or trusted arithmetic oracle.
- `BigFlt/of_str`; decimal parsing would introduce rounding into the exact interior API.

## Future compatibility constraint

A future rational extension may add an odd denominator when a real workload requires exact quotients as interior operands. Stage 1 preserves that option only by keeping the representation private, exporting abstract laws, and avoiding public theorems that claim every `BigFlt` has a dyadic constructor form. The extension itself is not part of this specification set.

## Completion order

1. Complete [`03_BIG_INT_LAWS_SPEC.md`](03_BIG_INT_LAWS_SPEC.md).
2. Complete [`04_BIG_FLT_CORE_SPEC.md`](04_BIG_FLT_CORE_SPEC.md).
3. Complete the executable and law layers in their dependency order.
4. Complete [`09_BIG_FLT_BOUNDARY_PROOFS_SPEC.md`](09_BIG_FLT_BOUNDARY_PROOFS_SPEC.md).
5. Run the repository done bar, confirm proof erasure and runtime representation, and update permanent documentation.

## Overall completion criteria

- Every sub-specification is complete.
- `BigFlt` runtime values contain only the intended integer fields and no proof objects.
- The native boundary handles normals, subnormals, both signed zeros, infinities, NaNs, overflow, underflow, halfway cases, and significand carries according to the documented contracts.
- Generated behavioral comparisons agree with a trusted IEEE-754 reference, while Curios proofs independently establish the specified mathematical results.
- `AGENTS.md`, `ROADMAP.md`, standard-library documentation, and relevant rustdoc describe the landed design.
