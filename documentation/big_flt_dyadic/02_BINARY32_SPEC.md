# Dyadic `BigFlt` and binary32 conversion

Working implementation specification for native byte reinterpretation, exact conversion from binary32, correctly rounded conversion to binary32, and behavioral validation after the dyadic `BigFlt` core has landed.

This pre-bootstrap document specifies executable conversion behavior. Formal round-trip and optimality theorems belong to the post-bootstrap boundary-proof effort.

## Boundary architecture

Native `Flt` is IEEE-754 binary32 stored bitwise in `curios-base/src/flt.rs`. Term identity is bitwise: NaN equals itself as a term and `+0.0` differs from `-0.0`, unlike IEEE numeric equality.

Open native intrinsics are opaque to reduction. `Flt/to_le_bytes` and `Flt/of_le_bytes` are therefore the explicit trust boundary, while all inspectable conversion logic operates on `Bytes`.

Do not add a conversion rule asserting `of_le_bytes(to_le_bytes(x)) ≡ x`; that would be a postulate disguised as reduction.

## `Flt/of_le_bytes`

```crs
Flt/of_le_bytes : Bytes -> Flt
```

The intrinsic requires exactly four little-endian bytes, assembles their reflected Nat values into an i32 bit pattern, and emits `F32ReinterpretI32`. Invalid lengths trap through the established intrinsic-level `Unreachable` behavior.

The intrinsic uses `Byte/to_nat` internally; no user-visible Nat-byte convention remains.

Its compiler footprint includes the intrinsic models and printers across Core, Ersd, Cont, Wasm lowering, text lowering, standard-library exposure, optimization walkers, scalar evaluation, and codegen tests. Preserve round-trip fixtures for positive and negative zero, normal values, subnormals, payloaded NaNs, and both infinities. Compare bytes, never native `Flt` equality.

## Exact conversion from binary32

```crs
BigFlt/of_flt_bytes : Bytes -> Option(BigFlt)
```

Return `none` unless the input length is exactly four. Decode the binary32 fields as follows:

- exponent `1..254`: `of_dyadic(±(mantissa_field + 0x800000), exponent_field - 150)`;
- exponent `0` with a nonzero mantissa: `of_dyadic(±mantissa_field, -149)`;
- either signed-zero pattern: canonical BigFlt zero;
- exponent `255`: `none` for infinity and NaN.

Field extraction reflects bytes to Nat only at arithmetic boundaries and should share any existing native-Flt decoding helper rather than duplicating bit layout.

Nothing rounds during conversion. The native wrapper is:

```text
of_flt(f) = of_flt_bytes(Flt/to_le_bytes(f))
```

Collapsing negative zero to canonical mathematical zero is deliberate.

## Correctly rounded conversion to binary32

```crs
BigFlt/to_flt_bytes : BigFlt -> Bytes
```

Compute the unbiased binary exponent from the magnitude bit length and exact exponent. Then:

- overflow after rounding produces the appropriate signed infinity pattern;
- normal-range values retain the leading 24 significand bits, accumulate guard and sticky information, round to nearest with ties to even, propagate a carry, and recheck overflow;
- subnormal-range values round on the `2^-149` grid rather than retaining a fixed 24 leading bits;
- a nonzero value that rounds to zero preserves its sign in the emitted byte pattern.

Use structural bit views, exact shifts, and comparisons. Do not implement conversion through opaque native division.

The native wrapper is:

```text
to_flt = Flt/of_le_bytes ∘ to_flt_bytes
```

## Integer helpers owned by this boundary

This work may add helpers for bit length, leading precision extraction, guard/sticky accumulation, and exact comparison of shifted magnitudes. Put generally reusable operations beside BigNat or BigInt, but keep float-specific scheduling and rounding policy in this boundary layer.

Every helper used by a proof needs a structural specification connecting it to exact multiplication by powers of two and comparison. Native Nat division and remainder on symbolic operands are not proof oracles.

## Behavioral verification

- Test conversion from binary32 for normals, subnormals, both zeros, infinities, and multiple NaN payloads.
- Test conversion to binary32 at normal/subnormal boundaries, the binary32 overflow boundary, underflow, exact halfway cases, and significand carry.
- Compare emitted bytes with a trusted correctly rounded IEEE-754 reference over a broad generated corpus.
- Test `to_flt_bytes(of_flt_bytes(bytes))` behavior separately from the formal theorem suite.
- Benchmark the boundary loops and record pathological exponent or magnitude behavior.

## Non-goals

- `ratio_to_flt_bytes`, which belongs to the post-bootstrap ratio-narrowing effort.
- Formal nearest-value or ties-to-even proofs.
- Laws about native floating arithmetic.
- Decimal parsing or formatting.

## Completion criteria

- Every finite binary32 pattern converts to its exact mathematical value.
- Every `BigFlt` value converts to binary32 according to the specified round-to-nearest-even behavior.
- Behavioral reference tests cover all format boundaries and special byte patterns.
- The executable algorithm exposes the structural facts required by the boundary proof layer.
- Before this specification is deleted, the byte reinterpretation trust boundary, exact decode contract, rounding policy, and structural helper obligations are recorded in the owning `/std/Flt`, `/std/BigFlt`, and applicable compiler documentation and tests; remaining plans refer to landed functions and lemmas rather than this file; the roadmap entry is a checked unlinked summary; and no reference to this filename remains.
