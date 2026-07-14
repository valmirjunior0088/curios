# Representation specification PT4 — exact `BigFlt` and native-float boundaries

Working implementation specification for exact binary-rational arithmetic (`BigFlt`), proof-carrying conversion to and from native `Flt` byte patterns, and the obligations needed exclusively by that layer. This is the final part of the ordered representation series: [PT1](REPRESENTATION_SPEC_PT1_BIN.md) supplies primitive `Byte` and packed Bin; [PT2](REPRESENTATION_SPEC_PT2_NUMERIC.md) supplies packed `BigNat` and `BigInt`; [PT3](REPRESENTATION_SPEC_PT3_CHARACTER.md) supplies the final `Char`/`Str` presentation layer.

Stage 1 ships `BigFlt` as a certified dyadic rational, an arbitrary-precision element of ℤ[1/2]. Stage 2 is deferred until a real workload demands exact interior division; it extends the same private carrier with an odd denominator, yielding full ℚ while preserving dyadics as the `denominator = 1` stratum.

This is a working reference rather than permanent architecture documentation. Fold durable conclusions into `AGENTS.md`, `ROADMAP.md`, relevant rustdoc, and standard-library documentation as they land, then delete the complete representation-specification series.

## Why this work is postponed

`BigFlt` depends on the exact integer layers but does not determine their representation. Mixing its rounding loops, float-byte primitives, order bounds, and future rational contract into PT2 would enlarge the critical packed-BigNat and conversion experiment with obligations that no current integer consumer needs.

PT4 therefore owns every requirement forced exclusively by `BigFlt`:

- the `Flt/of_le_bin` primitive and its byte-pattern tests;
- BigNat helpers used only for float top-bit, guard, sticky, or quotient extraction;
- BigFlt-specific canonicity, arithmetic, order, and reflection lemmas;
- `widen_b`, `narrow_b`, and `narrow_ratio_b`;
- correctly rounded byte-boundary theorems;
- Flt-facing wrappers;
- the deferred denominator extension and its strong-induction, divmod, gcd, and reduced-form proof obligations.

General BigNat and BigInt operations and ring/order laws remain in PT2 even where PT4 consumes them, because they are honest properties of those standalone types.

## Motivation

Native `Flt` arithmetic compiles to opaque `/sys/Flt` Wasm primitives. It carries no algebraic laws and cannot have a structural induction principle: IEEE-754 addition and multiplication are not associative and `Flt` is not a free monoid on a generator.

The opacity also fixes the boundary architecture. Primitive applications on open terms are stuck, conversion over them is congruence-only, and there is no Flt eliminator. A quantified theorem mentioning `Flt/to_le_bin(f)` or `Flt/of_le_bin(bytes)` for open `f` cannot prove even that the resulting byte sequence has length four. Consequently every theorem in PT4 is stated at the byte level:

```text
Bin/X -> BigFlt -> Bin/X
```

Flt-facing functions are thin unproved wrappers around `to_le_bin` and `of_le_bin`. Those reinterpret primitives are the explicit trust boundary. Do not add a conversion rule asserting `of_le_bin(to_le_bin(x)) ≡ x`; that would be a postulate disguised as reduction.

`BigFlt` represents `mantissa · 2^exponent` with `mantissa : BigInt` and `exponent : BigInt`. Every finite `f32` is exactly dyadic, so widening loses nothing. Addition, subtraction, and multiplication are closed and exact; canonicalization requires only stripping powers of two from the mantissa. Interior division is deliberately absent from stage 1. Formulas can clear denominators, while boundary quotients use a correctly rounded `narrow_ratio`, which is stronger and more useful than exact-divide-then-round.

Flocq and HOL float formalizations provide the relevant precedent: reason about finite binary floats as exact `m · 2^e` values, not as the behavior of native floating instructions.

## Design keystones

**Exact mathematical value, opaque native wrapper.** All algebra and proofs live over BigInt and BigFlt. Native `Flt` appears only at explicit byte reinterpret boundaries.

**Certificates only where fields interact.** PT2's BigNat is canonical through its certificate and BigInt cannot express signed zero. BigFlt needs an additional joint certificate because `mantissa = 2, exponent = 0` and `mantissa = 1, exponent = 1` denote the same value.

**Representation privacy preserves stage-2 mobility.** `BigFlt` is a `struct`. Its certificate enforces inhabitance; privacy prevents external code and theorems from depending on the dyadic field layout that stage 2 extends.

**BigInt owns sign reasoning.** BigFlt arithmetic consumes PT2's ring and order laws. No BigFlt proof repeats the full sign-case product.

**Correct rounding happens once at the boundary.** Stage 1 has no exact division operation or `Div` witness. `narrow_b` and `narrow_ratio_b` implement round-to-nearest-even directly from exact values.

## Goals for stage 1

- `BigFlt` as a certified, representation-private canonical dyadic rational.
- Exact closed `add`, `sub`, `mul`, `neg`, and `abs` with ring and cancellation laws.
- An order layer sufficient for boundary correctness: `cmp`, Bln comparisons, reflected Props, antisymmetry, transitivity, and required monotonicity/absolute-difference lemmas.
- `widen_b : Bin/X -> Option(BigFlt)`, exact and total on byte sequences.
- `narrow_b : BigFlt -> Bin/X`, correctly rounded to an IEEE-754 binary32 byte pattern using round-to-nearest-even.
- `narrow_ratio_b : (BigFlt, BigFlt) -> Bin/X`, the only division in stage 1, correctly rounding the exact quotient at the boundary.
- Byte-level round-trip, nearest-value, tie-to-even, half-ulp, and denominator-cleared quotient theorems.
- Thin `widen`, `narrow`, and `narrow_ratio` wrappers over native Flt reinterpretation.
- Zero postulates and proof definitions acceptable to a future termination/positivity checker.

## Non-goals for stage 1

- Laws about native `/sys/Flt` arithmetic or an assumption that Wasm IEEE operations satisfy Curios propositions.
- Exact interior division, field laws, or a `Div(BigFlt)` witness.
- Exact square roots or constructive reals.
- Exact decimal arithmetic. Decimal values such as `0.1` call for a separate decimal type, not the rational extension of binary float reasoning.
- Replacing native `Int` or `Flt` as pragmatic runtime defaults.
- A postulate mechanism or trusted arithmetic oracle.
- `BigFlt/of_str`; a decimal parser would round in stage 1 and muddy the exact contract.

## Background facts verified against the codebase

- Native `Flt` is IEEE-754 single precision stored bitwise in `curios-base/src/flt.rs`. Term identity is bitwise: NaN equals itself as a term and `+0.0` differs from `-0.0`, unlike IEEE `==`.
- `Flt/to_le_bin` exists and emits the four little-endian bytes through `I32ReinterpretF32`. `F32ReinterpretI32` already exists in `curios-wasm`.
- `Flt/of_le_bin` was delivered in commit `3ed05d35`; PT4 retains its contract and migrates its type from bare Bin/Nat-byte conventions to `Bin/X`/`Byte`.
- Open-term primitive opacity is the reason all theorems are byte-level.
- `Flt/of_str` is not correctly rounded, so PT4 cannot reuse it as a proof boundary.
- Native Int is type-level ℤ but runtime i31. BigFlt uses BigInt for its exponent so pathological exact computations do not acquire a runtime exponent overflow absent from type-level proofs.
- Existing Dragon4 `divmod` is a repeated-subtraction decimal digit extractor with a small quotient and large scaled divisor. It is not general exact division and is not the PT4 quotient engine.
- Proof irrelevance and single-field collapse make certificates runtime-free.
- Curios has no user-code trap primitive, termination checker, strict-positivity checker, or strong-induction library. Stage 1 is designed not to require strong induction.

## Part 1 — `Flt/of_le_bin`

The primitive assembles a native Flt from exactly four little-endian bytes by OR-ing their reflected Nat values into an i32 and emitting `F32ReinterpretI32`. It traps through the primitive-level `Unreachable` precedent unless the `Bin/X` length is exactly four.

Its contract after PT1 is:

```crs
Flt/of_le_bin : Bin/X -> Flt
```

The primitive implementation uses `Byte/to_nat` semantics internally; no user-visible Nat-byte convention remains.

The delivered implementation footprint, which must be preserved during the PT1 migration, includes:

- `curios-core/src/prim.rs`, `elaborate/prim.rs`, `erase/prim.rs`, `convert/prim.rs`, `reduce/prim.rs`, `zonk.rs`, and `print.rs`;
- `curios-ersd/src/prim.rs`, `into_cont/lower_prim.rs`, `optimize/{evaluate,rewrite}.rs`, and `print.rs`;
- `curios-cont/src/module.rs`, `print.rs`, `into_wasm/code_emitter.rs`, `optimize/walk.rs`, and `optimize/scalar_eval.rs`;
- `curios-text/src/prim.rs`, `prelude.rs`, `into_core/lowerer.rs`, `print.rs`, and the `curios-text/std/Flt.crs` re-export;
- `curios/src/tests/codegen/code_flt.rs` and codegen registration.

Round-trip fixtures cover `0.0`, `-0.0`, normal values, subnormals, payloaded NaNs, and both infinities. Compare their `to_le_bin` bytes; never use native Flt equality for these tests.

## Part 2 — prerequisite integer helpers owned by PT4

PT2 supplies canonical packed BigNat and BigInt plus their general arithmetic and theorem corpus. PT4 adds only helpers whose required contract arises from float encoding or quotient extraction:

- `bit_len` if not already justified independently by PT2;
- repeated structural `div2` or an equivalent packed shift view;
- extraction of the leading precision bits;
- guard and sticky-bit accumulation;
- exact comparison of shifted magnitudes;
- the base-2 compare-subtract-double quotient digit loop;
- lemmas tying those helpers to multiplication by powers of two and exact integer comparison.

Keep these helpers in BigNat/BigInt modules when that is their natural API home, but track their implementation and proof obligation here. PT2 must not be blocked on them.

The provable fragment remains structural bit algebra. Native Nat `-`, `/`, and `%` on symbolic operands are opaque and cannot justify any theorem in this part.

## Part 3 — representation and canonicity

```crs
pub struct BigFlt : Type {
    mantissa : BigInt,
    exponent : BigInt,
    canonical : Canonical(mantissa, exponent)
}
```

The value is `mantissa · 2^exponent`. A canonical value has an odd nonzero mantissa, or is the single zero representation with both mantissa and exponent zero. BigInt already excludes a signed zero mantissa.

The exported smart constructor `mk(mantissa, exponent)` is total. It strips trailing zero bits from the magnitude, adding the number stripped to the exponent, and forces the canonical zero. The result type itself demands the certificate, so strip correctness gates the constructor rather than being an optional theorem added later.

The canonicity kernel contains:

- **strip correctness:** value preservation and an odd-or-zero result, proved by structural recursion over packed low bits;
- **uniqueness:** if `a·2^e = b·2^f` with nonzero odd `a` and `b`, then `a = b` and `e = f`, by parity and power-of-two alignment;
- **`mk` respects value equality:** equal raw aligned-pair values produce structurally equal certified results.

Certificates do not appear as meaningful data in equality goals. If a proof attempts to compare certificate inhabitants, the statement or reduction route is wrong.

Stage 2's additional summit is reduced-fraction uniqueness via Euclid's lemma and coprimality. That work is deliberately absent from stage 1.

## Part 4 — exact arithmetic and ring laws

Addition and subtraction align exponents, shift the higher-exponent-scaled mantissa as needed, perform one BigInt addition/subtraction, and call `mk`. Multiplication multiplies mantissas, adds exponents, and calls `mk`. Negation and absolute value act on the BigInt mantissa. Equality is structural because certification gives a unique representation.

Proofs are staged:

1. prove the laws for raw aligned pairs from PT2's BigInt ring laws and power-of-two interaction lemmas;
2. transport through `mk` using the canonicity kernel.

Export:

- `add_comm`, `add_assoc`, left/right additive cancellation;
- `mul_comm`, `mul_assoc`, left/right multiplication cancellation under the appropriate nonzero premise;
- left/right distributivity;
- negation and subtraction laws needed by order and boundary proofs.

Cancellation is the integral-domain substitute for inverse reasoning. No inverse or field law appears in stage 1.

## Part 5 — order layer

Implement `Order`-valued `cmp` by exponent alignment and BigInt comparison, with Bln `lt`, `lte`, `gt`, and `gte`. Define proposition-level relations by boolean reflection rather than new inductives:

```text
Lte(x, y) := Eq(BigFlt/lte(x, y), true)
Lt(x, y) := Eq(BigFlt/lt(x, y), true)
NonZero(x) := Eq(BigFlt/eql(x, zero), false)
```

The required lemma stack is intentionally limited to boundary proofs:

- reflexivity, antisymmetry, transitivity, and totality;
- addition monotonicity;
- multiplication monotonicity under nonnegativity;
- absolute value and subtraction manipulation sufficient to compare rounding errors.

Antisymmetry concludes structural `Eq` because the type is certified. These BigFlt-specific order lemmas belong to PT4 even though they consume PT2 BigInt order.

## Part 6 — exact widening

```crs
BigFlt/widen_b : Bin/X -> Option(BigFlt)
```

Return `none` unless the input length is exactly four. Reflect Byte fields to Nat for extraction, following the same bit layout used by native `Flt/to_str`:

- Normal exponent field `1..254`: `mk(±(mantissa_field + 0x800000), exponent_field - 150)`.
- Subnormal exponent field `0` with nonzero mantissa: `mk(±mantissa_field, -149)`.
- Either signed-zero pattern: canonical BigFlt zero.
- Exponent field `255`: `none` for infinity and NaN.

The field extraction is ordinary byte arithmetic, for example `exponent_field = Byte/to_nat(b3) % 0x80 * 2 + Byte/to_nat(b2) / 0x80`, and should be factored with any existing native-Flt decoding helper rather than duplicated inconsistently.

Nothing rounds. The wrapper is:

```text
widen(f) = widen_b(Flt/to_le_bin(f))
```

The sign collapse for `-0.0` is deliberate and must be documented because native Flt term identity distinguishes the two zeros while mathematical BigFlt does not.

## Part 7 — correctly rounded narrowing

```crs
BigFlt/narrow_b : BigFlt -> Bin/X
```

Compute the unbiased exponent from magnitude bit length plus the exact exponent. Then:

- values overflowing after rounding produce the appropriate signed infinity pattern;
- normal-range values retain the leading 24 significand bits, compute guard and sticky information from the remainder, round to nearest with ties to even, propagate a carry, and recheck exponent overflow;
- values below the normal range round on the `2^-149` subnormal grid rather than retaining a fixed 24 leading bits;
- a nonzero value rounding to zero preserves its sign in the emitted byte pattern.

Use shifts, structural bit views, and comparisons. Do not implement narrowing through opaque general division.

```crs
BigFlt/narrow_ratio_b : BigFlt -> BigFlt -> Bin/X
```

This is stage 1's only division. Use a base-2 compare-subtract-double digit loop against the denominator magnitude, producing enough leading, guard, and sticky information to round the exact quotient once. Edge semantics are explicit:

- `0/0` produces a NaN byte pattern;
- nonzero divided by zero produces signed infinity;
- finite nonzero denominators use round-to-nearest-even.

This loop is also stage 2's future `narrow` engine, so implementing it now prepays that migration.

Wrappers are:

```text
narrow = Flt/of_le_bin ∘ narrow_b
narrow_ratio = Flt/of_le_bin ∘ narrow_ratio_b
```

## Part 8 — byte-level boundary theorems

Statements avoid postfix `!` in types by carrying successful decode hypotheses as equations.

### Round-trip

For `b : Bin/X`, `Eq(widen_b(b), Option/some(x))`, and `b` not equal to the negative-zero pattern, prove `Eq(narrow_b(x), b)`. Prove separately that negative zero widens to canonical zero and canonical zero narrows to positive zero. No stronger bitwise statement is true.

### Correct rounding

For `x` whose narrowed output is finite, let `r` be that output widened back. The precise range premise must account for the binary32 overflow rounding boundary `2^128 - 2^103`, not merely the largest finite decoded value. For every finite four-byte pattern `b` widening to `y`, prove:

```text
abs(r - x) <= abs(y - x)
```

The finiteness/range premise must account for overflow **after** rounding; otherwise widening the resulting infinity is undefined. Add the ties-to-even refinement and half-ulp corollary.

### Ratio correctness

With a nonzero denominator, prove that `narrow_ratio_b` is the correctly rounded exact quotient. State comparisons denominator-cleared, cross-multiplying inequalities and using absolute value for denominator sign. This demonstrates the technique users apply to exact formulas without introducing interior division.

Expected difficult cases are the subnormal boundary, exact halfway cases, exponent extremes, carry into a larger exponent, and signed zero.

## Part 9 — module and witness placement

`curios-text/std/BigFlt.crs` is registered after PT2's BigNat and BigInt in `curios-text/std.crs` and the `include_str!` table in `curios-text/src/prelude.rs`.

`Add`, `Sub`, `Mul`, `Eql`, and `Cmp` witnesses belong in the `/std` operator facade modules, following project convention. `Show` and `Ord` may be supplied where useful. There is no `Div(BigFlt)` witness in stage 1; rejecting `x / y` is the honest API and reserves that operator for stage 2.

Presentation functions consume PT3 `Str` and `Char` but do not depend on their representation.

## Soundness discipline

- Every Prop definition uses checked structural elimination, checked inductive case analysis, or recursion on an evident structural subterm.
- `mk` stripping is structural on packed low bits. Boundary loops are computations; proof claims about them must be separately derived rather than assumed from general recursion.
- The certificate is proof-irrelevant and never inspected.
- Prefer congruence and explicit `Eq/trans` chains to elaboration-order-fragile match rewrites, following PT2's proof idioms.
- Export only abstract laws and existential/value statements. Do not export a representation-completeness or dyadic case-analysis theorem that would be falsified by stage 2.
- All stage-1 proof definitions should remain acceptable to future termination and positivity checking.

## Stage 2 — deferred denominator extension

### Trigger

Proceed only for a real workload requiring exact quotients as interior operands where denominator clearing is untenable, such as nested rational expressions, exact linear algebra, or rational oracles. Speculative completeness is not a trigger. Exact decimal requirements point to a separate decimal type.

### Contract imposed on stage 1

- BigFlt representation remains `struct`-private.
- No exported theorem claims every BigFlt is a dyadic constructor form.
- Exported laws mention only the abstract type and operations.
- No `Option`-valued placeholder exact division ships in stage 1.

### Extension

Add:

```text
denominator : BigNat
```

The value becomes `mantissa · 2^exponent / denominator`. The certificate requires an odd denominator, coprimality between `abs(mantissa)` and the denominator, and denominator `1` for canonical zero. Powers of two remain in the exponent; only the odd part is materialized.

Dyadics are exactly the `denominator = 1` stratum. Multiplication combines mantissas, exponents, and denominators, then applies cross-gcd reduction, which is trivial when both denominators are one. Dyadic workloads retain their stage-1 representation and cost.

Division becomes exact subject to an explicit zero-denominator API decision. The repository currently has no user-level trap mechanism, so choose deliberately between a checked result and a newly engineered trap rather than assuming one. Only then add `Div(BigFlt)`.

### New proof work

- strong or bounded induction over Nat;
- BigNat `divmod` and `gcd` with `a = b·q + r` and `r < b` specifications;
- Euclid's lemma and coprimality infrastructure;
- uniqueness of the reduced odd-denominator form;
- certificate preservation through the extended smart constructor.

PT1, PT2's general integer theorem base, the stage-1 dyadic canonicity kernel as the denominator-one stratum, byte-boundary theorem statements, and `narrow_ratio_b` all survive.

## Build order

1. Confirm PT1–PT3 and migrate delivered `Flt/of_le_bin` to `Bin/X`/`Byte`.
2. Add only the BigNat/BigInt helpers listed in Part 2 and their structural specifications.
3. Implement BigFlt representation, stripping, certificate, uniqueness, and `mk`; these proofs gate all later value construction.
4. Implement arithmetic and ring laws.
5. Implement the BigFlt order layer required by correctness statements.
6. Implement `widen_b`.
7. Implement `narrow_b` and `narrow_ratio_b` plus native wrappers.
8. Prove the byte-level boundary theorems.
9. Register modules and witnesses, run the complete done bar, and update permanent documentation.

The exact widening and narrowing implementations can proceed in parallel with some arithmetic/order proofs once `mk` is available, but theorem completion depends on the order layer.

## Verification

- Preserve the delivered `Flt/of_le_bin` pattern tests across the PT1 byte migration.
- Test widening for normals, subnormals, both zeros, infinities, and multiple NaN payloads.
- Test narrowing at every normal/subnormal boundary, exponent overflow, underflow, exact halfway case, and significand carry.
- Compare emitted bytes with a trusted IEEE-754 reference for a broad generated corpus.
- Separately exercise theorem statements through Curios elaboration; reference tests do not replace proofs.
- Confirm certificates and all helper proofs erase.
- Confirm BigFlt runtime values contain only their relevant integer fields and no proof objects.
- Benchmark boundary loops independently from PT2's Dragon4 and conversion measurements.

## Open questions and risks

- Correct rounding, particularly at subnormal and overflow boundaries, remains the fiddliest implementation and proof work.
- The exact transparent statement of shifted-value equality must cooperate with PT2's packed canonicality without exposing representation publicly.
- BigInt exponent alignment must avoid accidentally returning to native Int at runtime.
- `narrow_ratio_b` needs an explicit sign and zero table before implementation.
- Stage 2's zero-denominator behavior remains intentionally undecided until the extension is triggered.
- Do not pull PT4-only helpers back into PT2 merely to make this file shorter; the dependency boundary exists to keep the packed-integer experiment focused.
