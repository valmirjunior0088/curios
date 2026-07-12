# BigFlt implementation specification

Design and build plan for exact binary-rational arithmetic (`BigFlt`) with proof-carrying conversion to and from the native `Flt` type, plus the one compiler primitive (`Flt/of_le_bin`) it depends on. Staged: **stage 1** ships `BigFlt` as a certified dyadic rational (arbitrary-precision binary float, ℤ[1/2]) on top of a new `BigInt` integer layer; **stage 2** — deferred until a real workload demands exact interior division — *extends* the same carrier with an odd `denominator` field, turning the type into full ℚ with the dyadics as the `denominator = 1` stratum. This document is a working implementation reference, not permanent architecture documentation.

## Motivation

Native `Flt` arithmetic (`curios-text/std/Flt.crs`) compiles to opaque `/sys/Flt` wasm primitives. They carry no algebraic laws, and — unlike `Nat`/`Bin`/`Lst`, which the elaborator treats as genuine free-monoid inductives (`elaborate_nat_match` in `curios-core/src/elaborate/match_.rs:80`, `curios-core/src/free_monoid.rs`) — `Flt` can have no structural induction principle: IEEE-754 add/mul are not associative and are not the free monoid on any generator.

The opacity goes further than arithmetic, and this drives the architecture: **any prim application on an open term is stuck** (`convert/prim.rs` is congruence-only; `reduce` fires on literals), and there is no eliminator on `Flt`, so a ∀-quantified `Eq` mentioning `to_le_bin(f)` or `of_le_bin(...)` for open `f` is unprovable — no proof can even establish that `to_le_bin(f)` has four bytes. Therefore every theorem in this plan is stated at the **byte level** (`Bin` → `BigFlt` → `Bin`), where everything is data and fully provable; the `Flt`-facing functions are thin unproved wrappers around the two reinterpret prims, which are the plan's entire (and explicitly named) trust boundary. No conversion rule equating `of_le_bin(to_le_bin(x)) ≡ x` is added to the compiler — that would be a postulate in reducer clothing.

`BigFlt` is a dyadic rational: `mantissa · 2^exponent` with `mantissa : BigInt`. Every finite `f32` is exactly such a number, so the embedding loses nothing; sums, differences, and products are closed and exact (ℤ[1/2] is a ring and an integral domain); and its canonical form (odd mantissa) needs only a parity argument, not gcd theory. Division is deliberately absent from stage 1: interior quotients are handled by denominator clearing, and boundary quotients by a correctly-rounded `narrow_ratio` — which is a strictly stronger contract (exact value, rounded once) than exact-divide-then-round. Precedent: Flocq and the HOL float formalizations model floats exactly this way (`m · 2^e` pairs), not as reduced fractions.

## Design keystones

**Certificates, not discipline.** `struct` privacy is an elaboration-time access check (`PrivateRepresentation`), **not** a restriction on the type's inhabitants — a ∀-quantified theorem ranges over all field tuples, canonical or not. Laws that conclude structural `Eq` from value-level premises are false over such a type: `x = ⟨mantissa 2, exponent 0⟩` and `y = ⟨mantissa 1, exponent 1⟩` are value-equal, so `lte` holds both ways and `add(a, x) = add(a, y)` (both canonicalize through `mk`) — but `Eq(x, y)` fails, so antisymmetry, `add_cancel`, and `mul_cancel` are unprovable as stated over a bare struct. The fix is the `/syn/Str` pattern (`record Str { bytes, valid : Valid(bytes) }`): every arithmetic type carries a **certificate field** reflecting a computable canonicality check, so non-canonical tuples cannot be extended to inhabitants at all and quantifiers only see canonical values. `Prop` is definitionally proof-irrelevant (`curios-core/src/convert.rs:168` — any two inhabitants of a `Prop` are definitionally equal, and conversion is type-directed), so certificates never obstruct equality goals: `Eq` between certified records reduces to `Eq` of the data fields. Certificates are non-informative and erase, so the runtime representation stays the bare data. Unlike `Str`, there is no per-literal derivation cost (`BigFlt` has no literals — the `RUST_MIN_STACK` gotcha has no analogue here) and no bespoke per-op proof plumbing: every operation ends in the smart constructor, which produces the certificate once.

**The certificate tower.** Each layer's check assumes the one below:

| Type | Data fields | Certificate (reflection of a `Bln` check) |
| --- | --- | --- |
| `BigNat` (existing, retrofitted) | `limbs : Lst(Nat)` | no trailing zero limb |
| `BigInt` (new) | `sign : Bln`, `magnitude : BigNat` | no negative zero (`magnitude` nonzero, or `sign` false) |
| `BigFlt` (new) | `mantissa : BigInt`, `exponent : BigInt` | `mantissa` odd, or the canonical zero (`mantissa` and `exponent` both zero) |

`BigNat` and `BigInt` are transparent `record`s: their representations are permanent, and the certificate is what makes transparency safe (a `−0` or an untrimmed limb list cannot be constructed anywhere — its certificate is unprovable). `BigFlt` is a `struct`: privacy is no longer needed for invariant enforcement (the certificate does that), but it is exactly what keeps stage 2's field extension a non-breaking change — **representation mobility**.

**`BigInt` as the sign layer.** Without it, every `BigFlt` ring law pays sign-magnitude splits × exponent-alignment splits × `mk`-transport as one case product. With it, the sign reasoning happens once in `BigInt` (against the `BigNat` semiring, no exponents in sight), and `BigFlt`'s laws see an opaque ring (alignment and transport only). `BigInt` is also stage 2's numerator type and a permanent stdlib asset in its own right — the stdlib has no true integer (`/sys/Int` is a runtime i31).

## Goals (stage 1)

- `BigInt`: sign-magnitude arbitrary-precision integers, certified no-negative-zero, with exact ring ops, proved ring laws (the home of all sign-case reasoning), and an order layer.
- `BigFlt` as a certified dyadic rational, canonical by inhabitance (certificate field), representation-private (`struct`) for stage-2 mobility.
- Exact, closed `add`/`sub`/`mul`/`neg`/`abs` with proved ring laws (commutativity, associativity, distributivity, additive and multiplicative cancellation — cancellation is the integral-domain substitute for inverse reasoning). Closure is **unconditional**: the `BigInt` exponent removes the runtime i31 overflow trap a pathological squaring chain could hit with a native-`Int` exponent.
- An order layer: `Order`-valued `cmp`, `Bln`-valued `lt`/`lte`/`gt`/`gte`, boolean-reflection `Prop`s, and the monotonicity lemmas Part 9's bounds consume. Antisymmetry is provable *because* the type is certified.
- `widen_b : Bin -> Option(BigFlt)`: exact, total byte-level decode. `narrow_b : BigFlt -> Bin`: correctly-rounded (RNE) byte-level encode. `narrow_ratio_b : (BigFlt, BigFlt) -> Bin`: correctly-rounded quotient — the only division anywhere, at the boundary, where rounding is definitionally the intent.
- Byte-level theorems: round-trip (with the `-0.0` carve-out), correct-rounding/half-ulp bound, `narrow_ratio_b` correctness (stated denominator-cleared).
- `Flt`-facing wrappers `widen`/`narrow`/`narrow_ratio` via `to_le_bin`/`of_le_bin` (Part 1) — unproved glue by design.
- Every proof through genuinely checked structural elimination; zero postulates; nothing that a future stricter checker (termination on `rec`, positivity on `induct`) would reject.

## Non-goals

- Nothing about the native `/sys/Flt` primitives themselves (unchanged from the original plan; would require trusting wasm IEEE conformance).
- No exact interior division, no field laws, no `Div` witness — stage 2. Formulas with interior quotients are restructured by denominator clearing (e.g. `n²·Var = n·Σx² − (Σx)²`) or deferred to `narrow_ratio`.
- No exact `sqrt`/constructive reals (unchanged). The order layer would support future two-sided-bound `sqrt` approximations; out of scope here.
- No exact decimal arithmetic: `0.1` is not dyadic *and not an `f32`* — float literals are binary before `BigFlt` ever sees them. If exact decimal reasoning is ever needed (money), that is a separate ℤ[1/10]-style type, not `BigFlt` and not a reason for the denominator extension.
- `BigInt` does not replace native `Int`: the i31 stays the pragmatic default for indices, counters, and codes; `BigInt` is for exact mathematics.
- No `postulate` mechanism, no termination/positivity checking (all confirmed absent; worked around by discipline).
- No `BigFlt/of_str` in stage 1 (a decimal parser would have to round, muddying "exact"; revisit in stage 2 where it can be exact).

## Background facts (verified against the codebase; do not re-derive)

- `Flt` is IEEE-754 single precision (`curios-base/src/flt.rs`, stored as `bits: u32`). **Term identity is bitwise**: `NaN == NaN` and `0.0 != -0.0` *as terms* (flt.rs doc), so propositional `Eq` on `Flt` is bit equality, not IEEE `==`. This is why the round-trip theorem must carve out `-0.0`.
- `Flt/to_le_bin` exists (`curios-cont/src/into_wasm/code_emitter.rs:918-951`, `I32ReinterpretF32`), used by `Flt/to_str`. No reverse primitive exists. `F32ReinterpretI32` is already in `curios-wasm` (`expr.rs:294`, writer/parser/printer) — Part 1 needs no new wasm capability.
- Open-term prim opacity (see Motivation) — the load-bearing fact behind the byte-level architecture.
- **`Prop` is definitionally proof-irrelevant** (`curios-core/src/convert.rs:168-169`; conversion is type-directed, driving η and irrelevance). `/syn/Str` (`record { bytes : Bin, valid : Valid(bytes) }`, `curios-text/syn/Str.crs`) is the stdlib precedent for certified records and for ops that produce the certificate alongside the data (`concat_closed`, `drop_valid`, `take_valid`).
- `Flt/of_str` is not correctly rounded (parses via native `Flt` arithmetic); nothing correctly-rounded exists to lean on. `narrow_b` is new work, structurally akin to `to_str`'s Dragon4 machinery but in base 2 with fixed precision.
- `BigNat` (`curios-text/std/BigNat.crs`) is a `record` over little-endian base-10000 `Lst(Nat)` limbs with `add`, `sub` (truncating), `mul_small`, `mul_pow2`, `Order`-valued `cmp` plus the `Bln` family — **no general `mul`** (Part 2 adds it). Its **no-trailing-zero-limb invariant is maintained by every existing op** (`trim` in `sub`, carry handling in `add_c`, `limbs_of_nat` stopping at 0) **but stated nowhere and load-bearing everywhere**: `cmp_limbs` is value-correct only on trimmed lists (`[3]` vs `[3, 0]` compares `lt` though both mean 3), `is_zero` is a length check that misclassifies `[0]`, and a naive structural `div2` emits a trailing zero limb even on trimmed input (`div2([0, 1]) = [5000, 0]`). Part 2 turns this invariant into the certificate.
- No `BigNat { … }` record literal and no `.limbs` projection exists outside `BigNat.crs` (`Flt.crs` consumes exported ops only), so the certificate retrofit touches one file.
- Native `Int` is type-level ℤ (`curios-base/src/int.rs`, `BigInt`-backed — "the type level pretends ℤ") but a runtime i31 carrier with overflow traps. This is why `BigFlt`'s `exponent` is a `BigInt`, not an `Int`: with an `Int` exponent, a squaring chain could trap at runtime while the type-level proofs stayed valid. `Int` literals still appear at the byte boundary (`widen_b`'s field arithmetic) and cross over via `BigInt/of_int`; `Int`'s 31 bits are ample there (`f32` needs `[-149, 127]`).
- `Flt.crs`'s existing `divmod` is a repeated-subtraction digit extractor (small *quotient*, large scaled divisor) — fine for Dragon4, unusable as general division; irrelevant to stage 1, which needs no division at all.
- `record` vs `struct` (`SYNTAX.md:284`): `struct` = representation private to the declaring module (`PrivateRepresentation` otherwise). Privacy is an elaboration check, **not** an inhabitance restriction — invariants come from certificates (see Design keystones); what privacy buys is **representation mobility**, which is why `BigFlt` alone is a `struct`.
- `rec` performs no termination check (`elaborate_rec`, `curios-core/src/elaborate/binding.rs:81-150`: assume binder types, check bodies, no decrease analysis) and accepts `Prop`-sorted bindings — `rec absurd : False = absurd;` compiles (re-verified empirically; unused items *are* elaborated, verified via an ill-typed dead item failing).
- Checked structural eliminators exist exactly for `Nat`/`Lst`/`Bin` (`; ih` arms; the ih is only available at the predecessor/tail by construction). `match` on an `induct` value is checked *case analysis* — exhaustiveness plus index-driven impossible-arm pruning — but provides no ih; recursion over derivations uses `rec` on a structural subterm (stdlib precedent: `lte_succ_r`, `lte_trans`), which a real termination checker would accept.
- No strict-positivity check; `Eq` and reflection-`Prop`s used here are manifestly positive. No `Acc`/strong induction in the stdlib — **stage 1 does not need it** (stage 2's Euclid does).
- `Eq.crs` provides `sym`/`trans`/`cong`/`subst`. `Lte` on `Nat` exists (`Nat.crs:82`).
- The stdlib has no user-code trap mechanism (no `panic`/`unwrap`). Moot in stage 1: dyadic `mk` is total. Stage 2 must decide zero-denominator behavior explicitly.

## Part 1 — `Flt/of_le_bin` compiler primitive

Assembles a native `Flt` from its 4 little-endian bytes, mirroring `Flt/to_le_bin`: OR the bytes into an `i32`, emit `F32ReinterpretI32`. Traps (prim-level `Unreachable`, precedent: `FltToNat` on negative) unless the `Bin` is exactly 4 bytes.

**Touch points** (grep-verified against `FltToLeBin`'s actual footprint):

- `curios-core/src/prim.rs` (`FltOfLeBin(Term)`), `elaborate/prim.rs` (`Bin -> Flt`), `erase/prim.rs`, `convert/prim.rs`, `reduce/prim.rs` (fold on closed 4-byte literals), `zonk.rs`, `print.rs`
- `curios-ersd/src/prim.rs`, `into_cont/lower_prim.rs`, `optimize/{evaluate,rewrite}.rs`, `print.rs`
- `curios-cont/src/module.rs`, `print.rs`, `into_wasm/code_emitter.rs`, **`optimize/walk.rs`** (exhaustive `walk_code_operands!` — the build breaks without the new arm), **`optimize/scalar_eval.rs`** (has a `_ => None` catch-all, so optional for compilation, but required for const-folding parity with `to_le_bin` — closed test programs const-fold, so parity affects what tests exercise)
- `curios-text/src/prim.rs`, `prelude.rs` (`unary("of_le_bin", bin(), flt(), ...)`), `into_core/lowerer.rs`, `print.rs`; `curios-text/std/Flt.crs` re-export list
- The `FltToLeBin` hits in `curios-cont/src/optimize/{constant_folding,evaluate_pure_calls}.rs` are `#[cfg(test)]`-only — no obligation (extending those tests is optional).
- Tests: `curios/src/tests/codegen/code_flt.rs` (+ `codegen.rs`, `codegen/module.rs` as needed). Round-trip fixtures for 0.0, `-0.0`, subnormals, NaN (payloaded), ±Inf — **compare via `to_le_bin` bytes, never `==`** (IEEE `eql` fails NaN and spuriously passes `-0.0 == +0.0`).

Independent of everything else; needed only by the Part 8 wrappers.

## Part 2 — `BigNat`: trimmed-limbs certificate, multiplication, lemma base

The carrier-independent foundation everything stands on. Changes to `BigNat.crs`:

- **Retrofit the certificate**: `limbs` gains a sibling `canonical : Eq(is_trimmed(limbs), true)` (`is_trimmed` = empty, or last limb nonzero). Every existing op already maintains the invariant; the retrofit makes each op's body *prove* it — construction sites are all inside `BigNat.crs`, each becoming a smart-constructor call or carrying its preservation lemma. `is_zero`'s length check becomes genuinely correct over all inhabitants.
- `mul(a, b) -> BigNat` — schoolbook over limbs (`mul_small` per limb + limb shift + `add`). Required by `BigFlt/mul` and by every algebraic law.
- Parity/binary infrastructure: `is_even`, `div2` (both **structural folds** — little-endian limb folds work because the base 10000 is even: half of `l + B·t` is `(l + r·B)/2 + q·B` with `t = 2q + r`, and the low bit is `l % 2`; **`div2` must re-trim**, since the naive fold emits a trailing zero limb), `bit_len`, top-bits extraction helpers for Part 8.
- The semiring lemma base, by `Lst`-`; ih` induction: `add_comm`, `add_assoc`, `mul_comm`, `mul_assoc`, `distrib`, `add_cancel`, `mul_pow2`/`add`/`mul` interaction lemmas, `cmp` order lemmas — now honestly stated over certified inhabitants, no trimmedness premises to thread. Permanent stdlib assets; they survive stage 2 unchanged.

## Part 3 — `BigInt`: signed integers and the sign-case proofs

```
pub record BigInt : Type {
    sign : Bln,
    magnitude : BigNat,
    canonical : Canonical(sign, magnitude)
}
```

New module, permanent stdlib asset, and the factoring that keeps `BigFlt`'s proofs small (see Design keystones). Certificate: no negative zero — `magnitude` nonzero, or `sign` false. Transparent `record`: safe because `−0`'s certificate is unprovable.

- Ops: `add`/`sub` (sign-magnitude case split over `BigNat/cmp`; `sub` is total — ℤ has inverses, unlike `BigNat`'s truncating `sub`), `mul` (xor signs, multiply magnitudes), `neg`/`abs`, `Order`-valued `cmp` + `Bln` `lt`/`lte`/`gt`/`gte`, structural `eql` (valid because certified), `of_nat`, `of_int` (the i31 boundary helper, via `Int/abs`), `is_even`/`div2`/oddness lifted from `magnitude`, `to_str`.
- Ring laws: `add_comm`, `add_assoc`, `mul_comm`, `mul_assoc`, `distrib`, `add_cancel`, `mul_cancel`, `neg` involution, `abs`/`sign` lemmas, order lemmas including antisymmetry (provable because certified). These are ℤ's laws, proved by sign case analysis citing Part 2's semiring lemmas — the sign splits happen here, once, and nowhere else.
- Witnesses (`Add`/`Sub`/`Mul`/`Eql`/`Cmp`, plus `Show`/`Ord` as useful) — homed in the `/std` operator facades (`std/Add.crs`, `std/Eql.crs`, `std/Cmp.crs`, …), following the `BigNat` precedent.

## Part 4 — `BigFlt` representation and canonicity kernel

```
pub struct BigFlt : Type {
    mantissa : BigInt,
    exponent : BigInt,
    canonical : Canonical(mantissa, exponent)
}
```

Value: `mantissa · 2^exponent`. Certificate: `mantissa` odd, or the canonical zero (`mantissa` and `exponent` both zero; `BigInt`'s own certificate already forbids a signed zero mantissa). A `struct` not for invariant enforcement — the certificate does that — but for **representation mobility**: stage 2 adds a field, which no outside code or proof can have mentioned.

The exported smart constructor `mk(mantissa, exponent)` is total: it strips trailing zero bits of the mantissa's magnitude into the exponent and forces the canonical zero. Because `mk`'s result type carries the certificate, **the strip-correctness proof is a prerequisite for defining `mk` at all** — the kernel gates the ops (see Build order).

The **canonicity kernel** — deliberately the smallest possible trusted-by-effort core, and the stage-1 summit:

- Uniqueness: `a·2^e = b·2^f` with `a`, `b` odd implies `a = b` and `e = f` (parity argument over `mul_pow2`; shallow).
- `strip` correctness (value preservation, odd-or-zero result): by fuel-indexed structural induction, with **fuel as an explicit argument shared by the computation and the proof** (fuel `14 · limb-count + 1` bounds the bit length — base 10000 < 2^14; the bound is a `Lst` induction). Explicit fuel makes `strip` total by construction and avoids a rec-vs-proof-model bridging lemma. Strong induction is not required.
- `mk`-respects: equal values (alignment equality on raw pairs: `mul_pow2(m₁, e₁−e₂) = m₂` at the common exponent) yield structurally `Eq` results. Certificates never appear in these goals — proof irrelevance discharges them.

Contrast deliberately noted: the stage-2 addition to this kernel is gcd-reduced-form uniqueness (Euclid's lemma, coprimality) — the reason the denominator is deferred.

## Part 5 — Arithmetic and ring laws

- `add`/`sub`: align exponents (`BigInt/sub` for the delta, its magnitude as the `mul_pow2` shift count, applied to the smaller-exponent operand), one `BigInt/add`/`sub`, through `mk`. `mul`: `BigInt/mul` mantissas, `BigInt/add` exponents, through `mk` (odd·odd is odd — `mk` strips only after equal-exponent add/sub, e.g. `x − x`, `1/2 + 1/2`). `neg`/`abs`: `BigInt` sign ops (zero stays canonical). `eql`: structural (valid because certified). `cmp`/`lt`/`lte`: align-and-compare via `BigInt/cmp` — no cross-multiplication. **No sign-case analysis anywhere in this part** — that is Part 3's job.
- Proofs, staged: (1) laws for the *raw aligned-pair* operations — these reduce to Part 3's `BigInt` ring laws plus `mul_pow2` interaction lemmas from Part 2; (2) transport through `mk` via the Part 4 kernel. Exported laws: `add_comm`, `add_assoc`, `mul_comm`, `mul_assoc`, `distrib`, `add_cancel`, `mul_cancel` (nonzero premise via reflection) — all provable as stated over the certified type, no canonicality premises to thread. No inverse laws — cancellation is the integral-domain substitute.

## Part 6 — Order layer

- `Prop`s by boolean reflection — `Lte(x, y) := Eq(BigFlt/lte(x, y), true)`, likewise `Lt`, `NonZero` — no new `induct`, hence no positivity vetting.
- Lemma stack, sized to what Part 9 consumes (not a full order-theory library): reflexivity/antisymmetry/transitivity/totality, `add`-monotonicity, `mul`-monotonicity under nonnegativity, `abs`/`sub` interaction (`Lte(abs(sub(a, b)), c)` manipulation). Antisymmetry concludes structural `Eq` and is provable exactly because the type is certified.

## Part 7 — `widen_b : Bin -> Option(BigFlt)`

Total byte-level decode; `none` unless the length is exactly 4. Field extraction with the same arithmetic `to_str` already uses (`exp_field = b3 % 0x80 * 2 + b2 / 0x80`, etc. — consider factoring a shared decode helper); the sign bit becomes the mantissa's `BigInt` sign, and the exponent crosses over via `BigInt/of_int`:

- Normalized (`exp_field` 1–254): `mk(±(mant_field + 0x800000), exp_field − 150)`.
- Subnormal (`exp_field` 0, `mant_field ≠ 0`): `mk(±mant_field, −149)`.
- Zero: the canonical zero — **both `+0.0` and `-0.0` byte patterns, by design** (documented collapse; see Part 9).
- `exp_field` 255 (Inf/NaN): `none`.

Exactness is definitional — nothing here rounds. Wrapper: `widen(f) = widen_b(Flt/to_le_bin(f))`.

## Part 8 — `narrow_b : BigFlt -> Bin` and `narrow_ratio_b`

- `narrow_b`: unbiased exponent from `bit_len(magnitude) + exponent`; sign from the mantissa; overflow (`> 127` after rounding) → ±Inf bytes; normal range → top 24 bits with guard/sticky from the rest, round-to-nearest-even, carry-propagate (may bump the exponent, re-check overflow); below `−126` → round on the `2^(−149)` grid (subnormal, reduced precision — *not* "leading 24 bits") or signed zero (the sign of a nonzero `x` that rounds to zero survives into the byte pattern). All shifts and comparisons — no division.
- `narrow_ratio_b(num, den)`: correctly-rounded quotient, the plan's only division, implemented as the base-2 compare-subtract-double digit loop (the Dragon4-`generate` shape) against `den`'s magnitude. Total, with IEEE edge semantics: `0/0` → NaN bytes, `x/0` → signed-Inf bytes. **Forward-compatibility**: this loop is precisely stage 2's `narrow` engine — building it now pre-pays that migration.
- Wrappers: `narrow = Flt/of_le_bin ∘ narrow_b`, `narrow_ratio = Flt/of_le_bin ∘ narrow_ratio_b` (needs Part 1).
- Dependencies: computationally this part needs only Parts 2–4 (bit helpers, `BigNat`/`BigInt` `cmp`, the representation); Part 6's order layer is consumed by Part 9's *statements*, not here.

## Part 9 — Boundary theorems (byte-level; all statable and provable with Parts 2–8)

Statements avoid `!` in type positions by carrying hypotheses as `Eq` premises.

- **Round-trip**: for `b : Bin` with `Eq(widen_b(b), Option/some(x))` and `b` not the `-0.0` pattern: `Eq(narrow_b(x), b)`. Companion lemma: the `-0.0` pattern widens to the canonical zero and `narrow_b(zero)` is the `+0.0` pattern — the sign collapse is a documented semantic fact, not a proof gap (`Flt` term equality is bitwise, so no stronger statement is true).
- **Correct rounding**: for `x` whose `narrow_b` output is a finite pattern — the precise hypothesis, since "in range" must mean *after* rounding: `|x|` below the `2^128 − 2^103` rounding boundary, carried as a premise on `narrow_b(x)`'s bytes (without it, "widened back" is undefined: `widen_b` of the Inf pattern is `none`) — no representable value is nearer: for all 4-byte `b` with `Eq(widen_b(b), some(y))`: `Lte(abs(sub(r, x)), abs(sub(y, x)))` where `r` is `narrow_b(x)` widened back; plus the ties-to-even refinement and the half-ulp corollary.
- **`narrow_ratio_b` correctness**: with `NonZero(den)`, the result is the correctly-rounded exact quotient — stated *denominator-cleared* (all inequalities cross-multiplied by `den`, with `abs` handling its sign), which the ring + order layers express without any exact division. The technique users will apply to their own interior quotients, exercised once in the stdlib's own spec.

Expected fiddly cases: subnormal boundary, exact ties, exponent extremes, zero. This remains the fiddliest part; it is now also actually provable.

## Module registration

- `curios-text/std/BigInt.crs` and `curios-text/std/BigFlt.crs`; `curios-text/std.crs` in the order `BigNat` → `BigInt` → `BigFlt` (witness binding order); the `include_str!` table in `curios-text/src/prelude.rs`.
- Operator witnesses (`Add`/`Sub`/`Mul`/`Eql`/`Cmp` for both new types) are declared in the `/std` operator facade modules (`std/Add.crs`, `std/Eql.crs`, `std/Cmp.crs`, …), following the `BigNat` precedent — not in the types' own modules. **No `Div` witness in stage 1** — `x / y` on `BigFlt` not typechecking is honest and reserves the operator for stage 2.

## Soundness discipline (binding for every part)

- Every `Prop`-typed definition is built via the checked eliminators (`Nat`/`Lst`/`Bin` `; ih` matches) and checked case analysis on `induct`/reflection values; recursion over derivations, if ever needed, is `rec` on a structural subterm (the `lte_succ_r` pattern) — never a bare self-reference (`rec p : P = p` typechecks today and must be rejected in review).
- Computations (`mk`'s strip, `narrow_b`'s loops) use `rec` freely; no proof depends on a `rec` computation's termination for its logical content (`strip` carries explicit fuel precisely so its proof doesn't have to).
- Certificates are reflection-`Eq`s over computable `Bln` checks — no new `induct`s to positivity-vet. Proof irrelevance means no obligation ever inspects a certificate; if an equality goal seems to need certificate equality, something is mis-stated.
- Everything above would be accepted unchanged by a future termination/positivity checker.

## Stage 2 — the denominator extension (deferred; its *contract* binds stage 1)

**Trigger**: a real workload needing exact quotients as interior operands where denominator clearing is untenable (nested ratios, exact linear algebra, rational oracles) — not speculative completeness. Exact-decimal demands point to a separate decimal type instead.

**Contract on stage 1** (what makes the extension a drop-in): representation stays `struct`-private; **no exported lemma may assert representation completeness** (no "every `BigFlt` is `mk(m, e)`" surjectivity, no case-analysis principle over the dyadic form, nothing that denies a denominator) — existential and law statements only; no `Option`-valued exact division ever ships; all exported statements mention only the abstract type and exported operations.

**The extension** (not a carrier swap): the struct gains one field — `denominator : BigNat` — and the certificate gains two conjuncts (`denominator` odd; `gcd(|mantissa|, denominator) = 1`), with the canonical zero pinning `denominator = 1`. Value: `mantissa · 2^exponent / denominator` — ℚ presented with the 2-part of every denominator kept in `exponent` and only the odd part materialized. The dyadics are exactly the `denominator = 1` stratum, so **every stage-1 value, algorithm, and proof survives as that stratum**: promotion and demotion are not operations, just what canonicalization computes (a result's reduced denominator either is 1 or is not). Ops stay uniform — mul is mantissa·mantissa, exponent+exponent, denominator·denominator, then cross-gcd reduction that is trivially absent when both denominators are 1 — so dyadic-only workloads keep stage-1 cost exactly, and no `2^k` denominator is ever materialized. `div` becomes total up to the explicit zero-denominator decision (`Option` or an engineered trap — no trap mechanism exists today) and gets the `Div` witness; `narrow_b` is reimplemented on `narrow_ratio_b`'s engine.

**New proof work then**: strong induction over `Nat` (bounded-induction derivation from `Lte`), `BigNat/divmod` + `gcd` with specs (`a = b·q + r`, `r < b` via reflection), the reduced-form uniqueness theorem (Euclid's lemma / coprimality — the true summit), and certificate preservation through the extended `mk`. **Survives unchanged**: Part 1, the entire Part 2 and Part 3 lemma bases, the Part 4 kernel (as the `denominator = 1` stratum), every exported statement, the byte-level scaffolding and theorem statements, `narrow_ratio_b`.

## Build order (stage 1)

1. Part 1 (`Flt/of_le_bin`) — independent, start immediately.
2. Part 2 (`BigNat` certificate + `mul` + parity + lemma base) — independent, start immediately.
3. Part 3 (`BigInt`) — needs Part 2.
4. Part 4 (`struct`, `mk`, canonicity kernel) — needs Parts 2–3; **its proofs gate Part 5's code** (`mk`'s type demands the certificate).
5. Part 5 (ops + ring laws) — needs Part 4.
6. Part 6 (order layer) — needs Part 5.
7. Part 7 (`widen_b`) — needs Part 4 only.
8. Part 8 (`narrow_b`, `narrow_ratio_b`) — needs Parts 2–4; wrappers need Part 1.
9. Part 9 (boundary theorems) — needs Parts 6–8.
10. Module registration — last.

Parts 1 and 2 are the two independent starting points; Parts 7 and 8 can proceed in parallel with Parts 5–6.
