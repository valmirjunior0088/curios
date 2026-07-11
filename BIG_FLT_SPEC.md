# BigFlt implementation specification

Design and build plan for exact binary-rational arithmetic (`BigFlt`) with proof-carrying conversion to and from the native `Flt` type, plus the one compiler primitive (`Flt/of_le_bin`) it depends on. Staged: **stage 1** ships `BigFlt` as a dyadic rational (arbitrary-precision binary float, ℤ[1/2]); **stage 2** — deferred until a real workload demands exact interior division — swaps the private carrier to a full rational behind the same API. This document is a working implementation reference, not permanent architecture documentation.

## Motivation

Native `Flt` arithmetic (`curios-text/std/Flt.crs`) compiles to opaque `/sys/Flt` wasm primitives. They carry no algebraic laws, and — unlike `Nat`/`Bin`/`Lst`, which the elaborator treats as genuine free-monoid inductives (`elaborate_nat_match` in `curios-core/src/elaborate/match_.rs:80`, `curios-core/src/free_monoid.rs`) — `Flt` can have no structural induction principle: IEEE-754 add/mul are not associative and are not the free monoid on any generator.

The opacity goes further than arithmetic, and this drives the architecture: **any prim application on an open term is stuck** (`convert/prim.rs` is congruence-only; `reduce` fires on literals), and there is no eliminator on `Flt`, so a ∀-quantified `Eq` mentioning `to_le_bin(f)` or `of_le_bin(...)` for open `f` is unprovable — no proof can even establish that `to_le_bin(f)` has four bytes. Therefore every theorem in this plan is stated at the **byte level** (`Bin` → `BigFlt` → `Bin`), where everything is data and fully provable; the `Flt`-facing functions are thin unproved wrappers around the two reinterpret prims, which are the plan's entire (and explicitly named) trust boundary. No conversion rule equating `of_le_bin(to_le_bin(x)) ≡ x` is added to the compiler — that would be a postulate in reducer clothing.

`BigFlt` is a dyadic rational: `(-1)^sign · mant · 2^exp`. Every finite `f32` is exactly such a number, so the embedding loses nothing; sums, differences, and products are closed and exact (ℤ[1/2] is a ring and an integral domain); and its canonical form (odd mantissa) needs only a parity argument, not gcd theory. Division is deliberately absent from stage 1: interior quotients are handled by denominator clearing, and boundary quotients by a correctly-rounded `narrow_ratio` — which is a strictly stronger contract (exact value, rounded once) than exact-divide-then-round. Precedent: Flocq and the HOL float formalizations model floats exactly this way (`m · 2^e` pairs), not as reduced fractions.

## Goals (stage 1)

- `BigFlt` as a private-representation dyadic rational, canonical by construction (compiler-enforced via `struct`).
- Exact, closed `add`/`sub`/`mul`/`neg`/`abs` with proved ring laws (commutativity, associativity, distributivity, additive and multiplicative cancellation — cancellation is the integral-domain substitute for inverse reasoning).
- An order layer: `Bln`-valued `cmp`/`lt`/`lte` plus boolean-reflection `Prop`s and the monotonicity lemmas Part 8's bounds consume.
- `widen_b : Bin -> Option(BigFlt)`: exact, total byte-level decode. `narrow_b : BigFlt -> Bin`: correctly-rounded (RNE) byte-level encode. `narrow_ratio_b : (BigFlt, BigFlt) -> Bin`: correctly-rounded quotient — the only division anywhere, at the boundary, where rounding is definitionally the intent.
- Byte-level theorems: round-trip (with the `-0.0` carve-out), correct-rounding/half-ulp bound, `narrow_ratio_b` correctness (stated denominator-cleared).
- `Flt`-facing wrappers `widen`/`narrow`/`narrow_ratio` via `to_le_bin`/`of_le_bin` (Part 1) — unproved glue by design.
- Every proof through genuinely checked structural elimination; zero postulates; nothing that a future stricter checker (termination on `rec`, positivity on `induct`) would reject.

## Non-goals

- Nothing about the native `/sys/Flt` primitives themselves (unchanged from the original plan; would require trusting wasm IEEE conformance).
- No exact interior division, no field laws, no `Div` witness — stage 2. Formulas with interior quotients are restructured by denominator clearing (e.g. `n²·Var = n·Σx² − (Σx)²`) or deferred to `narrow_ratio`.
- No exact `sqrt`/constructive reals (unchanged). The order layer would support future two-sided-bound `sqrt` approximations; out of scope here.
- No exact decimal arithmetic: `0.1` is not dyadic *and not an `f32`* — float literals are binary before `BigFlt` ever sees them. If exact decimal reasoning is ever needed (money), that is a separate ℤ[1/10]-style type, not `BigFlt` and not a reason for `Rat`.
- No `postulate` mechanism, no termination/positivity checking (all confirmed absent; worked around by discipline).
- No `BigFlt/of_str` in stage 1 (a decimal parser would have to round, muddying "exact"; revisit in stage 2 where it can be exact).

## Background facts (verified against the codebase; do not re-derive)

- `Flt` is IEEE-754 single precision (`curios-base/src/flt.rs`, stored as `bits: u32`). **Term identity is bitwise**: `NaN == NaN` and `0.0 != -0.0` *as terms* (flt.rs doc), so propositional `Eq` on `Flt` is bit equality, not IEEE `==`. This is why the round-trip theorem must carve out `-0.0`.
- `Flt/to_le_bin` exists (`curios-cont/src/into_wasm/code_emitter.rs:918-951`, `I32ReinterpretF32`), used by `Flt/to_str`. No reverse primitive exists. `F32ReinterpretI32` is already in `curios-wasm` (`expr.rs:294`, writer/parser/printer) — Part 1 needs no new wasm capability.
- Open-term prim opacity (see Motivation) — the load-bearing fact behind the byte-level architecture.
- `Flt/of_str` is not correctly rounded (parses via native `Flt` arithmetic); nothing correctly-rounded exists to lean on. `narrow_b` is new work, structurally akin to `to_str`'s Dragon4 machinery but in base 2 with fixed precision.
- `BigNat` (`curios-text/std/BigNat.crs`) is a `record` over little-endian base-10000 `Lst(Nat)` limbs with `add`, `sub` (truncating), `mul_small`, `mul_pow2`, `cmp`-family — **no general `mul`** (Part 2 adds it), no division, no `gcd`, and zero `Eq`/`Prop` usage yet.
- `Flt.crs`'s existing `divmod` is a repeated-subtraction digit extractor (small *quotient*, large scaled divisor) — fine for Dragon4, unusable as general division; irrelevant to stage 1, which needs no division at all.
- `record` vs `struct` (`SYNTAX.md:284`): `struct` = representation private to the declaring module (`PrivateRepresentation` otherwise). Beyond invariant enforcement, this buys **representation mobility** — the stage-2 carrier swap is safe because no outside code or proof can mention the fields.
- `rec` performs no termination check (`elaborate_rec`, `curios-core/src/elaborate/binding.rs:81-150`: assume binder types, check bodies, no decrease analysis) and accepts `Prop`-sorted bindings — `rec absurd : False = absurd;` compiles (re-verified empirically; unused items *are* elaborated, verified via an ill-typed dead item failing).
- Checked structural eliminators exist exactly for `Nat`/`Lst`/`Bin` (`; ih` arms; the ih is only available at the predecessor/tail by construction). `match` on an `induct` value is checked *case analysis* — exhaustiveness plus index-driven impossible-arm pruning — but provides no ih; recursion over derivations uses `rec` on a structural subterm (stdlib precedent: `lte_succ_r`, `lte_trans`), which a real termination checker would accept.
- No strict-positivity check; `Eq` and reflection-`Prop`s used here are manifestly positive. No `Acc`/strong induction in the stdlib — **stage 1 does not need it** (stage 2's Euclid does).
- `Eq.crs` provides `sym`/`trans`/`cong`/`subst`. `Lte` on `Nat` exists (`Nat.crs:82`).
- The stdlib has no user-code trap mechanism (no `panic`/`unwrap`). Moot in stage 1: dyadic `mk` is total. Stage 2 must decide zero-denominator behavior explicitly.
- `Int` literals carry a 31-bit payload — ample for exponents (`f32` needs `[-149, 127]`).

## Part 1 — `Flt/of_le_bin` compiler primitive

Assembles a native `Flt` from its 4 little-endian bytes, mirroring `Flt/to_le_bin`: OR the bytes into an `i32`, emit `F32ReinterpretI32`. Traps (prim-level `Unreachable`, precedent: `FltToNat` on negative) unless the `Bin` is exactly 4 bytes.

**Touch points** (grep-verified against `FltToLeBin`'s actual footprint):

- `curios-core/src/prim.rs` (`FltOfLeBin(Term)`), `elaborate/prim.rs` (`Bin -> Flt`), `erase/prim.rs`, `convert/prim.rs`, `reduce/prim.rs` (fold on closed 4-byte literals), `zonk.rs`, `print.rs`
- `curios-ersd/src/prim.rs`, `into_cont/lower_prim.rs`, `optimize/{evaluate,rewrite}.rs`, `print.rs`
- `curios-cont/src/module.rs`, `print.rs`, `into_wasm/code_emitter.rs`, **`optimize/walk.rs`** (exhaustive `walk_code_operands!` — the build breaks without the new arm), **`optimize/scalar_eval.rs`** (has a `_ => None` catch-all, so optional for compilation, but required for const-folding parity with `to_le_bin` — closed test programs const-fold, so parity affects what tests exercise)
- `curios-text/src/prim.rs`, `prelude.rs` (`unary("of_le_bin", bin(), flt(), ...)`), `into_core/lowerer.rs`, `print.rs`; `curios-text/std/Flt.crs` re-export list
- The `FltToLeBin` hits in `curios-cont/src/optimize/{constant_folding,evaluate_pure_calls}.rs` are `#[cfg(test)]`-only — no obligation (extending those tests is optional).
- Tests: `curios/src/tests/codegen/code_flt.rs` (+ `codegen.rs`, `codegen/module.rs` as needed). Round-trip fixtures for 0.0, `-0.0`, subnormals, NaN (payloaded), ±Inf — **compare via `to_le_bin` bytes, never `==`** (IEEE `eql` fails NaN and spuriously passes `-0.0 == +0.0`).

Independent of everything else; needed only by the Part 7 wrappers.

## Part 2 — `BigNat` multiplication and lemma base

The carrier-independent foundation both stages stand on, and the plan's biggest single work item. Add to `BigNat.crs`:

- `mul(a, b) -> BigNat` — schoolbook over limbs (`mul_small` per limb + limb shift + `add`). Required by `BigFlt/mul` and by every algebraic law.
- Parity/binary infrastructure: `is_even`, `div2` (both **structural folds** — little-endian limb folds work because the base 10000 is even: half of `l + B·t` is `(l + r·B)/2 + q·B` with `t = 2q + r`, and the low bit is `l % 2`), `bit_len`, top-bits extraction helpers for Part 7.
- The semiring lemma base, by `Lst`-`; ih` induction: `add_comm`, `add_assoc`, `mul_comm`, `mul_assoc`, `distrib`, `add_cancel`, `mul_pow2`/`add`/`mul` interaction lemmas, `cmp` order lemmas. Permanent stdlib assets; they survive stage 2 unchanged.

## Part 3 — `BigFlt` representation and canonicity kernel

```
pub struct BigFlt : Type { sign : Bln, mant : BigNat, exp : Int }
```

Value: `(-1)^sign · mant · 2^exp`. Canonical form: `mant` odd, or the unique zero `{ sign = false, mant = zero, exp = +0 }`. The exported smart constructor `mk(sign, mant, exp)` is **total** (no invalid inputs, nothing traps): it strips trailing zero bits of `mant` into `exp` (`rec` iteration of `div2` — computation, so `rec` is fine) and forces the canonical zero (sign *and* exponent) when `mant` is zero.

The **canonicity kernel** — deliberately the smallest possible trusted-by-effort core, and the stage-1 summit:

- Uniqueness: `a·2^e = b·2^f` with `a`, `b` odd implies `a = b` and `e = f` (parity argument over `mul_pow2`; shallow).
- `strip` correctness (value preservation, odd-or-zero result): by **fuel-indexed structural induction** (fuel `14 · limb-count + 1` bounds the bit length; the bound is a `Lst` induction) — strong induction is *not* required.
- `mk`-respects: equal values (alignment equality on raw pairs: `mul_pow2(m₁, e₁−e₂) = m₂` at the common exponent) yield structurally `Eq` results.

Contrast deliberately noted: the stage-2 analogue of this kernel is gcd-reduced-form uniqueness (Euclid's lemma, coprimality) — the reason Rat is deferred.

## Part 4 — Arithmetic and ring laws

- `add`/`sub`: align exponents (`mul_pow2` shift to the smaller), sign-magnitude case split on `BigNat/cmp`, then `BigNat` add/sub; through `mk`. `mul`: multiply mantissas, add exponents; through `mk` (odd·odd is odd — `mk` strips only after equal-exponent add/sub, e.g. `x − x`, `1/2 + 1/2`). `neg`/`abs`: sign ops (zero stays canonical). `eql`: structural (valid because canonical). `cmp`/`lt`/`lte`: sign split, then align-and-compare — no cross-multiplication.
- Proofs, staged: (1) laws for the *raw pair* operations over alignment equality — these reduce to Part 2's `BigNat` lemmas; (2) transport through `mk` via the Part 3 kernel. Exported laws: `add_comm`, `add_assoc`, `mul_comm`, `mul_assoc`, `distrib`, `add_cancel`, `mul_cancel` (nonzero premise via reflection). No inverse laws — cancellation is the integral-domain substitute.

## Part 5 — Order layer

- `Prop`s by boolean reflection — `Lte(x, y) := Eq(BigFlt/lte(x, y), true)`, likewise `Lt`, `NonZero` — no new `induct`, hence no positivity vetting.
- Lemma stack, sized to what Part 8 consumes (not a full order-theory library): reflexivity/antisymmetry/transitivity/totality, `add`-monotonicity, `mul`-monotonicity under nonnegativity, `abs`/`sub` interaction (`Lte(abs(sub(a, b)), c)` manipulation).

## Part 6 — `widen_b : Bin -> Option(BigFlt)`

Total byte-level decode; `none` unless the length is exactly 4. Field extraction with the same arithmetic `to_str` already uses (`exp_field = b3 % 0x80 * 2 + b2 / 0x80`, etc. — consider factoring a shared decode helper):

- Normalized (`exp_field` 1–254): `mk(sign, mant_field + 0x800000, exp_field − 150)`.
- Subnormal (`exp_field` 0, `mant_field ≠ 0`): `mk(sign, mant_field, −149)`.
- Zero: the canonical zero — **both `+0.0` and `-0.0` byte patterns, by design** (documented collapse; see Part 8).
- `exp_field` 255 (Inf/NaN): `none`.

Exactness is definitional — nothing here rounds. Wrapper: `widen(f) = widen_b(Flt/to_le_bin(f))`.

## Part 7 — `narrow_b : BigFlt -> Bin` and `narrow_ratio_b`

- `narrow_b`: unbiased exponent from `bit_len(mant) + exp`; overflow (`> 127` after rounding) → ±Inf bytes; normal range → top 24 bits with guard/sticky from the rest, round-to-nearest-even, carry-propagate (may bump the exponent, re-check overflow); below `−126` → round on the `2^(−149)` grid (subnormal, reduced precision — *not* "leading 24 bits") or signed zero; assemble bytes. All shifts and comparisons — no division.
- `narrow_ratio_b(num, den)`: correctly-rounded quotient, the plan's only division, implemented as the base-2 compare-subtract-double digit loop (the Dragon4-`generate` shape) against `den`'s mantissa. Total, with IEEE edge semantics: `0/0` → NaN bytes, `x/0` → signed-Inf bytes. **Forward-compatibility**: this loop is precisely stage 2's `narrow` engine — building it now pre-pays that migration.
- Wrappers: `narrow = Flt/of_le_bin ∘ narrow_b`, `narrow_ratio = Flt/of_le_bin ∘ narrow_ratio_b` (needs Part 1).

## Part 8 — Boundary theorems (byte-level; all statable and provable with Parts 2–7)

Statements avoid `!` in type positions by carrying hypotheses as `Eq` premises.

- **Round-trip**: for `b : Bin` with `Eq(widen_b(b), Option/some(x))` and `b` not the `-0.0` pattern: `Eq(narrow_b(x), b)`. Companion lemma: the `-0.0` pattern widens to the canonical zero and `narrow_b(zero)` is the `+0.0` pattern — the sign collapse is a documented semantic fact, not a proof gap (`Flt` term equality is bitwise, so no stronger statement is true).
- **Correct rounding**: for `x` in finite range, no representable value is nearer — for all 4-byte `b` with `Eq(widen_b(b), some(y))`: `Lte(abs(sub(r, x)), abs(sub(y, x)))` where `r` is `narrow_b(x)` widened back; plus the ties-to-even refinement and the half-ulp corollary.
- **`narrow_ratio_b` correctness**: with `NonZero(den)`, the result is the correctly-rounded exact quotient — stated *denominator-cleared* (all inequalities cross-multiplied by `den`), which the ring + order layers express without any exact division. The technique users will apply to their own interior quotients, exercised once in the stdlib's own spec.

Expected fiddly cases: subnormal boundary, exact ties, exponent extremes, zero. This remains the fiddliest part; it is now also actually provable.

## Module registration

- `curios-text/std/BigFlt.crs`; `curios-text/std.crs` (place after `BigNat` — witness binding order); the `include_str!` table in `curios-text/src/prelude.rs`. Operator witnesses for `BigFlt` (`Add`/`Sub`/`Mul`/`Eql`/`Cmp`) may be declared; **no `Div` witness in stage 1** — `x / y` on `BigFlt` not typechecking is honest and reserves the operator for stage 2.

## Soundness discipline (binding for every part)

- Every `Prop`-typed definition is built via the checked eliminators (`Nat`/`Lst`/`Bin` `; ih` matches) and checked case analysis on `induct`/reflection values; recursion over derivations, if ever needed, is `rec` on a structural subterm (the `lte_succ_r` pattern) — never a bare self-reference (`rec p : P = p` typechecks today and must be rejected in review).
- Computations (`mk`'s strip, `narrow_b`'s loops) use `rec` freely; no proof depends on a `rec` computation's termination for its logical content.
- New `Prop`s are reflection-`Eq`s only — no new `induct`s to positivity-vet in stage 1.
- Everything above would be accepted unchanged by a future termination/positivity checker.

## Stage 2 — the Rat migration (deferred; its *contract* binds stage 1)

**Trigger**: a real workload needing exact quotients as interior operands where denominator clearing is untenable (nested ratios, exact linear algebra, rational oracles) — not speculative completeness. Exact-decimal demands point to a separate decimal type instead.

**Contract on stage 1** (what makes the swap a drop-in): representation stays `struct`-private; **no exported lemma may assert representation completeness** (no "every `BigFlt` is `mk(s, m, e)`" surjectivity, no case-analysis principle over the dyadic form) — existential and law statements only; no `Option`-valued exact division ever ships; all exported statements mention only the abstract type and exported operations.

**The swap**: carrier becomes `{ sign : Bln, num : BigNat, den : BigNat }`; `mk(sign, mant, exp)` remains (still exactly representable — every stage-1 statement survives verbatim); new `mk(sign, num, den)` with gcd reduction, canonical zero (`sign` positive, `den = 1`), and an explicit zero-denominator decision (`Option` or an engineered trap — no trap mechanism exists today); total `div` + the `Div` witness; `narrow_b` reimplemented on `narrow_ratio_b`'s engine. Rat restricted to dyadics agrees with stage-1 semantics exactly, and stage-1 programs can only construct dyadics, so **no observable behavior changes**.

**New proof work then**: strong induction over `Nat` (bounded-induction derivation from `Lte`), `BigNat/divmod` + `gcd` with specs (`a = b·q + r`, `r < b` via reflection), and the reduced-form uniqueness theorem (Euclid's lemma / coprimality — the true summit), then re-transport of the ring laws through the new `mk`. **Survives unchanged**: Part 1, the entire Part 2 lemma base, every exported statement, the byte-level scaffolding and theorem statements, `narrow_ratio_b`.

## Build order (stage 1)

1. Part 1 (`Flt/of_le_bin`) — independent, start immediately.
2. Part 2 (`BigNat` `mul` + parity + lemma base) — independent, start immediately.
3. Part 3 (`struct`, `mk`, canonicity kernel) — needs Part 2's parity/`div2`.
4. Part 4 (ops + ring laws) — needs Parts 2–3.
5. Part 5 (order layer) — needs Part 4.
6. Part 6 (`widen_b`) — needs Part 3 only.
7. Part 7 (`narrow_b`, `narrow_ratio_b`; wrappers) — needs Parts 2–5; wrappers need Part 1.
8. Part 8 (boundary theorems) — needs Parts 6–7.
9. Module registration — last.

Parts 1 and 2 are the two independent starting points.
