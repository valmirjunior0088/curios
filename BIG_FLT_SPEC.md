# BigFlt implementation specification

Design and build plan for exact rational arithmetic (`BigFlt`) with proof-carrying, seamless conversion to and from the native `Flt` type, plus the one compiler primitive (`Flt/of_le_bin`) it depends on. This document is a working implementation reference, not permanent architecture documentation — it captures the reasoning and decisions from the design discussion so implementation can proceed without re-deriving them.

## Motivation

Native `Flt` arithmetic (`add`/`sub`/`mul`/`div`/`sqrt`, in `curios-text/std/Flt.crs`) compiles straight to opaque `/sys/Flt` wasm primitives. They carry no algebraic laws the compiler can reason about, and — unlike `Nat`/`Bin`/`Lst`, which the core elaborator treats as genuine free-monoid inductives (`curios-core/src/elaborate/match_.rs`'s `elaborate_nat_match`, `curios-core/src/free_monoid.rs`) — there is no way to give `Flt`'s arithmetic an analogous structural induction principle, because IEEE-754 float addition and multiplication are not associative and are not the free monoid on any generator. That fact is true, not missing tooling, so no compiler change closes the gap for `Flt` itself.

The goal is instead an exact numeric type, `BigFlt`, built the same way `BigNat` already is (a from-scratch recursive Curios definition, not a `/sys` foreign primitive), whose arithmetic is real Curios code amenable to genuine `Eq`-based algebraic proofs (associativity, commutativity, distributivity), plus a lossless embedding from `Flt` and a correctly-rounded, provably-bounded projection back down to `Flt`. Users who want proofs route through `BigFlt` at the boundary of a computation; users who only want fast floating point never touch it.

## Goals

- `BigFlt` is a full `Rat` (arbitrary-precision numerator/denominator, sign-magnitude), named `BigFlt` because its purpose is bridging to and from `Flt`, not because it mirrors `Flt`'s bit layout.
- `BigFlt` arithmetic is closed and exact under `+`, `-`, `*`, `/` (division of rationals never rounds — cross-multiplication, not the operand's own division).
- `widen : Flt -> Option(BigFlt)` is an exact, total (on finite values), zero-postulate embedding.
- `narrow : BigFlt -> Flt` is a correctly-rounded (round-to-nearest-even), zero-postulate projection, implemented as real Curios code whose rounding-bound property is proved from its own definition, not assumed.
- Every algebraic and round-trip proof is built through genuinely checked structural elimination (`match` on `Nat`/`Lst`/`Bin`/`induct` values), never through a bare `rec` self-reference, so nothing in the proof suite would clash with a future stricter/"safe subset" checker.
- `Flt/of_le_bin` (bytes to float) is added to the compiler as the mirror of the existing `Flt/to_le_bin`, needed to assemble `narrow`'s final result.

## Non-goals

- Not proving anything about the native `/sys/Flt` primitives themselves (e.g. `Flt/add(a,b) == narrow(BigFlt/add(widen(a), widen(b)))`). That would require trusting IEEE-754 conformance of the wasm runtime — a legitimate, narrow, standard-mandated assumption, but a postulate, and out of scope here. Nothing in this plan needs it: every proof stays inside `widen` → `BigFlt` arithmetic → `narrow`.
- Not supporting exact `sqrt` or a general constructive-reals type. `BigFlt` is a `Rat`; square roots of rationals are generally irrational. `stddev`-style proofs that need `sqrt` stay out of scope; only the closed operations (`+`, `-`, `*`, `/`) get algebraic proofs.
- Not adding a `postulate`/`axiom` mechanism to the compiler. Confirmed none exists today (every `Prop` in the stdlib is actually constructed); this plan doesn't need one.
- Not adding termination checking to `rec` or strict-positivity checking to `induct`. Both are confirmed absent and are intentional design choices (Curios is `Type : Type` with general recursion everywhere, PL-first, safe subset planned later). This plan works around their absence by discipline (see Soundness discipline below), not by changing them.

## Background facts established during design (do not re-derive these)

- `Flt` is IEEE-754 **single precision** (`f32`: 1 sign bit, 8 exponent bits, 23 mantissa bits, bias 127) — `curios-base/src/flt.rs`. Not double precision.
- `Flt/to_le_bin` exists and decomposes a float into 4 little-endian bytes via the wasm instruction `I32ReinterpretF32` (`curios-cont/src/into_wasm/code_emitter.rs:918-951`), used today by `Flt/to_str`. No reverse primitive exists.
- `Flt/of_str` (decimal string to float) is **not** correctly-rounded today — it parses via native `Flt` arithmetic (`Nat/to_flt(int_val) + Nat/to_flt(frac_val) / pow10(...)`, then multiplies by a power-of-ten factor), accumulating the very rounding error a real parser would avoid. There is no existing correctly-rounded conversion in the codebase to lean on; `narrow` is genuinely new algorithmic work, structurally closest to `Flt/to_str`'s Dragon4-style `scale_up`/`scale_down`/`generate` (exact big-integer scaling and comparison), not to `of_str`.
- `BigNat` (`curios-text/std/BigNat.crs`) is a real recursive definition over `Lst(Nat)` limbs (base 10000), not a `/sys` foreign primitive — but it has zero `Eq`/`Prop` usage today; no property of its own arithmetic (not even commutativity of `add`) has been proved yet.
- `BigNat` has no general division or `gcd` yet — only `mul_small`, `mul_pow2`, and `Flt.crs`'s `divmod`, which produces one decimal digit at a time against a known small divisor (used by the Dragon4 renderer), not general `BigNat ÷ BigNat`.
- `record` vs. `struct` (`SYNTAX.md:284`): `record` makes a type's representation public (any caller constructs/projects fields directly); `struct` makes it private to the declaring module (construct/project only through exported helpers, else a `PrivateRepresentation` compile error). `BigNat` is a `record`. `BigFlt` must be a `struct`, so its "always reduced to lowest terms" invariant is compiler-enforced, not merely conventional.
- `rec` performs no termination check at all (`elaborate_rec`, `curios-core/src/elaborate/binding.rs:81-140`: assumes each recursive binder's type, checks the body, no decrease analysis). This is intentional and matches existing precedent (`Flt.crs`'s `divmod`/`scale_up`/`scale_down` already use non-structural, repeated-subtraction-style recursion and compile today) — so computing `BigNat` division/`gcd` via `rec` is unblocked.
- `rec`'s type-check does not exclude `Prop`-sorted bindings, so `rec absurd : False = absurd;` typechecks today. Genuine proofs must never be written this way (see Soundness discipline).
- No strict-positivity check exists for user `induct` declarations (grepped, no hits). Not a live problem for anything in this plan (`Eq`/`Lte`, the only inductives this plan reuses, are both manifestly, trivially strictly positive by inspection), but any new `Prop`-classified inductive this plan introduces must be manually vetted.
- No strong/well-founded induction principle exists in the stdlib (no `Acc`, no `strong_induction`). Needed because Euclid's algorithm decreases by `a mod b`, not by `-1`, so it is out of reach of ordinary (weak) `Nat` induction directly. Buildable from the existing `Lte` (`curios-text/std/Nat.crs`) via the standard "bounded induction" derivation — see Part 2.
- The structural eliminators for `Nat`/`Bin`/`Lst` (`elaborate_nat_match`, the free-monoid destructors) are a genuinely separate, checked code path from `elaborate_rec`, and really do enforce structural decrease (the induction hypothesis is only available at the actual predecessor/tail, by construction of the eliminator). All proofs in this plan must go through this path.

## Part 1 — `Flt/of_le_bin` compiler primitive

Assembles a native `Flt` from its 4 little-endian bytes (`Bin` of length 4), the mirror of `Flt/to_le_bin`.

**Wasm codegen**: OR the 4 bytes back into an `i32` (reverse of `to_le_bin`'s shift-and-split at `code_emitter.rs:918-951`), then emit `F32ReinterpretI32` (the existing counterpart to `to_le_bin`'s `I32ReinterpretF32`). No new wasm capability required.

**Trapping**: traps immediately if the input `Bin` is not exactly 4 bytes long. Confirmed acceptable — matches how other primitives already trap on their own invalid-input cases.

**Touch points**, mirroring every existing occurrence of `to_le_bin` (confirmed by grep — a new primitive touches the same set of files as any existing one, this is the standard recipe, not novel architecture):
- `curios-core/src/prim.rs` (new `Prim` variant, e.g. `FltOfLeBin(Term)`)
- `curios-core/src/elaborate/prim.rs` (type rule: `Bin -> Flt`)
- `curios-core/src/erase/prim.rs`, `curios-core/src/convert/prim.rs`, `curios-core/src/reduce/prim.rs`, `curios-core/src/zonk.rs`, `curios-core/src/print.rs`
- `curios-ersd/src/prim.rs`, `curios-ersd/src/into_cont/lower_prim.rs`, `curios-ersd/src/optimize/{evaluate,rewrite}.rs`, `curios-ersd/src/print.rs`
- `curios-cont/src/module.rs`, `curios-cont/src/print.rs`, `curios-cont/src/into_wasm/code_emitter.rs` (the actual codegen described above)
- `curios-text/src/prim.rs`, `curios-text/src/prelude.rs`, `curios-text/src/into_core/lowerer.rs`, `curios-text/src/print.rs`
- `curios-text/std/Flt.crs` (add `of_le_bin` to the `pub use /sys/Flt/{...}` re-export list alongside `to_le_bin`)
- Codegen tests: `curios/src/tests/codegen/code_flt.rs`, `curios/src/tests/codegen.rs`, `curios/src/tests/codegen/module.rs` (new round-trip test: `of_le_bin(to_le_bin(f)) == f` for representative values including 0.0, -0.0, subnormals, NaN, ±Inf)

Independent of everything in Part 2 onward; can be built any time. Needed as the final step of `narrow` (Part 7).

## Part 2 — Strong induction over `Nat`

Prerequisite for proving anything about `BigNat`'s `gcd`/`divmod` (Part 3), since Euclid's algorithm's recursion is not directly expressible via ordinary weak `Nat` induction.

Derive via the standard "bounded induction" technique, using the already-existing `Lte` (`curios-text/std/Nat.crs`):

1. Prove `bounded(@P : (Nat) -> Type, step : ..., n : Nat) -> (k : Nat) -> Lte(k, n) -> P(k)` by ordinary structural induction on `n` (a genuine `match n | 0 => ... | pred + 1; ih => ...`, going through the checked `Nat`-switch eliminator).
2. Define `strong_induction(n) = bounded(n, n, lte_refl(n))`, giving `P(n)` from a step function that may assume `P(k)` for all `k` with `Lte(k, n)` (i.e. all `k <= n`, not just `k = n - 1`).

This is new stdlib code (belongs in `Nat.crs` or a new file), composes only existing inductives (`Nat`, `Lte`), and needs no new `induct` type — so no positivity vetting is required for it.

## Part 3 — `BigNat` division and `gcd`

Add to `curios-text/std/BigNat.crs`:

- `divmod(a : BigNat, b : BigNat) -> {BigNat, BigNat}` — general long division (quotient, remainder), not Dragon4's specialized single-digit search in `Flt.crs`. A repeated-subtraction or shift-subtract implementation is fine computationally (unblocked by the absence of a `rec` termination check, matching `Flt.crs`'s existing `divmod`/`scale_up`/`scale_down` precedent); shift-subtract (subtracting `b` scaled by the largest power of `base` that still fits, per limb) is the performance-conscious version and is recommended over naive repeated subtraction, which would be slow for large operands.
- `gcd(a : BigNat, b : BigNat) -> BigNat` — Euclidean algorithm using `divmod`'s remainder.
- Correctness properties needed downstream (e.g. `gcd` actually divides both inputs, `divmod`'s quotient/remainder satisfy `a == b * q + r` with `r < b`) proved using Part 2's strong induction, via genuine `match`-based structural proofs — never a bare `rec` Prop-typed definition.

**Performance note**: if built via repeated subtraction rather than shift-subtract, `gcd`/`divmod` will be slow for large operands. Acceptable for `widen`/`narrow`'s bounded use (an `f32`'s exact value only ever needs numerator/denominator on the order of 2^149), a real risk for arbitrary user `BigFlt` arithmetic composed many times. Not a blocker; fixable later without touching the public API.

## Part 4 — `BigFlt` representation

```
pub struct BigFlt : Type { sign : Bln, num : BigNat, den : BigNat }
```

`struct`, not `record` — private representation, so the "always reduced to lowest terms" invariant can only be established through an exported smart constructor:

- `mk(sign : Bln, num : BigNat, den : BigNat) -> BigFlt` — traps if `den` is zero; otherwise divides both `num` and `den` by `BigNat/gcd(num, den)` (Part 3) before constructing.

Because every `BigFlt` is canonical by construction, structural `Eq` on the record's fields coincides with mathematical equality — no separate cross-multiplication equivalence relation is needed, which keeps every later algebraic proof a direct `Eq`/`cong`/`subst` argument rather than requiring an extra "canonical form respects the relation" layer.

## Part 5 — `BigFlt` arithmetic and algebraic-law proofs

- `add`, `sub`, `mul`, `div`, `neg`, `abs`, `eql` (structural, valid because canonical), `cmp`/`lt` (via cross-multiplication, sign-aware) — each expressed in terms of `BigNat` operations and routed through `mk` (Part 4) to stay canonical.
- Algebraic lemmas: commutativity and associativity of `add`/`mul`, distributivity of `mul` over `add`, proved by induction over the limb-list structure of the underlying `BigNat` operations (`Eq/cong`/`Eq/trans`/`Eq/subst`, the same combinator style `Eq.crs` already establishes) — the real, from-scratch proof work this whole plan exists to enable. No new `induct` type is needed for this (it's built entirely from `Eq`, `Nat`, `Lst`, `BigNat`, `BigFlt`), so no additional positivity vetting is required here either.
- Every one of these proofs must be a `match`-based structural definition, never a bare `rec` self-reference (Soundness discipline, below).

## Part 6 — `widen : Flt -> Option(BigFlt)`

Exact, total on finite values. Decompose using the existing bit layout `Flt/to_le_bin` already exposes:

- Sign bit, 8-bit exponent field (bias 127), 23-bit mantissa.
- Normalized (exponent field 1-254): value = `(-1)^sign * (1 + mantissa/2^23) * 2^(exponent-127)`, expressed exactly as a `BigFlt` with denominator a power of two (implicit leading 1 folded into the numerator), then reduced via `mk`.
- Subnormal (exponent field 0, mantissa != 0): value = `(-1)^sign * (mantissa/2^23) * 2^(-126)`.
- Zero (exponent field 0, mantissa 0): `BigFlt` zero, sign-aware (`+0.0` and `-0.0` both widen to the same canonical zero `BigFlt`, since `Rat` has no signed zero).
- Infinity/NaN (exponent field 255): `Option/none()` — `BigFlt` has no representation for either.

## Part 7 — `narrow : BigFlt -> Flt`

Correctly-rounded (round-to-nearest-even) projection. Structurally the same family of algorithm as `Flt/to_str`'s Dragon4 machinery (`scale_up`/`scale_down`/`generate` in `Flt.crs`) — exact big-integer scaling and comparison — but targeting a fixed 24 significant bits (base 2) instead of the shortest round-tripping decimal:

1. Determine the binary exponent `e` such that the value's magnitude falls in `[2^e, 2^(e+1))`, via `BigNat` comparisons against power-of-two scaled denominators (the `scale_up`/`scale_down` technique, base 2 instead of base 10).
2. Extract the leading 24 bits (1 implicit + 23 explicit mantissa) by repeated doubling of the remaining numerator against the denominator (the `generate`-style digit loop, base 2).
3. Round to nearest, ties to even, at the 24th bit; propagate any resulting carry into the exponent (mantissa overflow).
4. Handle exponent overflow (result rounds to a magnitude `>= 2^128`ish) by producing signed infinity; handle exponent underflow by producing a subnormal or signed zero.
5. Assemble the final sign/exponent/mantissa bits into a `Bin` of 4 bytes and call `Flt/of_le_bin` (Part 1) to produce the native `Flt`.

This is the one place the two parts of this plan connect: `narrow` cannot be finished without Part 1.

## Part 8 — Round-trip proof

`Eq(narrow(widen(f)!), f)` for every finite `f : Flt`. Expected to be the fiddliest part of the whole plan — likely needs case-by-case handling of: normalized values, subnormals, positive and negative zero, values exactly on a rounding tie, and the maximum/minimum exponent boundaries. Comparable in nature (not mechanism) to how fiddly the existing `to_str`/`of_str` are; no existing code directly covers this, since `of_str` is not correctly-rounded (see Background facts).

## Module registration

Three touch points, per existing convention for a new `/std` module:
- `curios-text/std/BigFlt.crs` — the new module itself.
- `curios-text/std.crs` — add `BigFlt` to the manifest.
- `curios-text/src/prelude.rs` — add `BigFlt` to the module array.

## Soundness discipline (binding for every part of this plan)

- Every `Prop`-typed definition (every lemma, every proof) must be constructed via `match` on an actual `Nat`, `Lst`, `Bin`, or `induct` value — through the genuinely checked structural eliminators — never via a bare `rec`/`let rec` self-reference. The compiler will not stop a bare-`rec` "proof" from typechecking (confirmed: `rec absurd : False = absurd;` typechecks today), so this is an unenforced convention that must be followed by hand and checked in review.
- Ordinary computation (`BigNat/divmod`, `BigNat/gcd`, and any other `Type`-sorted, non-proof code) may use `rec` as freely as `Flt.crs`'s existing `divmod`/`scale_up`/`scale_down` do. This is fine and does not need to terminate-check, because no proof in this plan may ever depend on trusting that a `rec`-defined computation terminates for its logical content — every proof re-derives what it needs structurally (e.g. via Part 2's strong induction).
- If any part of this plan needs a new `Prop`-classified `induct` type beyond `Eq` and `Lte` (not currently expected — see Parts 2 and 5), its constructors must be manually checked for strict positivity before use, since the compiler does not check this either.
- Following the above means every proof in this plan would be accepted unchanged by a future stricter checker (real termination checking on `rec`, strict positivity on `induct`), because none of it ever relied on the parts such a checker would newly restrict.

## Build order

1. Part 1 (`Flt/of_le_bin`) — independent, can start immediately.
2. Part 2 (strong induction over `Nat`) — independent, can start immediately, and is a prerequisite for Part 3's proofs.
3. Part 3 (`BigNat` division/`gcd`) — computation is unblocked without Part 2; proofs about it need Part 2.
4. Part 4 (`BigFlt` representation) — needs Part 3's `gcd`.
5. Part 5 (arithmetic + algebraic proofs) — needs Part 4.
6. Part 6 (`widen`) — needs Part 4; independent of Part 5.
7. Part 7 (`narrow`) — needs Part 4 and Part 1.
8. Part 8 (round-trip proof) — needs Parts 6 and 7.
9. Module registration — last, once the module's public surface is settled.

Parts 1 and 2 have no dependencies on each other or on anything else in this plan and are the two reasonable starting points.
