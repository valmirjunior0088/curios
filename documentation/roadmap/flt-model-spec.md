# `Flt` specified by a hardware-independent model

Working implementation specification for giving `Flt` a semantics that is stated in the repository rather than inherited from whatever machine the compiler runs on: a binary32 model over unbounded integers that every compiler stage folds with, two decided bounds that make the narrowings to `Nat` and `Int` total in the prelude's usual style, and a runtime held to the model at the two places it could otherwise disagree.

The decision this specification implements is [`Flt` is specified by a model, and the runtime conforms to it](../design/language/flt-is-specified-by-a-model-and-the-runtime-conforms.md). Nothing below has landed; the milestones in [§6](#6-milestones) are the order.

## 1. The semantics

**`Flt` is IEEE 754-2019 binary32 with exactly one NaN.** Its values are the finite dyadic rationals binary32 represents, two signed zeros, two signed infinities, and one NaN, whose bit pattern is `0x7fc00000`. Term identity is bitwise, and with one NaN that *is* value identity: `0.0` and `-0.0` are distinct values, `nan` is one value, and two terms of type `Flt` convert exactly when they denote the same value. This is the choice Lean, Rocq and Agda each made independently, for the reason the Wasm specification makes explicit: a NaN's sign and payload are the only thing IEEE leaves to the implementation.

**Every operation computes exactly and rounds once.** Rounding is to nearest, ties to even; subnormals are honored; a result past the largest finite value rounds to the infinity of its sign. The choices IEEE leaves open are pinned here rather than inherited from a target:

- `min`/`max` propagate a NaN and order `-0.0` below `+0.0` on an equal pair — IEEE 754-2019's `minimum`/`maximum`, which Wasm also mandates;
- `nearest` is ties-to-even;
- `rem` is exact `fmod`: the dividend's sign, never a rounding, `x % inf = x`, and NaN when the dividend is infinite or the divisor is zero;
- `sqrt(-0.0) = -0.0`, and the root of any other negative is NaN;
- `neg`, `abs` and `copysign` act on the sign bit; `copysign(x, nan) = abs(x)`, since the one NaN has no sign;
- `floor`, `ceil`, `trunc` and `nearest` of a zero or an infinity are the argument.

Special-value tables for the arithmetic are by reference to IEEE 754-2019 §6 and are not restated.

**Conversions carry values, never bits.** `Nat/to_flt` and `Int/to_flt` are the correctly rounded narrowing of the unbounded value — `0 ↦ +0.0`, overflow to the infinity of the sign — and are total, as every precedent's integer-to-float conversion is: rounding is the canonical extension of the embedding, forced by the structure the way monus is forced for `Nat/sub`. `Flt/to_nat` and `Flt/to_int` truncate toward zero and answer the exact unbounded natural or integer on their stated domain (§3). A Core value that no runtime carrier holds — `to_nat(3.0e9)` is the natural `3000000000` — is refused downstream exactly as an overflowing `Nat` is, per [Numeric carriers narrow by refusing, never by changing a value](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md).

**Literals are spec-defined.** A float literal `D·10^E` is narrowed to the nearest binary32 by the model's own division routine, once. Rust's `str::parse::<f32>` gives the same bits on every input and is what the parser uses today; moving the definition into the repository changes no program and is what makes "every `Flt` value a program can spell is defined by the model" true.

**Bytes.** `to_le_bytes` of the NaN is `0x7fc00000` little-endian; `of_le_bytes` of *any* NaN pattern is the NaN. The round-trip laws are therefore `of_le_bytes(to_le_bytes(x)) = x` for every `x`, and `to_le_bytes(of_le_bytes(b)) = b` for every `b` that is not a non-canonical NaN pattern — theorems of the model, no longer the postulate [`big-flt-dyadic/02`](big-flt-dyadic/02-binary32-spec.md) warns against.

**Width.** `Flt` is the one carrier whose Core width equals its erased width, because binary32 is its definition rather than a backend fact. There is no narrowing boundary for `Flt` itself; the only narrowings are the two conversions out of it, and they refuse. The unbounded counterpart is the `BigFlt` sequence, not a wider `Flt`.

## 2. The model

`curios_num::Floating` becomes the model. It is a bit pattern with the invariant *a NaN is `0x7fc00000`*, enforced by a canonicalizing constructor, so the derived `Eq` and `Hash` are value identity. Every operation unpacks the pattern to one of signed zero, signed infinity, NaN, or `(sign, m, e)` with `m < 2²⁴`; computes exactly over `Natural`; and packs through one shared `round(sign, magnitude, exponent) → bits` that owns the subnormal grid, carry renormalization, and overflow. No method calls an `f32` operation.

- `add`/`sub`/`mul`: align exponents by shifting the larger-exponent mantissa *left*, so alignment is exact; integer add, subtract, multiply; one rounding. An exact zero sum takes `+0.0`, the IEEE rule under round-to-nearest.
- `div`/`sqrt`: scale so the integer quotient (or integer square root) carries 26 bits, keep one sticky bit for a nonzero remainder, round once. This is `/std/Flt`'s `quotient`/`assemble` restated in Rust.
- `rem`: align as integers and take the integer remainder; the result is always representable, so `round` drops no bits.
- `floor`/`ceil`/`trunc`/`nearest`: integer part and fraction of the unpacked value; exact.
- Comparisons and `min`/`max`: value comparison on the unpacked form, the two zeros equal.
- `of_natural`/`of_integer`, `to_natural`/`to_integer`, `of_decimal`: as §1 states.

`scalar`'s `flt_*` functions call the model, so `curios-core`, `curios-ersd` and `curios-cont` fold identically on every host, `curios-js` included; `flt_min`/`flt_max`'s NaN decline disappears, the model defining the answer. `flt_to_nat`/`flt_to_int` keep their `u32`/`i32` domains: those are the erased carrier's refusals, not the semantics'.

**Trusted-base accounting**, in the form the kernel decision asks for: the additions are `Natural`'s integer square root and `Floating`'s unpack, pack, `round`, and operation table. Nothing else. `/std/Flt`'s `assemble`, `round_bits`, `quotient` and `of_decimal` are the model's Curios twin — deliberately duplicated across the perimeter, as `/std/BigNat` is `Natural`'s — and the dyadic `BigFlt` sequence's `to_flt_bytes` is its eventual proved form.

## 3. The type level

**Every `Flt` arm folds on literal operands by calling the model.** There is no decline gate: with one NaN and a conforming runtime (§4) there is nothing the model leaves undetermined. `Flt/div(1.0, 0.0)` is `+inf`, `0.0 / 0.0` is the NaN, `copysign(1.0, 0.0 / 0.0)` is `1.0`, and each is true of the running program. A symbolic operand rebuilds the neutral term as today.

**Two decided propositions in `/syn/Flt`**, spelled by the `/sys` generator through registry slots `proof.flt_finite` and `proof.flt_non_neg`, bodies over the raw `/sys` comparisons in the `/syn/Int` style:

```crs
let max: Flt = 3.4028235e38;
let min: Flt = -3.4028235e38;

pub let Finite(a: Flt) -> Prop =
    match Flt/le(min, a) && Flt/le(a, max) | true => True | false => False end;

pub let NonNeg(a: Flt) -> Prop =
    match Flt/ge(a, +0.0) && Flt/le(a, max) | true => True | false => False end;
```

They read as "a is a number" and "a is a non-negative number": NaN and the infinities are not numbers, which is why `/syn/Int/NonNeg` never had to say finite. `-0.0` satisfies `NonNeg` because IEEE says `-0.0 >= +0.0`, which the existing fixture pins. The bounds are spelled with the two extreme finite values rather than `neg_inf < a < pos_inf` so that they depend on no fold and on no `/std/Flt` item; `min` and `max` are private helpers of the two propositions, not `/syn` names. A conjunction body is a new shape for a decided proposition, and it needs no compiler change: `&&` is the strict `BoolAnd`, a guard is one refinable scrutinee, and registration already sees through concept-dispatched operands — §5 records the probe.

**Bounded `/sys` rows** through `guarded_unary`, the proof named in the body so that it reaches Core and the kernel re-checks it:

```text
/sys/Flt/to_int(a: Flt, @ok: Finite(a)) -> Int     Intrinsic::FltToInt { flt, finite }
/sys/Flt/to_nat(a: Flt, @ok: NonNeg(a)) -> Nat     Intrinsic::FltToNat { flt, non_neg }
```

`Nat/to_flt` and `Int/to_flt` stay unbounded. `FourBytes` on `of_le_bytes` is unchanged.

**`/std/Flt`** gains the decision procedures `finite(f) -> Option(Finite(f))` and `non_neg(f) -> Option(NonNeg(f))` in the `Int/non_neg` idiom, `try_to_int` and `try_to_nat` are written over them, and `pos_inf`, `neg_inf` and `nan` keep their spellings, which now reduce to literals.

## 4. The runtime obligation

The runtime executes Wasm `f32.*`, which the Wasm specification defines by the same exact-then-round clauses as §1, so conformance on every non-NaN result is what an engine's spec-test suite already certifies. What Wasm leaves open is a computed NaN's sign and payload, and the emitter closes it at the only two operations whose non-NaN result reads a NaN's bits:

- `FltToLeBytes`: reinterpret, then select `0x7fc00000` when the operand is a NaN — what Lean's `lean_float32_to_bits` does.
- `FltCopysign`: `abs(x)` when the sign operand is a NaN.

Everything else is unobservable: a NaN flows through arithmetic, `min`/`max`, `neg`/`abs` as *some* NaN and is read only through those two. `of_le_bytes` needs nothing at runtime, because the pattern it builds is read only through them as well. The list is closed by the argument that closes it for the model — an operation either returns a NaN or does not read NaN bits — and a future intrinsic that reads NaN bits into a non-NaN result joins the list or is not admitted.

`/std/Toml`'s "implementation-defined NaN encoding" caveat stops being true and is removed.

## 5. What holds it

**Model against the host.** The host's `f32` is the test oracle — never the definition. An `#[ignore]`d measurement test beside `Floating`, in `stored_prelude_measurements`' shape (command, date, what it last printed), checks every unary operation over all 2³² inputs and the binary operations over vectors generated in the test: the IEEE edge list and all its pairs; exact results constructed on a rounding boundary, tested from both sides; pairs with exponent differences `0..27` for cancellation; and a seeded stream. Agreement is bit-for-bit on non-NaN results and by NaN-ness on NaN results. The ordinary suite runs the edge grid and a small seeded sample in seconds.

**Runtime against the model.** A differential program in `curios`'s codegen tests reads operand bit patterns from the host — so nothing folds — computes every operation, and writes `to_le_bytes`; the test compares the bytes with the model's, NaN cases included, since after §4 both sides define them.

**Laws as `refl`**, in `curios`'s numeric tests where both checkers see them: `Eq(1.0 + 1.0, 2.0)`; `0.1 + 0.2 != 0.3` decided; a tie; a subnormal; `-0.0 + 0.0 = +0.0`; `to_nat(Nat/to_flt(n)) = n` at a literal below `2²⁴`; both byte round-trips.

**Bounds**: discharge by literal, by guard, and through `finite`/`non_neg`; refusal on a NaN, an infinity, and a negative; the prelude build re-certifies the two bounded rows.

**Perimeter row.** A new entry beside [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md): *assumes the model is binary32 and the engine conforms at the two canonicalizing sites*; graded **probed** by the two differentials above.

**What was measured before any of it landed**, 2026-08-23, against the built compiler and a scratch crate, so that the claims above are taken from a run rather than from IEEE's text. These figures are superseded by the in-repository tests' own output once milestone 1 lands, and must not be cited after that.

- *Conjunction bounds.* A `Nat` proposition `match Nat/le(2, a) && Nat/le(a, 9) …` stated as an `@ok` implicit discharged on a closed subject, behind a guard written with operators, behind a guard written with the raw operations, and through an `Option(Between(n))` decision in the `Int/non_neg` idiom; a guard over a different conjunction and a guard over one conjunct alone were each refused with `'ok' was not inferred`. The same four shapes over today's *opaque* `Flt`, with `NonNeg` spelled as in §3, discharged by guard and by the `Option` idiom and routed a NaN to `none` at runtime; only the closed literal was refused. So the fold's contribution to the bounds is exactly the closed-literal call, and the prelude's "could not be decided" comments overstate what was missing.
- *The model.* 323 lines over `num-bigint`, built with the same pin as `curios-num`, held to the host's `f32`: 80,000,000 random, sparse and close-exponent binary cases, 119,072 edge pairs, 10,014,884 comparisons, 40,000,328 conversion cases including the tie at every 24-bit boundary from `2²⁵` to `2¹²⁷` and overflow at `2¹²⁸`, and 1,000,016 literals — zero mismatches, except 6,564 `copysign(x, nan)` cases where the host reads the NaN's sign bit, which is the divergence §1 defines and §4 closes. Exhaustive over all 2³² inputs on 12 threads: `of_bits` 8.5 s, `neg` 2.3 s, `abs` 2.3 s, `trunc` 23.6 s, `floor` 32.6 s, `ceil` 31.6 s, `nearest` 24.9 s, `sqrt` 38.5 s, zero mismatches each. About 80 ns per `Natural`-backed operation, so constant folding pays nothing measurable.

## 6. Milestones

1. **Model core and bounds.** `Floating`'s unpack, pack, `round`, comparisons, the four conversions and the byte reinterpretations; `Natural::isqrt`; the `/syn/Flt` propositions and registry slots; the bounded `/sys` rows with their Core proof operands; `/std/Flt`'s `finite`, `non_neg`, `try_to_int`, `try_to_nat`; the fold arms for what the model covers, the arithmetic arms staying stuck meanwhile; the oracle probe for what exists; the bound fixtures. The prelude build is the detector for the `/syn`/`/sys`/`/std` half, and `cargo test -p curios --lib` for the folds.
2. **Arithmetic and runtime conformance.** The remaining operations in the model and their fold arms; `scalar` over the model; the two emitter sites; the runtime differential; the `refl` laws; the perimeter row.
3. **Literals through the model, and the documentation sweep.** The parser narrows `D·10^E` through `of_decimal`. Then: `roadmap.md`'s `Flt` line and its count of kernel-rechecked bounds, which becomes eleven; [`big-flt-dyadic/02`](big-flt-dyadic/02-binary32-spec.md)'s and [`big-flt-dyadic-proofs/04`](big-flt-dyadic-proofs/04-boundary-proofs-spec.md)'s "opaque trust boundary" premises; `Floating`'s and `scalar`'s doc comments; the fold table's doc comment at `reduce_flt_binary`; `/std/Flt`'s and `curios/src/tests/big_nat.rs`'s "could not be decided" comments; `/std/Toml`'s NaN caveat; and the two opacity fixtures in `curios-elab`'s reduce tests, which become folding fixtures. When the last milestone lands, this specification is retired into those owners and the roadmap item is checked.

## 7. Still open

- `Nat/to_flt` exactness — `to_nat(to_flt(n)) = n` below `2²⁴` — is a lemma, not a precondition, and is out of scope here.
- The `IntToNat` row states `NonNeg` without naming the proof in its body, so its bound is checked by the elaborator and not re-verified by the kernel; the two `Flt` rows carry theirs from the start, and bringing `IntToNat` in line is a separate decision.
