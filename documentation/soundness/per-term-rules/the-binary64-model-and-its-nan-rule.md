# The binary64 model and its NaN rule

**Assumes.** `curios_num::Floating` computes IEEE 754-2019 binary64 over all 2⁶⁴ bit patterns, each a distinct value, and the engine executing the emitted `f64.*` agrees with it bit for bit. That includes which NaN an operation answers, which Wasm leaves to the engine and the emitter pins with a check after every instruction that can produce one.

**Status.** **probed** by three differentials:
- The model against the host `f64` over every unary operation at **every one of the 2048 exponent fields** and, in the ignored sweep, over all 2¹⁶ low mantissa bits at each. This runs beside the binary operations and `fma` at the IEEE edge grid, a cancellation sweep and a seeded sample.
- The model against an exact rational oracle, in every rounding direction.
- The fold against the emitted program over the operation family, with a table of NaNs assembled from bytes.

What stays **argued**:
- The closure claim: every instruction that can produce a NaN is followed by the check.
- Mantissa coverage above the low sixteen bits.

**What the widening cost this entry, stated rather than glossed.** While `Flt` was binary32 the model was checked against the host at *all 2³² inputs*, which is the strongest evidence any entry on this perimeter carries. 2⁶⁴ cannot be enumerated, so completeness moved to the axis that decides an answer's shape rather than its digits: the exponent field selects zero, subnormal, normal, infinity or NaN and picks the subnormal grid a result is rounded on, so sweeping it whole covers every one of those cases at every scale. The mantissa is covered by corners — the ends, the carry boundary at `2^51`, and the low bits `round` reads — with the low sixteen taken exhaustively at every exponent. This is a weaker claim than the one it replaces. The retired 2³² sweep remains evidence only for the rounding algorithm at the instantiation it ran on.

Every `Flt` operation folds at the type level by calling the model, so this entry admits by [the perimeter's shared route](../../design/language/the-soundness-perimeter.md), with the shared function in `curios-num` rather than `curios-core`. The second opinion here is not the other checker; it is the host, an exact oracle, and then the engine.

**The model against the host.** The host's `f64` is an oracle and never the definition, which is the whole reason a disagreement is readable as a defect rather than as a difference of opinion. `curios-num`'s `floating::tests` runs the operations the host has — `neg`, `abs`, `sqrt` and round-to-integral in the four directions the host spells, over every exponent field at fourteen mantissa corners in the ordinary suite (`every_exponent_agrees_with_the_host_at_the_mantissa_corners`) and over every exponent field's whole low-sixteen-bit mantissa range in the ignored one (`an_exhaustive_low_mantissa_sweep_agrees_with_the_host`). Beside those it runs the binary operations and `fma` over the IEEE corner grid with every pair or triple, an exponent-difference sweep where cancellation decides the answer, and a seeded sample. The host agrees on every number bit for bit and on a NaN only by NaN-ness: it is no oracle for *which* NaN, since hardware answers its own. The bit operations `neg`, `abs` and `copysign` are the exception, where IEEE fixes the bits and the host agrees on every pattern, NaNs included.

**The model against an exact oracle.** Past the default direction the host has nothing to say. `every_direction_rounds_the_edge_grid_the_way_it_names`, `every_direction_rounds_a_seeded_sample_the_way_it_names` and `every_direction_converts_an_integer_the_way_it_names` compute the exact value of each operation as a rational. They hold every direction's answer to the inequality that defines it — toward negative, `r ≤ e < next_up(r)`, and so on — which reads no line of the model's `round`, so the two cannot share a mistake.

**The NaN rule, and the table that holds it.** Which NaN an operation answers is pinned by the model rather than left open, because the running program has to agree with the fold:
- An invalid operation with no NaN operand answers the default NaN `+0x7ff8_0000_0000_0000`.
- An operation with NaN operands answers the greatest of their quieted patterns, read unsigned, which does not read their order and so keeps every commutative operation commutative on every pattern.
- A difference is the sum with its subtrahend negated, the NaN's sign included.
- `neg`, `abs`, `copysign` and the byte conversions touch no bit but what they name.

`a_nan_result_is_the_greatest_quieted_nan_operand_or_the_default` states the rule as a table over NaNs of both signs and kinds, with and without payloads, in every direction.

**The fold against the engine.** `curios`' `tests::numeric::unbounded_tests` compiles each row twice — closed, so it folds, and with a runtime-zero perturbation, so it executes — and demands identical output. That is what holds `curios-core`'s fold, `curios-ersd`'s and `curios-cont`'s partial evaluators, and the emitted Wasm to one semantics. All three compile-time folders call the model, so what the differential compares is the model against the engine. `folded_and_executed_scalar_ops_agree` holds the numbers, and `folded_and_executed_nans_agree` holds the NaN rule, printing each result's bytes, the one observation that reads a NaN's sign and payload.

**The check after the instruction, and why a differential over it is easy to write vacuously.** Wasm leaves a computed NaN's sign and payload to the engine; x86's default NaN is negative where aarch64's is positive, and when two operands are NaN each engine picks its own. `into_wasm` keeps the hardware instruction and tests its result with `r != r`, which reads no bits. Only a NaN takes the call to `flt/nan`, which recomputes from the operands alone the answer the model's rule gives.

A row built on a *computed* NaN tests little of that, because an engine whose default matches the model passes it whether or not the check is emitted. The rows that stand assemble their NaNs from bytes with payloads and a runtime-tainted byte, because reinterpreting a chosen pattern is bit-preserving on every engine. **Measured 2026-09-22 on x86_64-unknown-linux-gnu** with the check's NaN arm replaced by the hardware result: the first row to fail was `add(signaling, quiet)`, where the engine answered its first operand quieted, `:1:0:0:0:0:0:248:127`, against the model's greater pattern, `:2:18:0:0:0:0:248:255`. The reproduction is recorded beside the rows.

**The directed helpers and `fma` against the model.** Wasm's `f64.*` round ties to even only, so the other four directions and `fma` in every direction run as integer soft-float helpers `into_wasm/flt_emitter.rs` writes — `flt/add`, `flt/mul`, `flt/div`, `flt/sqrt`, `flt/fma`, the ties-to-away round to an integral value, and the `flt/round` and `flt/pack` they share — and a `Nat` or `Int` past the i31 converts through `big/to_f64` in the direction asked for. No hardware answer stands beside these to compare with; `curios`' `tests::numeric::rounding_tests::every_direction_executes_as_the_model_rounds` holds each helper to the model's fold over the edge grid in every direction, with operands tainted at run time so the helper executes. `the_exact_rounding_executes_as_the_model_rounds` holds `/std`'s own rounding, `Flt/rounded/of_dyadic`, executed against `Floating::of_dyadic` over the same grid in every direction and sign.

**What is argued rather than probed** is that every instruction able to produce a NaN is followed by the check. The argument is by cases:
- Every arithmetic instruction, `min`/`max`, `sqrt`, round-to-integral and the `flt/rem` helper can produce one, and each is lowered through `emit_flt_checked`.
- `neg`, `abs` and `copysign` are bit operations in Wasm as in IEEE, so they produce exactly the pattern the model does.
- `of_le_bytes` and `to_le_bytes` reinterpret, which is bit-preserving.
- The comparisons produce no float.
- The conversions from `Nat` and `Int` produce no NaN.
- The directed helpers and `fma` compute the model's NaN rule themselves, through `flt/nan`, and are held to it by the directed differential above.

A future instruction that can produce a NaN is lowered through the check or is not admitted, and nothing checks that obligation mechanically.

**The narrowings out of `Flt` are this row's, the bounds are not.** `Flt/to_nat` and `Flt/to_int` answer the exact unbounded natural or integer on the domain `/std/Flt/NonNeg` and `/std/Flt/Finite` state, and `Flt/mantissa` and `Flt/exponent` the finite value's exact `mantissa · 2^exponent` on `Finite`'s; the emitter reads the last two off the encoding, and `tests::numeric::unbounded_tests::folded_and_executed_decompositions_agree` holds them to the model's `to_dyadic` at each of the encoding's arms. Outside it the fold declines and the operation stays stuck; a well-typed call cannot reach that, and reduction does not rely on being handed only well-typed terms. How the bound itself is discharged is [A bound is stated in a decided proposition](../../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)'s, and that the proof reaches Core where the kernel re-checks it is [Intrinsic signatures](intrinsic-signatures.md)'s.
