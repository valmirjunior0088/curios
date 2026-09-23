# `Flt` laws and the elementary functions

Working specification for what `/std/Flt` still lacks past its operations: the laws a proof about floats needs — decided identities, theorems over the bits, and the reflected model tying each primitive to its exact value — and IEEE 754-2019's recommended operations of §9.2, the correctly rounded elementary functions. It closes with the parts of §8's alternate exception handling not yet provided. The two halves are independent; the elementary functions consume the laws only in their proofs, never in their execution.

## What this builds on

- **The model.** `curios_num::Floating` is binary64 with every one of the 2⁶⁴ bit patterns a distinct value, one symmetric NaN rule — an invalid operation with no NaN operand answers `+0x7ff8_0000_0000_0000`, and otherwise the greatest quieted NaN operand, read unsigned, which keeps `add`, `mul`, `min`, `max` and `fma`'s product commutative bit for bit — and five rounding directions. [The binary64 model and its NaN rule](../soundness/per-term-rules/the-binary64-model-and-its-nan-rule.md) holds the running program to it.
- **The primitives.** Exactly IEEE's compute-exactly-round-once operations fold in one step through the model, each carrying its direction: `add`, `sub`, `mul`, `div`, `sqrt`, `fma`, the roundings to an integral value, and the conversions from `Nat` and `Int`; beside them the comparisons, `min`/`max`, the sign operations, the byte conversions, and `to_int`, `to_nat`, `mantissa` and `exponent`, the last four under `Flt/Finite` or `Flt/NonNeg`.
- **The exact layer.** `/std/Dyadic` holds a finite float's exact value, `mantissa · 2^exponent`, with arithmetic that never rounds; `Flt/rounded/of_dyadic(r, d)` is `Floating`'s rounding written in Curios, line for line, and every `/std` operation that rounds a computed value rounds through it.
- **The surface.** `Flt/rounded/<op>(r, …)` in every direction, `Flt/signals/<op>(r, …)` answering the `Exceptions` IEEE raises, and `Flt/Env`, the floating-point environment as a monad that rounds in its direction and records what is raised. Text reads and writes decimal and hexadecimal in every direction; §9.4's reductions and §9.5's augmented operations are exact then rounded once.
- **The law grid.** `curios/src/tests/laws.rs` states laws as held and refused rows put to both checkers, and [a law is decided where it neither respells nor invents](../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) is the bar a row is lifted to.

## Decided laws

These hold for every bit pattern under the model, NaNs included, so each is a candidate for a fold or a probe-side decision rather than a lemma:

- **Commutativity** of `add`, `mul`, `min` and `max` in every direction, and of `fma`'s two factors — `eql` and `neq` are held already. The NaN rule is what makes these hold of the carrier rather than of numbers alone. Decided probe-side: sorting the operands in a fold would respell a term a guard's refinement is keyed on.
- **The sign operations**: `neg(neg(x)) = x`, `abs(abs(x)) = abs(x)`, `abs(neg(x)) = abs(x)`, `copysign(copysign(x, y), z) = copysign(x, z)` and `neg(copysign(x, y)) = copysign(x, neg(y))` — each a bit operation, so each holds of every pattern. Folds.
- **Subtraction** is addition of the negation, `sub(r, a, b) = add(r, a, neg(b))`, in every direction — the model defines it so. A fold.
- **The roundings to an integral value are idempotent**, `round_integral(r, round_integral(s, x)) = round_integral(s, x)`. A fold.
- **The byte round trip, both ways.** `of_le_bytes(to_le_bytes(f)) = f` is held today; `to_le_bytes(of_le_bytes(b, @e)) = b` is refused in the grid's "Flt against Bytes" row and is now true, since every pattern is a value and no NaN is canonicalized. That row's refusal becomes a candidate law.

## Theorems over the bits

Provable in `/std` without further trust:

- `ord` is reflexive, transitive and total, antisymmetric to `Eq`, and `ord(a, b) = eq` exactly when `Eq(a, b)` — so `Ord(Flt)` is IEEE's `totalOrder` in fact as well as in intent.
- A `Key(Flt)` over `to_le_bytes` is a congruence, since the byte round trip is injective. Whether to give one is a separate decision: `/std/Map` withholds it because `/std/ops/Eql`'s witness is IEEE `==`, which calls `+0.0` and `-0.0` equal and a NaN equal to nothing, and a map keyed on propositional equality would disagree with it at both ends. The theorem lands whichever way that goes.

## The reflected model

Each primitive equals the exact rounding of its exact result: for finite operands and a finite result, `Dyadic/of(rounded/add(r, a, b)) = Dyadic/of(rounded/of_dyadic(r, Dyadic/of(a) + Dyadic/of(b)))`, and the same for `sub`, `mul`, `fma`, `div` and `sqrt` through their exact quotient and root, the roundings to an integral value, and the conversions. Beside them stand the facts that tie comparisons to the encoding: `Finite(f)` holds exactly when `f`'s exponent field is not all ones, and `f < g` for finite `f` and `g` exactly when `Dyadic/of(f) < Dyadic/of(g)`.

**The trust question this settles.** The model is trusted Rust, and `rounded/of_dyadic` is its Curios twin; the reflection connects them. Two ways:

- **Admit one reflection rule per primitive**, backed by the cross-check that already runs every direction of `of_dyadic` against the model (`curios/src/tests/numeric/rounding_tests.rs`) and by the folded-against-executed differential. The trusted base grows by one stated equation per primitive, each a fact the tests already hold.
- **Fold primitives by reducing the twin** instead of the model, shrinking the trusted base to nothing new. Measured on 2026-09-22 at ten to thirty-five times a model fold's cost at the type level, with the kernel's budget exhausted first; [the `Flt` design decision](../design/language/flt-is-specified-by-a-model-and-the-runtime-conforms.md) rejects it for folding, and it remains the fallback should admitting rules be refused.

The first is preferred, and the choice is recorded in a perimeter entry when it is made.

**What reflection unlocks.** Enclosure for the directed operations — `rounded/add(toward_negative(), a, b) ≤ a + b ≤ rounded/add(toward_positive(), a, b)` over exact values; monotonicity of every rounding; exact scaling by powers of two away from overflow and underflow; Sterbenz's lemma; the error-free transformations TwoSum, FastTwoSum and Dekker's product, which `augmented_add` and `augmented_mul` compute and could then be proved to; and the boundary proofs of [`rat-laws-spec.md`](rat-laws-spec.md), which prove `of_dyadic` itself correct in every direction and so make the twin a proved one.

## What is not a law

Kept as controls, refused at every direction they apply to, each with its counterexample:

- associativity of `add` and `mul`, and distributivity — rounding;
- `f + 0.0 = f` — false at `f = -0.0`, whose sum with `+0.0` is `+0.0`;
- `f * 1.0 = f` — a signaling NaN is quieted;
- `f - f = 0.0` — an infinity or a NaN;
- `f * 0.0 = 0.0` — an infinity or a NaN, and the sign of a negative `f`;
- `f == f` — a NaN;
- `lt(a, b) = not(ge(a, b))` — a NaN on either side.

## The elementary functions

§9.2 recommends thirty-nine functions and requires of each one provided that it be correctly rounded in every direction. None is provided today. All thirty-nine:

- **exponentials**: `exp`, `expm1`, `exp2`, `exp2m1`, `exp10`, `exp10m1`;
- **logarithms**: `log`, `log2`, `log10`, `logp1`, `log2p1`, `log10p1`;
- **powers and roots**: `hypot`, `rSqrt`, `compound`, `rootn`, `pown`, `pow`, `powr`;
- **trigonometric**: `sin`, `cos`, `tan`, `sinPi`, `cosPi`, `tanPi`;
- **inverse trigonometric**: `asin`, `acos`, `atan`, `atan2`, `asinPi`, `acosPi`, `atanPi`, `atan2Pi`;
- **hyperbolic**: `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`.

**The surface.** Each takes the four forms every rounded operation has: `Flt/<f>(x)` rounding ties to even, `Flt/rounded/<f>(r, x)`, `Flt/signals/<f>(r, x)` and `Flt/Env/<f>(x)`, named in `snake_case` (`r_sqrt`, `sin_pi`, `atan2_pi`, `log2p1`). Each is total over `Flt`, as every `Flt` operation is: a point outside the mathematical domain answers the NaN and raises invalid, as Table 9.1 lists, rather than taking a bound — the domain is IEEE's to state, and `log(-1.0)` is a float like `0.0 / 0.0` is.

**The architecture: exact enclosures, then Ziv's loop.** Correct rounding needs the exact value's position relative to the rounding points of the direction asked for. Each function computes, at a working precision `p`, an enclosure `[lo, hi]` of its exact value as two dyadics, from argument reduction and a series with a proved remainder bound, all in `Dyadic` arithmetic, which never rounds. If `rounded/of_dyadic(r, lo)` and `rounded/of_dyadic(r, hi)` agree, that is the answer; otherwise `p` doubles and the enclosure is recomputed. The loop runs on fuel, because a type must be able to use the functions: the bound is the worst-case precision correct rounding of binary64 needs for the function, taken from the published worst cases (Lefèvre and Muller's searches, and the CORE-MATH project's) and stated per function beside its recipe, and the fuel exhausted answers what the final enclosure's midpoint rounds to, which the verification below shows no input reaches.

**Exact cases.** Ziv's loop cannot finish on a value that is exactly representable — its enclosure straddles a rounding point forever — so each function first answers the inputs whose value is exact: `exp(0) = 1`, `log(1) = 0`, `exp2(n)` and `log2(2ⁿ)` for integral `n`, `exp10(n)` for the `n` whose power of ten is a float, `pow` over its exact cases, `sinPi` at integers, `cosPi` at half-integers, and the rest each recipe names. The tables are the ones the CORE-MATH functions carry, and a function without a table is not done.

**Exceptions.** Table 9.1: invalid for a point outside the domain and for a signaling NaN, division by zero for an exact infinity from a finite operand (`log(0)`, `atanh(1)`), and overflow, underflow and inexact from the enclosure as `signals` derives them for every rounded operation. Tininess is detected before rounding, the one reading this library takes.

**The recipes.** Each function's section, written with its implementation, gives: its argument reduction and why the reduced argument is exact or enclosed; the series and its remainder bound; its exact-case table; its worst-case precision and the fuel it implies; and its Table 9.1 row. The shared machinery lives privately in `Flt/elementary`: enclosures of `ln 2`, `ln 10` and `π` to any precision, as the constants every reduction needs; the reduction `x = k · ln 2 + r` with `|r| ≤ ln 2 / 2`; and the Payne–Hanek reduction by `π/2` for the trigonometric family, exact in `Dyadic` for any finite argument.

**The fast path, deferred.** An interval in directed-rounding double-double arithmetic over the primitives decides most inputs in a few dozen primitive operations, and falls to the exact path only near a rounding point. It changes cost, never the answer, and it waits until the exact path is measured on a real workload: at the type level the exact path's cost is what the kernel's budget pays, and the numbers decide whether the fast path must come first there.

## Alternate exception handling

§8 is provided at block scope: `Env/checked(e, m)` is delayed transfer, answering `none` when `m` raised any exception of `e`, and `Env/substitute(e, v, m)` replaces `m`'s answer with `v`. Not provided:

- **`abruptUnderflow`** — flushing a tiny result to zero, or to the least normal, in place of a subnormal;
- **`substituteXor`** — substitution with the sign the operands' signs would give the result;
- **substitution and recording per operation**, rather than per block — §8's attributes attach to an operation, and an `Env` block of one operation is the nearest the surface comes today.

Each is an `Env` combinator over the status the monad already threads; the per-operation form needs the `rounded`/`signals` pair of an operation to be reachable from the combinator, which `Env`'s own operations already hold.

## Verification

**The laws**, to the bar [a law is decided where it neither respells nor invents](../design/toolchain/a-law-is-decided-where-it-neither-respells-nor-invents.md) sets: each row is stated in `curios/src/tests/laws.rs` first, at every direction it applies to, with a control beside it — the non-laws above are the controls — and moves from refused to held when its rule lands; a value grid in `curios-core` pins each fold to its reduct over a grid of patterns that includes both zeros, both infinities, subnormals, and quiet and signaling NaNs with payloads of both signs; one mutation per rule is run and caught; and the perimeter entry names the mutation. Until a rule lands, the grid holds its row refused and its comment points here.

**The elementary functions**: against the exact path at a raised precision over a generated sample and the edge grid; at every worst case the published searches list for the function; at every entry of its exact-case table; folded against executed, with the operand tainted at run time; and by type-level `Eq/refl()` facts at closed inputs, which is what shows the fuel suffices for a type.

## Completion criteria

- Every decided law above is held in the grid, each with its controls refused, its value grid and its caught mutation.
- The bits theorems are proved in `/std`, and the reflected model is admitted or proved, with the choice recorded as a perimeter entry.
- All thirty-nine functions of §9.2 are provided in their four forms, correctly rounded in every direction, with the exceptions of Table 9.1.
- The three §8 items are provided, or each is recorded as declined with its reason.
- Before this specification is deleted, what it states is recorded in `/std/Flt`'s documentation, signatures and tests and in the perimeter, the roadmap entry is a checked summary, and no reference to this filename remains.
