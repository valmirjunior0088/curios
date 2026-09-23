# `Flt` elementary functions and alternate exception handling

Working specification for what `/std/Flt` still lacks of IEEE 754-2019 past its operations: the correctly rounded elementary functions §9.2 recommends, and the parts of §8's alternate exception handling not yet provided. [The `Flt` laws](flt-laws-spec.md) are independent of it; the elementary functions consume them only in their proofs, never in their execution.

## What this builds on

- **The model.** `curios_num::Floating` is binary64 with every one of the 2⁶⁴ bit patterns a distinct value, one symmetric NaN rule and five rounding directions, as [the `Flt` design decision](../design/language/flt-is-specified-by-a-model-and-the-runtime-conforms.md) states; [The binary64 model and its NaN rule](../soundness/per-term-rules/the-binary64-model-and-its-nan-rule.md) holds the running program to it.
- **The exact layer.** `/std/Dyadic` holds a finite float's exact value, `mantissa · 2^exponent`, with arithmetic that never rounds; `Flt/rounded/of_dyadic(r, d)` is `Floating`'s rounding written in Curios, line for line, and every `/std` operation that rounds a computed value rounds through it.
- **The surface.** `Flt/rounded/<op>(r, …)` in every direction, `Flt/signals/<op>(r, …)` answering the `Exceptions` IEEE raises, and `Flt/Env`, the floating-point environment as a monad that rounds in its direction and records what is raised. Text reads and writes decimal and hexadecimal in every direction; §9.4's reductions and §9.5's augmented operations are exact then rounded once.

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

Each elementary function is checked against the exact path at a raised precision over a generated sample and the edge grid; at every worst case the published searches list for the function; at every entry of its exact-case table; folded against executed, with the operand tainted at run time; and by type-level `Eq/refl()` facts at closed inputs, which is what shows the fuel suffices for a type.

## Completion criteria

- All thirty-nine functions of §9.2 are provided in their four forms, correctly rounded in every direction, with the exceptions of Table 9.1.
- The three §8 items are provided, or each is recorded as declined with its reason.
- Before this specification is deleted, what it states is recorded in `/std/Flt`'s documentation, signatures and tests, the roadmap entry is a checked summary, and no reference to this filename remains.
