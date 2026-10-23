# Correctly rounded elementary functions

**Not refined yet.** Each function waits for its own consumer. The next delivery is one named function, with its signatures, rounding and exception contracts, recipe, boundary detection and independent reference settled before implementation. That consumer releases no other function; shared machinery is built only as that delivery needs it.

This specification reserves the correctly rounded elementary functions IEEE 754-2019 §9.2 recommends. [Alternate exception handling](304-flt-exception-handling.md) is a separate, independently retirable capability. The float laws — the [declared](../../design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md) identities and `/std/Flt`'s theorems over the bits, `eq_of_le_bytes` and `total_key_injective` — are independent of this delivery; the elementary functions consume them only in their proofs, never in their execution. It waits for a consumer, and it runs at run time alone (*Termination*, below).

## What this builds on

- **The model.** `curios_num::Floating` is binary64 with every one of the 2⁶⁴ bit patterns a distinct value, one symmetric NaN rule and five rounding directions, as [the `Flt` design decision](../../design/arithmetic/flt-is-specified-by-a-model-and-the-runtime-conforms.md) states;
- **The exact layer.** `Flt/internal/Dyadic` holds a finite float's exact value, `mantissa · 2^exponent`, with arithmetic that never rounds; `Dyadic/to_flt(r, d)` is `Floating`'s rounding written in Curios, line for line, and every `/std` operation that rounds a computed value rounds through it.
- **The surface.** `Flt/rounded/<op>(r, …)` in every direction, `Flt/signals/<op>(r, …)` answering the `Exceptions` IEEE raises, and `Flt/Env`, the floating-point environment as a monad that rounds in its direction and records what is raised. Text reads and writes decimal and hexadecimal in every direction; §9.4's reductions and §9.5's augmented operations are exact then rounded once.

## The elementary functions

§9.2 recommends thirty-nine functions and requires of each one provided that it be correctly rounded in every direction. None is provided today. All thirty-nine:

- **exponentials**: `exp`, `expm1`, `exp2`, `exp2m1`, `exp10`, `exp10m1`;
- **logarithms**: `log`, `log2`, `log10`, `logp1`, `log2p1`, `log10p1`;
- **powers and roots**: `hypot`, `rSqrt`, `compound`, `rootn`, `pown`, `pow`, `powr`;
- **trigonometric**: `sin`, `cos`, `tan`, `sinPi`, `cosPi`, `tanPi`;
- **inverse trigonometric**: `asin`, `acos`, `atan`, `atan2`, `asinPi`, `acosPi`, `atanPi`, `atan2Pi`;
- **hyperbolic**: `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`.

**The surface.** Each takes the four forms every rounded operation has: `Flt/<f>(x)` rounding ties to even, `Flt/rounded/<f>(r, x)`, `Flt/signals/<f>(r, x)` and `Flt/Env/<f>(x)`, named in `snake_case` (`r_sqrt`, `sin_pi`, `atan2_pi`, `log2p1`). Each takes every `Flt`, as every `Flt` operation does: a point outside the mathematical domain answers the NaN and raises invalid, as Table 9.1 lists, rather than taking a bound — the domain is IEEE's to state, and `log(-1.0)` is a float like `0.0 / 0.0` is.

**The architecture: exact enclosures, then Ziv's loop.** Correct rounding needs the exact value's position relative to the rounding points of the direction asked for. Each function computes, at a working precision `p`, an enclosure `[lo, hi]` of its exact value as two dyadics, from argument reduction and a series with a proved remainder bound, all in `Dyadic` arithmetic, which never rounds. If `Dyadic/to_flt(r, lo)` and `Dyadic/to_flt(r, hi)` agree, that is the answer; otherwise `p` doubles and the enclosure is recomputed. The loop takes no fuel, so each function is partial and runs at run time alone. Where a type needs one, a fueled twin answering `Option` is added beside it, its bound supported by evidence covering every admitted input and direction and stated beside its recipe. Published hard cases from Lefèvre and Muller's searches and the CORE-MATH project supply tests; they supply a global precision bound only where the search or proof establishes that coverage.

**Boundary and exact cases.** A nonzero-width enclosure may straddle a rounding boundary at every precision: representable results for directed rounding, and midpoints between adjacent representable results for nearest rounding. A sufficiently narrow enclosure around a representable result can decide nearest rounding, but exactness still has to be established for its exception status. Each function detects and resolves its boundary cases before relying on precision doubling ([Lauter and Lefèvre, *An efficient rounding boundary test for pow(x,y) in double precision*](https://www.vinc17.net/research/papers/rr_power-cr.pdf)). For example, `pown(3.0, 34)` is exactly 16,677,181,699,666,569, midway between the binary64 values 16,677,181,699,666,568 and 16,677,181,699,666,570; it needs a nearest-rounding tie decision despite not being representable. The exact-result cases include: `exp(0) = 1`, `log(1) = 0`, `exp2(n)` and `log2(2ⁿ)` for integral `n`, `exp10(n)` for the `n` whose power of ten is a float, `pow` over its exact cases, `sinPi` at integers, `cosPi` at half-integers, and the rest each recipe names. The tables are the ones the CORE-MATH functions carry where it has the function; its binary64 list holds no `compound`, `rootn`, `pown` or `powr`, whose boundary tests are this library's to derive. Each recipe states a complete boundary test or characterization, with its evidence; a finite table is used only where it covers the cases.

**Exceptions.** Table 9.1: invalid for a point outside the domain and for a signaling NaN, division by zero for an exact infinity from a finite operand (`log(0)`, `atanh(1)`), and overflow, underflow and inexact from the enclosure as `signals` derives them for every rounded operation. Tininess is detected before rounding, the one reading this library takes.

**The recipes.** Each function's section, written with its implementation, gives: its argument reduction and why the reduced argument is exact or enclosed; the series and its remainder bound; its boundary test and exact-result cases, with their coverage evidence; a proven precision bound where a fueled twin needs one; and its Table 9.1 row. The shared machinery lives privately in `Flt/elementary`: enclosures of `ln 2`, `ln 10` and `π` to any precision, as the constants every reduction needs; the reduction `x = k · ln 2 + r` with `|r| ≤ ln 2 / 2`; and Payne–Hanek reduction by `π/2` for the trigonometric family, determining the quadrant correctly and enclosing the reduced argument with a proved error bound for every finite input. The arithmetic on dyadic bounds is exact; a mathematical remainder such as `2 - π/2` is irrational and has no exact `Dyadic` representation.

**The fast path, deferred.** An interval in directed-rounding double-double arithmetic over the primitives decides most inputs in a few dozen primitive operations, and falls to the exact path only near a rounding point. It changes cost, never the answer, and it waits until the exact path is measured on a real workload.

## Termination

Decided on 2026-10-01: the precision-doubling loop takes no fuel. Each function is partial, run at run time and refused where a type reaches it as not known to terminate, as any loop the compiler cannot see end is; a fueled twin answering `Option` is added only where a type needs one. Termination of precision doubling rests on complete boundary detection and enclosures converging away from those boundaries: a missed boundary can leave the loop running forever. Exact-result detection also establishes the exception status. A fueled twin states the evidence that its bound covers every admitted input in every rounding direction; published hard cases and sampled agreement are useful evidence and do not by themselves establish that its `none` is unreachable.

## Verification

Each elementary function is checked against the exact path at a raised precision over a generated sample and the edge grid; at every worst case the published searches list for the function; at its exact-result and rounding-boundary cases, including nearest midpoints and directed-rounding boundaries; folded against executed, with the operand tainted at run time; and, for a fueled twin, by type-level `Eq/refl()` facts at closed inputs. The tree holds no reference for a last bit beyond the primitives, `curios_num::Floating` covering those alone, so a reference outside the exact path is this specification's to choose.

## Completion criteria

- Each delivered function is complete in its four forms, correctly rounded in every direction, with the exceptions of Table 9.1, a boundary test with coverage evidence and any fueled twin's bound established. Its delivery waits on no unrelated function.
- As a function lands, its public contracts go to `/std/Flt` and its name is removed from the pending list above. The remaining functions keep their own consumer gates; this specification retires only when no function remains pending.
- Before this specification is deleted, what it states is recorded in `/std/Flt`'s documentation, signatures and tests, no reference to this filename remains.

Retirement does not wait for alternate exception handling. Update its link to this specification to the permanent elementary-function contracts if that work remains pending.
