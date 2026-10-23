# A right shift and a remainder state their bounds

**Not refined yet.** This specification reserves the laws that give two operations a meaning the logic can read: `Nat`'s and `Int`'s right shift, which conversion ties to no quotient, and `Int`'s remainder, whose bound nothing declares. Each has a consumer waiting. It is not an implementation plan. Each law belongs to the declared operations ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)), so each is designed in that decision first.

## What is missing

**A right shift's quotient.** Conversion holds of `shr` its zero laws, a count's literal floor taken in two steps and `shr(x, y) <= x`, and ties it to no division: a fold may not build a division node, whose proof that the divisor is nonzero it would invent. So `shr(2 * x, 1)` is not `x` by conversion, by [the elaborator's procedure](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), which reads a shift as a free atom, or by a lemma, since nothing a proof could split on reduces it. Its consumers: [the binary64 conversion proofs](305-binary64-conversion-proofs.md), whose subject, `/std/Flt/exact`'s `round_in`, computes what it keeps as `Nat/shr(m, s)` and what it drops as `m - Nat/shl(kept, s)`; and the lemmas `/std/Nat` and `/std/Int` wait on it for, a right shift undoing a left one, `Int`'s right shift as a floor, and its agreement with `Nat`'s on a widened natural.

**The bound on `Int`'s remainder.** Of `Int`'s truncated division conversion decides Euclid's identity and an exact multiple, which pin neither half: the grid's candidate `i % 3 < +3` is refused, a remainder's magnitude below its divisor's being a bound through `abs` that nothing declares. `abs` and `sign` are declared operations, so the law has its vocabulary: the magnitude of `i % d` is below `d`'s, and its sign is its dividend's or zero. Its consumer is parity through the sign, that an integer's remainder by two is zero exactly when its magnitude's is, which `/std/Int` waits on it for, its signed scale (`/std/Int/shl`) stated on the magnitude meanwhile.

## Refinement

- **The shift's law.** The form the law takes — the two bounds `pow(2, k) * shr(x, k) <= x < pow(2, k) * shr(x, k) + pow(2, k)` in the comparison, which hold at `Int` as at `Nat` and pin the shift, or a peel that holds `shr(x, k)` and `x / pow(2, k)` one stuck value at `Nat`, which builds no node — and which of the two the elaborator's procedure reads a quotient's bounds from.
- **The remainder's law**: the magnitude's bound through `abs` and the sign through `sign`, and which of them the elaborator's procedure reads.
- **Check**: the grid's candidate `i % 3 < +3` accepted and a row for the shift beside it in `curios`'s `tests::laws::written`; then the lemmas each consumer waits on, in `/std/Nat` and `/std/Int`.
