# A shift and a mask are a division by a power of two, and a remainder is a natural

**Refined.** The logic reads a right shift, a mask and a remainder on both carriers through one statement: a right shift by `k` and a mask of `k` low bits are the quotient and the remainder by `2ᵏ`, and a division has one convention, the Euclidean one, whose remainder is a natural. The next step is stage 1, a delivery.

## What is missing

**`/std` floors an `Int` by hand.** At `Int`, `/` and `%` are Euclid's ([`curios-num`'s README](../../../curios-num/README.md#int-divides-as-euclid-does)), `/sys/Int/rem` answers a natural, and conversion and [the elaborator's procedure](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) read an `Int` division, shift and mask as they read a natural's ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)). `/std` divides an `Int` through `Int/DivMod/of`, a certified floor written over `Int/view` and `Nat`'s division, at eight sites in `/std/time` and `/std/Io`, where `/` and `%` compute the same split at the positive divisors those sites use; and `/std/Int/shl/unique` states its odd mantissa on `Int/abs(m) % 2` for want of `m % 2`.

## Decided

- **`/std` divides an `Int` with `/` and `%`.** `Int/DivMod` is `/` and `%` themselves, and is deleted; an odd mantissa is `m % 2 == 1`.

**Rejected.**

- **A total division**, `x / 0 = 0` as Lean's is: a division here carries its divisor's precondition (`/std/ops/Div`'s `Ok`), which a total one would drop for every carrier to buy a law for one.

## Stages

1. **`/std` reads the convention.** `Int/DivMod` deleted and its eight callers dividing with `/` and `%`; `/std/Int/shl/unique` stated on `m % 2`. Check: `/std`'s tests, `/std/time`'s among them. One commit.

## Verification and retirement

Each stage's check, and `cargo xtask workspace lint`. The decisions land in [the law decision](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md), [the bound decision](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), `curios-algebra`'s, `curios-num`'s and `curios-emit`'s READMEs as their stages do; once stage 1 lands, nothing references this file and it is deleted.
