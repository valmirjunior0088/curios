# A shift and a mask are a division by a power of two, and a remainder is a natural

**Refined.** The logic reads a right shift, a mask and a remainder on both carriers through one statement: a right shift by `k` and a mask of `k` low bits are the quotient and the remainder by `2ᵏ`, and a division has one convention, the Euclidean one, whose remainder is a natural. The next step is stage 1, a delivery.

## What is missing

**The procedure reads no `Int` quotient.** Conversion reads a right shift and a mask as the quotient and the remainder by `2ᵏ` they are at `Nat` and at `Int`, through `curios-core`'s view of a division; at `Int`, `/` and `%` are Euclid's ([`curios-num`'s README](../../../curios-num/README.md#int-divides-as-euclid-does)) and `/sys/Int/rem` answers its remainder as the natural it is, which `%` widens ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)). [The elaborator's procedure](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) reads a `Nat` quotient's bounds and no `Int` quotient's: `proved()` refuses `2 * (i / 2) <= i` and `Int/shr(i, 4) < +16` from `+0 <= i` and `i < +256`, and the bound lemmas it writes for a natural's, `le/add_r` and `lt/add_mul_lt`, have no `Int` twins. `/std` floors where it divides an `Int`: `/std/time` and `/std/Io` divide through `Int/DivMod/of`, a certified floor written over `Int/view` and `Nat`'s division, at eight sites, and `/std/Int/shl/unique` states its odd mantissa on `Int/abs(m) % 2` for want of `m % 2`.

## Decided

- **The procedure reads an `Int` quotient as it reads a natural's.** Its bounds are `d * (i / d) <= i < d * (i / d) + |d|` in every spelling the view reads, a shift's and a mask's among them, the remainder a widened natural lifted out of the facts as a natural's is; and `Int/DivMod` is `/` and `%` themselves.

**Rejected.**

- **A total division**, `x / 0 = 0` as Lean's is: a division here carries its divisor's precondition (`/std/ops/Div`'s `Ok`), which a total one would drop for every carrier to buy a law for one.

## Stages

1. **The procedure reads an `Int` quotient.** `curios-elab`'s `entailment` reads an `Int` division in every spelling as it reads a `Nat` one, raising the remainder's bounds by the quotient's multiple through `Int/le/add`, as `Nat`'s lemmas raise a natural's, the names reached through the syntax registry. Check: `tests::bounds` rows of an `Int` quotient's bounds at a positive and a negative divisor, `Int/shr(i, 4) < +16` from `+0 <= i` and `i < +256`, and a mask lifted out of a fact. One commit.
2. **`/std` reads the convention.** `Int/DivMod` deleted and its eight callers dividing with `/` and `%`; `/std/Int/shl/unique` stated on `m % 2`. Check: `/std`'s tests, `/std/time`'s among them. One commit.

## Verification and retirement

Each stage's check, and `cargo xtask workspace lint`. The decisions land in [the law decision](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md), [the bound decision](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), `curios-algebra`'s, `curios-num`'s and `curios-emit`'s READMEs as their stages do; once stage 2 lands, nothing references this file and it is deleted.
