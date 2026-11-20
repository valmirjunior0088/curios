# A shift and a mask are a division by a power of two, and a remainder is a natural

**Refined.** The logic reads a right shift, a mask and a remainder on both carriers through one statement: a right shift by `k` and a mask of `k` low bits are the quotient and the remainder by `2ᵏ`, and a division has one convention, the Euclidean one, whose remainder is a natural. The next step is stage 1, a delivery.

## What is missing

**`Int`'s right shift is read by no proof, and the procedure reads no `Int` quotient.** At `Nat`, conversion and [the elaborator's procedure](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) read a right shift and a mask as the quotient and the remainder by `2ᵏ` they are, through `curios-core`'s view of a division; at `Int`, `/` and `%` are Euclid's ([`curios-num`'s README](../../../curios-num/README.md#int-divides-as-euclid-does)) and `/sys/Int/rem` answers its remainder as the natural it is, which `%` widens, so conversion holds Euclid's identity, the floor law and the split there and reads a remainder's bounds ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)). `Int/shr(i, k)` is the quotient by `2ᵏ` at every `i`, which conversion does not read — `Int/shr(i, 1)` is apart from `i / 2`, and `Int/shr(Nat/to_int(m), 2)` does not reduce through the widening — and the procedure reads no `Int` quotient's bounds: `proved()` refuses `i % 2 <= i` from `+0 <= i`. `/std` floors where it divides an `Int`: `/std/time` and `/std/Io` divide through `Int/DivMod/of`, a certified floor written over `Int/view` and `Nat`'s division, at eight sites, and `/std/Int/shl/unique` states its odd mantissa on `Int/abs(m) % 2` for want of `m % 2`.

## Decided

- **`Int`'s shift and mask are `Nat`'s.** `Int/shr(i, k)` is the quotient `i / pow(2, k)` at every `i`, a floor being Euclid's quotient by a positive divisor, so the view reads `Int`'s shift and mask as `Nat`'s, the procedure reading an `Int` quotient's bounds, `d * (i / d) <= i < d * (i / d) + |d|`, as it reads a natural's; `Nat/to_int` commutes with `/`, `%` and `shr`; and `Int/DivMod` is `/` and `%` themselves.

**Rejected.**

- **A total division**, `x / 0 = 0` as Lean's is: a division here carries its divisor's precondition (`/std/ops/Div`'s `Ok`), which a total one would drop for every carrier to buy a law for one.

## Stages

1. **`Int`'s shift and mask are read through the view.** `Int/shr(i, k)` as `i / pow(2, k)` and `Int/and(i, 2ᵏ - 1)` as `i % 2ᵏ`, in conversion and the procedure together, the procedure reading an `Int` quotient's bounds whatever its spelling; Check: rows of the shift's fold and recombination below zero, `Int/shr(i, n) <= i` kept a control, and `Nat/to_int` commuting with `/`, `%` and `shr`, which conversion holds apart today (`Nat/to_int(m) % +3` against `Nat/to_int(m % 3)`); `tests::bounds` rows of an `Int` quotient's bounds, `i % 2 <= i` from `+0 <= i` among them. One commit.
2. **`/std` reads the convention.** `Int/DivMod` deleted and its eight callers dividing with `/` and `%`; `/std/Int/shl/unique` stated on `m % 2`. Check: `/std`'s tests, `/std/time`'s among them. One commit.

## Verification and retirement

Each stage's check, and `cargo xtask workspace lint`. The decisions land in [the law decision](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md), [the bound decision](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), `curios-algebra`'s, `curios-num`'s and `curios-emit`'s READMEs as their stages do; once stage 2 lands, nothing references this file and it is deleted.
