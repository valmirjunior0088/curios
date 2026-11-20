# A shift and a mask are a division by a power of two, and a remainder is a natural

**Refined.** The logic reads a right shift, a mask and a remainder on both carriers through one statement: a right shift by `k` and a mask of `k` low bits are the quotient and the remainder by `2ᵏ`, and a division has one convention, the Euclidean one, whose remainder is a natural. The next step is stage 1, a delivery.

## What is missing

**`Int`'s right shift and remainder are unprovable, not only undecided.** At `Nat`, conversion and [the elaborator's procedure](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) read a right shift and a mask as the quotient and the remainder by `2ᵏ` they are, through `curios-core`'s view of a division ([A law is decided where it neither respells nor invents](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md)). `Int`'s `/` and `%` are Euclid's ([`curios-num`'s README](../../../curios-num/README.md#int-divides-as-euclid-does)), so `Int/shr(i, k)` is the quotient by `2ᵏ` and a remainder lies in `[0, |d|)`, and conversion reads neither: it reads no `Int` remainder as the natural it is. Conversion holds of `%` Euclid's identity and an exact multiple, and of `Int/shr` its zero laws and count split; neither `Nat/to_int(m) % +3` nor `Int/shr(Nat/to_int(m), 2)` reduces through the widening, nor `(+0 - i) % +3` through the negation, so a lemma by `Int/view` meets a stuck term in each arm, and `i % 3 < +3`, the law grid's refused candidate, has no proof at all. `/std` floors where it divides an `Int`: `/std/time` and `/std/Io` divide through `Int/DivMod/of`, a certified floor written over `Int/view` and `Nat`'s division, at eight sites, and `/std/Int/shl/unique` states its odd mantissa on `Int/abs(m) % 2` for want of `m % 2`.

**An `Int` division by a power of two is emitted as a division.** `curios-emit` lowers a `Nat` division and remainder by a literal power of two as the shift and the mask they are ([`curios-emit`'s README](../../../curios-emit/README.md#a-division-by-a-power-of-two-is-a-shift-and-a-mask)), and `Int`'s through `i32.div_s` and `i32.rem_s` stepped to Euclid's, whatever the divisor.

## Decided

- **The emitter reads the same view backwards.** A division and a remainder by a literal power of two are emitted as a shift and a mask, as `Nat`'s are, so a program never picks a spelling for its speed against the one a proof reads.
- **`Int`'s remainder is a natural.** `i % d` is the widening of Euclid's natural remainder, built so by its `/sys` row from the moment it enters Core, as `gt` and `ge` are built as their mirrors: its bound is `Nat`'s, read through the widening by the comparison's preimages. `Int/shr(i, k)` is then the quotient `i / pow(2, k)` at every `i`, a floor being Euclid's quotient by a positive divisor, so the view reads `Int`'s shift and mask as `Nat`'s; the floor law and the Euclidean split hold at `Int` and are its families, where the law decision holds them controls; `Nat/to_int` commutes with `/`, `%` and `shr`; and `Int/DivMod` is `/` and `%` themselves.

**Rejected.**

- **A total division**, `x / 0 = 0` as Lean's is: a division here carries its divisor's precondition (`/std/ops/Div`'s `Ok`), which a total one would drop for every carrier to buy a law for one.

## Stages

1. **`Int`'s remainder is a natural.** The `/sys` row builds `i % d` as the widening of the natural remainder, Euclid's recombination pairs it with `Int`'s quotient's multiple, the floor law and the split fold at `Int`, and the procedure reads an `Int` quotient's bounds; the law decision's `Int` bullet restated. Check: in `tests::laws::written` (*Int against Nat*), `i % 3 < +3` and `+0 <= i % 3` move from refused to held, beside `(i * 2 + 1) % 2 = 1`, `(i + 2) / 2 = 1 + i / 2` and `(2 * i + 1) / 2 = i`, which stop being controls; `tests::bounds` rows of an `Int` quotient's bounds; the audit at the new rows. One commit.
2. **`Int`'s shift and mask are read through the view.** `Int/shr(i, k)` as `i / pow(2, k)` and `Int/and(i, 2ᵏ - 1)` as `i % 2ᵏ`, in conversion and the procedure together; `curios-emit` lowers an `Int` division and remainder by a literal power of two as it lowers `Nat`'s. Check: rows of the shift's fold and recombination below zero, `Int/shr(i, n) <= i` kept a control, and `Nat/to_int` commuting with `shr`. One commit.
3. **`/std` reads the convention.** `Int/DivMod` deleted and its eight callers dividing with `/` and `%`; `/std/Int/shl/unique` stated on `m % 2`. Check: `/std`'s tests, `/std/time`'s among them. One commit.

## Verification and retirement

Each stage's check, and `cargo xtask workspace lint`; `curios-prelude`'s corpus tests for stage 2. The decisions land in [the law decision](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md), [the bound decision](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), `curios-algebra`'s, `curios-num`'s and `curios-emit`'s READMEs as their stages do; once stage 3 lands, nothing references this file and it is deleted.
