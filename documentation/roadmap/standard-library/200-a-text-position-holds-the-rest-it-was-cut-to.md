# A text position holds the rest it was cut to

This specification makes a step over text cut once from what the step before left, where each step cuts the rest from the whole text again today, in the type and in the erased program alike. Its next step is stage 1, a delivery.

## What is missing

**A step over text costs a type several times a step over bytes.** Over 32 letters at `85da54b97`, `Parse/str/take_while` costs each checker about 48 900 units where `Parse/bytes/take_while` costs about 9 900, and `Parse/str/literal` about 74 900 against 18 400, where a reader that takes no step costs about 8 500 (`parse_primitive_measurements`, `curios/src/tests/reduction.rs`, counted); a `Str` literal's validity, a fold the closed machine runs over the same bytes, costs 69 units a character (`str_literal_cost_measurements`, beside it). A text walk decodes a character, builds the position past it and descends on a proof of accessibility a step (`Str/span` and `Str/follows` over `Str/At/forward`).

**Half of it is the position past built again.** Counted at `7d28c0209`, the units the elaborator spends on a claim, proved by `Eq/refl()`, that a walk over a literal of `n` letters counts `n`, read at 32 and at 64 letters and divided by the 32 between them, each the `budget::consumed` samples of the claim's declaration in the stream a profiling build files for `wonder`: 1,192 a character for `Str/span(s, At/start(s), (_c) => true).past.offset`, the walk as it rebuilt its answer around its recursion, where it hands its proof down since, 1,097 for a count by `At/forward` over `At/try_next`, 960 for one over `At/skip` behind `at.offset < Bytes/len(s.bytes)`, 356 for `Nat/descend` counting up to `n` over the measure `n - k`, and 120 for the structural `plain(k + 1) = 1 + plain(k)`. So decoding is about 140 of a character's units and the span's own answer about 95; the descent, its comparison with it, is at most 356; and about 600 is `skip` building the position past, the rest cut off (`Str/after`), its first character's width read (`hop`) and a position of the whole text built from the offset (`of_after`).

**The erased step keeps the cut.** What the erased program is spared is the proofs and not the cut. `Str/At/next` prints at `cps-optm`, on top of `ddf5bbf85`, as `Bytes/slice(s, at, Bytes/len(s) - at)`, the rest cut from the whole text again, a `Bytes/get` of its first byte, `Bytes/rest` of it past that byte, the decoding of what follows, and the position past as a sum of offsets: a position is its offset and holds no bound, and each step makes two cuts it hands to no step after it.

## Decisions

- **A position holds its rest as text.** `At(s)` is its offset, the rest of `s` it was cut to as a `Str`, and the bound and the cut, both erased:

  ```crs
  pub struct At(s: Str): pub Type {
      offset: Nat,
      rest: Str,
      @within: Bool/Holds(offset <= Bytes/len(s.bytes)),
      @cut: Eq()(rest.bytes, Bytes/drop(s.bytes, offset, @within)),
  }
  ```

  The boundary a position holds as a proof of its own today follows from the rest being valid text, which no continuation byte begins, so it goes, and `Str/after(s, at)` reads the rest. Decided on 2026-11-16.

## Stages

1. **`At` holds its rest.** The structure above; the walks that build a position past — `skip`, `next`, `forward`, `Str/span`, `Str/follows` — cutting once from the rest they hold; and `/std/Str`'s proofs over a position restated over the cut. Check: `parse_primitive_measurements` read again, `Parse/str/take_while` and `Parse/str/literal` against their bytes twins, and `Str/At/next` at `cps-optm` cutting once.

## Retirement

Once stage 1 lands, `/std/Str/At`'s documentation states what a position holds, the measurement's figures move with it, and this file is deleted.
