# Arithmetic modulo a word

**Not refined yet.** This specification reserves how a program computes modulo `2^32` or `2^64`, which [a random sequence replays from its seed](../standard-library/301-a-random-sequence-replays-from-its-seed.md) and [bytes are written as text and digested](../standard-library/300-bytes-are-written-as-text-and-digested.md) both wait on. It is not an implementation plan: it waits on the choice under *Refinement*, which those two make once here.

## What is missing

**A 32- or 64-bit step allocates.** `Nat` and `Int` are an i31 until they outgrow it ([Nat and Int are an i31 until they outgrow it](../../design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)), so a step whose intermediate passes `2^30` boxes its result, and `/sys/Byte` is the one carrier of a fixed width. Every seeded generator the standard library's peers ship steps in 32 or 64 bits, and so does every round of SHA-1, SHA-2 and CRC-32. What is written today stays below `2^30` instead: `Hash/digest`'s three lanes, because "`Nat` has no wrapping arithmetic, so the modulus is what bounds a lane" (`curios-std/src/Hash.crs`), and the measurement corpus's Lehmer step `75 * x % 65537`, of period at most 65,536 (`programs/rng_state.crs` and three more).

## The choices

- **A step that stays below `2^30`**, as the lanes and the corpus do. Nothing in arithmetic changes; a generator of this kind is short by today's measure (Wichmann and Hill's 1982 combination of three congruential generators has a period near `7 · 10^12`), and no digest a standard states is one.
- **`Nat` or `Int` reduced by a mask at each step**, paying what [a chain of `Nat` or `Int` steps runs in a word](../compilation/203-a-chain-of-nat-or-int-steps-runs-in-a-word.md) reserves until that cost is taken back.
- **A carrier of a fixed width beside `/sys/Byte`**, with modular operations whose laws are declared as [the declared operations](../../design/arithmetic/a-law-is-decided-where-it-neither-respells-nor-invents.md) are. A carrier reaches every constant folder that shares `scalar`, `curios-emit`'s fast paths and the law grid, which is what it costs.

## Refinement

- **The choice**, read against the two consumers' steps, a SplitMix or PCG step and a SHA-256 round, each counted in allocations under each choice.
- **For a carrier**: its widths, whether it is signed, its operations — addition, multiplication, shifts, rotations and the bitwise ones — its conversions with `Nat`, `Int` and `Byte`, and which of its laws conversion decides.
- **Check**: both steps run with no allocation, and the two specifications above answer their word from here.
