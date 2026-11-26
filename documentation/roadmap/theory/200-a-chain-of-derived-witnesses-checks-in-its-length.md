# A chain of derived witnesses checks in its length

**Not refined yet.** Each link of a chain of recursive families with derived witnesses doubles the check, where each should add its own cost. It waits on its first stage, a keyed climb profiled at two sizes, which names what still grows.

## What is wrong today

- Over a chain of `n` recursive families, each holding the one before it, each with a derived `Spell` witness, the last compared through a definition that spells as it does, a check takes 0.46 s, 0.91 s, 1.85 s, 3.86 s and 7.92 s at `n` from 2 to 6 (the debug compiler at `5763feb48`, the median of three interleaved rounds, each within 0.04 s of it).
- Counted there under `--profile`: `group_held` (`curios-analysis/src/variance.rs`) climbs 2, 6, 14, 30, 62 and 126 groups in the elaborator's declaration at `n` from 1 to 6, `2^(n+1) - 2`, and 4, 14, 34, 74, 154 and 314 in the certifier's check of it; `group_totality` runs 9, 26, 57, 116, 231 and 458 times, 440 of the 458 inside a climb, and is half the check's time under the recorder; and the certifier's `reduce_forced` runs 376, 1,254, 3,277, 7,600, 16,523 and 34,646 times, 21,255 of the last directly inside a climb, about 68 a climb.
- A climb's readings are kept by the group as it stands (`Climbs::settled`), so a group another walk opened at fresh binders (`Free(Mint { .. })`) is a new key, and its totality is walked again with it. A group's totality is kept by nothing: `read_by_unfolding` (`curios-analysis/src/reading.rs`) decides it again at the head of every climb.
- Thirteen stacks sampled over the check at six links at `4ef7e7a05`, a stack a run under gdb, all lie inside a climb: eight in a forced reduction, four in a totality walk and one in the climb's own walk, nine of the thirteen in the certifier's check.

## Known

- Keyed instead by the group with each local the driver holds nothing of renamed to the reading's own, in the order they were minted (a patch over `c3b30957b`, kept out of the tree), the climbs were 30, 38 and 46 at `n` from 4 to 6 and the reductions inside them doubled, so the chain's time doubled a link with or without the keys.

## Open

- What a keyed climb forces that grows with the chain.

## Stages

1. **The keyed climbs again, profiled.** The keyed climbs written again over a copy of the tree and one climb profiled at four and six links. Check: the forced reductions inside a climb counted at each size, and what grows named.
2. **The fix that holds the chain linear**, the keyed climbs among its parts or not. Check: a chain growth in `curios`'s `tests::growth`, and the derived-witness test of `tests::universes::instance` within its budget.

## Verification and retirement

Retired once the chain's check grows linearly in its links, held by stage 2's growth.
