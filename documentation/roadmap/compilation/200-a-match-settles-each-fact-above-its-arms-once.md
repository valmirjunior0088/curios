# A match settles each fact above its arms once

**Not refined yet.** A `match` settles every fact above an arm again at each arm's height, so a match of `n` arms settles a square of times and asks a cube of decisions, and `/std`'s build pays for it. It waits on the choice of lever, tried over the two matches below.

## What is wrong today

- Each arm holds the cases above it as facts — a guard's failure, a literal's mismatch — and the elaborator settles each of those again at every arm's height (`settle`, `curios-elab/src/reduce.rs`), each settlement probing every fact in view (`refined_reduct`).
- Counted at `5763feb48` from standard input under `--profile`: a `match n` over literal arms that each build a constructor carrying two bounds, the shape of `/std/Tui/Input/decode`'s `tilde`, settles 55, 210 and 820 times at 10, 20 and 40 arms, once for each pair of an arm and an arm above it, and makes 330, 2,660 and 21,320 `decide_bool` calls, eight times as many a doubling; a chain of `code == k` guards whose arms discharge two bounds settles 20, 90, 380 and 1,560 times at 5, 10, 20 and 40 arms, twice for each such pair, and makes 80, 960, 9,120 and 79,040.
- At 40 arms the literal match checks in 1.05 s and the guard chain in 3.5 s (the debug compiler, one reading each, under load, no recorder).
- Every settlement again at an inner arm's height comes back as the spelling it was reduced on from: the 780 of the literal match's 820 at 40 arms, and the 342 of the guard chain's 380 at 20 (counted by a note in `settle_at`, in a copy of the tree).
- In one `cargo xtask profile prelude` reading at `6c348e728`, `/std/Tui/Input/decode/kitty`, 26 guards, is 23.7 s of `/std`'s 292 s, with 1,859 settlements of 51 distinct facts, and `tilde`, 20 literal arms, 3.4 s.

## Known

- An arm inside adds only facts about other constants, which answer no stuck part of an outer fact's spelling.
- The certifier settles each entry once (`settle`'s doc).
- Lean 4 now proves each literal `match` equation once as an instance of its congruence equation, where each proof refuted every preceding alternative (lean4#16502), and finds the hypothesis that refutes each `x = v` test through an index, where a pass over every alternative did (lean4#16465).

## Open

- Which lever to pull: a settlement carried across an arm whose equations answer no stuck part of it, the scan of the facts in view made a lookup, or both.

## Stages

1. **A settlement carried across arms.** A fact settled at an outer arm's height is kept at an inner arm's whose equations answer no stuck part of its spelling. Check: the two matches above settle linearly in their arms, counted under `--profile`, and `/std` builds with each declaration's verdict unchanged.
2. **The lookup**, where stage 1 leaves the decisions past linear. Check: `decide_bool` calls linear in the arms over both matches.

## Verification and retirement

Verified by a growth in `curios`'s `tests::growth` over the literal match and the guard chain, and by `/std`'s build; retired with `curios-elab`'s README stating what a settlement is kept across.
