# A scoped answer stands for as long as what it read

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) remembers a judgment for as long as what it read stands, and [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md) has an answer read off the scope go at each boundary of the scope that can reach it. The elaborator lets an answer go at a frame's exit whether or not it read that frame, so a probe under an arm reduces again what the arm before it reduced.

## What is wrong

Where a frame that registers a refinement leaves, `Caches::invalidate_frame_exit` (`curios-elab/src/context/caches.rs`) clears the scoped reducts, the forced values, the closed machine's answers, the classified operations, what erasure's classifier answered, the kernel's spellings it remembers and the elaborations, and wherever a frame leaves it clears the sorts and the verdicts: one may have read the frame, so every one goes, and an answer that read only the frames beneath goes with them.

Of matches each on the `let` before it (`ChainedMatches`), at `1a53c9734`, counted in the profile build, elaboration allocates 117.5, 257.1 and 613.3 MB at 100, 200 and 400 links, and `refined_reduct` 27.3, 78.9 and 252.3 MB of its own, its probe's operands put in weak-head normal form through every link before them again (`reduct_spelling`); `growth_slopes` reads the row's instructions at 2.89 per doubling in `elaborate` from 100 links, under `HELD_SLOPE` (`curios/src/tests/instrumented/growth.rs`), which holds the row while the square is too small to fail it.

## What is known

- **A settlement already stands for as long as what it read.** While one is under way, every lookup that answers from a frame counts that frame's depth (`Frames::begin_reads`, `end_reads`, `read`, `read_scope`, `curios-elab/src/context/frames.rs`); what it remembered is kept under the refinements it read and answers while the deepest frame it read stands, checked by the stand and the frame's number at that depth (`Prefix`, `still_stand`). The chain of matches is settled once a link that way since `27ed90cc7`.
- **The protocol to extend is wide.** Ten methods of `Caches` write a scoped table, called from eighteen sites in `curios-elab`; each computation that files must count what it reads, and a hit must raise the asking computation's count by the depth its answer was filed at, or a computation that read through a remembered answer would file itself as reading less than it did. A settlement escapes the second rule today only because it sets the live window aside and starts from what it kept.
- **An elaboration reads more than frames.** It reads and mints metavariables, and is filed only where it wrote nothing (`ElaborationStamp`); what else it reads is what its stage must state.
- **The audit holds the rule.** `std_memo_census` (`curios-pipeline`) elaborates `/std` with every table kept and with every boundary clearing every table (`Context::wholesale`), and requires one module both ways: how long a table keeps an answer is a cost and never what a declaration elaborates to.
- **The kernel keeps its own lives** (`curios-cert`'s `Memos`): its local-bearing tables go where an equation is assumed or retracted. Two checkers given one budget must cost alike, so what the elaborator keeps longer is read against `kernel_memo_charge_measurements` and the parity readings.

## Stages

1. **The reductions.** `reduction`, `reduction_erased`, `forced`, `closed`, the conversion-classed `classified`, `erasable` and `kernel_spellings`: each entry filed with the `Prefix` of the deepest frame its computation read, a hit raising the asker's count by it, and the clear at a frame's exit retired for them. Check: `ChainedMatches` within a tenth of 2.0 per doubling in `elaborate` from 100 links, counted; `/std`'s image unchanged at one path; `std_memo_census`.
2. **The sorts and the verdicts**, `sorts` and `same`, which a frame's exit clears whatever it registered: filed and read as the reductions are. Check: as the first, and the growth rows that hold telescopes and nested lambdas.
3. **The elaborations**, `elaboration` and `oracle`, with what an elaboration reads stated where it counts. Check: as the first.

## Open questions

- **What a read bracket costs.** A count pushed and popped at every reduction that files, against the reductions a kept answer saves, read over `profile-prelude elaboration`.
- **Whether the third stage pays.** An elaboration under an arm is asked again less often than a reduct; its stage is taken where a growth names it, and otherwise the clear stays for it with its reason stated in `caches.rs`.

## Verification and retirement

`cargo xtask workspace lint` and `cargo xtask workspace test` after each stage, with its check before them. Once the stages land, the rule is stated in `caches.rs`'s module documentation and `frames.rs`'s, `ChainedMatches` is held at its slope with no square left to hide, and this file is deleted.
