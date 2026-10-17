# A judgment's identities running out is a refusal

**Not refined yet.** [`curios-cert`'s README](../../../curios-cert/README.md#a-type-is-accepted-by-typing-it-and-reduction-is-total-on-arbitrary-terms) decides that shapes are made safe by typing and counts by the boundary, and that neither can abort. The kernel panics where a judgment's binder identities pass thirty-two bits, and a remembered reduct's replay may take it there. It is not an implementation plan: what carries the count is not known.

## What is missing

`Spend::fresh` (`curios-cert/src/kernel/spend.rs`) mints a binder's identity from a counter and `expect`s it to fit a `u32`, `binder space exhausted`, where every other limit of a judgment is a refusal. The counter is raised by `fresh`, by `reserve`, which the kernel calls with identities it minted itself, and by `charge_nothing`, which adds what a remembered reduct's computation advanced it by, the advances of the hits inside that computation included (`replay_since`). [The kernel memoizes its own evaluation](../../../curios-cert/README.md#the-kernel-memoizes-its-own-evaluation-and-a-memo-changes-only-resource-verdicts) replays a reduct's identities so that a hit mints what a recomputation would, and gives the replay up for the typing table because there it counts what the table avoids: one typing per path, which outgrows the identity space.

## What is known

- **Shown once.** With `Reader::subtractions` and `Reader::extrema` (`curios-elab/src/entailment/facts.rs`) extending `self.read.facts` where they extend `self.read.common`, so that what both cases of a split state is read among the scope's facts, the library elaborates and certifies (`cargo xtask workspace lint`), the two bound proofs of `/std/time/DateTime/of_timestamp` are written another way, and `curios-pipeline`'s `standard::tests::the_first_part_of_the_library_reads_back` panics in `fresh` while the kernel checks `of_timestamp_as_printed`, that `let`'s printed form, under a budget of 30,000,000 steps; taken on the commit over `d36bb8a58`.
- **Not known**: which reduct's replay carries the count, and whether a program a reader would write reaches it; the test's build is optimized and a debugger reads no local of it.

## Refinement

- **What carries the count**, read off the edit above with a count of what each replay adds.
- **The rule.** The space running out is the refusal an exhausted budget is, and what a replay adds is bounded by what its judgment's budget lets a recomputation mint. The rule is the trusted base's, so it is stated in the kernel's decision first and made a step at a time under supervision.
- **Check**: the edit above, with the read-back ending in a verdict.
