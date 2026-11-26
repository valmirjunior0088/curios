# A totality refusal names the call path that does not descend

**Not refined yet.** A definition refused as not known to terminate is named, and the call path that closes to no descent is not: `let spin(n: Nat) -> True = spin(n)` reads "a proof in `spin` reaches `spin`, which is not known to terminate", its caret on the call, and nothing says that `n` is handed on unchanged, or which of a group's calls compose to the path that repeats. A report is read to be acted on ([A diagnostic spells what its reader can write](../../design/tooling/a-diagnostic-spells-what-its-reader-can-write.md)), and the action here is changing that path. It waits on how the path reaches the refusal.

## What is wrong today

- `decide` (`curios-analysis/src/totality.rs`) folds a group's calls into `Totality::Unfounded` alone: the closure (`close`, `curios-analysis/src/totality/matrix.rs`) composes each `Call`, a caller, a callee and a matrix, with no record of the sites a composite is made of, so the idempotent call that fails to descend is found and dropped.
- The elaborator's refusal (`curios-elab/src/error/display.rs`) names the definition reached and stands at a read of it.

## Goal

The elaborator's refusal of a definition not known to terminate names the call path that closes to no descent, each call at its place, and what each hands on in place of a smaller argument; the stored verdict and the certifier's refusal, read about the compiler, stay as they are.

## Settled

- The verdict is unchanged: what a refusal names is found on the refusal's path, after the verdict, so no stored `Totality` and no judgment either checker makes depends on it.
- It extends the one closure rather than writing a second: [`close`'s](../../../curios-analysis/src/totality/matrix.rs) composition is the owner of which calls compose.

## Open

- How the path is kept: each composite carrying the generators it was made of, kept only on the refusal's path, or the closure run again there with provenance.
- How a path through several members is worded, and how the refusal reads a matrix's row: which argument each call hands where a smaller one was wanted.

## Stages

1. **The path found.** A failing group's non-descending path, its calls and their places, found by the closure on the refusal's path. Check: a unit test of the shared closure over `spin`, a two-member cycle and a permuting group, each naming its path.
2. **The refusal names it.** The elaborator's refusal names the path and what each call hands on. Check: `spin`'s refusal and a mutual pair's pinned beside the totality refusals' tests.

## Verification and retirement

Retired once a refused definition's report names its non-descending path, and the language reference's totality section links what the refusal says.
