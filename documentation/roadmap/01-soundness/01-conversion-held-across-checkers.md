# The two checkers' conversion held to each other

**Not refined yet.** This specification reserves the differential that would hold the elaborator's conversion and the kernel's to one verdict. It is not an implementation plan.

## What is missing

The two checkers are held to each other on non-informativeness, universe-context validity, the memos and the recorded board fixtures ([An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)), and on conversion only wherever the corpus happens to send both. Conversion is where they differ most: the elaborator's recurrence key renames minted labels and records no local context, where the kernel's records both ([Conversion recurrence](../../design/soundness/conversion/conversion-recurrence.md)); a child the kernel compares at `Type` forfeits eta and irrelevance ([Eta and untyped child positions](../../design/soundness/conversion/eta-and-untyped-child-positions.md)); and the elaborator parks what the kernel decides. A disagreement in the admitting direction is a false definitional equation one checker believes.

## Refinement

Which goals are put to both — every goal the elaborator decides over a corpus, or a generated grid of pairs — how a disagreement is classified (admitting, refusing, budget), where the tally is kept, and whether it runs in the gate or beside it as `kernel_disagreements` does.
