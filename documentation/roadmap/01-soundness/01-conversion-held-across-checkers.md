# The two checkers' conversion held to each other

**Not refined yet.** This specification reserves the differential that would hold the elaborator's conversion and the kernel's to one verdict. It is not an implementation plan.

## What is missing

The two checkers are held to each other on non-informativeness, universe-context validity, the memos and the recorded board fixtures ([An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)), and on conversion only wherever the corpus happens to send both. Conversion is where they differ most: the elaborator's recurrence key renames minted labels and records no local context, where the kernel's records both ([Conversion recurrence](../../design/soundness/conversion/conversion-recurrence.md)); a child the kernel compares at `Type` forfeits eta and irrelevance ([Eta and untyped child positions](../../design/soundness/conversion/eta-and-untyped-child-positions.md)); and the elaborator parks what the kernel decides. A disagreement in the admitting direction is a false definitional equation one checker believes.

Two disagreements in the refusing direction are ones a program reaches, and no corpus sent both checkers to either ([What conversion still decides by spelling or by cap](../04-arithmetic/01-decided-by-spelling-or-cap.md), whose protocol states the programs). With `p1`, `p2` two proofs of one bound and `q1`, `q2` of another, `Eq/refl()` at `Eq()(f(a, p1) * f(b, q1), f(b, q2) * f(a, p2))` is accepted by the elaborator and refused by the kernel at some orders of its binders, and accepted or refused by both at the others. And the elaborator takes an annotation as a step where the kernel compares end to end: a `Vec(Nat, f(a, p1) * f(b, q1))` returned at `Vec(Nat, f(b, q2) * f(a, p2))` through a `let` annotated at `Vec(Nat, f(a, p2) * f(b, q1))` is accepted by the elaborator at every binder order and refused by the kernel at most of them.

## Refinement

Which goals are put to both — every goal the elaborator decides over a corpus, or a generated grid of pairs — how a disagreement is classified (admitting, refusing, budget), where the tally is kept, and whether it runs in the gate or beside it as `kernel_disagreements` does.
