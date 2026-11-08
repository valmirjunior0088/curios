# Each refusal of a rule stands beside its control

[The soundness board](../../design/soundness/the-soundness-board.md) places a held attack's program test in `curios`'s `tests::rules`, a file to a rule, each refusal beside its control: the sound neighbour the rule admits, run as the program it is and checked by what it prints. A refusal with no control passes as well when the rule has come to refuse everything of its shape, so the refusals alone cannot tell a rule that holds from one that refuses too much.

## What is wrong today

Every refusal in `tests::rules` is derived from its control, but `rules::test_support`'s `refused(source)`, which asks for no control, is still public, so nothing keeps the next refusal from being put without one.

## Settled

- **A refusal is derived from its control.** `rules::test_support`'s `control(source, prints)` runs its program and checks what it prints, and `Control::refused(edits)` puts the control with each edit made, each edit's text standing once in the program as the edits before it left it: the one thing the refusal turns on, and what the refused program needs besides, such as `False` where the control states `True`. Editing the control carries every refusal derived from it, and an edit the control has moved away from fails. Every file of `tests::rules` is written so.
- **A run-only test stays where it states a claim of its own**, a control broader than one refusal's or the twin of a kernel test under its name; one that only controlled the refusal before it is folded into that refusal's derivation.
- **The last stage closes the door**: `refused` becomes private to `rules::test_support`, so a refusal without its control does not compile.

## Stages

Each stage writes every refusal of its files through a control: a bare one gains the control it needs, the same construction with the one thing changed that the refusal turns on, and one beside a neighbouring control is derived from it, or from a nearer one where the neighbour controls another construction. A control prints a value its construction computes where it computes one.

1. **The door closed**: `refused` private to `rules::test_support`.

Each stage's check is its files' tests, run by name, and `cargo xtask workspace lint`. Once the last lands, nothing references this specification, and it is deleted.
