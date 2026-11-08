# Each refusal of a rule stands beside its control

[The soundness board](../../design/soundness/the-soundness-board.md) places a held attack's program test in `curios`'s `tests::rules`, a file to a rule, each refusal beside its control: the sound neighbour the rule admits, run as the program it is and checked by what it prints. A refusal with no control passes as well when the rule has come to refuse everything of its shape, so the refusals alone cannot tell a rule that holds from one that refuses too much.

## What is wrong today

Part of `tests::rules` still puts a refusal with `refused(source)`, which asks for no control, so nothing holds a refusal beside one and those files drift from the rule: 154 tests there put a refusal that way, 70 of them with no test running a program before or after them, counted after the elimination files were written through controls (counted). By file, bare of refusals: `totality_proof_release_tests` 12 of 14, `sequence_tests` 11 of 11, `totality_partiality_tests` 10 of 14, `coherence_tests` 7 of 11, `positivity_composition_tests` 6 of 14, `proposition_tests` 5 of 12, `effect_tests` 4 of 8, `subsumption_tests` 4 of 14, `one_inhabitant_tests` 3 of 12, `positivity_computed_type_tests` 3 of 6, `metavariable_tests` 2 of 4, `totality_descent_tests` 2 of 11, `positivity_occurrence_tests` 1 of 4; and none bare of their refusals in `chain_tests` (2), `eta_tests` (8), `group_tests` (2), `irrelevance_tests` (2), `plicity_tests` (3) and `positivity_index_tests` (2), whose refusals stand beside a control as a neighbouring test rather than derived from it. The count reads adjacency and not what a control controls.

## Settled

- **A refusal is derived from its control.** `rules::test_support`'s `control(source, prints)` runs its program and checks what it prints, and `Control::refused(edits)` puts the control with each edit made, each edit's text standing once in the program as the edits before it left it: the one thing the refusal turns on, and what the refused program needs besides, such as `False` where the control states `True`. Editing the control carries every refusal derived from it, and an edit the control has moved away from fails. `totality_tests` and the elimination files — `arm_tests`, `coverage_tests`, `fold_tests`, `index_tests`, `large_elimination_tests`, `proposition_elimination_tests` and `spine_tests` — are written so.
- **A run-only test stays where it states a claim of its own**, a control broader than one refusal's or the twin of a kernel test under its name; one that only controlled the refusal before it is folded into that refusal's derivation.
- **The last stage closes the door**: `refused` becomes private to `rules::test_support`, so a refusal without its control does not compile.

## Stages

Each stage writes every refusal of its files through a control: a bare one gains the control it needs, the same construction with the one thing changed that the refusal turns on, and one beside a neighbouring control is derived from it, or from a nearer one where the neighbour controls another construction. A control prints a value its construction computes where it computes one.

1. **Formation**: `proposition_tests`, `subsumption_tests`, `plicity_tests` and the positivity files.
2. **Conversion**: `one_inhabitant_tests`, `sequence_tests`, `chain_tests`, `eta_tests` and `irrelevance_tests`.
3. **Totality**: `totality_descent_tests`, `totality_partiality_tests`, `totality_proof_release_tests` and `group_tests`.
4. **Admission**: `effect_tests`, `metavariable_tests` and `coherence_tests`.
5. **The door closed**: `refused` private to `rules::test_support`.

Each stage's check is its files' tests, run by name, and `cargo xtask workspace lint`. Once the last lands, nothing references this specification, and it is deleted.
