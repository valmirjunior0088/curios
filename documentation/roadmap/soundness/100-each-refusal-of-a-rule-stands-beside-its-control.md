# Each refusal of a rule stands beside its control

[The soundness board](../../design/soundness/the-soundness-board.md) places a held attack's program test in `curios`'s `tests::rules`, a file to a rule, each refusal beside its control: the sound neighbour the rule admits, run as the program it is and checked by what it prints. A refusal with no control passes as well when the rule has come to refuse everything of its shape, so the refusals alone cannot tell a rule that holds from one that refuses too much.

## What is wrong today

107 of the 211 refusals in `tests::rules` have no control beside them, counted at `5761bab67` by whether the test before or after each runs a program (counted). By file: `arm_tests` 11 of 11, `index_tests` 13 of 21, `totality_proof_release_tests` 12 of 14, `sequence_tests` 10 of 11, `totality_tests` 9 of 9, `totality_partiality_tests` 8 of 12, `coherence_tests` 7 of 11, `positivity_composition_tests` 6 of 14, `fold_tests` 5 of 9, `large_elimination_tests` 5 of 9, `proposition_tests` 5 of 12, `coverage_tests` 3 of 3, `effect_tests` 3 of 7, `one_inhabitant_tests` 3 of 11, `metavariable_tests` 2 of 4, `positivity_computed_type_tests` 2 of 5, `subsumption_tests` 2 of 11, `positivity_occurrence_tests` 1 of 4. The count reads adjacency and not what a control controls, so a control standing elsewhere in a file may already answer for some of these, and one beside a refusal may control another construction.

## Stages

Each stage writes, beside each refusal of its rules that has none, the control it needs: the same construction with the one thing changed that the refusal turns on, run and checked by what it prints. Where a control already in the file answers for a refusal, the refusal moves beside it instead, or its comment names it.

1. **Elimination**: `arm_tests`, `coverage_tests`, `fold_tests`, `index_tests`, `large_elimination_tests`.
2. **Formation**: `proposition_tests`, `subsumption_tests` and the positivity files.
3. **Conversion**: `one_inhabitant_tests` and `sequence_tests`.
4. **Totality**: the totality files.
5. **Admission**: `effect_tests`, `metavariable_tests` and `coherence_tests`.

Each stage's check is its controls, run by name, and `cargo xtask workspace lint`.
