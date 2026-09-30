# Flex–flex problems with distinct heads

**Not refined yet.** This specification reserves intersection for two metavariables met against each other. It is not an implementation plan.

## What is missing

Two distinct metavariables over compatible telescopes, met through one live name — `?0(x) ~ ?1(x)` — park and stay undecided: the solver does no intersection, so it never solves one as the other through a renaming. Where nothing else pins either, the item ends with an unsolved metavariable a program's author did not write. `curios-elab`'s `convert/solve_tests.rs` holds the case as undecided.

## Refinement

Pruning to the intersection of the two spines' variables, as Agda and Lean do, and whether it changes any settled program's solution.
