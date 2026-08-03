# Unification solver refinements

Not refined yet. This umbrella placeholder reserves the specification location for the remaining unification solver work in `curios-elab` until each item is refined into a working implementation specification.

## Covered roadmap items

- Pruning of out-of-scope metavariables
- η-equating metavariable heads
- Surfacing residual unification constraints (distinguishing postponed from rigid-mismatch diagnostics)

The third item's checking-shaped half has landed: a parked *checking* problem that survives every retry now reports `cannot check expression: its expected type never gained structure` at the expression's own span (`Error::PostponedCheck`), rather than an unlocated `cannot infer`. The item's remaining scope is the conversion-shaped residue — a parked conversion goal still reports as a plain mismatch at its origin, distinguished only when it stands between witness holes.
