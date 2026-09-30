# K-like reduction

**Not refined yet.** This specification reserves reduction of an elimination on a proof whose constructor is not yet known. It is not an implementation plan, and it waits for a program that needs it.

## What is missing

Definitional K licenses index inversion's deletion rule, and every `Eq/subst` and `refl` match in `/std` lands in a proposition, where irrelevance decides it ([`Prop` is strict, proof-irrelevant, and definitionally K](../../design/types/prop-is-strict-proof-irrelevant-and-definitionally-k.md)). A relevant match on a stuck `Eq` proof does not reduce: nothing fires K as a reduction step.

## Previously discussed

Tried before the scrutinee is forced; decided by `invert_indices` with the constructor's payload binders flexible, firing only when every binder is solved and no position refused; the scrutinee's instance synthesized from its neutral spelling; the step written in each reducer and the classification shared. Lean's `to_cnstr_when_K` asks about the major premise's type before reducing it, and inversion already refuses metavariables, as Lean's elaborator refuses to fire K by assigning `?x := y`.
