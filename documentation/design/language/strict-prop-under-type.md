# Strict Prop under Type

**Decision.** Alongside the cumulative `Type` hierarchy, Curios has a strict `Prop` with definitional proof irrelevance. `Prop : Type 0` holds, proposition types are admitted at `Type` through the existing subsumption and cumulativity, and large elimination out of `Prop` is guarded.

**Rationale.** Proof irrelevance is what makes proofs erasable by construction: any two proofs of a proposition are definitionally equal, so no program can depend on which proof it received, and erasure drops them wholesale. The large-elimination guard is what keeps that erasure sound.
