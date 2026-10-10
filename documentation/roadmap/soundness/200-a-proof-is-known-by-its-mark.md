# A proof is known by its mark

**Not refined yet.** This specification reserves the form in which conversion and erasure read whether a term is a proof off the term, where each reads it off a type today. It is not an implementation plan.

[Conversion is one relation in both checkers](../../design/soundness/conversion-is-one-relation-in-both-checkers.md) has two terms at a position handed no type decided by the type a lookup gives both. That is sound for as long as what a checker remembers of a verdict is dropped wherever a binder's assumed type moves, which is a discipline its stores keep and no property of the relation.

## What this builds on

- **Whether a term is a proof is a fact about the term.** No proposition stands where a type is wanted ([Subsumption is a relation, not a traversal order](../../design/theory/subsumption-is-a-relation-not-a-traversal-order.md)), so a binder's type is a proposition under every substitution or under none, and an arm's case never turns data into a proof.
- **Reduction reads no binder's type.** A match that takes a proposition into data states the instance it eliminates ([`Prop` is strict, proof-irrelevant, and definitionally K](../../design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md)), and no type has one inhabitant by its shape.
- **Marks exist, held one way.** A scope's label states whether its binder binds a proof (`Label::Local`, `curios-core/src/scope.rs`): `Term::stating_proofs` states it for a function literal's and a `let`'s binders, an arm's binders take it from their constructor's signature, and a definition carries its own (`Definition::proof`). The kernel refuses a mark at a type that is no proposition and accepts a binder at a proposition that bears none (`hold_proof`, `curios-cert/src/obligation.rs`). The closed machine alone reads them ([The closed machine binds a proof unreduced](../../design/compilation/the-closed-machine-binds-a-proof-unreduced.md)).
- **Peers.** Rocq's kernel compares untyped and decides irrelevance by a relevance mark every binder carries, which typing checks against the binder's sort; a case node and a projection carry one too. Lean's kernel infers the type of both sides to decide it, and Agda's conversion is typed throughout (Lennon-Bertrand, *What does it take to certify a conversion checker?*, FSCD 2025, §4.4).

## What it costs today

**Two neutrals at a position handed no type are compared by a lookup of their types.** A stuck elimination's scrutinee, a projection's head and an instance's head are compared at `Type`, and what a type directs there is asked of the type a lookup gives each side: `by_their_own_type` and `looked_up` in `curios-cert/src/kernel/convert.rs`, and their twins in `curios-elab/src/convert.rs`, each through `synth_neutral`, which reads what a binder is assumed at. So a verdict the kernel remembers under two terms depends on the scope as well as on the terms, and `Scope::retract` (`curios-cert/src/kernel/scope.rs`) clears what is remembered where an arm's re-typing closes, for that reason alone. The motive's and the arms' binders of two stuck eliminations are opened at a looked-up type the same way (`scrutinee_type`), where neither states its instance.

**A mark is not exact, so nothing may read it in its type's place.** A binder at a proposition may bear no mark, and a function type's and a motive's binders state none. A reader that took the mark for the sort would hold two proofs apart wherever the mark is missing, and labels are outside a scope's identity, so two terms that differ in a mark are one term to every store.

**Erasure decides what is kept by reducing a type.** `is_erasable` (`curios-elab/src/into_ersd`) reads a sort where the mark already says it, which is the second form [A reader of finished terms holds no elaboration state](../compilation/201-a-reader-of-finished-terms-holds-no-elaboration-state.md) weighs for what erasure reduces through.

## The form

**A mark is exact, and the kernel holds it both ways.** Every binder of a finished term states whether it binds a proof, and a binder at a proposition that bears no mark is refused as one marked at a type is.

**A term is a proof where its head says so.** A variable by its binder's mark, a declared name by its definition's, an application and a projection by their head, a constructor's value by its family's sort, an elimination by a mark of its own, which states what its result lands in, as Rocq's case node states it. Nothing is looked up and nothing is reduced.

**Conversion between two terms handed no type asks that and nothing else.** Two proofs are one; anything else is compared by its head. What is remembered of two terms then stands for as long as the equations it read do, and the clearing at an arm's close goes.

**Erasure reads the mark where it reads a sort.**

## Not known

- Whether a mark belongs in a scope's identity, or a term that differs from another in a mark alone is refused before any store can hold both.
- What a projection out of a record states: a field at a proposition makes the projection a proof and the record's own head does not say which field is one.
- How many comparisons the lookup decides today that the head would not. Certifying `/sys` and `/std` at `f748db3f4`, irrelevance decided eight comparisons between two different terms, counted with a line added to `compare` and removed; how many of them stood at a position handed no type was not counted.
- Whether the elaborator's conversion can take the same form while a binder's type is still a metavariable, or keeps a lookup until the type is solved.
- What a binder at a sort its declaration leaves to a caller states. It binds a proof at one assignment and data at another ([A declaration leaves a sort to its caller](../../design/theory/a-declaration-leaves-a-sort-to-its-caller.md)), so a mark that is exact is a quality, stated at an instance as the sort is.

## Retirement

Before this specification is deleted, the conversion decision states the rule with its rejected alternatives, the certifier's README states what the kernel checks of a mark, the roadmap entry is a checked summary, and no reference to this filename remains.
