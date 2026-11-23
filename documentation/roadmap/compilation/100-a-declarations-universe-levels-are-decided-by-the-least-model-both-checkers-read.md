# A declaration's universe levels are decided by the least model both checkers read

**Adopted by the user on 2026-11-22; its next step is stage 1, a ready delivery.** [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md) has one counter decide whether a declaration type-checks, and the elaborator's universe solver spends a second: its consistency search, `SEARCH_BUDGET`, 200,000 visits a search, fresh at each and charged to no budget (`curios-elab/src/universes/solver.rs`). The certifier already decides the same theory without one, by forward reasoning to a least model ([`curios-cert`'s README](../../../curios-cert/README.md#level-entailment-is-forward-reasoning-to-a-least-model)). Lifted into the shared layer, those two decisions answer the elaborator's questions too, and the search, its cap and the cache built around it go.

## What is wrong today

- **A second counter decides what a declaration elaborates to.** Where the full check exhausts `SEARCH_BUDGET`, `SearchExhausted` refuses the declaration for a limit the budget does not see; where the cached entailment search, `ConsistencyCache::refuses`, exhausts it, `entails` reads the exhaustion as a refusal of the negation, so as entailed, and the weak join drops a run for want of search. At `e45585da6` the prelude's build exhausts the cached search 287 times in ten `/std/Map` declarations, with up to 194 branches, and the full check never, so which weak pairs those ten keep, and which level an irrelevant occurrence lands on, follows the cap.
- **Two procedures decide one theory.** The certifier decides whether a constraint set forces `lower ≤ upper`, and whether it has a model, by Bezem and Coquand's forward reasoning, in polynomial time (`curios-cert/src/entail.rs`, `level_model.rs`, `satisfy.rs`); the elaborator decides its store's consistency by a branching search over right-hand maxima, exponential in the branches and capped, and entailment by refuting the negation with that search.
- **The weak join asks what one model answers.** `relations_stand` asks every ordered pair of a declaration's held levels whether the store entails each gap, 272 questions a run at seventeen levels, and the join tries every pair conversion met, a repeat included: at `e45585da6`, `/std`'s joins meet 83,160 pairs, 29,793 of them distinct, `Map/internal/Node/canonical` 7,397 and 625, and dropping each repeat after its first copy cuts the joins' runs from 48,077 to 11,133 and `canonical` from 33.5 s to 8.5 s in one reading under load. With the cap reading exhaustion as entailment, dropping repeats moves which pairs five of the ten `Map` declarations keep, so it waits on this.

## Goal

- `entails` and `satisfiable` live in `curios-analysis`, over `curios-core`'s `UniverseConstraint` and `Level` as they stand, and both checkers call them; the certifier's verdicts do not move.
- The elaborator's consistency check and its entailment are answered by them: the search, `SEARCH_BUDGET`, `SearchExhausted` and the consistency cache with `refused_as_decided` are deleted.
- The weak join reads a held level's relations off one least model a level rather than a question a pair, and drops a repeated pair before it is tried.

## Settled

- **Sharing the decisions costs no second opinion.** [An independent certifier re-checks what the elaborator accepts](../../design/soundness/an-independent-certifier-re-checks-what-the-elaborator-accepts.md) shares "what arithmetic over the representation yields, which two copies could only disagree on through a transcription slip", and a decision procedure for one fixed theory, the semilattice with a successor, is such a function of a constraint set: the second opinion on levels stands in what each checker collects, the elaborator from its solving and the certifier from its own typing. That decision and `curios-cert`'s *The universe decisions are the shared layer's* state it.
- **`curios-analysis` is the owner.** It already depends on `curios-core`, so the three modules move as they are; `curios-algebra`, which by its decision names no `curios-core` type, would need them restated over its atoms.
- **Peers.** Rocq's refinement of the same procedure by Sozeau and Bezem is one algorithm for elaboration's consistency and the kernel's entailment, at about 15% over its previous checker on average ([TYPES 2025](https://msp.cis.strath.ac.uk/types2025/abstracts/TYPES2025_paper21.pdf)); Rocq keeps its weak constraints as a set of unordered pairs (`UPairSet`, `engine/univMinim.ml`), so a repeat is never tried.

**Rejected.** The search kept and its visits charged to the declaration's budget: a second procedure for one theory, still exponential where the model is polynomial. Entailment shared alone: the search and its cap stay for consistency. A second statement of forward reasoning in the elaborator: a second engine for one rule. Rocq's unordered set for the weak pairs: a pair's direction picks which meta `add_eq`'s shape default pins, so collapsing `(u, v)` with `(v, u)` moves which level lands where ordered deduplication does not.

## Open questions

- **What a least model per check costs the elaborator** against the cached search it replaces, a check following every constraint a declaration adds. Settled by the prelude profiled before and after stage 2.

## Stages

**1. The decisions lifted.** `entail.rs`, `level_model.rs` and `satisfy.rs` moved into `curios-analysis`, named in `curios-cert`'s module of what its verdicts reach, and *Level entailment is forward reasoning to a least model* moved to `curios-analysis`'s README. Check: the prelude's images unchanged, and `curios-cert`'s universe tests passing where they stand.

**2. The elaborator answered by them.** Its consistency check and `entails` call the shared decisions; the search, `SEARCH_BUDGET`, `SearchExhausted` and the consistency cache deleted. Check: the prelude builds, the ten `Map` declarations keep the pairs the model entails, and the profile is retaken against the open question.

**3. The weak join read off models.** A held level's relations read off one least model, and a repeated pair dropped before it is tried. Check: the prelude's images unchanged from stage 2's, and the joins' runs counted.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. Once stage 3 lands, this file is deleted.
