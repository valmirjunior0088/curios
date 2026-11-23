# A declaration's universe levels are decided by the least model both checkers read

**Adopted by the user on 2026-11-22 and rewritten from its headline on 2026-11-23; its next step is stage 1, a ready delivery.** [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md) has one counter decide whether a declaration type-checks, and the elaborator's universe solver spends a second: its consistency search, `SEARCH_BUDGET`, 200,000 visits a search, fresh at each and charged to no budget (`curios-elab/src/universes/solver.rs`). The shared layer decides whether a constraint set has a model in the naturals without one, by forward reasoning to a least model ([`curios-analysis`'s README](../../../curios-analysis/README.md#level-entailment-is-forward-reasoning-to-a-least-model)), and the certifier and the elaborator's weak join already read it. Asked of the elaborator's store too, it decides the store's consistency; the search, its cap and the cache built around it go, and a refusal names the constraints that cannot hold together.

## What is wrong today

The figures are counted at `27dfb63e0`, over the prelude's build as `cargo xtask profile prelude` files it, in a copy of the tree that asks the certifier's `satisfiable` beside each of the solver's consistency checks.

- **A second counter can refuse a declaration.** Where the full check exhausts `SEARCH_BUDGET`, `SearchExhausted` refuses the declaration for a limit the budget does not see. The prelude's build never runs it out.
- **Consistency is decided twice.** The elaborator decides its store's consistency by a branching search over right-hand maxima, exponential in the branches and capped; the shared layer by Bezem and Coquand's forward reasoning, polynomial in the set and its offsets as written (`curios-analysis/src/satisfy.rs`, `level_model.rs`). On the 63,557 checks of the prelude's build that decide a store not decided before, the two agree.

## Goal

- The elaborator's consistency is `satisfiable` over its store, a model a check; the search, `Potential`, `DifferenceGraph`, `SEARCH_BUDGET`, `SearchExhausted` and `ConsistencyCache` go, and `universe_context_validate` is a context's closure and `satisfiable`.
- A reported inconsistency names a least set of the store's constraints that cannot hold together, each kept only where the rest without it is satisfiable, worded from the one raised last; a speculative check asks `satisfiable` alone.

## Settled

- **Sharing the decisions costs no second opinion.** [An independent certifier re-checks what the elaborator accepts](../../design/soundness/an-independent-certifier-re-checks-what-the-elaborator-accepts.md) shares "what arithmetic over the representation yields, which two copies could only disagree on through a transcription slip", and a decision procedure for one fixed theory, the semilattice with a successor, is such a function of a constraint set: the second opinion on levels stands in what each checker collects, the elaborator from its solving and the certifier from its own typing. That decision and `curios-cert`'s *The universe decisions are the shared layer's* state it.
- **A model a check, nothing kept between checks.** A check reads its store whole: in the prelude's build, 63,557 checks over at most 1,953 constraints, 283 on average. Rocq keeps its model incremental, with a union-find for equalities, against a global graph that lives for a whole library; a store here lives for one declaration, and its equalities are solved by `merge_forced_equalities`.
- **A refusal names a least core.** It costs a satisfiability run a constraint, paid where a refusal is reported and nowhere else. A loop traced through the model's raises follows one part of a right-hand maximum and leaves out what bounds the others — `u + 1 ≤ max(u, v)` named without `v ≤ u` — so it can name a set that holds.
- **Peers.** Lean and Agda solve level equations by unification with `max` and stop at `max(?l, ?k) = max(?u, ?v)`, which a user annotates away (TYPES 2025); Curios has no level syntax, so a store and a decision over it are its only route.

**Rejected.** The search kept and its visits charged to the declaration's budget: a second procedure for one theory, still exponential where the model is polynomial. A second statement of forward reasoning in the elaborator: a second engine for one rule. The search kept to explain a refusal: a second engine for a refusal's words. A loop traced through the model: it can name a set that holds. A model kept between checks: a cache no cost has shown.

## Stages

**1. The elaborator's consistency decided by the model.** `check_consistent` and `universe_context_validate` answered by `satisfiable`, a reported inconsistency by its least core; the search, `Potential`, `DifferenceGraph`, `SEARCH_BUDGET`, `SearchExhausted` with its spelling, and `ConsistencyCache` deleted, with the tests that held the search against itself and the certifier. Check: the prelude's images unchanged, the inconsistency tests passing, and the prelude's build profiled against `27dfb63e0`.

## Verification and retirement

Each stage's check, then `cargo xtask format`, `cargo xtask workspace lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. Once stage 1 lands, this file is deleted.
