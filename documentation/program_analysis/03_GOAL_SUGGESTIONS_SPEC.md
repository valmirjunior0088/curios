# Goal suggestions (`? ≈`) — sandboxed local and application fits

This document specifies candidate suggestions in goal reports: for an unsolved written goal, the report may offer terms the elaborator has verified would fit, computed by machinery the compiler already runs and rendered as `? ≈ candidate` lines.

## Problem

A goal report states the obligation but ventures nothing, even when elaboration could answer it outright: a base-case goal `? : Eq(0 + 0, 0 * 2)` is machine-solvable by reducing the indices, filtering constructors through index inversion, and unifying — all machinery already in the compiler. Every fact needed sits in the report's hands at collection time: the frozen telescope, the expected type, and the live elaboration context.

## Contract

For each *unsolved* goal in a batch — a solved goal already prints `? =`, and a suggestion beside an answer is noise — the report may append up to **3** candidate lines, rendered `? ≈ candidate` between the `? : type` clause and the snippet.

Candidates are observation-only text: the compiler re-checks whatever the author pastes, so a wrong suggestion costs nothing and the trusted base is never consulted. Candidates flow through the goal-report display pipeline — tolerant materialization, universe erasure, operator folding — so they are spelled the way the source could write them, with residual holes spelled `?`.

## The sandbox

Every attempt runs inside `Context::solution_mark` / `rollback_solutions` — the transactional bracket re-validation and witness resolution already use — with one ordering rule: on a hit, the candidate term is **materialized (committed solutions spliced) before rollback**, or the pinned arguments the display should show would be lost with the transaction.

The pass restores the reduction budget once before running (the `finalize_and_check` precedent), a reduction error in any probe skips that candidate and never fails the report, and a `Blocked` conversion verdict counts as a non-fit — the suggestion pass parks nothing and retries nothing. The pass is infallible by construction.

## Local fits (S1)

- **Scope fits.** For each binder in the goal's frozen telescope, probe its type against the goal type with the sandboxed conversion witness resolution already uses (`probe_match`); a definite fit yields the bare binder as a candidate.
- **Constructor fits.** Reduce the goal type to weak head normal form; when it is an inductive application, run the loop match elaboration already runs for omitted arms, for the opposite verdict: per constructor, instantiate its telescope at the goal's parameters, read its index targets, and run the shared inversion unifier (`invert_indices`) against the goal's indices. `Impossible` filters the constructor out; a solution pinning every payload binder yields a complete candidate (`Eq/refl()`); a partial solution yields a refinement with holes (`Vec/cons(?, ?)`). A struct-typed goal analogously suggests its literal shape with a hole per field.

## Application fits (S2)

- **Attempt shape.** The witness-table instantiation generalized to an arbitrary candidate: instantiate the candidate's function-type telescope with fresh metavariables — every plicity becomes a hole; `use` slots stay holes, with no witness resolution inside the pass — probe the instantiated output type against the goal, materialize on a definite fit, and roll back regardless. Arguments the unification pinned therefore display filled: a goal `Eq(3, 3)` against `mk : (n : Nat) -> Eq(n, n)` suggests `mk(3)`.
- **Candidate pool**, in order: the goal's telescope binders with function types; the entry module's own definitions; then every global the module's items already reference. The full prelude surface is deliberately excluded — probing hundreds of names per goal without an index is cost without precision, and indexing belongs to the deferred tier below.
- **Ranking.** Complete fits, then fewest residual holes, then pool order. Attempts are capped per goal (an internal constant, stated in the pass's documentation); the rendered list is capped at 3.

## Deferred: recursive search

Considered and deliberately not designed here, with the evidence recorded so the future decision starts warm. Chained search — filling a suggestion's holes recursively — is real proof search: the motivating step case needs `Eq/cong((x) => x + 2, ih)`, whose function must be *invented* from `f(p + p) ≡ (p + p) + 2`, a compound-argument problem outside the decidable Miller fragment (general higher-order unification). A useful search over applications additionally needs hint indexing (discrimination trees, which the workspace does not have) and an open-ended search-policy and quality loop. In its favor when the time comes: witness resolution is already a bounded backtracking search with recursive premise goals, conversion has history and deadline discipline for loop control, and the two tiers above become its leaf steps unchanged. The decision is deferred until someone reaches for it.

## Non-goals

- Witness resolution inside attempts; parking or retrying suggestion goals.
- Suggestions for solved goals.
- Tactics or any interaction protocol.
- Prelude-wide candidate pools or indexes.
- Any change to checking semantics: the pass is display-only and infallible.

## Tests

- **S1:** the motivating base case reports `? ≈ Eq/refl()` end to end; an indexed family at an impossible index suggests only its possible constructors (`Vec(T, 0)` suggests `nil`, never `cons`); a telescope binder whose type converts is suggested; a solved goal gets no `? ≈` line; suggestion order is deterministic.
- **S2:** a goal matching a module function's output suggests the application with pinned arguments displayed (`mk(3)`); complete fits rank before refinements; pool order breaks ties.
- **Both:** pasting a suggested complete candidate compiles the fixture past that goal.

## Milestones

- **S1 — Local fits** (`curios-elab`): the suggestion pass, `GoalReport` candidates, the `? ≈` clause, tests. One commit.
- **S2 — Application fits** (`curios-elab`): the pool, the generalized instantiate-probe, ranking and caps, tests. One commit.

S2 depends only on S1's plumbing; each lands green on its own.

## Retirement criteria

- Before this specification is deleted: suggestion behavior and the materialize-before-rollback rule are recorded in the owning module documentation and tests; the roadmap subitem is a checked unlinked summary; the recursive-search deferral record moves to `DESIGN.md`; and no reference to this filename remains.
