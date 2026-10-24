# An elimination reads its scrutinee as conversion does

[A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) holds that an elimination reads its scrutinee as conversion does, Coq Modulo Theory's elimination modulo its theory, and [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) names what that theory's premise needs, each condition held at every declared law by `tests::laws::audit`. Elimination modulo the theory is neither named there nor held at the laws, and the procedure that proves a bound keeps a rung the rule leaves nothing to.

**Ready**: the design is settled under *Design*, and stage 1 is the next step.

## What is wrong

- **No law is held to an elimination.** `tests::laws::structural`'s seeds state iota modulo the theory at a handful of scrutinees, and `tests::laws::audit` holds every declared law reversed, chained, at compound terms and through a solved metavariable, but states none with its sides under the elimination of its carrier. [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) names the conditions Coq Modulo Theory's premise needs and leaves elimination modulo the theory out.
- **The procedure that proves a bound keeps a rung nothing reaches.** `entailment::tautology` (`curios-elab/src/entailment.rs`) writes `Bool/holds_of_eq` over `Eq/refl()` for a decision conversion equates with `true` and reduction does not. `Holds` is a `Bool` match, which reads such a decision as conversion does, its atoms classed in a judgment's reduction, so the fill (`trivially_inhabited`) reduces the bound to `True` and proves it by `True/qed()` before the procedure is asked, as it now proves `/std/http/Response/empty`'s invariant.

## What is known

- **Prior art.** Coq Modulo Theory's `ι_T` fires an eliminator where "C(ū) simplifies t": `t` is not constructor-headed and `t ↔*_T C(ū)`, decided by the theory's own procedure (Jouannaud and Strub, [*Coq without Type Casts*](https://easychair.org/publications/paper/BKQ), LPAR-21, 2017), among the conditions under which strong normalization and confluence are proved.

## Design

- **What the audit asks.** Each `Bool` law with both sides under a `Bool` match, and each `Nat` law with its first variable a product the fold leaves standing on the left and that product distributed on the right, both under a structural `Nat` match: a side the theory reads as a value reaches its arm only where the elimination reads it so.
- **What it retires.** `entailment::tautology` with the registry's `holds_of_eq` and `refl`, which nothing else reads.

## Stages

1. **The audit holds every law under its carrier's elimination.** `tests::laws::audit` states the laws as *Design* says; [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) names elimination modulo the theory among the conditions its premise needs. Check: the audit holds in both checkers.
2. **The procedure that proves a bound drops its tautology rung.** `entailment::tautology` and the registry's `holds_of_eq` and `refl` go, once the rung is shown to prove nothing over `/std` and the workspace's tests. Check: the bound and entailment tests; `/std`'s image unchanged at one path.

## Verification and retirement

`cargo xtask workspace lint` and the touched crates' tests after each stage, `cargo xtask workspace test` once at the end. Retired once the stages land and nothing cites this file; the figures it moves are named: `/std`'s image, its elaboration and certification units and instructions, the scrutinized and proved doors of `scrutinee_refinement_measurements`, the scrutinized rows of `combinator_web_cost_measurements`, and `runaway_measurements` where a connective's flattening is charged.
