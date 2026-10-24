# An elimination reads its scrutinee as conversion does

[A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) holds that an elimination reads its scrutinee as conversion does, and `/sys`'s `Holds` is a `Bool` match, so a decision conversion equates with `true` is a bound reduction discharges. The procedure that proves a bound from the facts in scope still keeps a rung for such a decision, which nothing reaches.

**Ready**: the design is settled under *Design*, and stage 1 is the next step.

## What is wrong

- **The procedure that proves a bound keeps a rung nothing reaches.** `entailment::tautology` (`curios-elab/src/entailment.rs`) writes `Bool/holds_of_eq` over `Eq/refl()` for a decision conversion equates with `true` and reduction does not. `Holds` reads such a decision as conversion does, its atoms classed in a judgment's reduction, so the fill (`trivially_inhabited`) reduces the bound to `True` and proves it by `True/qed()` before the procedure is asked, as it now proves `/std/http/Response/empty`'s invariant.

## Design

- **What it retires.** `entailment::tautology` with the registry's `holds_of_eq` and `refl` (`curios-utilities`' `EntailmentSyntax`), which nothing else reads, and their fills.

## Stages

1. **The procedure that proves a bound drops its tautology rung.** `entailment::tautology` and the registry's `holds_of_eq` and `refl` go, once the rung is shown to prove nothing over `/std` and the workspace's tests. Check: the bound and entailment tests; `/std`'s image unchanged at one path.

## Verification and retirement

`cargo xtask workspace lint` and the touched crates' tests, `cargo xtask workspace test` once at the end. Retired once the stage lands and nothing cites this file; the figures it moves are named: `/std`'s image, its elaboration and certification units and instructions, the scrutinized and proved doors of `scrutinee_refinement_measurements`, the scrutinized rows of `combinator_web_cost_measurements`, and `runaway_measurements` where a connective's flattening is charged.
