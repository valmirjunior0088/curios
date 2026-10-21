# Unification solves a metavariable inside a polynomial

**Not refined yet.** This specification reserves broader polynomial and sign reasoning, and solving proposals beyond today's, so unification can propose a solution where a metavariable sits inside a polynomial equation. It is not an implementation plan. It waits for a consumer that needs such a solution, and refinement establishes its fragment, algorithm contract and acceptance criteria before any implementation.

## What is missing

Today the elaborator solves the one unsolved metavariable an equation over `Nat` or `Int` is linear in, by exact division of the equation's linear view, and puts the quotient to conversion (`curios-elab`'s `convert::linear`): `?w * (y + z)` against `x * z + x * y` gives `x`. It declines, and the equation stays undecided, where two metavariables are unsolved, where one stands at a power — `?w * ?w + ?w * y` against `x * x + x * y`, which `x` solves — and where one stands inside an atom, as an argument of a call.

## What is known

- **Previously discussed.** A ring procedure sharing the appropriate mathematics of `Nat` and `Int`, with its solver on the elaborator's side of the certifier's dependency closure; the elaborator chooses solutions and the certifier checks what reaches it.

## Refinement

- Whether a metavariable at a power is solved at `Nat`, once a consumer needs it — a square matrix stored flat, `Vec(T, n * n)`, is the likely one. With the unknown on one side only, that side is strictly increasing in it, so the equation has at most one natural solution; the solution is an exact root, which the elaborator would propose and conversion check as it checks a quotient. At `Int` a square has two roots, so the equation stays refused there.
- Whether several equations are solved together as the linear system they are in their metavariables, which the worklist approximates by retrying a parked equation once another has solved one of its unknowns.
- The fragment, the treatment of natural coefficients and integer signs, opaque atoms, and where a proposal ends and a search belongs to [the elaborator's search over the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) instead.

It retires when unification proposes a solution, and conversion checks it, for the metavariables refinement chooses: one at a power at `Nat` first among them.
