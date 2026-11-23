//! Whether a universe constraint set can be satisfied at all: whether some assignment of naturals to its heads meets every constraint.
//!
//! An unsatisfiable set is a hypothesis set from which everything follows — [`entails`](crate::entails) proves whatever it is asked under one — so a set is decided here before anything is assumed of it.
//!
//! # The decision
//!
//! Exactly one of two things holds of a constraint set: it holds a *loop* — some `t + 1 ≤ t` follows from it — or it has a model in the naturals (Bezem and Coquand's Corollary 3.5, in *Loop-checking and the uniform word problem for join-semilattices with an inflationary endomorphism*, TCS 913, 2022). So the question is whether forward reasoning finds a loop. [`LevelModel::throughout`](crate::LevelModel::throughout) starts every head at the largest offset any upper side carries; passes raise it towards the least model above that start; a value climbing past the model's ceiling is a loop, and a pass that raises nothing is the least model, which exists only where there is none.
//!
//! The naturals' zero is the model's floor, below every head, which is the fact no constraint states — a parameter ranges over the naturals — and what makes `P0 + 1 ≤ P1` with `P1 ≤ 0` a loop through the zero rather than a set with a model over the integers. A model whose zero sits above 0 shifts down to one whose zero is 0, since every relation is invariant under a uniform shift, so a model the corollary promises is one with the parameters ranging over the naturals.
//!
//! **No search, and so no budget.** Read the other way, a right-hand maximum is a disjunction: `max(1, P0) ≤ max(1, P1)` bounds `P0` by either `P1` or the constant, and a search would choose an alternative per lower part, commit each as a difference constraint against a feasible potential and backtrack on a negative cycle, under a node budget it could run out of. As a clause the maximum is a conjunction to be derived rather than a choice to be made, and the decision is polynomial and exact in both directions.

#[cfg(test)]
mod tests;

use {
    super::{LevelModel, Settled},
    curios_core::UniverseConstraint,
};

/// Whether some assignment of naturals to parameters satisfies every constraint.
pub fn satisfiable<'a>(
    constraints: impl IntoIterator<Item = &'a UniverseConstraint, IntoIter: Clone>,
) -> bool {
    let constraints = constraints.into_iter();
    let mut model = LevelModel::throughout(constraints.clone());
    model.settle(constraints, |_| false) != Settled::Loop
}
