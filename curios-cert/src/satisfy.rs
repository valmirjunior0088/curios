//! Whether a declaration's universe constraints can be satisfied at all.
//!
//! The kernel *assumes* an item's [`UniverseContext`](curios_core::UniverseContext) while checking it — that is what lets a correct polymorphic definition through, since its recorded constraints are exactly the hypotheses its level questions need. An unsatisfiable set is therefore not a harmless oddity but a hypothesis set from which everything follows: `entails` starts proving whatever it is asked, `check_instance` stops discharging anything, and the universe discipline that keeps the paradox out stops applying.
//!
//! `curios-elab` decides the same question in its solver, and this is deliberately a *second* implementation rather than a copy. A transcription would inherit whatever the original gets wrong and agree for that reason, which is the failure the shared analyses already demonstrate; written from the constraint semantics instead, the two can disagree, and a disagreement is a signal. It lives in the certifier rather than among the shared analyses because the kernel is the only checker that runs it, and it takes a constraint set rather than an `Env`, so nothing reaches it through the seam — the reason `entails` lives here too.
//!
//! That argument incurs an obligation, and the obligation is discharged rather than assumed. A disagreement is a signal only where something can observe one, and nothing in a compile can: `universe_context_validate` refuses during elaboration, so a context it rejects never becomes part of a module, and this function is asked only about contexts it has already passed — for every program in the corpus. `curios-elab`'s `universe_solver::tests::both_checkers_decide_universe_context_validity_alike` therefore puts the two decisions to each other directly, which is the only place this second opinion is worth anything. The unsound direction is *this* side being the more permissive of the two, because the kernel assumes a context while checking under it.
//!
//! # The closure half is not here
//!
//! Whether a context is *closed* — every parameter index below the declared count and no level holding a metavariable — has essentially one implementation, so a second copy would agree by construction rather than by independence, a second opinion worth nothing (see `documentation/design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md`). It is [`UniverseContext::is_closed`](curios_core::UniverseContext::is_closed), decided once on the data it is about.
//!
//! Satisfiability is the opposite case and stays written twice, because here there is real algorithmic freedom for the two to differ in: this reads a least model, and the elaborator's is a run of its solver's search. That is the line — a property of the data is read once; a question that needs a procedure is answered twice.
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

use {super::LevelModel, curios_core::UniverseConstraint};

/// Whether some assignment of naturals to parameters satisfies every constraint.
pub fn satisfiable(constraints: &[UniverseConstraint]) -> bool {
    let mut model = LevelModel::throughout(constraints);
    let ceiling = model.ceiling(constraints);

    loop {
        let raised = model.pass(constraints);

        if model.climbs_past(ceiling) {
            return false;
        }
        if !raised {
            return true;
        }
    }
}
