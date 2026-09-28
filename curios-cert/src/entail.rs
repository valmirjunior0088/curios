//! The level entailment oracle: whether an assumed constraint set forces `lower <= upper`. Sound and deliberately incomplete — it derives what the hypotheses force by forward reasoning alone, the Horn-clause reading of the level algebra, and refuses what needs the naturals' total order on top. It is a rule that admits programs (levels the hypotheses force equal are equal in every instance satisfying them), which is why it lives in the certifier rather than beside the representation.
//!
//! # Forward, to a least model
//!
//! Whether `lower ≤ upper` follows is whether every part of `lower` can be *derived* from the parts of `upper` by firing the hypotheses as clauses, which is Bezem and Coquand's characterisation of entailment for the semilattice with an inflationary successor (*Loop-checking and the uniform word problem for join-semilattices with an inflationary endomorphism*, TCS 913, 2022, Theorem 2.2). The derivation is computed rather than searched for: [`LevelModel`](crate::LevelModel) starts from the facts `upper` states and passes over the hypotheses until one raises nothing, and the answer is whether that least model bounds `lower`.
//!
//! **This replaced a backward search, and the search was the cost.** It bounded one atom of `lower` at a time through the first hypothesis mentioning it, recursing into every part of that hypothesis's upper side, with a path guard against cycles and fuel against growth and nothing remembered between branches. `/std/Try`'s lift between two `Try`s assumes 27 constraints whose sides are maxima of up to seventeen parameters, and its costliest question — a bound the hypotheses state verbatim — took the search five seconds of dead ends to reach. Forward, the hypothesis stating it fires in the first pass. The trade is recorded in the crate's README.

#[cfg(test)]
mod tests;

use {
    super::LevelModel,
    curios_core::{Level, UniverseConstraint},
    std::collections::BTreeSet,
};

/// Whether `assumed` proves `lower ≤ upper` — the entailment a generic definition is checked under, where `assumed` is its own declared constraint set with the parameters held abstract.
///
/// Sound and deliberately incomplete, in the kernel's stated direction: a refusal is a visible disagreement, an over-eager acceptance is silent. Every step of the derivation is valid over the naturals — a left maximum is bounded exactly when each of its parts is, a hypothesis may be shifted up and never down, and a parameter carries its offset whatever it is assigned. What it cannot see is a consequence of the naturals being *totally* ordered: `u + 1 ≤ max(u, v)` forces `u + 1 ≤ v` in every instance, and no clause says so.
pub(crate) fn entails(assumed: &[UniverseConstraint], lower: &Level, upper: &Level) -> bool {
    curios_profile::profile!("entails");
    let mut model = LevelModel::of(upper);

    // The model `upper` states on its own is exactly `Level::structurally_leq`'s, so the fast path is the first answer rather than a second predicate beside this one.
    if model.bounds(lower) {
        return true;
    }
    // A head no hypothesis can raise and `upper` does not state is never reached, which answers most refusals before a pass is spent.
    let raisable = assumed
        .iter()
        .flat_map(|constraint| constraint.lower.atoms().map(|(head, _)| head))
        .collect::<BTreeSet<_>>();
    if lower
        .atoms()
        .any(|(head, _)| !model.reaches(head) && !raisable.contains(&head))
    {
        return false;
    }

    let ceiling = model.ceiling(assumed);
    loop {
        let raised = model.pass(assumed);

        if model.bounds(lower) {
            return true;
        }
        // A pass that raised nothing is the least model, and it does not bound `lower`. A value past the ceiling is climbing without end, which only a loop in the hypotheses can make it do; such a set has no instance in the naturals, the walk refuses it as unsatisfiable before assuming it, and refusing here is this crate's direction.
        if !raised || model.climbs_past(ceiling) {
            return false;
        }
    }
}
