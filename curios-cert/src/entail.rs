//! The level entailment oracle: whether an assumed constraint set forces `lower <= upper`. Sound and deliberately incomplete — it derives what the hypotheses force by forward reasoning alone, the Horn-clause reading of the level algebra, and refuses what needs the naturals' total order on top. It is a rule that admits programs (levels the hypotheses force equal are equal in every instance satisfying them), which is why it lives in the certifier rather than beside the representation.
//!
//! # Forward, to a least model
//!
//! A hypothesis `L ≤ U` is a Horn clause: wherever every part of `U` is bounded, every part of `L` is. So is each of its upward shifts, since the successor distributes over a maximum — `L + s ≤ U + s` for every `s ≥ 0`, and never for a negative one. Whether `lower ≤ upper` follows is then whether every part of `lower` can be *derived* from the parts of `upper` by firing those clauses, which is Bezem and Coquand's characterisation of entailment for the semilattice with an inflationary successor (*Loop-checking and the uniform word problem for join-semilattices with an inflationary endomorphism*, TCS 913, 2022, Theorem 2.2); a parameter ranging over the naturals adds one rule of its own, that `h + k` bounds the constant `k`.
//!
//! The derivation is computed rather than searched for. [`Model`] holds, for each head it has reached, the highest offset derived below `upper`, and the highest constant; each hypothesis fires at the largest shift the model admits for its upper side, raising its lower side; passes repeat until one raises nothing — the least model above the one `upper` states (their Theorem 3.2), reached in a number of passes the values' bound limits (their Corollary 4.2). The answer is whether that model bounds `lower`.
//!
//! **This replaced a backward search, and the search was the cost.** It bounded one atom of `lower` at a time through the first hypothesis mentioning it, recursing into every part of that hypothesis's upper side, with a path guard against cycles and fuel against growth and nothing remembered between branches. `/std/Try`'s lift between two `Try`s assumes 27 constraints whose sides are maxima of up to seventeen parameters, and its costliest question — a bound the hypotheses state verbatim — took the search five seconds of dead ends to reach. Forward, the hypothesis stating it fires in the first pass. The trade is recorded in the crate's README.

#[cfg(test)]
mod tests;

use {
    curios_core::{Level, LevelHead, UniverseConstraint},
    std::collections::{BTreeMap, BTreeSet},
};

/// Whether `assumed` proves `lower ≤ upper` — the entailment a generic definition is checked under, where `assumed` is its own declared constraint set with the parameters held abstract.
///
/// Sound and deliberately incomplete, in the kernel's stated direction: a refusal is a visible disagreement, an over-eager acceptance is silent. Every step of the derivation is valid over the naturals — a left maximum is bounded exactly when each of its parts is, a hypothesis may be shifted up and never down, and a parameter carries its offset whatever it is assigned. What it cannot see is a consequence of the naturals being *totally* ordered: `u + 1 ≤ max(u, v)` forces `u + 1 ≤ v` in every instance, and no clause says so.
pub(crate) fn entails(assumed: &[UniverseConstraint], lower: &Level, upper: &Level) -> bool {
    curios_profile::profile!("entails");
    let mut model = Model::of(upper);

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
        .any(|(head, _)| !model.reached.contains_key(&head) && !raisable.contains(&head))
    {
        return false;
    }

    let bound = model_bound(assumed, lower, upper);
    loop {
        let raised = model.pass(assumed);

        if model.bounds(lower) {
            return true;
        }
        // A pass that raised nothing is the least model, and it does not bound `lower`. A value past the bound is climbing without end, which only a loop in the hypotheses — some `t + 1 ≤ t` — can make it do; such a set has no instance in the naturals, the walk refuses it as `universe_verdict`, and refusing here is this crate's direction.
        if !raised || model.floor > bound {
            return false;
        }
    }
}

/// What forward reasoning from `upper` has derived: each head reached with the highest offset `k` for which `head + k ≤ upper` follows, and the highest constant that does.
///
/// A head absent from `reached` is one nothing has bounded yet, which is not the same as one bounded at offset zero — `head ≤ upper` is a fact, and absence is the lack of one. `floor` is never below any reached offset, because a parameter carries its offset: `head + k ≤ upper` gives `k ≤ upper`.
struct Model {
    reached: BTreeMap<LevelHead, u64>,
    floor: u64,
}

impl Model {
    /// The facts `upper` states about itself: each of its atoms, and its floor.
    fn of(upper: &Level) -> Self {
        Self {
            reached: upper
                .atoms()
                .map(|(head, offset)| (head, u64::from(offset)))
                .collect(),
            floor: u64::from(floor(upper)),
        }
    }

    /// The largest `s` for which `level + s ≤ upper` follows from what is derived — `None` when not even `level ≤ upper` does.
    ///
    /// A maximum is bounded exactly when every part is, so the shift is the least any part allows: the constant's distance to the floor, and each atom's distance to its head's reached offset. An unreached head allows none. The largest shift is the only one worth firing a hypothesis at, since every smaller one derives facts the largest already implies.
    fn admits(&self, level: &Level) -> Option<u64> {
        let mut shift = self.floor.checked_sub(u64::from(level.constant_part()))?;
        for (head, offset) in level.atoms() {
            let reached = self.reached.get(&head)?;
            shift = shift.min(reached.checked_sub(u64::from(offset))?);
        }

        Some(shift)
    }

    /// Whether what is derived bounds `level` — the goal test, and [`Model::admits`] at shift zero.
    fn bounds(&self, level: &Level) -> bool {
        self.admits(level).is_some()
    }

    /// One pass over `assumed`: every hypothesis whose upper side the model admits fires at the largest shift admitted, raising its lower side. Whether anything rose — a pass that raised nothing has reached the least model.
    fn pass(&mut self, assumed: &[UniverseConstraint]) -> bool {
        let mut raised = false;
        for constraint in assumed {
            if let Some(shift) = self.admits(&constraint.upper) {
                raised |= self.raise(&constraint.lower, shift);
            }
        }

        raised
    }

    /// Record `level + shift ≤ upper`: every atom at its shifted offset, and the floor at the level's shifted floor. Whether anything rose.
    fn raise(&mut self, level: &Level, shift: u64) -> bool {
        let mut raised = false;
        for (head, offset) in level.atoms() {
            let value = u64::from(offset).saturating_add(shift);
            match self.reached.get_mut(&head) {
                Some(reached) if *reached >= value => {}
                Some(reached) => {
                    *reached = value;
                    raised = true;
                }
                None => {
                    self.reached.insert(head, value);
                    raised = true;
                }
            }
        }

        let floor = u64::from(floor(level)).saturating_add(shift);
        if floor > self.floor {
            self.floor = floor;
            raised = true;
        }

        raised
    }
}

/// How high a value of the least model can climb when the hypotheses hold no loop: the highest value `upper` states, raised once per head by the most one hypothesis can add (Bezem and Coquand's Corollary 4.2, with the constant as one more head). Past it, a value is climbing without end.
///
/// What one hypothesis can add is its *gain*: how far its lower side's floor stands above the least part of its upper side that admitted it.
fn model_bound(assumed: &[UniverseConstraint], lower: &Level, upper: &Level) -> u64 {
    let heads = assumed
        .iter()
        .flat_map(|constraint| [&constraint.lower, &constraint.upper])
        .chain([lower, upper])
        .flat_map(|level| level.atoms().map(|(head, _)| head))
        .collect::<BTreeSet<_>>();
    let gain = assumed
        .iter()
        .map(|constraint| {
            let least = constraint
                .upper
                .atoms()
                .map(|(_, offset)| offset)
                .min()
                .unwrap_or(constraint.upper.constant_part());
            u64::from(floor(&constraint.lower).saturating_sub(least))
        })
        .max()
        .unwrap_or(0);

    u64::from(floor(upper)).saturating_add((heads.len() as u64 + 1).saturating_mul(gain))
}

/// The value `level` cannot go below, whatever its parameters are assigned: a parameter ranges over the naturals, so an atom at offset `k` already carries `k`, and the level is at least the largest of those and its own constant.
///
/// This is the one fact both constant rules rest on, named once rather than spelled twice. [`Level::structurally_leq`] reads it on the *upper* side — a constant is dominated when the upper level's floor reaches it — and [`Model::raise`] reads it on a hypothesis's *lower* side, where it is what lets a premise carrying atoms bound a bare constant at all.
fn floor(level: &Level) -> u32 {
    level
        .atoms
        .values()
        .copied()
        .chain([level.constant])
        .max()
        .unwrap_or(level.constant)
}
