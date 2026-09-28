//! Forward reasoning over a universe constraint set: the least model the certifier's two level decisions are read off.
//!
//! A constraint `L ≤ U` is a Horn clause: wherever every part of `U` is bounded, every part of `L` is, and so is each of its upward shifts, since the successor distributes over a maximum — `L + s ≤ U + s` for every `s ≥ 0`, and never for a negative one. [`LevelModel`] holds what firing those clauses has derived, and one pass fires every clause at the largest shift the model admits. The procedure is Bezem and Coquand's (*Loop-checking and the uniform word problem for join-semilattices with an inflationary endomorphism*, TCS 913, 2022): the least model above any starting point is computed by such passes (their Theorem 3.2), its finite values stay under a ceiling (their Corollary 4.2), and a value climbing past it is climbing without end, which only a loop — some `t + 1 ≤ t` — makes it do. A parameter ranging over the naturals adds one rule of its own, which the model's floor carries: `h + k` bounds the constant `k`, the zero being below every head.
//!
//! Two decisions read it. [`entails`](crate::entails) starts from the facts the upper level of its question states and asks whether the least model bounds the lower one (their Theorem 2.2). [`satisfiable`](crate::satisfiable) starts every head at the largest offset any upper side carries and asks whether the least model exists at all (their Corollary 3.5: exactly one of a loop and a model in the naturals).

use {
    curios_core::{Level, LevelHead, UniverseConstraint},
    std::collections::{BTreeMap, BTreeSet},
};

/// What forward reasoning has derived below some top: each head reached with the highest offset `k` for which `head + k` is bounded, and the highest constant that is.
///
/// A head absent from `reached` is one nothing has bounded yet, which is not the same as one bounded at offset zero — `head ≤ top` is a fact, and absence is the lack of one. `floor` is never below any reached offset, because a parameter carries its offset: `head + k` bounded gives `k` bounded.
pub(crate) struct LevelModel {
    reached: BTreeMap<LevelHead, u64>,
    floor: u64,
}

impl LevelModel {
    /// The facts `upper` states about itself, with `upper` as the top: each of its atoms, and its floor.
    pub(crate) fn of(upper: &Level) -> Self {
        Self {
            reached: upper
                .atoms()
                .map(|(head, offset)| (head, u64::from(offset)))
                .collect(),
            floor: u64::from(floor(upper)),
        }
    }

    /// Every head `constraints` mention, and the floor, at the largest offset any upper side carries — Bezem and Coquand's starting point for loop-checking, from which every clause is admitted at a shift of zero or more.
    pub(crate) fn throughout(constraints: &[UniverseConstraint]) -> Self {
        let start = constraints
            .iter()
            .map(|constraint| u64::from(floor(&constraint.upper)))
            .max()
            .unwrap_or(0);

        Self {
            reached: heads(constraints).map(|head| (head, start)).collect(),
            floor: start,
        }
    }

    /// Whether `head` has been bounded at any offset.
    pub(crate) fn reaches(&self, head: LevelHead) -> bool {
        self.reached.contains_key(&head)
    }

    /// The largest `s` for which `level + s` is bounded by what is derived — `None` when not even `level` is.
    ///
    /// A maximum is bounded exactly when every part is, so the shift is the least any part allows: the constant's distance to the floor, and each atom's distance to its head's reached offset. An unreached head allows none. The largest shift is the only one worth firing a clause at, since every smaller one derives facts the largest already implies.
    pub(crate) fn admits(&self, level: &Level) -> Option<u64> {
        let mut shift = self.floor.checked_sub(u64::from(level.constant_part()))?;
        for (head, offset) in level.atoms() {
            let reached = self.reached.get(&head)?;
            shift = shift.min(reached.checked_sub(u64::from(offset))?);
        }

        Some(shift)
    }

    /// Whether what is derived bounds `level` — [`LevelModel::admits`] at shift zero.
    pub(crate) fn bounds(&self, level: &Level) -> bool {
        self.admits(level).is_some()
    }

    /// One pass over `constraints`: every clause whose upper side the model admits fires at the largest shift admitted, raising its lower side. Whether anything rose — a pass that raised nothing has reached the least model.
    pub(crate) fn pass(&mut self, constraints: &[UniverseConstraint]) -> bool {
        let mut raised = false;
        for constraint in constraints {
            if let Some(shift) = self.admits(&constraint.upper) {
                raised |= self.raise(&constraint.lower, shift);
            }
        }

        raised
    }

    /// The height no value of the least model above this one reaches when `constraints` hold no loop: the highest value now, raised once per head by the most one clause can add — Bezem and Coquand's Corollary 4.2, with the zero as one more head. Past it, a value is climbing without end.
    ///
    /// What one clause can add is its *gain*: how far its lower side's floor stands above the least part of its upper side that admitted it.
    pub(crate) fn ceiling(&self, constraints: &[UniverseConstraint]) -> u64 {
        let heads = heads(constraints)
            .chain(self.reached.keys().copied())
            .collect::<BTreeSet<_>>();
        let gain = constraints
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

        self.floor
            .saturating_add((heads.len() as u64 + 1).saturating_mul(gain))
    }

    /// Whether some value has passed `ceiling`, the floor standing at or above every value.
    pub(crate) fn climbs_past(&self, ceiling: u64) -> bool {
        self.floor > ceiling
    }

    /// Record `level + shift` as bounded: every atom at its shifted offset, and the floor at the level's shifted floor. Whether anything rose.
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

/// Every head either side of `constraints` mentions.
fn heads(constraints: &[UniverseConstraint]) -> impl Iterator<Item = LevelHead> + '_ {
    constraints
        .iter()
        .flat_map(|constraint| constraint.lower.atoms().chain(constraint.upper.atoms()))
        .map(|(head, _)| head)
}

/// The value `level` cannot go below, whatever its parameters are assigned: a parameter ranges over the naturals, so an atom at offset `k` already carries `k`, and the level is at least the largest of those and its own constant.
///
/// This is the one fact both constant rules rest on, named once rather than spelled twice. [`Level::structurally_leq`] reads it on the *upper* side — a constant is dominated when the upper level's floor reaches it — and [`LevelModel::raise`] reads it on a clause's *lower* side, where it is what lets a premise carrying atoms bound a bare constant at all.
fn floor(level: &Level) -> u32 {
    level
        .atoms
        .values()
        .copied()
        .chain([level.constant])
        .max()
        .unwrap_or(level.constant)
}
