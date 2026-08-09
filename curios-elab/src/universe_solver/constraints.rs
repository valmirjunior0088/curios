//! Indexed storage for the solver's live inequalities.
//!
//! Two invariants make the solver's read paths cheap, and both are the reason this storage is a type rather than a pair of fields.
//!
//! Every stored constraint is *already normalized* against the solutions committed so far. A reader never re-zonks; instead [`ConstraintStore`] rewrites the affected constraints when an assignment lands, which the occurrence index makes proportional to the assigned level's degree rather than to the whole store.
//!
//! Every rewrite records its pre-image, so a solver mark can restore the exact state it named. Truncating the trailing constraints is not enough once an assignment may edit an older one in place.

use {
    curios_core::{Level, LevelHead, UniverseConstraint},
    std::collections::{BTreeMap, BTreeSet},
};

fn heads(constraint: &UniverseConstraint) -> impl Iterator<Item = LevelHead> + '_ {
    constraint
        .lower
        .atoms()
        .chain(constraint.upper.atoms())
        .map(|(head, _)| head)
}

/// How much of the store's history a [`super::UniverseMark`] covers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct StoreMark {
    len: usize,
    rewrites: usize,
}

#[derive(Debug, Clone, Default)]
pub(super) struct ConstraintStore {
    constraints: Vec<UniverseConstraint>,
    occurrences: BTreeMap<LevelHead, BTreeSet<usize>>,
    rewrites: Vec<(usize, UniverseConstraint)>,
}

impl ConstraintStore {
    pub(super) fn len(&self) -> usize {
        self.constraints.len()
    }

    pub(super) fn as_slice(&self) -> &[UniverseConstraint] {
        &self.constraints
    }

    pub(super) fn iter(&self) -> impl ExactSizeIterator<Item = &UniverseConstraint> {
        self.constraints.iter()
    }

    pub(super) fn get(&self, index: usize) -> Option<&UniverseConstraint> {
        self.constraints.get(index)
    }

    pub(super) fn mark(&self) -> StoreMark {
        StoreMark {
            len: self.constraints.len(),
            rewrites: self.rewrites.len(),
        }
    }

    /// The constraints mentioning `head`, in insertion order.
    ///
    /// Solving reads a flexible level's bounds through this index. Scanning every constraint per level instead is what made finalization quadratic in the number of levels a declaration touches.
    pub(super) fn mentioning(&self, head: LevelHead) -> impl Iterator<Item = usize> + '_ {
        self.occurrences
            .get(&head)
            .into_iter()
            .flat_map(|indices| indices.iter().copied())
    }

    fn index(&mut self, position: usize) {
        for head in heads(&self.constraints[position]).collect::<Vec<_>>() {
            self.occurrences.entry(head).or_default().insert(position);
        }
    }

    fn unindex(&mut self, position: usize) {
        for head in heads(&self.constraints[position]).collect::<Vec<_>>() {
            if let Some(indices) = self.occurrences.get_mut(&head) {
                indices.remove(&position);
                if indices.is_empty() {
                    self.occurrences.remove(&head);
                }
            }
        }
    }

    pub(super) fn push(&mut self, constraint: UniverseConstraint) -> usize {
        let position = self.constraints.len();
        self.constraints.push(constraint);
        self.index(position);
        position
    }

    /// Drop the most recently pushed constraint. Only valid when nothing has rewritten it since, which is how the insertion path uses it: a constraint that fails its consistency check never survives to be substituted into.
    pub(super) fn pop(&mut self) {
        if self.constraints.is_empty() {
            return;
        }
        let position = self.constraints.len() - 1;
        self.unindex(position);
        self.constraints.pop();
    }

    /// Restore the store to `mark`: undo every rewrite recorded since, newest first, then drop the constraints appended since.
    pub(super) fn rollback(&mut self, mark: StoreMark) {
        while self.rewrites.len() > mark.rewrites {
            let (position, previous) = self.rewrites.pop().expect("rewrite log is non-empty");
            self.unindex(position);
            self.constraints[position] = previous;
            self.index(position);
        }
        while self.constraints.len() > mark.len {
            self.pop();
        }
    }

    pub(super) fn clear(&mut self) {
        self.constraints.clear();
        self.occurrences.clear();
        self.rewrites.clear();
    }

    /// Drop every constraint failing `keep` and rebuild the index.
    ///
    /// Retaining renumbers the survivors, so the rewrite journal — which names positions — cannot outlive it. Callers use this only at a declaration boundary, past any mark that could still be rolled back to.
    pub(super) fn retain(&mut self, keep: impl FnMut(&UniverseConstraint) -> bool) {
        self.constraints.retain(keep);
        self.occurrences.clear();
        self.rewrites.clear();
        for position in 0..self.constraints.len() {
            self.index(position);
        }
    }

    /// Replace `head` by `solution` in every constraint mentioning it, moving the index by the *delta* the substitution makes rather than rebuilding each rewritten constraint's whole entry.
    ///
    /// The delta is the same for every constraint touched — `head` leaves, `solution`'s heads arrive — so it is computed once. The previous form took an opaque rewriting closure, which hid exactly that fact, and so had to `unindex` then `index` each constraint: two BTree operations per head it *already* carried, against one removal plus one insertion per head the solution *adds*. That mattered because the carried width is the number that grows — a solution is a maximum, so every substitution widens the head sets of the constraints it lands in, and each later assignment then pays more.
    ///
    pub(super) fn substitute_head(
        &mut self,
        head: LevelHead,
        solution: &Level,
    ) -> Result<(), super::UniverseError> {
        curios_profile::profile!("universe::substitute");
        let positions = self.mentioning(head).collect::<Vec<_>>();
        let arrived = solution
            .atoms()
            .map(|(atom, _)| atom)
            .filter(|atom| *atom != head)
            .collect::<Vec<_>>();

        for position in positions {
            let rebuilt = {
                let constraint = &self.constraints[position];
                let lower = constraint
                    .lower
                    .substitute(|found| (found == head).then(|| solution.clone()))?;
                let upper = constraint
                    .upper
                    .substitute(|found| (found == head).then(|| solution.clone()))?;
                if lower == constraint.lower && upper == constraint.upper {
                    continue;
                }
                UniverseConstraint {
                    lower,
                    upper,
                    origin: constraint.origin.clone(),
                }
            };

            let previous = std::mem::replace(&mut self.constraints[position], rebuilt);
            self.rewrites.push((position, previous));
            for atom in &arrived {
                self.occurrences.entry(*atom).or_default().insert(position);
            }
        }

        // `head` is solved, so nothing mentions it any more — including the constraints the substitution left alone, which by definition did not mention it.
        self.occurrences.remove(&head);
        Ok(())
    }
}
