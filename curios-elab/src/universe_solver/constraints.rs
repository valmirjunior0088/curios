//! Indexed storage for the solver's live inequalities.
//!
//! Two invariants make the solver's read paths cheap, and both are the reason this storage is a type rather than a pair of fields.
//!
//! Every stored constraint is *already normalized* against the solutions committed so far. A reader never re-zonks; instead [`ConstraintStore`] rewrites the affected constraints when an assignment lands, which the occurrence index makes proportional to the assigned level's degree rather than to the whole store.
//!
//! Every rewrite records its pre-image, so a solver mark can restore the exact state it named. Truncating the trailing constraints is not enough once an assignment may edit an older one in place.
//!
//! That journal is recorded only while a speculative scope is open. A scope that succeeds releases, and once none is live the pre-images are unreachable — nothing can name a state before them — so they are neither kept nor, at depth zero, ever taken. Recording them unconditionally is what made a declaration's peak footprint superlinear: a substitution *widens* the constraints it lands in, so an unconditional journal keeps every intermediate width of every constraint it ever rewrote.
//!
//! Change detection therefore cannot read the journal's length, which no longer counts rewrites. An [`Entropy`] counts them instead — monotonically, and independently of whether a pre-image was stored — and its count is what a [`StoreMark`] compares. That is the same currency the cache stamp above this store already ticks, rather than a second bespoke counter beside it.

use {
    curios_base::Entropy,
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
///
/// `len` and `rewrites` are where a rollback unwinds to; `epoch` is what a reader compares to decide whether anything changed. The two are separate because the journal is now conditional and its length no longer moves on every rewrite.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct StoreMark {
    len: usize,
    rewrites: usize,
    epoch: usize,
}

/// Where a speculative scope began: the position to unwind to, and the depth to *restore* rather than decrement.
///
/// Restoring is what makes the depth self-healing. A bracket that returns early without closing leaves its scope open, and a decrementing counter would then stay above zero for the rest of the declaration, silently disarming the journal's one economy. Because closing an enclosing scope assigns the depth outright, any scope left open inside it is closed with it, and a declaration boundary resets whatever survives that.
///
/// Deliberately not part of [`StoreMark`]: a state token compares marks to decide whether the store changed, and entering a scope is not a change to it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct StoreScope {
    mark: StoreMark,
    depth: usize,
}

#[derive(Debug, Clone, Default)]
pub(super) struct ConstraintStore {
    constraints: Vec<UniverseConstraint>,
    occurrences: BTreeMap<LevelHead, BTreeSet<usize>>,
    rewrites: Vec<(usize, UniverseConstraint)>,
    /// Ticked once per rewrite actually applied, so a no-op substitution stays invisible to change detection exactly as it was when the journal's length carried that signal.
    epoch: Entropy,
    /// How many speculative scopes are open. Pre-images are recorded only above zero, and dropped when it returns there.
    speculation: usize,
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

    /// Snapshot the store's position without opening a scope — what a reader compares to decide whether anything moved.
    pub(super) fn mark(&self) -> StoreMark {
        StoreMark {
            len: self.constraints.len(),
            rewrites: self.rewrites.len(),
            epoch: self.epoch.count(),
        }
    }

    /// Open a speculative scope, recording both where the store stood and how deep it already was.
    pub(super) fn enter(&mut self) -> StoreScope {
        let scope = StoreScope {
            mark: self.mark(),
            depth: self.speculation,
        };
        self.speculation += 1;
        scope
    }

    /// End a scope, keeping whatever it left in place. Once nothing is open, the pre-images recorded under the closed scopes can no longer be named and are dropped.
    ///
    /// The depth may only ever be wrong in one direction. A bracket that returns without closing leaves it too *high*, so the journal is dropped later than it could be — wasted memory, and an enclosing close restores past it anyway. Forcing it low while a scope is still open is the unsafe direction: the drop below would take pre-images a pending rollback still needs, and that rollback would then silently restore nothing. Nothing resets this counter outright for exactly that reason.
    pub(super) fn release(&mut self, scope: StoreScope) {
        self.speculation = scope.depth;
        if self.speculation == 0 {
            self.rewrites.clear();
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

    /// Drop the most recently pushed constraint. Only valid when nothing has rewritten it since — which [`ConstraintStore::rollback`], its one caller, guarantees by unwinding the rewrite journal before truncating.
    pub(super) fn pop(&mut self) {
        if self.constraints.is_empty() {
            return;
        }
        let position = self.constraints.len() - 1;
        self.unindex(position);
        self.constraints.pop();
    }

    /// Restore the store to where `scope` began: undo every rewrite recorded since, newest first, then drop the constraints appended since.
    ///
    /// Undoing is not ending. A caller may roll back and keep working inside the same scope, so closing it is [`ConstraintStore::release`]'s job alone and the bracket that opened it is what calls that.
    pub(super) fn rollback(&mut self, scope: StoreScope) {
        while self.rewrites.len() > scope.mark.rewrites {
            let (position, previous) = self.rewrites.pop().expect("rewrite log is non-empty");
            self.unindex(position);
            self.constraints[position] = previous;
            self.index(position);
        }
        while self.constraints.len() > scope.mark.len {
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
            self.epoch.fresh();
            // Only a live scope can ever ask for this pre-image back. Outside one it is garbage the moment it is taken, and taking it at all is what a declaration's peak footprint was paying for.
            if self.speculation > 0 {
                self.rewrites.push((position, previous));
            }
            for atom in &arrived {
                self.occurrences.entry(*atom).or_default().insert(position);
            }
        }

        // `head` is solved, so nothing mentions it any more — including the constraints the substitution left alone, which by definition did not mention it.
        self.occurrences.remove(&head);
        Ok(())
    }
}
