//! The shared fixpoint substrate: a lattice of facts keyed by value identity, and the solver that iterates a client's constraints to their least fixpoint.
//!
//! Every analysis in this crate answers the same shape of question — what is established about a value, given what is established about the values feeding it — and before this module each one would have carried its own lattice, its own iteration, and its own convergence test. The one that existed carried all three privately inside the specializer.
//!
//! Keys are [`CpsValueId`] alone, which covers more than it looks: a function's parameters and a continuation's parameters *are* values, so a single key space spans ordinary bindings, call arguments, and join points without a sum type over the three.
//!
//! **That keying is also the limit: a fact belongs to a value, never to a program point.** No client can state something that holds inside one [`super::CpsNode::Switch`] arm and not outside it, so refining a scrutinee's tag where an arm establishes it is not expressible here. It is a per-program-point extension the key space could grow into rather than a property anything currently relies on, and no client to date has needed it.
//!
//! **The solver has no direction of its own.** [`Solver::solve`] seeds the keys and re-runs a client-supplied round closure until a round establishes nothing new; which facts flow from which is stated entirely inside that closure. A backward analysis and a forward one therefore differ in the client and not here, and neither asks anything of this module that the other does not.
//!
//! **Absence is meaningful and is not `bottom`.** A key the solver was never seeded with has no fact at all, which a client may legitimately read differently from a key seeded and still sitting at `bottom` — the constant propagation in [`super::analysis`] does exactly that, treating an unseeded value as an unobservable runtime value rather than as one it has yet to learn about. [`Solver::facts`] therefore hands out the map so absence stays visible, rather than a total lookup that would quietly answer `bottom` for both.
//!
//! The names below join the flat `cps` namespace rather than being reached through this module, so they are spelled for that namespace: [`Solver::solve`] is the entry point rather than a bare `solve`, which would be far too general a name to sit beside `CpsModule`.

use {super::CpsValueId, std::collections::BTreeMap};

/// A lattice of facts, ordered by information content.
///
/// The three laws the solver depends on: [`Lattice::join`] is commutative, associative, and idempotent, and [`Lattice::bottom`] is its identity. Monotonicity is what makes the iteration in [`Solver::solve`] terminate at a least fixpoint rather than oscillate, so a `join` that can *lose* information — one that ever moves a fact back down the order — will not merely give a wrong answer, it may not converge.
pub(crate) trait Lattice: Clone + PartialEq {
    /// The identity of [`Lattice::join`] — "nothing established yet", the value every seeded key starts at.
    fn bottom() -> Self;

    /// Raise this fact to the least upper bound of itself and `incoming`.
    fn join(&mut self, incoming: Self);
}

/// The facts established so far, and whether the round in progress has changed any of them.
///
/// Handed to a client's round closure by [`Solver::solve`]. Reads see every join performed earlier in the same round, which is deliberate: a chaotic iteration reaches the same least fixpoint as a round-synchronised one but typically in fewer rounds, and the fixpoint is what the client's answer is defined as.
pub(crate) struct Solver<F> {
    facts: BTreeMap<CpsValueId, F>,
    changed: bool,
}

impl<F: Lattice> Solver<F> {
    /// The facts established so far. Absence means *unseeded*, never `bottom` — see the module documentation.
    pub(crate) fn facts(&self) -> &BTreeMap<CpsValueId, F> {
        &self.facts
    }

    /// Raise `id`'s fact to include `incoming`, recording whether that changed anything.
    ///
    /// A key absent from the seeding is inserted rather than rejected, so a client may discover targets as it iterates instead of enumerating them all up front.
    pub(crate) fn join(&mut self, id: CpsValueId, incoming: F) {
        let current = self.facts.entry(id).or_insert_with(F::bottom);
        let mut updated = current.clone();
        updated.join(incoming);
        if updated != *current {
            *current = updated;
            self.changed = true;
        }
    }

    /// Seed every key at `bottom`, then run `round` until it establishes nothing new.
    ///
    /// `round` states the client's whole constraint system: it reads facts through [`Solver::facts`] and raises them through [`Solver::join`], and is re-run in full each time anything moved. Termination rests on the lattice laws rather than on a round cap — a `join` obeying them can only move a finite lattice upward — so there is deliberately no iteration limit here to mask a lattice that does not.
    pub(crate) fn solve(
        keys: impl IntoIterator<Item = CpsValueId>,
        mut round: impl FnMut(&mut Self),
    ) -> BTreeMap<CpsValueId, F> {
        let mut solver = Self {
            facts: keys.into_iter().map(|key| (key, F::bottom())).collect(),
            changed: false,
        };

        loop {
            solver.changed = false;
            round(&mut solver);
            if !solver.changed {
                break solver.facts;
            }
        }
    }
}

#[cfg(test)]
mod tests;
