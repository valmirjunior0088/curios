//! The shared fixpoint substrate: a lattice of facts keyed by a client's ordered identities, and the solver that runs its rules to their least fixpoint.
//!
//! Every analysis in this crate answers the same shape of question — what is established about a value, given what is established about the values feeding it — and each one would otherwise carry its own lattice, its own iteration, and its own convergence test.
//!
//! Keys default to [`ValueId`]: a function's parameters and a continuation's parameters are values, so that space spans ordinary bindings, call arguments and join points. An analysis can choose a different ordered key when its facts also belong to identities that bind no value, such as function-return positions.
//!
//! Current clients attach facts to identities, never program points: none states something that holds inside one [`super::Node::Switch`] arm and not outside it. A branch-sensitive analysis would have to choose keys and rules that express that distinction.
//!
//! **The solver has no direction of its own.** A client states its constraint system as rules ([`Rules::add`]), each reading facts through [`Solver::fact`] and raising them through [`Solver::join`]; [`Solver::solve`] seeds the keys, runs every rule once, and runs a rule again only where a fact it read has moved, until none has. Which facts flow from which is stated entirely inside the rules, so a backward analysis and a forward one differ in the client and not here.
//!
//! **A rule runs again where what it read moved, and nowhere else.** Run whole until a pass changes nothing, a system carries a fact one link a pass along a chain whose links it visits in the other order, so a chain of n costs n passes over every rule — the square of a nested program. A rule's reads are observed as it runs rather than declared, as Seidl's top-down solver discovers its dependencies ([Seidl and Vogler](https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/three-improvements-to-the-topdown-solver/CE9A5F2A3FD62ECF9E6683598322CD52)), and a rule is queued again where a join moves a fact it read, as LLVM's sparse propagation queues an instruction whose lattice value changed ([`SparsePropagation`](https://llvm.googlesource.com/llvm/+/refs/heads/release_36/lib/Analysis/SparsePropagation.cpp)); so a rule runs once more for each move of each fact it reads, which a lattice of finite height bounds. Rejected: passes over every rule in the order a client's facts flow, which settle a chain in a few passes and cost a loop its nesting, and are quadratic again for a client whose order is wrong.
//!
//! **Absence is meaningful and is not `bottom`.** A key the solver was never seeded with has no fact at all, which a client may legitimately read differently from a key seeded and still sitting at `bottom` — the constant propagation in [`super::analysis`] does exactly that, treating an unseeded value as an unobservable runtime value rather than as one it has yet to learn about. [`Solver::fact`] therefore answers `None` for an unseeded key, rather than a total lookup that would quietly answer `bottom` for both.
//!
//! The names below join the crate's flat namespace rather than being reached through this module, so they are spelled for that namespace: [`Solver::solve`] is the entry point rather than a bare `solve`, which would be far too general a name to sit beside `Module`.

use {
    super::ValueId,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, VecDeque},
    },
};

/// A lattice of facts, ordered by information content.
///
/// The three laws the solver depends on: [`Lattice::join`] is commutative, associative, and idempotent, and [`Lattice::bottom`] is its identity. Monotonicity is what makes [`Solver::solve`] terminate at a least fixpoint rather than oscillate, so a `join` that can *lose* information — one that ever moves a fact back down the order — will not merely give a wrong answer, it may not converge.
pub(crate) trait Lattice: Clone + PartialEq {
    /// The identity of [`Lattice::join`] — "nothing established yet", the value every seeded key starts at.
    fn bottom() -> Self;

    /// Raise this fact to the least upper bound of itself and `incoming`.
    fn join(&mut self, incoming: Self);
}

/// A rule of a client's constraint system: what it reads through [`Solver::fact`] and raises through [`Solver::join`].
type Rule<'r, F, K> = Box<dyn Fn(&mut Solver<F, K>) + 'r>;

/// A client's constraint system, stated rule by rule for [`Solver::solve`].
pub(crate) struct Rules<'r, F, K = ValueId> {
    rules: Vec<Rule<'r, F, K>>,
}

impl<'r, F, K> Rules<'r, F, K> {
    /// A rule, run once and again wherever a fact it read moves.
    pub(crate) fn add(&mut self, rule: impl Fn(&mut Solver<F, K>) + 'r) {
        self.rules.push(Box::new(rule));
    }
}

/// The facts established so far, and the rules each was read by.
///
/// Handed to a rule by [`Solver::solve`]. Reads see every join performed before them, the rule's own among them, which is deliberate: the fixpoint is what the client's answer is defined as, and the order rules run in reaches the same one.
pub(crate) struct Solver<F, K = ValueId> {
    facts: BTreeMap<K, F>,
    /// The rules that read each key, to run again where it moves. Written through a shared reference, since a read is one.
    readers: RefCell<BTreeMap<K, BTreeSet<usize>>>,
    /// The rule running.
    running: usize,
    /// The rules to run again, each once however many of its reads moved.
    pending: VecDeque<usize>,
    queued: Vec<bool>,
}

impl<F: Lattice, K: Ord + Clone> Solver<F, K> {
    /// The fact `key` holds, remembered as read by the rule running: `None` where it was never seeded nor joined — absence, not `bottom`, see the module documentation.
    pub(crate) fn fact(&self, key: &K) -> Option<&F> {
        self.readers
            .borrow_mut()
            .entry(key.clone())
            .or_default()
            .insert(self.running);
        self.facts.get(key)
    }

    /// Raise `id`'s fact to include `incoming`, queueing again every rule that read it where that changed it.
    ///
    /// A key absent from the seeding is inserted rather than rejected, so a client may discover targets as it runs instead of enumerating them all up front.
    pub(crate) fn join(&mut self, id: K, incoming: F) {
        let current = self.facts.entry(id.clone()).or_insert_with(F::bottom);
        let mut updated = current.clone();
        updated.join(incoming);
        if updated == *current {
            return;
        }
        *current = updated;
        if let Some(readers) = self.readers.get_mut().get(&id) {
            for &rule in readers {
                if !self.queued[rule] {
                    self.queued[rule] = true;
                    self.pending.push_back(rule);
                }
            }
        }
    }

    /// Seed every key at `bottom`, state the rules, and run them until no fact a rule read moves.
    ///
    /// Termination rests on the lattice laws rather than on a cap — a `join` obeying them can only move a finite lattice upward, and a rule runs again only for a move — so there is deliberately no limit here to mask a lattice that does not.
    pub(crate) fn solve<'r>(
        keys: impl IntoIterator<Item = K>,
        state: impl FnOnce(&mut Rules<'r, F, K>),
    ) -> BTreeMap<K, F> {
        let mut rules = Rules { rules: Vec::new() };
        state(&mut rules);
        let mut solver = Self {
            facts: keys.into_iter().map(|key| (key, F::bottom())).collect(),
            readers: RefCell::new(BTreeMap::new()),
            running: 0,
            pending: (0..rules.rules.len()).collect(),
            queued: vec![true; rules.rules.len()],
        };

        while let Some(rule) = solver.pending.pop_front() {
            solver.queued[rule] = false;
            solver.running = rule;
            (rules.rules[rule])(&mut solver);
        }
        solver.facts
    }
}

#[cfg(test)]
mod tests;
