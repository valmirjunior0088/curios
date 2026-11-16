//! The shared fixpoint substrate: a lattice of facts keyed by a client's ordered identities, and the solver that runs its rules to their least fixpoint.
//!
//! Every analysis in this crate answers the same shape of question — what is established about a value, given what is established about the values feeding it — and each one would otherwise carry its own lattice, its own iteration, and its own convergence test.
//!
//! Keys default to [`ValueId`]: a function's parameters and a continuation's parameters are values, so that space spans ordinary bindings, call arguments and join points. An analysis can choose a different ordered key when its facts also belong to identities that bind no value, such as function-return positions.
//!
//! Current clients attach facts to identities, never program points: none states something that holds inside one [`super::Node::Switch`] arm and not outside it. A branch-sensitive analysis would have to choose keys and rules that express that distinction.
//!
//! **The solver has no direction of its own.** A client states its constraint system as rules ([`Rules::add`]), each reading facts through [`Solver::fact`] and raising them through [`Solver::join`], and stating copies from one key to another through [`Solver::copy`]; a watch ([`Rules::watch`]) is a rule handed what one key gains. [`Solver::solve`] seeds the keys, runs every rule once, and runs a rule again only where a fact it read has moved, hands on what each copied or watched key gains, and stops where nothing moves. Which facts flow from which is stated entirely inside the rules, so a backward analysis and a forward one differ in the client and not here.
//!
//! **A rule runs again where what it read moved, and nowhere else.** Run whole until a pass changes nothing, a system carries a fact one link a pass along a chain whose links it visits in the other order, so a chain of n costs n passes over every rule — the square of a nested program. A rule's reads are observed as it runs rather than declared, as Seidl's top-down solver discovers its dependencies ([Seidl and Vogler](https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/three-improvements-to-the-topdown-solver/CE9A5F2A3FD62ECF9E6683598322CD52)), and a rule is queued again where a join moves a fact it read, as LLVM's sparse propagation queues an instruction whose lattice value changed ([`SparsePropagation`](https://llvm.googlesource.com/llvm/+/refs/heads/release_36/lib/Analysis/SparsePropagation.cpp)); so a rule runs once more for each move of each fact it reads, which a lattice of finite height bounds. Rejected: passes over every rule in the order a client's facts flow, which settle a chain in a few passes and cost a loop its nesting, and are quadratic again for a client whose order is wrong.
//!
//! **A copy hands on what its source gained.** A rule reads a fact whole, so run again for a move of a set it copies and merges the whole set to hand on the members it gained, and a system over sets thousands wide costs its runs times their width. A copy and a watch each read one key, declared where it is stated, and are handed what that key gained since it was last handed on — a set's new members, any other fact the one it became ([`Lattice::gain`]) — so a member crosses each copy once, whatever order the solve takes. This is the difference propagation of inclusion-based pointer analysis ([Pearce, Kelly and Hankin](https://whileydave.com/publications/PKH03_SCAM_preprint.pdf)), a copy its subset constraint and a watch its complex one: a watch states copies for what its key gained, as a load or a call through a pointer adds the edges its new targets ask. The solver runs the rules it has queued before it hands anything on, so a copy a rule states before the solve moves finds its source at bottom and its source's members cross it a gain at a time. Rejected: a rule handed what each fact it read gained since it last ran, which leaves a rule over two facts to state by hand what the gains of each meet of the other, so a pair it misses is a flow missed; and a solver beside this one for one client's sets, a second iteration in a crate whose analyses share one.
//!
//! **Absence is meaningful and is not `bottom`.** A key the solver was never seeded with has no fact at all, which a client may legitimately read differently from a key seeded and still sitting at `bottom` — the constant propagation in [`super::analysis`] does exactly that, treating an unseeded value as an unobservable runtime value rather than as one it has yet to learn about. [`Solver::fact`] therefore answers `None` for an unseeded key, rather than a total lookup that would quietly answer `bottom` for both.
//!
//! The names below join the crate's flat namespace rather than being reached through this module, so they are spelled for that namespace: [`Solver::solve`] is the entry point rather than a bare `solve`, which would be far too general a name to sit beside `Module`.

use {
    super::ValueId,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashSet, VecDeque},
    },
};

/// A lattice of facts, ordered by information content.
///
/// The three laws the solver depends on: [`Lattice::join`] is commutative, associative, and idempotent, and [`Lattice::bottom`] is its identity. Monotonicity is what makes [`Solver::solve`] terminate at a least fixpoint rather than oscillate, so a `join` that can *lose* information — one that ever moves a fact back down the order — will not merely give a wrong answer, it may not converge.
pub trait Lattice: Clone + PartialEq {
    /// The identity of [`Lattice::join`] — "nothing established yet", the value every seeded key starts at.
    fn bottom() -> Self;

    /// Raise this fact to the least upper bound of itself and `incoming`.
    fn join(&mut self, incoming: Self);

    /// [`Lattice::join`] with `incoming`, answering what this fact gained: `None` where it did not move, and otherwise a fact no greater than the one it became which, joined into the one it was, makes the one it became. What a copy hands on and a watch is handed: any fact may answer the one it became, and a set answers the members it gained.
    fn gain(&mut self, incoming: &Self) -> Option<Self> {
        let mut raised = self.clone();
        raised.join(incoming.clone());
        if raised == *self {
            return None;
        }
        *self = raised.clone();

        Some(raised)
    }
}

/// A rule of a client's constraint system: what it reads through [`Solver::fact`] and raises through [`Solver::join`].
type Rule<'r, F, K> = Box<dyn Fn(&mut Solver<F, K>) + 'r>;

/// A rule handed what one key gained.
type Watch<'r, F, K> = Box<dyn Fn(&mut Solver<F, K>, &F) + 'r>;

/// A client's constraint system, stated rule by rule for [`Solver::solve`].
pub struct Rules<'r, F, K = ValueId> {
    rules: Vec<Rule<'r, F, K>>,
    watches: Vec<(K, Watch<'r, F, K>)>,
}

impl<'r, F, K> Rules<'r, F, K> {
    /// A rule, run once and again wherever a fact it read moves.
    pub fn add(&mut self, rule: impl Fn(&mut Solver<F, K>) + 'r) {
        self.rules.push(Box::new(rule));
    }

    /// A rule handed each gain of `key` ([`Lattice::gain`]), so it is handed all `key` ever holds a part at a time, and run on nothing else. What it states of a fact it must state of the gains that make it up, as a rule over a set states what each member asks; a fact it reads through [`Solver::fact`] does not run it again.
    pub fn watch(&mut self, key: K, rule: impl Fn(&mut Solver<F, K>, &F) + 'r) {
        self.watches.push((key, Box::new(rule)));
    }
}

/// The facts established so far, the rules each was read by, and the copies and watches each is handed on to.
///
/// Handed to a rule by [`Solver::solve`]. Reads see every join performed before them, the rule's own among them, which is deliberate: the fixpoint is what the client's answer is defined as, and the order rules run in reaches the same one.
pub struct Solver<F, K = ValueId> {
    /// Each key's place in the vectors below. Written through a shared reference, since a read places the key it reads.
    places: RefCell<BTreeMap<K, usize>>,
    /// What each place holds, `None` where nothing seeded or joined it. Shorter than `places` where a read placed a key nothing has written since.
    facts: Vec<Option<F>>,
    /// The rules that read each place, to run again where it moves. Written through a shared reference, since a read is one.
    readers: RefCell<Vec<BTreeSet<usize>>>,
    /// The rule running, `None` while a watch is.
    running: Option<usize>,
    /// The rules to run again, each once however many of its reads moved.
    pending: VecDeque<usize>,
    queued: Vec<bool>,
    /// The places each place is copied to, each pair once.
    copies: Vec<Vec<usize>>,
    copied: HashSet<(usize, usize)>,
    /// The watches each place is handed to.
    watchers: Vec<Vec<usize>>,
    /// What each copied or watched place gained since it was last handed on; a place is queued in `handing` while it holds one.
    gained: Vec<Option<F>>,
    handing: VecDeque<usize>,
}

impl<F: Lattice, K: Ord + Clone> Solver<F, K> {
    /// The fact `key` holds, remembered as read by the rule running: `None` where it was never seeded nor joined — absence, not `bottom`, see the module documentation.
    pub fn fact(&self, key: &K) -> Option<&F> {
        let place = self.place(key);
        if let Some(rule) = self.running {
            self.readers.borrow_mut()[place].insert(rule);
        }
        self.facts.get(place).and_then(Option::as_ref)
    }

    /// Raise `id`'s fact to include `incoming`, queueing again every rule that read it where that changed it, and handing on what it gained to what copies or watches it.
    ///
    /// A key absent from the seeding is inserted rather than rejected, so a client may discover targets as it runs instead of enumerating them all up front.
    pub fn join(&mut self, id: K, incoming: F) {
        let place = self.place_written(&id);
        self.raise(place, &incoming);
    }

    /// Raise `to` to hold what `from` holds, now and at each gain of `from` from here on. `to` is inserted where absent, as a join inserts it, and a copy stated again is the one already standing.
    pub fn copy(&mut self, from: K, to: K) {
        let from = self.place_written(&from);
        let to = self.place_written(&to);
        self.facts[to].get_or_insert_with(F::bottom);
        if from == to || !self.copied.insert((from, to)) {
            return;
        }
        self.copies[from].push(to);
        if let Some(held) = self.facts[from].clone() {
            self.raise(to, &held);
        }
    }

    /// The place of `key`, made where it has none.
    fn place(&self, key: &K) -> usize {
        let found = self.places.borrow().get(key).copied();
        if let Some(place) = found {
            return place;
        }
        let mut places = self.places.borrow_mut();
        let place = places.len();
        places.insert(key.clone(), place);
        self.readers.borrow_mut().push(BTreeSet::new());
        place
    }

    /// [`Solver::place`], with room for what a write keeps there.
    fn place_written(&mut self, key: &K) -> usize {
        let place = self.place(key);
        let count = self.places.get_mut().len();
        if self.facts.len() < count {
            self.facts.resize_with(count, || None);
            self.copies.resize_with(count, Vec::new);
            self.watchers.resize_with(count, Vec::new);
            self.gained.resize_with(count, || None);
        }
        place
    }

    fn raise(&mut self, place: usize, incoming: &F) {
        let fact = self.facts[place].get_or_insert_with(F::bottom);
        let Some(gain) = fact.gain(incoming) else {
            return;
        };
        for &rule in &self.readers.get_mut()[place] {
            if !self.queued[rule] {
                self.queued[rule] = true;
                self.pending.push_back(rule);
            }
        }
        if self.copies[place].is_empty() && self.watchers[place].is_empty() {
            return;
        }
        match &mut self.gained[place] {
            Some(held) => held.join(gain),
            unheld @ None => {
                *unheld = Some(gain);
                self.handing.push_back(place);
            }
        }
    }

    /// Seed every key at `bottom`, state the rules, and run them until no fact a rule read moves and nothing copied or watched has a gain left to hand on.
    ///
    /// Termination rests on the lattice laws rather than on a cap — a `join` obeying them can only move a finite lattice upward, a rule runs again only for a move, and a gain is handed on only where one was made — so there is deliberately no limit here to mask a lattice that does not.
    pub fn solve<'r>(
        keys: impl IntoIterator<Item = K>,
        state: impl FnOnce(&mut Rules<'r, F, K>),
    ) -> BTreeMap<K, F> {
        let mut rules = Rules {
            rules: Vec::new(),
            watches: Vec::new(),
        };
        state(&mut rules);
        let mut solver = Self {
            places: RefCell::new(BTreeMap::new()),
            facts: Vec::new(),
            readers: RefCell::new(Vec::new()),
            running: None,
            pending: (0..rules.rules.len()).collect(),
            queued: vec![true; rules.rules.len()],
            copies: Vec::new(),
            copied: HashSet::new(),
            watchers: Vec::new(),
            gained: Vec::new(),
            handing: VecDeque::new(),
        };
        for key in keys {
            let place = solver.place_written(&key);
            solver.facts[place] = Some(F::bottom());
        }
        for (watch, (key, _)) in rules.watches.iter().enumerate() {
            let place = solver.place_written(key);
            solver.watchers[place].push(watch);
        }

        loop {
            if let Some(rule) = solver.pending.pop_front() {
                solver.queued[rule] = false;
                solver.running = Some(rule);
                (rules.rules[rule])(&mut solver);
            } else if let Some(place) = solver.handing.pop_front() {
                let gain = solver.gained[place]
                    .take()
                    .expect("a place is queued while it holds a gain");
                solver.running = None;
                for copy in 0..solver.copies[place].len() {
                    let to = solver.copies[place][copy];
                    solver.raise(to, &gain);
                }
                for watcher in 0..solver.watchers[place].len() {
                    let watch = solver.watchers[place][watcher];
                    (rules.watches[watch].1)(&mut solver, &gain);
                }
            } else {
                break;
            }
        }

        let mut facts = solver.facts;
        solver
            .places
            .into_inner()
            .into_iter()
            .filter_map(|(key, place)| {
                facts
                    .get_mut(place)
                    .and_then(Option::take)
                    .map(|fact| (key, fact))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests;
