//! The evaluation memos: what a term weak-head reduces to, the sort a type inhabits, and the type inferred for a term — a reduct and a sort for the rest of the declaration where the term is local-free and otherwise for as long as what it read of the scope stands, an inferred type for a local-free term outside every arm.
//!
//! This is not the kind of store the [`kernel`](super) module documentation warns about. A metavariable heap or a refinement layer *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. Refusing even that multiplies the cost of a whole-prelude re-check, spent re-deriving the same prelude spines for every recursive group the totality gate walks; `curios-prelude-archive`'s `kernel_memo_parity` runs that walk both ways.
//!
//! Five invariants make it an evaluation strategy rather than a second source of truth, and all five live here.
//!
//! **Keys are valid by construction.** [`Memos::invalidate`] is called whenever a name is *overwritten*, so validity never rests on an append-only assumption. A whnf entry for a *local-free* term — every free variable a definition name — has a scope-independent key: whnf reads no local (locals carry no values by design) and no case equation, so the reduct is a function of the definition store alone and no key can dangle a retracted binder. A whnf entry for a *local-bearing* term is a function of one thing more, the case equations in force, and its key cannot say which were; so those entries live in tables of their own that [`Memos::begin_equations`] clears wherever that set changes — an equation assumed, a bracket retracting one, a settlement withholding and restoring them — and within one such span the reduct is as fixed as a closed term's is within a declaration. The binders a key names are minted once and never recur, so a stale entry is unreachable rather than wrong. Which table a term belongs to is decided here, on both the store and the lookup, rather than at the call sites that would have to remember it.
//!
//! **A judgment that reads the scope lives as long as what it read.** A sort rests on more of the scope than a reduct does: on the equations in force, which decide what a type reduces to, and on the type each local stands at, since a neutral's sort is read off its binder. So the scoped sorts are cleared where an equation moves, as the local-bearing reducts are, and where an arm re-assumes a local at its specialized type; and each is filed with the binders in scope when it was read — a [`Prefix`] — and answers only while they stand, because a stale sort is *wrong* where a stale reduct is merely unreachable: a name handed in from outside can be assumed again, at another type, once its first binder is closed. A local-free type's sort is the declaration's inside an arm as outside one, for the reason its reduct is: classifying it reduces local-free terms and terms naming only the binders the classification itself opens, and no case equation is about either — an equation's subject names a local of the walk, and reduction never introduces one.
//!
//! **They are consulted at every reduction level, not only at the crate's boundary.** `whnf_within` probes on entry and stores on exit, so a scrutinee, an application's head, each turn of `force`'s loop and every other internal call is served by the same table. The elaborator's reducer does the same; probing only at the two [`Reducer`](curios_core::Reducer) methods would leave every internal call re-deriving what the table already holds. Reaching every level is safe because the admission test is one test: [`Memos::whnf`] gates the lookup exactly as [`Memos::store_whnf`] gates the store. A sort is probed the same way, at every field and domain [`Sort::of`] classifies, which is what makes a type's sort cost its graph.
//!
//! **Every table lives exactly as long as a budget does.** [`Memos::begin_declaration`] clears them wherever `Spend::restore_budget` fires, and that alignment is what lets a hit on them be free. The paragraph above argues an entry's *content* is semantically determined — "the reduct is a function of the definition store alone" — but its *presence* is historical in any table that outlives the budget: a free hit on one would make what one declaration spends depend on which declarations were walked before it, which is the property the per-declaration budget exists to hold. Clearing removes the dependence rather than asking anyone to trust a cache policy, and it measured free on every probe. No table here outlives a declaration: a name-keyed table of definition unfolds that did, charging on a hit what its first computation spent, would count that computation's own free hits, so the declaration that unfolded a name first would decide what every later one is charged for it — and the closed machine reads a body directly, so it needs none. `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md` states the rule for both checkers.
//!
//! **A store cannot charge.** [`Memos`] hands back a [`Replay`] and nothing else; applying one to the entropy counter is [`Spend::charge_nothing`](super::Spend::charge_nothing)'s job. A component that could both remember and charge is one that could charge twice. The tables of sorts and of inferred types hand back the answer alone, because a hit on either has nothing to apply: it spends nothing and mints nothing, for the reason [`Kernel::infer_hit`](super::Kernel::infer_hit) gives.

use {
    super::{Prefix, Replay, Sort},
    curios_analysis::Erased,
    curios_core::{Bound, Term},
    std::collections::HashMap,
};

/// One answer per type, under the two lives an answer read off a type's sort has.
struct Lives<V> {
    /// The answers for local-free types, which are a function of the declaration alone.
    declaration: HashMap<Term, V>,
    /// The answers read off the scope, for types naming a local, each beside the binders it was read under.
    scope: HashMap<Term, (Prefix, V)>,
}

impl<V: Clone> Lives<V> {
    fn new() -> Self {
        Self {
            declaration: HashMap::new(),
            scope: HashMap::new(),
        }
    }

    /// The remembered answer for `type_`, with the binders it was read under where it was read off the scope, or `None` for a type with a loose index, which no table keeps.
    fn get(&self, type_: &Term) -> Option<(Option<Prefix>, V)> {
        match (type_.reach() == 0, type_.has_local_free()) {
            (false, _) => None,
            (true, false) => self
                .declaration
                .get(type_)
                .map(|value| (None, value.clone())),
            (true, true) => self
                .scope
                .get(type_)
                .map(|(prefix, value)| (Some(*prefix), value.clone())),
        }
    }

    /// Remember the answer for `type_`, in the table [`Lives::get`] reads it from, read under the binders `prefix` holds.
    fn insert(&mut self, type_: Term, prefix: Prefix, value: V) {
        match (type_.reach() == 0, type_.has_local_free()) {
            (false, _) => {}
            (true, false) => {
                self.declaration.insert(type_, value);
            }
            (true, true) => {
                self.scope.insert(type_, (prefix, value));
            }
        }
    }

    fn begin_declaration(&mut self) {
        self.declaration.clear();
        self.scope.clear();
    }

    fn begin_equations(&mut self) {
        self.scope.clear();
    }
}

pub(super) struct Memos {
    /// Whether the memos are consulted at all. On by default; `Kernel::uncached` exists so a test can assert that switching them off changes no *semantic* verdict — the property that makes them an evaluation strategy rather than a store.
    enabled: bool,
    /// Weak-head reducts of local-free terms, per entry point: plain, and rec-forced. Free on a hit, and cleared at every declaration boundary — see the module documentation for why those two go together, and for why `whnf` reaches this at every level rather than only at the crate's edge.
    whnf: HashMap<Term, Replay>,
    forced: HashMap<Term, Replay>,
    /// The same two tables for *local-bearing* terms, whose reducts are a function of the definition store **and the case equations in force**. The key cannot carry the second, so the tables live only as long as that set does: [`Memos::begin_equations`] clears them wherever an equation is assumed, retracted, withheld or restored, and [`Memos::begin_declaration`] with the rest. Within one such span a term's reduct is as fixed as a closed term's is within a declaration, and the web of definitions the index inversion forces at `Eq()(top(n), 0)` — each naming the one before it twice, a local in every one — would be re-derived `2^n` times without exactly this.
    local: HashMap<Term, Replay>,
    local_forced: HashMap<Term, Replay>,
    /// The types inferred for local-free terms, for the declaration in progress: `infer`'s own answer, remembered as the reducts are. A reduct is a graph whose tree can be exponential in its depth, and typing walks what it meets; remembered by term, a subterm shared across that tree is typed once. Free on a hit and cleared with the whnf tables, and for their reason. A type alone is kept, not a [`Replay`], because a hit replays nothing — see [`Kernel::infer_hit`](super::Kernel::infer_hit) for why, and for the equations it may not outlive.
    types: HashMap<Term, Term>,
    /// The sort of each type classified: [`Sort::of`]'s own answer. A type is a graph — a record of two fields at one type holds one node twice — and its sort asks each field's, so remembered by type a shared field is classified once where recomputing classifies it once per path. Free on a hit; a sort alone is kept, as a type alone is.
    sorts: Lives<Sort>,
    /// Which erased half a position at each type belongs to — a type, a proof, or neither — which is its sort read once more: what `Kernel::record_checked` asks at every position it records, so that the question costs one answer per type and not one per position.
    halves: Lives<Option<Erased>>,
}

impl Memos {
    pub(super) fn new(enabled: bool) -> Self {
        Self {
            enabled,
            whnf: HashMap::new(),
            forced: HashMap::new(),
            local: HashMap::new(),
            local_forced: HashMap::new(),
            types: HashMap::new(),
            sorts: Lives::new(),
            halves: Lives::new(),
        }
    }

    /// The remembered weak-head reduct of `term` at the given entry point, still to be applied — its identities minted, and its steps deliberately not spent.
    pub(super) fn whnf(&self, term: &Term, forced: bool) -> Option<Replay> {
        if !self.enabled {
            return None;
        }

        // Dispatched by whether the term mentions a local, on the lookup as on the store, so a closed term is never answered from the scoped tables nor a local-bearing one from the tables that outlive a scope.
        match (term.has_local_free(), forced) {
            (false, false) => self.whnf.get(term).cloned(),
            (false, true) => self.forced.get(term).cloned(),
            (true, false) => self.local.get(term).cloned(),
            (true, true) => self.local_forced.get(term).cloned(),
        }
    }

    /// Remember `term`'s weak-head reduct at the given entry point, and its consumption.
    pub(super) fn store_whnf(&mut self, term: Term, forced: bool, replay: Replay) {
        if !self.enabled {
            return;
        }

        match (term.has_local_free(), forced) {
            (false, false) => self.whnf.insert(term, replay),
            (false, true) => self.forced.insert(term, replay),
            (true, false) => self.local.insert(term, replay),
            (true, true) => self.local_forced.insert(term, replay),
        };
    }

    /// The remembered type of a local-free `term`.
    pub(super) fn infer(&self, term: &Term) -> Option<Term> {
        if !self.enabled || !Self::typeable_alone(term) {
            return None;
        }

        self.types.get(term).cloned()
    }

    /// Remember a local-free `term`'s type.
    pub(super) fn store_infer(&mut self, term: Term, type_: Term) {
        if self.enabled && Self::typeable_alone(&term) {
            self.types.insert(term, type_);
        }
    }

    /// The remembered sort of `type_`, with the binders it was read under where it was read off the scope, which the caller asks the scope about before taking the answer.
    pub(super) fn sort(&self, type_: &Term) -> Option<(Option<Prefix>, Sort)> {
        self.sorts.get(type_).filter(|_| self.enabled)
    }

    /// Remember `type_`'s sort, read under the binders `prefix` holds.
    pub(super) fn store_sort(&mut self, type_: Term, prefix: Prefix, sort: Sort) {
        if self.enabled {
            self.sorts.insert(type_, prefix, sort);
        }
    }

    /// The remembered erased half of a position at `type_`, as [`Memos::sort`] hands back a sort.
    pub(super) fn half(&self, type_: &Term) -> Option<(Option<Prefix>, Option<Erased>)> {
        self.halves.get(type_).filter(|_| self.enabled)
    }

    /// Remember the erased half of a position at `type_`, read under the binders `prefix` holds.
    pub(super) fn store_half(&mut self, type_: Term, prefix: Prefix, half: Option<Erased>) {
        if self.enabled {
            self.halves.insert(type_, prefix, half);
        }
    }

    /// Whether a term's type is a function of the term and the declaration alone: no local it would read a type off, and no loose index a binder outside it would give meaning to.
    fn typeable_alone(term: &Term) -> bool {
        !term.has_local_free() && term.reach() == 0
    }

    /// Discard every remembered reduct, sort and type. Called wherever the budget is restored, which is what makes a hit on them free rather than order-dependent.
    pub(super) fn begin_declaration(&mut self) {
        self.whnf.clear();
        self.forced.clear();
        self.types.clear();
        self.sorts.begin_declaration();
        self.halves.begin_declaration();
        self.begin_equations();
    }

    /// Discard what was read off the scope: the local-bearing reducts and the scoped sorts. Called wherever the set of case equations in force changes, which is the one thing besides the definition store a local-bearing reduct is a function of, and wherever a local is re-typed, which a sort is a function of besides.
    pub(super) fn begin_equations(&mut self) {
        self.local.clear();
        self.local_forced.clear();
        self.sorts.begin_equations();
        self.halves.begin_equations();
    }

    /// Discard every remembered reduct. Called when a definition is overwritten, which is the one event that can invalidate one.
    pub(super) fn invalidate(&mut self) {
        self.begin_declaration();
    }
}
