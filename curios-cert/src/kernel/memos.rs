//! The evaluation memos: what a definition's body, and what a term, weak-head reduce to — a local-free term for the rest of the declaration, a local-bearing one for as long as the case equations in force stand.
//!
//! This is not the kind of store the [`kernel`](super) module documentation warns about. A metavariable heap or a refinement layer *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. The measured cost of refusing even that was a 10× whole-prelude re-check, spent re-deriving the same prelude spines for every recursive group the totality gate walks.
//!
//! Three invariants make it an evaluation strategy rather than a second source of truth, and all three live here.
//!
//! **Keys are valid by construction.** A definition body is closed, so an unfold entry depends on nothing but the definition store; [`Memos::invalidate`] is called whenever a name is *overwritten*, so validity never rests on an append-only assumption. A whnf entry for a *local-free* term — every free variable a definition name — has a scope-independent key: whnf reads no local (locals carry no values by design) and no case equation, so the reduct is a function of the definition store alone and no key can dangle a retracted binder. A whnf entry for a *local-bearing* term is a function of one thing more, the case equations in force, and its key cannot say which were; so those entries live in tables of their own that [`Memos::begin_equations`] clears wherever that set changes — an equation assumed, a bracket retracting one, a settlement withholding and restoring them — and within one such span the reduct is as fixed as a closed term's is within a declaration. The binders a key names are minted once and never recur, so a stale entry is unreachable rather than wrong. Which table a term belongs to is decided here, on both the store and the lookup, rather than at the call sites that would have to remember it.
//!
//! **They are consulted at every reduction level, not only at the crate's boundary.** `whnf_within` probes on entry and stores on exit, so a scrutinee, an application's head, each turn of `force`'s loop and every other internal call is served by the same table. That is what the elaborator's reducer has always done; the kernel probed only its two [`Reducer`](curios_core::Reducer) methods, which left fifteen internal calls re-deriving what the table already held and cost 5.3× the elaborator on a `Str` literal's UTF-8 scan — same terms, same peak depth, one price list. What makes reaching further safe is that the admission test did not move: [`Memos::storable`] gates the lookup as well as the store, so every site is protected by exactly the test the two old ones were.
//!
//! **The two term-keyed tables live exactly as long as a budget does.** [`Memos::begin_declaration`] clears them wherever `Spend::restore_budget` fires, and that alignment is what lets a hit on them be free. The paragraph above argues an entry's *content* is semantically determined — "the reduct is a function of the definition store alone" — but its *presence* was historical, because the budget is restored at every declaration boundary and these tables were not. A free hit under that asymmetry would make what one declaration spends depend on which declarations were walked before it, which is the property the per-declaration budget exists to hold. Clearing removes the dependence rather than asking anyone to trust a cache policy, and it measured free on every probe. The name-keyed `unfold` table is deliberately *not* cleared: it is what makes certifying a whole module affordable, and a hit on it is charged, so cross-declaration sharing of definition unfolds concedes nothing new. That lifetime is also what decides which table the retention allowance prices: an entry that dies with the budget that built it is bounded by that budget already, so the two term-keyed tables are stored for nothing, and only the `unfold` table — the one whose entries outlive a declaration — pays. See `curios-core`'s `retention` module for the rule stated once.
//!
//! **A store cannot charge.** [`Memos`] hands back a [`Replay`] and nothing else; applying one to the budget and the entropy counter is [`Spend`](super::Spend)'s job — [`Spend::charge`](super::Spend::charge) for the table that pays, [`Spend::charge_nothing`](super::Spend::charge_nothing) for the two that do not. A component that could both remember and charge is one that could charge twice.

use {
    super::Replay,
    curios_core::{Free, Term},
    std::collections::HashMap,
};

pub(super) struct Memos {
    /// Whether the memos are consulted at all. On by default; `Kernel::uncached` exists so a test can assert that switching them off changes no *semantic* verdict — the property that makes them an evaluation strategy rather than a store.
    enabled: bool,
    /// The weak-head reduct each *monomorphic* definition's body reaches, keyed by name and computed on first delta — the analog of Lean's `m_unfold`, which its trusted `type_checker` holds beside `m_whnf` and `m_whnf_core`. Charged on every hit, and kept across declarations.
    unfold: HashMap<Free, Replay>,
    /// Weak-head reducts of local-free terms, per entry point: plain, and rec-forced. Free on a hit, and cleared at every declaration boundary — see the module documentation for why those two go together, and for why `whnf` reaches this at every level rather than only at the crate's edge.
    whnf: HashMap<Term, Replay>,
    forced: HashMap<Term, Replay>,
    /// The same two tables for *local-bearing* terms, whose reducts are a function of the definition store **and the case equations in force**. The key cannot carry the second, so the tables live only as long as that set does: [`Memos::begin_equations`] clears them wherever an equation is assumed, retracted, withheld or restored, and [`Memos::begin_declaration`] with the rest. Within one such span a term's reduct is as fixed as a closed term's is within a declaration, and the web of definitions the index inversion forces at `Eq(top(n), 0)` — each naming the one before it twice, a local in every one — was re-derived `2^n` times for want of exactly this.
    local: HashMap<Term, Replay>,
    local_forced: HashMap<Term, Replay>,
}

impl Memos {
    pub(super) fn new(enabled: bool) -> Self {
        Self {
            enabled,
            unfold: HashMap::new(),
            whnf: HashMap::new(),
            forced: HashMap::new(),
            local: HashMap::new(),
            local_forced: HashMap::new(),
        }
    }

    /// The remembered reduct of `name`'s body, still to be charged.
    pub(super) fn unfold(&self, name: &Free) -> Option<Replay> {
        self.enabled.then(|| self.unfold.get(name).cloned())?
    }

    /// Remember what `name`'s body reduces to, and what computing it consumed.
    pub(super) fn store_unfold(&mut self, name: Free, replay: Replay) {
        if self.enabled {
            self.unfold.insert(name, replay);
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

    /// Discard the term-keyed reducts, leaving the name-keyed ones. Called wherever the budget is restored, which is what makes a hit on them free rather than order-dependent.
    pub(super) fn begin_declaration(&mut self) {
        self.whnf.clear();
        self.forced.clear();
        self.begin_equations();
    }

    /// Discard the local-bearing reducts. Called wherever the set of case equations in force changes, which is the one thing besides the definition store that such a reduct is a function of.
    pub(super) fn begin_equations(&mut self) {
        self.local.clear();
        self.local_forced.clear();
    }

    /// Discard every remembered reduct. Called when a definition is overwritten, which is the one event that can invalidate one.
    pub(super) fn invalidate(&mut self) {
        self.unfold.clear();
        self.begin_declaration();
    }
}
