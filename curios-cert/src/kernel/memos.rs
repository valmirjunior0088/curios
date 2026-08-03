//! The evaluation memos: what a definition's body, and what a local-free term, weak-head reduce to.
//!
//! This is not the kind of store the [`kernel`](super) module documentation warns about. A metavariable heap or a refinement layer *injects* answers a term alone could not produce; a memo replays the kernel's own pure function of `(term, definitions)`, computed once. The measured cost of refusing even that was a 10× whole-prelude re-check, spent re-deriving the same prelude spines for every recursive group the totality gate walks.
//!
//! Two invariants make it an evaluation strategy rather than a second source of truth, and both live here.
//!
//! **Keys are valid by construction.** A definition body is closed, so an unfold entry depends on nothing but the definition store; [`Memos::invalidate`] is called whenever a name is *overwritten*, so validity never rests on an append-only assumption. A whnf entry is stored only for a *local-free* term — every free variable a definition name — which is what makes the key scope-independent: whnf reads no local (locals carry no values by design) and no case equation, so the reduct is a function of the definition store alone and no key can dangle a retracted binder. That test is applied here, on both the store and the lookup, rather than at the call sites that would have to remember it.
//!
//! **A store cannot charge.** [`Memos`] hands back a [`Replay`] and nothing else; applying one to the budget and the entropy counter is [`Spend`](super::Spend)'s job. A component that could both remember and charge is one that could charge twice.

use {
    super::Replay,
    curios_core::{Free, Term},
    std::collections::HashMap,
};

pub(super) struct Memos {
    /// Whether the memos are consulted at all. On by default; `Kernel::uncached` exists so a test can assert that switching them off changes no verdict — the property that makes them an evaluation strategy rather than a store.
    enabled: bool,
    /// The weak-head reduct each *monomorphic* definition's body reaches, keyed by name and computed on first delta — the analog of Lean's `m_unfold`, which its trusted `type_checker` holds beside `m_whnf` and `m_whnf_core`.
    unfold: HashMap<Free, Replay>,
    /// Weak-head reducts of local-free terms, per entry point: plain, and rec-forced.
    whnf: HashMap<Term, Replay>,
    forced: HashMap<Term, Replay>,
}

impl Memos {
    pub(super) fn new(enabled: bool) -> Self {
        Self {
            enabled,
            unfold: HashMap::new(),
            whnf: HashMap::new(),
            forced: HashMap::new(),
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

    /// The remembered weak-head reduct of `term` at the given entry point, still to be charged.
    pub(super) fn whnf(&self, term: &Term, forced: bool) -> Option<Replay> {
        if !self.storable(term) {
            return None;
        }

        match forced {
            false => self.whnf.get(term).cloned(),
            true => self.forced.get(term).cloned(),
        }
    }

    /// Remember `term`'s weak-head reduct at the given entry point, and its consumption.
    pub(super) fn store_whnf(&mut self, term: Term, forced: bool, replay: Replay) {
        if !self.storable(&term) {
            return;
        }

        match forced {
            false => self.whnf.insert(term, replay),
            true => self.forced.insert(term, replay),
        };
    }

    /// Discard every remembered reduct. Called when a definition is overwritten, which is the one event that can invalidate one.
    pub(super) fn invalidate(&mut self) {
        self.unfold.clear();
        self.whnf.clear();
        self.forced.clear();
    }

    /// Whether `term` may key a whnf entry: the memos must be on, and the term local-free. See the module documentation for why the second is what makes a key scope-independent.
    fn storable(&self, term: &Term) -> bool {
        self.enabled && !term.has_local_free()
    }
}
