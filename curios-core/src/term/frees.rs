//! The lazy home of a node's memoized free-variable identities.
//!
//! Unlike the eager scalars, this derivation is wanted by a minority of nodes on a given compilation, so it stays its own once-filled cell rather than joining [`ScalarCache`](super::scalars::ScalarCache)'s packed words. The two read paths are deliberately distinct methods: [`FreeCache::get`] hands out the shared set, while [`FreeCache::contains`] answers a membership probe without touching the set's refcount — the reduction-cache invalidation probes every cached WHNF, and cloning each entry's set there would swamp the walk the cache exists to avoid.

use {
    super::Free,
    std::{cell::OnceCell, collections::BTreeSet, rc::Rc},
};

/// One node's computed free-variable set, in the sharing state the computation discovered: `Shared` hands a single carrier child's `Rc` through unchanged — the chain-shaped common case, where every link above the one free occurrence would otherwise copy the same set — and `Owned` is a freshly built union.
pub(crate) enum FreeVars {
    Shared(Rc<BTreeSet<Free>>),
    Owned(BTreeSet<Free>),
}

/// A node's memoized free-variable set. `Default` is the unfilled state, which is also what an archived node restores to (the field is `rkyv`-skipped).
#[derive(Default)]
pub(crate) struct FreeCache {
    frees: OnceCell<Rc<BTreeSet<Free>>>,
}

impl FreeCache {
    pub(crate) fn is_filled(&self) -> bool {
        self.frees.get().is_some()
    }

    pub(crate) fn get(&self) -> Option<&Rc<BTreeSet<Free>>> {
        self.frees.get()
    }

    /// Memoize the node's set — once; the post-order fill visits each node exactly once.
    ///
    /// An `Owned` empty set is normalized to one canonical shared `Rc`: most nodes on a closed data spine have no free variables, and giving each its own allocation would make the empty answer the cache's dominant residency cost. Thread-local because `Rc` is `!Send`.
    pub(crate) fn fill(&self, frees: FreeVars) {
        thread_local! {
            static EMPTY: Rc<BTreeSet<Free>> = Rc::new(BTreeSet::new());
        }

        let frees = match frees {
            FreeVars::Shared(shared) => shared,
            FreeVars::Owned(owned) if owned.is_empty() => EMPTY.with(Rc::clone),
            FreeVars::Owned(owned) => Rc::new(owned),
        };
        let filled = self.frees.set(frees);
        debug_assert!(filled.is_ok(), "a free-variable cache fills at most once");
    }

    /// Whether `name` is in the filled set. A membership lookup, not a set handout — and only meaningful after the owning term's walk has filled this node, which every caller guarantees by warming first.
    pub(crate) fn contains(&self, name: &Free) -> bool {
        self.frees
            .get()
            .expect("a free-variable cache is warmed before it is probed")
            .contains(name)
    }
}

#[cfg(test)]
mod tests;
