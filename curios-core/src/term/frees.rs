//! The lazy home of a node's memoized free-variable identities.
//!
//! Unlike the eager scalars, this derivation is wanted by a minority of nodes on a given compilation, so it stays its own once-filled cell rather than joining [`ScalarCache`](super::scalars::ScalarCache)'s packed words. The two read paths are deliberately distinct methods: [`FreeCache::get`] hands out the shared set, while [`FreeCache::contains`] answers a membership probe without touching the set's refcount — the reduction-cache invalidation probes every cached WHNF, and cloning each entry's set there would swamp the walk the cache exists to avoid.

use {
    super::Free,
    std::{cell::OnceCell, collections::BTreeSet, rc::Rc},
};

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
    pub(crate) fn fill(&self, frees: Rc<BTreeSet<Free>>) {
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
