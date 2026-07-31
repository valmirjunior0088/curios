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
    ///
    /// An empty set is normalized to one canonical shared `Rc`: most nodes on a closed data spine have no free variables, and giving each its own allocation would make the empty answer the cache's dominant residency cost. Thread-local because `Rc` is `!Send`.
    pub(crate) fn fill(&self, frees: BTreeSet<Free>) {
        thread_local! {
            static EMPTY: Rc<BTreeSet<Free>> = Rc::new(BTreeSet::new());
        }

        let frees = if frees.is_empty() {
            EMPTY.with(Rc::clone)
        } else {
            Rc::new(frees)
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
mod tests {
    use super::*;

    #[test]
    fn empty_fills_share_one_canonical_set() {
        let first = FreeCache::default();
        let second = FreeCache::default();
        first.fill(BTreeSet::new());
        second.fill(BTreeSet::new());

        assert!(Rc::ptr_eq(first.get().unwrap(), second.get().unwrap()));
    }

    #[test]
    fn nonempty_fills_keep_their_own_set() {
        let name = Free::local(0, Some("x"));
        let cache = FreeCache::default();
        cache.fill(BTreeSet::from([name.clone()]));

        assert!(cache.is_filled());
        assert!(cache.contains(&name));
        assert_eq!(cache.get().unwrap().len(), 1);
    }
}
