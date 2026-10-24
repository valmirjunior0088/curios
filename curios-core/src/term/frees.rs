//! The lazy home of a node's memoized free-variable identities.
//!
//! Unlike the eager scalars, this derivation is wanted by a minority of nodes on a given compilation, so it stays its own once-filled cell rather than joining [`ScalarCache`](super::scalars::ScalarCache)'s packed words. A node's set is handed out as [`Frees`], a pointer's clone, and asked a membership through [`FreeCache::contains`] without touching its refcount — the reduction-cache invalidation probes every cached WHNF, and cloning each entry's set there would swamp the walk the cache exists to avoid.

use {
    super::Free,
    std::{
        cell::OnceCell,
        collections::{BTreeSet, btree_set},
        fmt,
        rc::Rc,
    },
};

/// A term's free-variable identities: one set, which every node holding the same names through one child shares, handed out as a pointer's clone. Read in `Free`'s order, ascending: the globals first, then the locals by mint.
#[derive(Clone)]
pub struct Frees(Rc<BTreeSet<Free>>);

impl Frees {
    /// The set of no name: one allocation every empty set shares, since most nodes of a closed data spine hold none. Thread-local because `Rc` is `!Send`.
    pub fn empty() -> Self {
        thread_local! {
            static EMPTY: Frees = Frees(Rc::new(BTreeSet::new()));
        }
        EMPTY.with(Frees::clone)
    }

    /// The set of `name` alone.
    pub(crate) fn single(name: Free) -> Self {
        Self(Rc::new(BTreeSet::from([name])))
    }

    /// The union of `sets` ([`Union`]).
    pub(crate) fn union(sets: impl IntoIterator<Item = Frees>) -> Self {
        let mut union = Union::default();
        for set in sets {
            union.add(&set);
        }
        union.finish()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn contains(&self, name: &Free) -> bool {
        self.0.contains(name)
    }

    /// Every name, ascending.
    pub fn iter(&self) -> FreesIter<'_> {
        FreesIter(self.0.range::<Free, _>(..))
    }

    /// The locals alone, ascending by mint: the names past the globals, read from the least local without passing the globals.
    pub fn locals(&self) -> FreesIter<'_> {
        FreesIter(self.0.range(Free::local(0, None)..))
    }

    /// Whether the two are one set, shared: so where a node's set is its child's.
    pub(crate) fn shares(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Debug for Frees {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_set().entries(self.iter()).finish()
    }
}

/// A walk of a [`Frees`] in its order.
pub struct FreesIter<'a>(btree_set::Range<'a, Free>);

impl<'a> Iterator for FreesIter<'a> {
    type Item = &'a Free;

    fn next(&mut self) -> Option<&'a Free> {
        self.0.next()
    }
}

impl<'a> IntoIterator for &'a Frees {
    type Item = &'a Free;
    type IntoIter = FreesIter<'a>;

    fn into_iter(self) -> FreesIter<'a> {
        self.iter()
    }
}

/// The union of sets taken one at a time: the one set holding any name where only one does, shared as it stands — the chain-shaped common case, where every link above the one free occurrence would otherwise copy the same set — and otherwise a set built of them all.
#[derive(Default)]
pub(crate) struct Union {
    carrier: Option<Frees>,
    merged: Option<BTreeSet<Free>>,
}

impl Union {
    pub(crate) fn add(&mut self, set: &Frees) {
        if set.is_empty() {
            return;
        }
        match (&self.carrier, &mut self.merged) {
            (None, _) => self.carrier = Some(set.clone()),
            // The carrier again, as both operands of `f(x, x)` hold it, adds nothing.
            (Some(first), _) if first.shares(set) => {}
            (Some(first), None) => {
                let mut merged = (*first.0).clone();
                merged.extend(set.iter().copied());
                self.merged = Some(merged);
            }
            (Some(_), Some(merged)) => merged.extend(set.iter().copied()),
        }
    }

    pub(crate) fn finish(self) -> Frees {
        match (self.carrier, self.merged) {
            (_, Some(merged)) => Frees(Rc::new(merged)),
            (Some(shared), None) => shared,
            (None, None) => Frees::empty(),
        }
    }
}

/// Names a reader asks a term for at once, prepared once: sorted, so one is found by its place, with the oldest local among them where every one is a local, so a term whose newest local is older holds none of them ([`Term::mentions_any`](super::Term::mentions_any)).
#[derive(Debug, Clone, Default)]
pub struct Names {
    sorted: Vec<Free>,
    oldest_local: Option<u64>,
}

impl Names {
    pub fn new(names: impl IntoIterator<Item = Free>) -> Self {
        let mut sorted = names.into_iter().collect::<Vec<_>>();
        sorted.sort_unstable();
        sorted.dedup();
        let oldest_local = match sorted.first() {
            None => None,
            Some(_) => sorted.iter().try_fold(u64::MAX, |oldest, name| {
                name.local_index().map(|index| oldest.min(index))
            }),
        };

        Self {
            sorted,
            oldest_local,
        }
    }

    pub fn len(&self) -> usize {
        self.sorted.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sorted.is_empty()
    }

    pub fn contains(&self, name: &Free) -> bool {
        self.sorted.binary_search(name).is_ok()
    }

    /// The names, ascending in `Free`'s order.
    pub fn iter(&self) -> impl Iterator<Item = &Free> {
        self.sorted.iter()
    }

    /// The oldest local among the names, where every one is a local.
    pub(crate) fn oldest_local(&self) -> Option<u64> {
        self.oldest_local
    }

    /// The one name, where there is one.
    pub(crate) fn only(&self) -> Option<&Free> {
        match self.sorted.as_slice() {
            [only] => Some(only),
            _ => None,
        }
    }
}

/// A node's memoized free-variable set. `Default` is the unfilled state, which is also what an archived node restores to (the field is `rkyv`-skipped).
#[derive(Default)]
pub(crate) struct FreeCache {
    frees: OnceCell<Frees>,
}

impl FreeCache {
    pub(crate) fn is_filled(&self) -> bool {
        self.frees.get().is_some()
    }

    pub(crate) fn get(&self) -> Option<&Frees> {
        self.frees.get()
    }

    /// Memoize the node's set — once; the post-order fill visits each node exactly once.
    pub(crate) fn fill(&self, frees: Frees) {
        let filled = self.frees.set(frees);
        assert!(filled.is_ok(), "a free-variable cache fills at most once");
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
