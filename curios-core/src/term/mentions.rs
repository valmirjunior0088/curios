//! The lazy home of a node's memoized mentions: the free variables it holds and the declarations it names.
//!
//! Unlike the eager scalars, this derivation is wanted by a minority of nodes on a given compilation, so it stays its own once-filled cell rather than joining [`ScalarCache`](super::scalars::ScalarCache)'s packed words. A node's set is handed out as [`Mentions`], a pointer's clone, and asked a membership through [`MentionCache::contains`] without touching its refcount — the reduction-cache invalidation probes every cached WHNF, and cloning each entry's set there would swamp the walk the cache exists to avoid.

use {
    crate::{Free, Global, Mention},
    curios_common::{InOrder, Sorted},
    std::{cell::OnceCell, fmt, rc::Rc},
};

/// What a term mentions — the declarations it names and its free variables — handed out as a pointer's clone. Read in [`Mention`]'s order, ascending: the declarations first, then the locals by mint.
///
/// **A node pays for what it adds.** The set is `curios-common`'s persistent treap ([`Sorted`]), whose shape is a function of the names it holds, and a node's is its largest carrying child's with the others' names put into it, the smaller into the larger ([`Sorted::union_with`]): it shares every node of the larger and copies the paths to the names it adds, so a chain whose every level names one more local holds a set per level at a path apiece, and a node whose children add nothing to the largest holds the largest itself.
#[derive(Clone)]
pub struct Mentions(Rc<Sorted<Mention, ()>>);

impl Mentions {
    /// The set of no name: one allocation every empty set shares, since most nodes of a closed data spine hold none. Thread-local because `Rc` is `!Send`.
    pub fn empty() -> Self {
        thread_local! {
            static EMPTY: Mentions = Mentions(Rc::new(Sorted::new()));
        }
        EMPTY.with(Mentions::clone)
    }

    /// The set of `name` alone.
    pub(crate) fn single(name: Mention) -> Self {
        let mut set = Sorted::new();
        set.insert(name, ());
        Self(Rc::new(set))
    }

    /// The union of `sets` ([`Union`]).
    pub(crate) fn union(sets: impl IntoIterator<Item = Mentions>) -> Self {
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

    pub fn contains(&self, name: impl Into<Mention>) -> bool {
        self.0.contains_key(&name.into())
    }

    /// Every name, ascending.
    pub fn iter(&self) -> MentionsIter<'_> {
        MentionsIter(self.0.iter())
    }

    /// The free variables alone, ascending by mint: the names past the declarations, read from the least local without passing the declarations.
    pub fn locals(&self) -> impl Iterator<Item = &Free> {
        MentionsIter(self.0.iter_from(&Mention::Local(Free::local(0, None))))
            .filter_map(Mention::as_local)
    }

    /// The declarations alone, ascending: the names before the first local.
    pub fn declarations(&self) -> impl Iterator<Item = &Global> {
        self.iter().map_while(Mention::as_declaration)
    }

    /// The set with `name` in it, copying the path to it and sharing every other node with the version it was.
    pub(crate) fn insert(&mut self, name: Mention) {
        Rc::make_mut(&mut self.0).insert(name, ());
    }

    /// The set with `name` taken out.
    pub(crate) fn remove(&mut self, name: &Mention) {
        Rc::make_mut(&mut self.0).remove(name);
    }

    /// Whether the set holds any of `names`: the smaller of the two probing the other.
    pub fn holds_any(&self, names: &Names) -> bool {
        match self.len() < names.len() {
            true => self.iter().any(|name| names.contains(name)),
            false => names.iter().any(|name| self.contains(name)),
        }
    }

    /// Whether the two are one version of one set, sharing every node: so where a node's set is its child's.
    pub(crate) fn shares(&self, other: &Self) -> bool {
        self.0.same_version(&other.0)
    }
}

impl fmt::Debug for Mentions {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_set().entries(self.iter()).finish()
    }
}

/// A walk of a [`Mentions`] in its order, which allocates nothing for a set of the sizes a node holds.
pub struct MentionsIter<'a>(InOrder<'a, Mention, ()>);

impl<'a> Iterator for MentionsIter<'a> {
    type Item = &'a Mention;

    fn next(&mut self) -> Option<&'a Mention> {
        self.0.next().map(|(name, ())| name)
    }
}

impl<'a> IntoIterator for &'a Mentions {
    type Item = &'a Mention;
    type IntoIter = MentionsIter<'a>;

    fn into_iter(self) -> MentionsIter<'a> {
        self.iter()
    }
}

/// The union of sets taken one at a time, each the smaller put into the larger ([`Sorted::union_with`]): the one set holding any name where only one does, shared as it stands — the chain-shaped common case, where every link above the one free occurrence would otherwise copy the same set — and otherwise the larger of the two with the smaller's names it lacks, so the union so far is the largest operand's set itself wherever the others add nothing to it.
#[derive(Default)]
pub struct Union {
    union: Option<Mentions>,
}

impl Union {
    pub fn add(&mut self, set: &Mentions) {
        if set.is_empty() {
            return;
        }
        let Some(union) = &mut self.union else {
            self.union = Some(set.clone());
            return;
        };
        // The same set again, as both operands of `f(x, x)` hold it, adds nothing.
        if union.shares(set) {
            return;
        }
        let merged = union.0.union_with(&set.0, |_, (), ()| None);
        if merged.same_version(&set.0) {
            *union = set.clone();
        } else if !merged.same_version(&union.0) {
            *union = Mentions(Rc::new(merged));
        }
    }

    pub fn finish(self) -> Mentions {
        self.union.unwrap_or_else(Mentions::empty)
    }
}

/// Names a reader asks a term for at once, prepared once: sorted, so one is found by its place, with the oldest local among them where every one is a local, so a term whose newest local is older holds none of them ([`Term::mentions_any`](super::Term::mentions_any)).
#[derive(Debug, Clone, Default)]
pub struct Names {
    sorted: Vec<Mention>,
    oldest_local: Option<u64>,
}

impl Names {
    pub fn new(names: impl IntoIterator<Item = impl Into<Mention>>) -> Self {
        let mut sorted = names.into_iter().map(Into::into).collect::<Vec<_>>();
        sorted.sort_unstable();
        sorted.dedup();
        let oldest_local = match sorted.first() {
            None => None,
            Some(_) => sorted.iter().try_fold(u64::MAX, |oldest, name| {
                name.as_local().map(|local| oldest.min(local.index()))
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

    pub fn contains(&self, name: &Mention) -> bool {
        self.sorted.binary_search(name).is_ok()
    }

    /// The names, ascending in [`Mention`]'s order.
    pub fn iter(&self) -> impl Iterator<Item = &Mention> {
        self.sorted.iter()
    }

    /// The oldest local among the names, where every one is a local.
    pub(crate) fn oldest_local(&self) -> Option<u64> {
        self.oldest_local
    }

    /// The one name, where there is one.
    pub(crate) fn only(&self) -> Option<&Mention> {
        match self.sorted.as_slice() {
            [only] => Some(only),
            _ => None,
        }
    }
}

/// A node's memoized mentions. `Default` is the unfilled state, which is also what an archived node restores to (the field is `rkyv`-skipped).
#[derive(Default)]
pub struct MentionCache {
    mentions: OnceCell<Mentions>,
}

impl MentionCache {
    pub(crate) fn is_filled(&self) -> bool {
        self.mentions.get().is_some()
    }

    pub(crate) fn get(&self) -> Option<&Mentions> {
        self.mentions.get()
    }

    /// Memoize the node's set — once; the post-order fill visits each node exactly once.
    pub(crate) fn fill(&self, mentions: Mentions) {
        let filled = self.mentions.set(mentions);
        assert!(filled.is_ok(), "a mention cache fills at most once");
    }

    /// Whether `name` is in the filled set. A membership lookup, not a set handout — and only meaningful after the owning term's walk has filled this node, which every caller guarantees by warming first.
    pub(crate) fn contains(&self, name: &Mention) -> bool {
        self.mentions
            .get()
            .expect("a mention cache is warmed before it is probed")
            .contains(*name)
    }
}

#[cfg(test)]
mod tests;
