//! A resolution map the entry program writes into, over one the prelude already established.
//!
//! Name resolution reads across the boundary and writes only inside it. A `pub use` in the entry may select a prelude name, so [`Scoped::get`] answers from either half; every insertion targets a module the entry declares, so [`Scoped::insert`] only ever touches its own. That asymmetry is what makes a layer sufficient where the lowerer used to copy the prelude's map per compilation and extend the copy.
//!
//! The base stays a [`BTreeMap`] because that is how `PreparedPrelude` archives it — ordered, so the image is byte-reproducible — while the overlay is a [`HashMap`], which is what the resolution algorithm wants for its own churn. Borrowing one and allocating the other is the whole of the change: nothing is converted, and nothing is copied.

use std::collections::{BTreeMap, HashMap};

pub(super) struct Scoped<'a, V> {
    /// What an earlier run of this resolution established. `None` when the entry *is* the prelude and there is nothing beneath it.
    base: Option<&'a BTreeMap<curios_base::Qualifier, V>>,
    own: HashMap<curios_base::Qualifier, V>,
}

impl<V> Default for Scoped<'_, V> {
    fn default() -> Self {
        Self {
            base: None,
            own: HashMap::new(),
        }
    }
}

impl<'a, V> Scoped<'a, V> {
    /// A map layered over `base`.
    pub(super) fn over(base: &'a BTreeMap<curios_base::Qualifier, V>) -> Self {
        Self {
            base: Some(base),
            own: HashMap::new(),
        }
    }

    /// The entry's own, then the base. A name the entry declares shadows one the base does — which cannot arise, since the entry cannot mount into a prelude root, and is stated so the type has an answer rather than a precondition.
    pub(super) fn get(&self, name: &curios_base::Qualifier) -> Option<&V> {
        self.own
            .get(name)
            .or_else(|| self.base.and_then(|base| base.get(name)))
    }

    pub(super) fn insert(&mut self, name: curios_base::Qualifier, value: V) -> Option<V> {
        self.own.insert(name, value)
    }

    pub(super) fn get_mut(&mut self, name: &curios_base::Qualifier) -> Option<&mut V> {
        self.own.get_mut(name)
    }

    /// Every entry in scope, the entry's own first, then the base's that it does not shadow.
    ///
    /// The union, because the passes that iterate are the ones that reason about *visibility* — who can see what, and what a public entry exposes — and those cross the boundary by nature: an entry program's public item may hand out a prelude type. A pass that only reports on the entry's own declarations wants [`Scoped::own`] instead.
    pub(super) fn iter(&self) -> impl Iterator<Item = (&curios_base::Qualifier, &V)> {
        self.own.iter().chain(
            self.base
                .into_iter()
                .flatten()
                .filter(|(name, _)| !self.own.contains_key(*name)),
        )
    }

    /// Only what the entry declares.
    ///
    /// Every walk that *reports* — dead-entry classification above all — wants this rather than the union: whether a prelude interface has an unused entry was settled when the prelude was prepared, and re-deciding it against one program's imports would answer about the wrong thing.
    pub(super) fn own(&self) -> &HashMap<curios_base::Qualifier, V> {
        &self.own
    }

    pub(super) fn into_own(self) -> HashMap<curios_base::Qualifier, V> {
        self.own
    }
}
