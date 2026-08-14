//! A resolution map the unit being lowered writes into, over the ones its scope already established.
//!
//! Name resolution reads across the boundary and writes only inside it. A `pub use` in the unit may select a name from anything in scope, so [`Scoped::get`] answers from every half; every insertion targets a module the unit declares, so [`Scoped::insert`] only ever touches its own. That asymmetry is what makes a layer sufficient where the lowerer used to copy the prelude's map per compilation and extend the copy.
//!
//! The bases stay [`BTreeMap`]s because that is how a unit archives its resolution state — ordered, so the image is byte-reproducible — while the overlay is a [`HashMap`], which is what the resolution algorithm wants for its own churn. Borrowing them and allocating the other is the whole of the change: nothing is converted, and nothing is copied.
//!
//! There is one base per unit in scope rather than one merged map, which is what keeps this a borrow: merging would copy every predecessor's table into every compilation, and copying the prelude per compilation is exactly what the environment types replaced.

use std::collections::{BTreeMap, HashMap};

pub(super) struct Scoped<'a, V> {
    /// What each unit already in scope established, in dependency order. Empty when the unit being lowered is the first and there is nothing beneath it.
    bases: &'a [&'a BTreeMap<curios_utilities::Qualifier, V>],
    own: HashMap<curios_utilities::Qualifier, V>,
}

impl<V> Default for Scoped<'_, V> {
    fn default() -> Self {
        Self {
            bases: &[],
            own: HashMap::new(),
        }
    }
}

impl<'a, V> Scoped<'a, V> {
    /// A map layered over `bases`, in dependency order.
    pub(super) fn over(bases: &'a [&'a BTreeMap<curios_utilities::Qualifier, V>]) -> Self {
        Self {
            bases,
            own: HashMap::new(),
        }
    }

    /// The unit's own, then its scope's, latest first. A name the unit declares shadows one a base does, and a later unit's shadows an earlier one's — neither can arise, since mount sets are pairwise disjoint, and the rule is stated so the type has an answer rather than a precondition.
    pub(super) fn get(&self, name: &curios_utilities::Qualifier) -> Option<&V> {
        self.own
            .get(name)
            .or_else(|| self.bases.iter().rev().find_map(|base| base.get(name)))
    }

    pub(super) fn insert(&mut self, name: curios_utilities::Qualifier, value: V) -> Option<V> {
        self.own.insert(name, value)
    }

    pub(super) fn get_mut(&mut self, name: &curios_utilities::Qualifier) -> Option<&mut V> {
        self.own.get_mut(name)
    }

    /// Every entry in scope, the unit's own first, then each base's that nothing nearer shadows.
    ///
    /// The union, because the passes that iterate are the ones that reason about *visibility* — who can see what, and what a public entry exposes — and those cross the boundary by nature: a unit's public item may hand out a type from its scope. A pass that only reports on the unit's own declarations wants [`Scoped::own`] instead.
    ///
    /// Deduplicated against nearer halves rather than against a merged set, which is the same shadowing rule [`Scoped::get`] answers by and is likewise unreachable while mount sets stay disjoint.
    pub(super) fn iter(&self) -> impl Iterator<Item = (&curios_utilities::Qualifier, &V)> {
        self.own.iter().chain(
            self.bases
                .iter()
                .enumerate()
                .flat_map(move |(index, base)| base.iter().map(move |entry| (index, entry)))
                .filter(|(index, (name, _))| {
                    !self.own.contains_key(*name)
                        && !self.bases[index + 1..]
                            .iter()
                            .any(|nearer| nearer.contains_key(*name))
                })
                .map(|(_, entry)| entry),
        )
    }

    /// Only what the unit declares.
    ///
    /// Every walk that *reports* — dead-entry classification above all — wants this rather than the union: whether a scope's interface has an unused entry was settled when that unit was prepared, and re-deciding it against one program's imports would answer about the wrong thing.
    pub(super) fn own(&self) -> &HashMap<curios_utilities::Qualifier, V> {
        &self.own
    }

    pub(super) fn into_own(self) -> HashMap<curios_utilities::Qualifier, V> {
        self.own
    }
}
