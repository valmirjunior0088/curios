//! A resolution map the unit being lowered writes into, over the ones its scope already established.
//!
//! Name resolution reads across the boundary and writes only inside it. A `pub use` in the unit may select a name from anything in scope, so [`Scoped::get`] answers from every half; every insertion targets a module the unit declares, so [`Scoped::insert`] only ever touches its own. That asymmetry is what makes a layer sufficient, where otherwise each compilation would copy the prelude's map and extend the copy.
//!
//! Every half is a [`Table`]: persistent, each module's entry shared, and read back in the order of the modules, which is how a unit archives its resolution state, so the image is byte-reproducible. A unit lowered over a baseline starts its own half as the baseline's, and changes the entries of the modules its edit reached ([`Scoped::from`]). Borrowing the bases is all this does: nothing is converted, and nothing is copied.
//!
//! There is one base per unit in scope rather than one merged map, which is what keeps this a borrow: merging would copy every predecessor's table into every compilation, and copying the prelude per compilation is what the environment types avoid.

use {
    curios_utilities::{Qualifier, Sorted},
    std::rc::Rc,
};

/// A unit's resolution map, each module's entry shared between the versions that hold it.
pub(crate) type Table<V> = Sorted<Qualifier, Rc<V>>;

pub(crate) struct Scoped<'a, V> {
    /// What each unit already in scope established, in dependency order. Empty when the unit being lowered is the first and there is nothing beneath it.
    bases: &'a [&'a Table<V>],
    own: Table<V>,
}

impl<V> Default for Scoped<'_, V> {
    fn default() -> Self {
        Self {
            bases: &[],
            own: Table::default(),
        }
    }
}

impl<'a, V: Clone> Scoped<'a, V> {
    /// A map layered over `bases`, in dependency order.
    pub(crate) fn over(bases: &'a [&'a Table<V>]) -> Self {
        Self::from(bases, Table::new())
    }

    /// A map layered over `bases`, in dependency order, its own half `own` to begin with.
    pub(crate) fn from(bases: &'a [&'a Table<V>], own: Table<V>) -> Self {
        Self { bases, own }
    }

    /// The unit's own, then its scope's, latest first. A name the unit declares shadows one a base does, and a later unit's shadows an earlier one's — neither can arise, since mount sets are pairwise disjoint, and the rule is stated so the type has an answer rather than a precondition.
    pub(crate) fn get(&self, name: &Qualifier) -> Option<&V> {
        self.own
            .get(name)
            .or_else(|| self.bases.iter().rev().find_map(|base| base.get(name)))
            .map(Rc::as_ref)
    }

    pub(super) fn insert(&mut self, name: Qualifier, value: V) {
        self.own.insert(name, Rc::new(value));
    }

    /// The unit's own entry for `name`, taken out.
    pub(super) fn remove(&mut self, name: &Qualifier) {
        self.own.remove(name);
    }

    /// The unit's own entries for `root` and every module under it, taken out, each named: what a module the unit no longer declares leaves.
    pub(super) fn remove_subtree(&mut self, root: &Qualifier) -> Vec<Qualifier> {
        let mut under = Vec::new();
        let mut at = self.own.get(root).map(|_| *root);
        if at.is_none() {
            at = self
                .own
                .after(root)
                .map(|(name, _)| *name)
                .filter(|name| name.is_within(root));
        }
        while let Some(name) = at {
            under.push(name);
            at = self
                .own
                .after(&name)
                .map(|(next, _)| *next)
                .filter(|next| next.is_within(root));
        }
        for name in &under {
            self.own.remove(name);
        }

        under
    }

    /// The unit's own entry for `name` changed in place by `change`, copied first where another version shares it. Whether the unit holds one.
    pub(super) fn update(&mut self, name: &Qualifier, change: impl FnOnce(&mut V)) -> bool {
        self.own.update(name, |held| change(Rc::make_mut(held)))
    }

    /// Every entry in scope, the unit's own first, then each base's that nothing nearer shadows.
    ///
    /// The union, because the walks that iterate ask what the whole scope offers — every interface that carries a label an unresolved name could have meant, every page a documentation record exposes a name on — and those cross the boundary by nature. A pass that decides or reports on the unit's own declarations wants [`Scoped::own`] instead: who can see a name is decided by the unit that declares it.
    ///
    /// Deduplicated against nearer halves rather than against a merged set, which is the same shadowing rule [`Scoped::get`] answers by and is likewise unreachable while mount sets stay disjoint.
    pub(super) fn iter(&self) -> impl Iterator<Item = (&Qualifier, &V)> {
        self.own
            .iter()
            .chain(
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
            .map(|(name, value)| (name, &**value))
    }

    /// Only what the unit declares.
    ///
    /// Every walk that *reports* — dead-entry classification above all — wants this rather than the union: whether a scope's interface has an unused entry was settled when that unit was prepared, and re-deciding it against one program's imports would answer about the wrong thing.
    pub(super) fn own(&self) -> &Table<V> {
        &self.own
    }

    pub(super) fn into_own(self) -> Table<V> {
        self.own
    }
}
