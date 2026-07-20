//! The erasure environment: the map from Core value names to the arena
//! operands holding their erased values.
//!
//! Opened binders receive globally fresh Core labels and top-level names are
//! module-unique, so one flat map is unambiguous without scope save/restore.

use super::BTreeMap;

/// What a Core name erases to.
#[derive(Debug, Clone, Copy)]
pub(super) enum Binding {
    /// The operand holding the name's erased value.
    Atom(curios_ersd::ErasedAtom),
}

#[derive(Debug, Default)]
pub(super) struct Environment {
    values: BTreeMap<String, Binding>,
}

impl Environment {
    pub(super) fn bind(&mut self, name: impl Into<String>, atom: curios_ersd::ErasedAtom) {
        self.values.insert(name.into(), Binding::Atom(atom));
    }

    pub(super) fn lookup(&self, name: &str) -> Option<Binding> {
        self.values.get(name).copied()
    }
}
