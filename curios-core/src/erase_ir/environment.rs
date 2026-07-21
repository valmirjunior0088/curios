//! The erasure environment: the maps from Core identities to the arena
//! identities they lower to.
//!
//! Opened binders receive globally fresh Core labels and top-level names are
//! module-unique, so one flat value map is unambiguous without scope
//! save/restore. The schema maps memoize the arena identities Core
//! declarations lower to — registered lazily on first use, when the
//! dominance-ordered item chain guarantees the declaration's dependencies are
//! already defined — so a type constructed or matched at many sites shares
//! one schema.

use super::BTreeMap;

/// What a Core name erases to.
#[derive(Debug, Clone, Copy)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(super) enum Binding {
    /// The operand holding the name's erased value.
    Atom(curios_ersd::Atom),
    /// A dropped (erasable) binder — a proof or a type with no runtime value.
    /// Referencing it yields the unit constant and records the dangle for the
    /// function-body collapse.
    Dropped,
}

/// A registered structure's layout. The mask is the declaration's opaque
/// signature mask (one flag per declared field, `true` where the field is
/// erased), computed once with the parameters abstract, so construction and
/// projection agree on the relevant-field arithmetic at every instantiation.
/// `schema` is `None` for a newtype — a single relevant field collapses to
/// its bare value, with no product node.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(super) struct ProductRow {
    pub(super) schema: Option<curios_ersd::ProductId>,
    pub(super) mask: Vec<bool>,
}

impl ProductRow {
    /// The runtime projection index of declared field `index`: the count of
    /// relevant fields before it.
    pub(super) fn relevant_before(&self, index: usize) -> u32 {
        self.mask[..index].iter().filter(|&&erased| !erased).count() as u32
    }
}

/// A registered constructor: its arena identity and the declaration's opaque
/// signature mask over its payload fields (`true` where a field is erased).
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(super) struct ConstructorRow {
    pub(super) id: curios_ersd::ConstructorId,
    pub(super) mask: Vec<bool>,
}

/// A registered inductive: its variant family and its constructors in
/// runtime-tag (registry) order.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(super) struct FamilyRow {
    pub(super) family: curios_ersd::FamilyId,
    pub(super) constructors: Vec<ConstructorRow>,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub(super) struct Environment {
    values: BTreeMap<String, Binding>,
    structures: BTreeMap<String, ProductRow>,
    inductives: BTreeMap<String, FamilyRow>,
    /// Anonymous tuple schemas, interned by relevant-field width — an
    /// arity-`n` product is one untyped layout regardless of which tuple
    /// built it.
    tuples: BTreeMap<usize, curios_ersd::ProductId>,
}

impl Environment {
    pub(super) fn bind(&mut self, name: impl Into<String>, atom: curios_ersd::Atom) {
        self.values.insert(name.into(), Binding::Atom(atom));
    }

    pub(super) fn bind_dropped(&mut self, name: impl Into<String>) {
        self.values.insert(name.into(), Binding::Dropped);
    }

    pub(super) fn lookup(&self, name: &str) -> Option<Binding> {
        self.values.get(name).copied()
    }

    pub(super) fn structure_row(&self, name: &str) -> Option<&ProductRow> {
        self.structures.get(name)
    }

    pub(super) fn register_structure_row(&mut self, name: impl Into<String>, row: ProductRow) {
        self.structures.insert(name.into(), row);
    }

    pub(super) fn inductive_row(&self, name: &str) -> Option<&FamilyRow> {
        self.inductives.get(name)
    }

    pub(super) fn register_inductive_row(&mut self, name: impl Into<String>, row: FamilyRow) {
        self.inductives.insert(name.into(), row);
    }

    pub(super) fn tuple_schema(&self, width: usize) -> Option<curios_ersd::ProductId> {
        self.tuples.get(&width).copied()
    }

    pub(super) fn register_tuple_schema(&mut self, width: usize, schema: curios_ersd::ProductId) {
        self.tuples.insert(width, schema);
    }
}
