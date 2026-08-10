//! The erasure environment: the maps from Core identities to the arena identities they lower to.
//!
//! Opened binders receive globally fresh Core labels and top-level names are module-unique, so one flat value map is unambiguous without scope save/restore. The schema maps memoize the arena identities Core declarations lower to — registered lazily on first use, when the dominance-ordered item chain guarantees the declaration's dependencies are already defined — so a type constructed or matched at many sites shares one schema.

use {
    curios_core::{Free, Global},
    std::collections::BTreeMap,
};

/// What a Core name erases to.
#[derive(Debug, Clone, Copy)]
#[curios_archive::archived]
pub(super) enum Binding {
    /// The operand holding the name's erased value.
    Atom(curios_ersd::Atom),
    /// A dropped (erasable) binder — a proof or a type with no runtime value. Referencing it yields the unit constant and records the dangle for the function-body collapse.
    Dropped,
}

/// A registered structure's layout. The mask is the declaration's opaque signature mask (one flag per declared field, `true` where the field is erased), computed once with the parameters abstract, so construction and projection agree on the relevant-field arithmetic at every instantiation. `schema` is `None` for a newtype — a single relevant field collapses to its bare value, with no product node.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(super) struct ProductRow {
    pub(super) schema: Option<curios_ersd::ProductId>,
    pub(super) mask: Vec<bool>,
}

impl ProductRow {
    /// Whether a product of `relevant` surviving fields collapses to the bare field — the newtype rule. Construction, projection, and schema registration must all take the same branch, so the `== 1` lives here and nowhere else.
    pub(super) fn collapses(relevant: usize) -> bool {
        relevant == 1
    }

    /// The runtime projection index of declared field `index`: the count of relevant fields before it.
    pub(super) fn relevant_before(&self, index: usize) -> u32 {
        self.mask[..index].iter().filter(|&&erased| !erased).count() as u32
    }
}

/// A registered constructor: its arena identity and the declaration's opaque signature mask over its payload fields (`true` where a field is erased).
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(super) struct ConstructorRow {
    pub(super) id: curios_ersd::ConstructorId,
    pub(super) mask: Vec<bool>,
}

/// A registered inductive: its variant family and its constructors in runtime-tag (registry) order.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(super) struct FamilyRow {
    pub(super) family: curios_ersd::FamilyId,
    pub(super) constructors: Vec<ConstructorRow>,
}

#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub(super) struct Environment {
    values: BTreeMap<Free, Binding>,
    struct_decls: BTreeMap<Global, ProductRow>,
    induct_decls: BTreeMap<Global, FamilyRow>,
    /// Anonymous tuple schemas, interned by relevant-field width — an arity-`n` product is one untyped layout regardless of which tuple built it.
    tuples: BTreeMap<usize, curios_ersd::ProductId>,
}

impl Environment {
    /// Rewrite every bound atom through a module compaction.
    ///
    /// The environment is the one thing outside the module that holds arena identities, and it is archived beside it — so a compaction that does not reach here leaves the stored image self-inconsistent in a way nothing would report: a stale index still addresses a live slot, just the wrong entity.
    ///
    /// An identity missing from the compaction is a *bug*, not a dead binding: this runs where nothing has been pruned, so every bound atom names something live. Failing loudly is the point — the alternative is passing the stale identity through.
    pub(super) fn remap(&mut self, compaction: &curios_ersd::Compaction) {
        for binding in self.values.values_mut() {
            let Binding::Atom(atom) = binding else {
                continue;
            };
            *atom = match *atom {
                curios_ersd::Atom::Value(value) => curios_ersd::Atom::Value(
                    *compaction
                        .values
                        .get(&value)
                        .unwrap_or_else(|| panic!("compaction dropped bound value {value}")),
                ),
                curios_ersd::Atom::Function(function) => curios_ersd::Atom::Function(
                    *compaction
                        .functions
                        .get(&function)
                        .unwrap_or_else(|| panic!("compaction dropped bound function {function}")),
                ),
                // Constants are interned in a plain vector that compaction never touches.
                constant => constant,
            };
        }
    }

    pub(super) fn bind(&mut self, name: &Free, atom: curios_ersd::Atom) {
        self.values.insert(name.clone(), Binding::Atom(atom));
    }

    pub(super) fn bind_dropped(&mut self, name: &Free) {
        self.values.insert(name.clone(), Binding::Dropped);
    }

    pub(super) fn lookup(&self, name: &Free) -> Option<Binding> {
        self.values.get(name).copied()
    }

    pub(super) fn struct_row(&self, name: &Global) -> Option<&ProductRow> {
        self.struct_decls.get(name)
    }

    pub(super) fn register_struct_row(&mut self, name: &Global, row: ProductRow) {
        self.struct_decls.insert(name.clone(), row);
    }

    pub(super) fn induct_row(&self, name: &Global) -> Option<&FamilyRow> {
        self.induct_decls.get(name)
    }

    pub(super) fn register_induct_row(&mut self, name: &Global, row: FamilyRow) {
        self.induct_decls.insert(name.clone(), row);
    }

    pub(super) fn tuple_schema(&self, width: usize) -> Option<curios_ersd::ProductId> {
        self.tuples.get(&width).copied()
    }

    pub(super) fn register_tuple_schema(&mut self, width: usize, schema: curios_ersd::ProductId) {
        self.tuples.insert(width, schema);
    }
}
