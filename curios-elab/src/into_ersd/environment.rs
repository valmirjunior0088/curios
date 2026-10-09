//! The erasure environment: the maps from Core identities to the arena identities they lower to.
//!
//! One environment serves one item's erasure, and its identities are that item's fragment's. Opened binders receive fresh Core labels and top-level names are module-unique, so one flat value map is unambiguous without scope save/restore while the erasure runs; what it hands on keeps the names the item declares and no other ([`Environment::exporting`]). The schema maps hold the fragment's identity for each Core declaration it lowers a row of. A row is its declaration's: the item that declares a structure or an inductive registers the row, from the registry entry, and every other item holds it as an import, a schema of the row's widths that says nothing of its fields and stands, in a program, for the declaration's own. An anonymous row is declared by no item and is each fragment's, one for a width.

use {
    curios_core::{Free, Global},
    std::collections::{BTreeMap, BTreeSet},
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
    /// The declarations whose rows above are imports: another item declares them, and this fragment's schema for one stands for that item's.
    imported: BTreeSet<Global>,
    /// Anonymous tuple schemas, interned by relevant-field width — an arity-`n` product is one untyped layout regardless of which tuple built it.
    tuples: BTreeMap<usize, curios_ersd::ProductId>,
}

impl Environment {
    /// This environment as an item's erasure hands it on: the bindings of the names the item declares, and its schema maps.
    ///
    /// A binder is bound while the walk that opened it reads it and by nothing after, and a name another item declares is bound here only to the import it is read through, which is this fragment's affair. Kept, either would be an identity the next reader could mistake for a declaration of this item.
    pub(super) fn exporting(mut self, declared: &[&Global]) -> Self {
        self.values.retain(|name, _| {
            name.as_global()
                .is_some_and(|global| declared.contains(&global))
        });

        self
    }

    pub(super) fn bind(&mut self, name: &Free, atom: curios_ersd::Atom) {
        self.values.insert(*name, Binding::Atom(atom));
    }

    pub(super) fn bind_dropped(&mut self, name: &Free) {
        self.values.insert(*name, Binding::Dropped);
    }

    pub(super) fn lookup(&self, name: &Free) -> Option<Binding> {
        self.values.get(name).copied()
    }

    /// Every top-level name bound here with what it is bound to, by name.
    pub(super) fn bindings(&self) -> impl Iterator<Item = (&Global, Binding)> {
        self.values
            .iter()
            .filter_map(|(name, binding)| Some((name.as_global()?, *binding)))
    }

    pub(super) fn struct_row(&self, name: &Global) -> Option<&ProductRow> {
        self.struct_decls.get(name)
    }

    /// File the row this fragment's item declares for `name`.
    pub(super) fn register_struct_row(&mut self, name: &Global, row: ProductRow) {
        self.struct_decls.insert(*name, row);
    }

    /// File the row another item declares for `name`, as this fragment holds it.
    pub(super) fn import_struct_row(&mut self, name: &Global, row: ProductRow) {
        self.struct_decls.insert(*name, row);
        self.imported.insert(*name);
    }

    /// The row this fragment's item declares for `name`: what another item's import of it is read off.
    pub(super) fn declared_struct_row(&self, name: &Global) -> Option<&ProductRow> {
        self.struct_decls
            .get(name)
            .filter(|_| !self.imported.contains(name))
    }

    /// Every structure this fragment holds a schema of, by the declaration it is the row of. A newtype's row has none.
    pub(super) fn struct_schemas(&self) -> impl Iterator<Item = (&Global, curios_ersd::ProductId)> {
        self.struct_decls
            .iter()
            .filter_map(|(name, row)| Some((name, row.schema?)))
    }

    pub(super) fn induct_row(&self, name: &Global) -> Option<&FamilyRow> {
        self.induct_decls.get(name)
    }

    /// File the row this fragment's item declares for `name`.
    pub(super) fn register_induct_row(&mut self, name: &Global, row: FamilyRow) {
        self.induct_decls.insert(*name, row);
    }

    /// File the row another item declares for `name`, as this fragment holds it.
    pub(super) fn import_induct_row(&mut self, name: &Global, row: FamilyRow) {
        self.induct_decls.insert(*name, row);
        self.imported.insert(*name);
    }

    /// The row this fragment's item declares for `name`: what another item's import of it is read off.
    pub(super) fn declared_induct_row(&self, name: &Global) -> Option<&FamilyRow> {
        self.induct_decls
            .get(name)
            .filter(|_| !self.imported.contains(name))
    }

    /// Whether this fragment's item declares a row.
    pub(super) fn declares_rows(&self) -> bool {
        self.struct_decls
            .keys()
            .chain(self.induct_decls.keys())
            .any(|name| !self.imported.contains(name))
    }

    /// The declarations whose rows this fragment holds as imports: the items a program links before it.
    pub(super) fn imported_rows(&self) -> impl Iterator<Item = &Global> {
        self.imported.iter()
    }

    /// Every inductive this fragment holds the family of, by its declaration.
    pub(super) fn families(&self) -> impl Iterator<Item = (&Global, curios_ersd::FamilyId)> {
        self.induct_decls
            .iter()
            .map(|(name, row)| (name, row.family))
    }

    pub(super) fn tuple_schema(&self, width: usize) -> Option<curios_ersd::ProductId> {
        self.tuples.get(&width).copied()
    }

    pub(super) fn register_tuple_schema(&mut self, width: usize, schema: curios_ersd::ProductId) {
        self.tuples.insert(width, schema);
    }

    /// Every anonymous row this fragment holds, by its width.
    pub(super) fn tuple_schemas(&self) -> impl Iterator<Item = (usize, curios_ersd::ProductId)> {
        self.tuples.iter().map(|(width, schema)| (*width, *schema))
    }
}
