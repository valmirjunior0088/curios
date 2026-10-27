//! The erasure environment: the maps from Core identities to the arena identities they lower to.
//!
//! One environment serves one form of one item's erasure, and its identities are that form's fragment's. Opened binders receive fresh Core labels and top-level names are module-unique, so one flat value map is unambiguous without scope save/restore while the erasure runs; what it hands on keeps the names the item declares and no other ([`Environment::exporting`]). A name another item declares is bound at the sorts it is read at, since each of that item's forms binds it apart; the item's own names and rows are its form's, bound at no sorts, so two assignments that erase alike make one fragment. The schema maps hold the fragment's identity for each Core declaration it lowers a row of. A row is its declaration's: the item that declares a structure or an inductive registers the row, from the registry entry, with its masks; every other item holds it as an import of what it reads, which stands, in a program, for the declaration's own — a structure's schema at its width, an inductive's family and the constructors the item names, each at its position — and reads a mask where the declaring item holds it. An anonymous row is declared by no item and is each fragment's, one for a width.

use {
    crate::{AtSorts, Sorts},
    std::collections::BTreeMap,
};

/// What a Core name erases to.
#[derive(Debug, Clone, Copy)]
#[curios_archive::archived]
pub enum Binding {
    /// The operand holding the name's erased value.
    Atom(curios_anf::Atom),
    /// A dropped (erasable) binder — a proof or a type with no runtime value. Referencing it yields the unit constant and records the dangle for the function-body collapse.
    Dropped,
}

/// A registered structure's layout. The mask is the declaration's opaque signature mask (one flag per declared field, `true` where the field is erased), computed once with the parameters abstract, so construction and projection agree on the relevant-field arithmetic at every instantiation. `schema` is `None` for a newtype — a single relevant field collapses to its bare value, with no product node.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ProductRow {
    pub schema: Option<curios_anf::ProductId>,
    pub mask: Vec<bool>,
}

impl ProductRow {
    /// Whether a product of `relevant` surviving fields collapses to the bare field — the newtype rule. Construction, projection, and schema registration must all take the same branch, so the `== 1` lives here and nowhere else.
    pub fn collapses(relevant: usize) -> bool {
        relevant == 1
    }
}

/// A registered constructor: its arena identity and the declaration's opaque signature mask over its payload fields (`true` where a field is erased).
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ConstructorRow {
    pub id: curios_anf::ConstructorId,
    pub mask: Vec<bool>,
}

/// A registered inductive: its variant family and its constructors in runtime-tag (registry) order.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct FamilyRow {
    pub family: curios_anf::FamilyId,
    pub constructors: Vec<ConstructorRow>,
}

/// An inductive's row as an item that imports it holds it: the family, and each constructor the item names, by its position in the declaration.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct ImportedFamily {
    pub family: curios_anf::FamilyId,
    pub constructors: BTreeMap<usize, curios_anf::ConstructorId>,
}

#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Environment {
    /// What each name is bound to, at the sorts it is read at: a binder and a name the item declares at none.
    values: BTreeMap<(curios_core::Free, Sorts), Binding>,
    /// The rows this fragment's item declares, by declaration and sorts: its own at none.
    struct_decls: BTreeMap<AtSorts, ProductRow>,
    induct_decls: BTreeMap<AtSorts, FamilyRow>,
    /// The rows another item declares, as this fragment imports them at the sorts it reads them at: a structure's schema, which a newtype has none of, and an inductive's family with the constructors named of it. Each stands for that item's form's.
    struct_imports: BTreeMap<AtSorts, Option<curios_anf::ProductId>>,
    induct_imports: BTreeMap<AtSorts, ImportedFamily>,
    /// Anonymous tuple schemas, interned by relevant-field width — an arity-`n` product is one untyped layout regardless of which tuple built it.
    tuples: BTreeMap<usize, curios_anf::ProductId>,
}

impl Environment {
    /// This environment as an item's erasure hands it on: the bindings of the names the item declares, and its schema maps.
    ///
    /// A binder is bound while the walk that opened it reads it and by nothing after, and a name another item declares is bound here only to the import it is read through, which is this fragment's affair. Kept, either would be an identity the next reader could mistake for a declaration of this item.
    pub fn exporting(mut self, declared: &[&curios_core::Global]) -> Self {
        self.values.retain(|(name, sorts), _| {
            sorts.is_none()
                && name
                    .as_global()
                    .is_some_and(|global| declared.contains(&global))
        });

        self
    }

    /// Bind `name`, a binder or a name the item declares, which stands at no sorts.
    pub fn bind(&mut self, name: &curios_core::Free, atom: curios_anf::Atom) {
        self.bind_at(name, Sorts::NONE, atom);
    }

    pub fn bind_dropped(&mut self, name: &curios_core::Free) {
        self.bind_dropped_at(name, Sorts::NONE);
    }

    /// Bind `name` as it is read at `sorts`.
    pub fn bind_at(&mut self, name: &curios_core::Free, sorts: Sorts, atom: curios_anf::Atom) {
        self.values.insert((*name, sorts), Binding::Atom(atom));
    }

    pub fn bind_dropped_at(&mut self, name: &curios_core::Free, sorts: Sorts) {
        self.values.insert((*name, sorts), Binding::Dropped);
    }

    pub fn lookup(&self, name: &curios_core::Free, sorts: Sorts) -> Option<Binding> {
        self.values.get(&(*name, sorts)).copied()
    }

    /// Every top-level name bound here at no sorts, which in an exported environment is every name the item declares, with what it is bound to.
    pub fn bindings(&self) -> impl Iterator<Item = (&curios_core::Global, Binding)> {
        self.values
            .iter()
            .filter(|((_, sorts), _)| sorts.is_none())
            .filter_map(|((name, _), binding)| Some((name.as_global()?, *binding)))
    }

    /// File the row this fragment declares for `name` at `sorts`: its own item's at none.
    pub fn register_struct_row(&mut self, name: AtSorts, row: ProductRow) {
        self.struct_decls.insert(name, row);
    }

    /// File the schema of the row another item declares for `name`, as this fragment imports it: `None` for a newtype, which has none.
    pub fn import_struct_row(&mut self, name: AtSorts, schema: Option<curios_anf::ProductId>) {
        self.struct_imports.insert(name, schema);
    }

    /// The row this fragment declares for `name`: what another item's import of it is read off.
    pub fn declared_struct_row(&self, name: AtSorts) -> Option<&ProductRow> {
        self.struct_decls.get(&name)
    }

    /// The schema this fragment imports the structure `name`'s row as, where it imports it: `Some(None)` for a newtype.
    pub fn imported_struct_row(&self, name: AtSorts) -> Option<Option<curios_anf::ProductId>> {
        self.struct_imports.get(&name).copied()
    }

    /// Every structure this fragment holds a schema of, declared or imported, by the declaration it is the row of. A newtype's row has none.
    pub fn struct_schemas(&self) -> impl Iterator<Item = (Origin, AtSorts, curios_anf::ProductId)> {
        self.struct_decls
            .iter()
            .map(|(name, row)| (Origin::Declared, *name, row.schema))
            .chain(
                self.struct_imports
                    .iter()
                    .map(|(name, schema)| (Origin::Imported, *name, *schema)),
            )
            .filter_map(|(origin, name, schema)| Some((origin, name, schema?)))
    }

    /// File the row this fragment declares for `name` at `sorts`: its own item's at none.
    pub fn register_induct_row(&mut self, name: AtSorts, row: FamilyRow) {
        self.induct_decls.insert(name, row);
    }

    /// File the family of the row another item declares for `name`, as this fragment imports it, with none of its constructors yet.
    pub fn import_induct_family(&mut self, name: AtSorts, family: curios_anf::FamilyId) {
        self.induct_imports.insert(
            name,
            ImportedFamily {
                family,
                constructors: BTreeMap::new(),
            },
        );
    }

    /// File the constructor at `position` of the imported inductive `name`, as this fragment imports it.
    pub fn import_induct_constructor(
        &mut self,
        name: AtSorts,
        position: usize,
        constructor: curios_anf::ConstructorId,
    ) {
        self.induct_imports
            .get_mut(&name)
            .expect("a constructor is imported with its family")
            .constructors
            .insert(position, constructor);
    }

    /// The inductive `name`'s row as this fragment imports it, where it does.
    pub fn imported_induct_row(&self, name: AtSorts) -> Option<&ImportedFamily> {
        self.induct_imports.get(&name)
    }

    /// The row of each inductive this item declares.
    #[cfg(feature = "test-support")]
    pub fn declared_induct_rows(&self) -> impl Iterator<Item = &FamilyRow> {
        self.induct_decls.values()
    }

    /// The row this fragment declares for `name`: what another item's import of it is read off.
    pub fn declared_induct_row(&self, name: AtSorts) -> Option<&FamilyRow> {
        self.induct_decls.get(&name)
    }

    /// Whether this fragment's item declares a row.
    pub fn declares_rows(&self) -> bool {
        !self.struct_decls.is_empty() || !self.induct_decls.is_empty()
    }

    /// The declarations, at their sorts, whose rows this fragment holds as imports: the forms a program links before it.
    pub fn imported_rows(&self) -> impl Iterator<Item = &AtSorts> {
        self.struct_imports.keys().chain(self.induct_imports.keys())
    }

    /// Every inductive this fragment holds the family of, declared or imported, by its declaration and sorts.
    pub fn families(&self) -> impl Iterator<Item = (Origin, AtSorts, curios_anf::FamilyId)> {
        self.induct_decls
            .iter()
            .map(|(name, row)| (Origin::Declared, *name, row.family))
            .chain(
                self.induct_imports
                    .iter()
                    .map(|(name, row)| (Origin::Imported, *name, row.family)),
            )
    }

    pub fn tuple_schema(&self, width: usize) -> Option<curios_anf::ProductId> {
        self.tuples.get(&width).copied()
    }

    pub fn register_tuple_schema(&mut self, width: usize, schema: curios_anf::ProductId) {
        self.tuples.insert(width, schema);
    }

    /// Every anonymous row this fragment holds, by its width.
    pub fn tuple_schemas(&self) -> impl Iterator<Item = (usize, curios_anf::ProductId)> {
        self.tuples.iter().map(|(width, schema)| (*width, *schema))
    }
}

/// Whether a fragment declares a row or imports another item's: a declared row at no sorts is the fragment's own item's, which a program links under each assignment the form stands for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Origin {
    Declared,
    Imported,
}
