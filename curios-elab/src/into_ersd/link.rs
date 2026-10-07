//! Linking a program: the erased forms it reaches, appended to one module in the order their items evaluate.
//!
//! **A program links what it reaches.** Its own items, its entry, and of its scope the items those import, transitively. An item of the scope the program never names is no part of the module the back half lowers, so a program is not handed a library to drop.
//!
//! **And what computes at initialization, whether or not it is named.** A top-level value whose evaluation is observable runs before the entry in every program its unit is in scope of, which is the language's meaning and not this pass's to change. Whether one is observable is read off the linked program by the optimizer's prune, which holds the summaries; here every item whose top-level statement computes at all is linked with what it reaches, and the prune keeps or drops each exactly as it does an item of the program's own.
//!
//! **The order is the scope's.** Units in dependency order, each one's items in the order it stores them, then the program's own and its entry: every item after the items it imports from, which is the order their statements evaluate in.
//!
//! **A schema is its declaration's.** The item that declares a structure or an inductive registers its row, and a fragment that imports the row is linked after that item's rows are, its schema resolved to the one the item registered. Only the rows: a declaration a program constructs or matches values of, and names nowhere else, contributes its schemas and none of what its own item erased to ([`curios_ersd::Module::link_rows`]). An anonymous row is declared by no item: the first fragment linked that holds one of a width registers it, and every later one is resolved to it.

use {
    super::{Binding, ErasedItem, ErasedProgram, ErasedUnit, Erasures},
    crate::Error,
    curios_core::Global,
    std::collections::{BTreeMap, BTreeSet},
};

/// A program under construction: the module, and where each name and each schema linked so far landed.
#[derive(Default)]
struct Linking {
    module: curios_ersd::Module,
    bound: BTreeMap<Global, curios_ersd::Atom>,
    structs: BTreeMap<Global, curios_ersd::ProductId>,
    families: BTreeMap<Global, curios_ersd::FamilyId>,
    tuples: BTreeMap<usize, curios_ersd::ProductId>,
}

impl Linking {
    /// Append one item's fragment, its imports standing for where the items that bind them landed.
    fn add(&mut self, erased: &ErasedItem) {
        let bound = |name: &Global| {
            *self.bound.get(name).unwrap_or_else(|| {
                panic!("{name} is imported before the item that binds it is linked")
            })
        };
        let resolution = curios_ersd::Resolution {
            values: erased.values().iter().map(bound).collect(),
            functions: erased
                .functions()
                .iter()
                .map(|name| match bound(name) {
                    curios_ersd::Atom::Function(function) => function,
                    atom => panic!("{name} is imported as a function and bound to {atom:?}"),
                })
                .collect(),
            ..self.held(erased)
        };

        let linked = self.module.link(erased.module(), &resolution);

        self.hold(erased, &linked);
        for (name, binding) in erased.environment().bindings() {
            if let Binding::Atom(atom) = binding {
                self.bound.insert(*name, linked.atom(atom));
            }
        }
    }

    /// Register one item's schemas and nothing else of its fragment: the rows a linked fragment imports from a declaration the program never reaches.
    fn add_rows(&mut self, erased: &ErasedItem) {
        let resolution = self.held(erased);
        let linked = self.module.link_rows(erased.module(), &resolution);

        self.hold(erased, &linked);
    }

    /// Which of `erased`'s schemas the program already holds, each with where: a row by the declaration it is the row of, an anonymous one by its width.
    fn held(&self, erased: &ErasedItem) -> curios_ersd::Resolution {
        let environment = erased.environment();
        for name in environment.imported_rows() {
            // A newtype's row has no schema, so nothing of it is resolved.
            let held = match environment.struct_row(name) {
                Some(row) => row.schema.is_none() || self.structs.contains_key(name),
                None => self.families.contains_key(name),
            };
            assert!(
                held,
                "the row of {name} is imported before the rows of the item that declares it are linked",
            );
        }

        let mut resolution = curios_ersd::Resolution::default();
        for (name, schema) in environment.struct_schemas() {
            if let Some(&held) = self.structs.get(name) {
                resolution.products.insert(schema, held);
            }
        }
        for (width, schema) in environment.tuple_schemas() {
            if let Some(&held) = self.tuples.get(&width) {
                resolution.products.insert(schema, held);
            }
        }
        for (name, family) in environment.families() {
            if let Some(&held) = self.families.get(name) {
                resolution.families.insert(family, held);
            }
        }

        resolution
    }

    /// Record where each schema of `erased` landed, for the fragments linked after it.
    fn hold(&mut self, erased: &ErasedItem, linked: &curios_ersd::Linked) {
        let environment = erased.environment();
        for (name, schema) in environment.struct_schemas() {
            self.structs.entry(*name).or_insert(linked.product(schema));
        }
        for (width, schema) in environment.tuple_schemas() {
            self.tuples.entry(width).or_insert(linked.product(schema));
        }
        for (name, family) in environment.families() {
            self.families.entry(*name).or_insert(linked.family(family));
        }
    }
}

/// What a program takes of each unit in scope, as the places of its items, ascending.
struct Reached {
    /// The items it links.
    items: Vec<BTreeSet<usize>>,
    /// The items whose rows it links: the declarations of the rows a linked item imports, with theirs.
    rows: Vec<BTreeSet<usize>>,
}

/// The unit and the place of the item declaring each of `names` that a unit in scope declares. A name none declares is the program's own, and every item of the program is linked.
fn places<'n>(scope: Erasures<'_>, names: impl Iterator<Item = &'n Global>) -> Vec<(usize, usize)> {
    names.filter_map(|name| scope.place(name)).collect()
}

/// What a program takes of each unit in scope: the items `roots` import, transitively, and every item that computes at initialization with what it imports; and, of the declarations whose rows any of those import, the rows.
fn reached<'a>(scope: Erasures<'_>, roots: impl Iterator<Item = &'a ErasedItem>) -> Reached {
    let units = scope.units();
    let mut reached = Reached {
        items: vec![BTreeSet::new(); units.len()],
        rows: vec![BTreeSet::new(); units.len()],
    };
    let mut items = Vec::new();
    let mut rows = Vec::new();

    for root in roots {
        items.extend(places(scope, root.values().iter().chain(root.functions())));
        rows.extend(places(scope, root.environment().imported_rows()));
    }
    for (unit, erased) in units.iter().enumerate() {
        items.extend(erased.computing().map(|place| (unit, place)));
    }
    while let Some((unit, place)) = items.pop() {
        if reached.items[unit].insert(place) {
            let erased = &units[unit].items()[place];
            items.extend(places(
                scope,
                erased.values().iter().chain(erased.functions()),
            ));
            rows.extend(places(scope, erased.environment().imported_rows()));
        }
    }
    // A row's fields name other rows, which the declaring fragment imports in its turn.
    while let Some((unit, place)) = rows.pop() {
        if reached.rows[unit].insert(place) {
            let erased = &units[unit].items()[place];
            rows.extend(places(scope, erased.environment().imported_rows()));
        }
    }

    reached
}

/// Link a program: `own`, its module's erased items, and `entry`, its entry's, with what they reach of `scope` — verified whole, as the module the back half lowers.
pub(super) fn link(
    scope: Erasures<'_>,
    own: &ErasedUnit,
    entry: &ErasedItem,
) -> Result<ErasedProgram, Error> {
    curios_profile::profile!("link_program");
    let reached = reached(scope, own.items().iter().chain([entry]));
    curios_profile::sample!(
        "link::computing",
        scope
            .units()
            .iter()
            .map(|unit| unit.computing().count())
            .sum::<usize>()
    );
    curios_profile::sample!(
        "link::reached",
        reached.items.iter().map(BTreeSet::len).sum::<usize>()
    );

    let mut linking = Linking::default();
    for (unit, (items, rows)) in scope
        .units()
        .iter()
        .zip(reached.items.iter().zip(&reached.rows))
    {
        // In the order the unit stores its items, which puts a declaration ahead of whatever imports its rows.
        for &place in items.union(rows) {
            let erased = &unit.items()[place];
            match items.contains(&place) {
                true => linking.add(erased),
                false => linking.add_rows(erased),
            }
        }
    }
    for erased in own.items() {
        linking.add(erased);
    }
    linking.add(entry);

    let Linking { module, bound, .. } = linking;
    // Every fragment was held to the recursion rules where it was erased: its own groups by its verification, what it applies of another through that function's interface, and a read of a value whose initializer reads itself where the read was erased. So nothing here is expected to be refused, and a refusal would name nothing a span was kept for.
    module
        .verify()
        .map_err(|error| Error::refused_by_verifier(error, &BTreeMap::new()))?;

    Ok(ErasedProgram::new(module, bound))
}
