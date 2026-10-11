//! Linking a program: the erased forms it reaches, appended to one module in the order their items evaluate.
//!
//! **A program links what it reaches.** Its own items, its entry, and of its scope the items those import, transitively. An item of the scope the program never names is no part of the module the back half lowers, so a program is not handed a library to drop.
//!
//! **And what computes at initialization, whether or not it is named.** A top-level value whose evaluation is observable runs before the entry in every program its unit is in scope of, which is the language's meaning and not this pass's to change. What one computes was computed where its unit was erased, wherever that could be finished (`evaluate`), and an item it was finished for computes nothing; here every item whose top-level statement still computes is linked with what it reaches, and the optimizer's prune, which holds the summaries, keeps or drops each exactly as it does an item of the program's own.
//!
//! **The order is the scope's.** Units in dependency order, each one's items in the order it stores them, then the program's own and its entry: every item after the items it imports from, which is the order their statements evaluate in.
//!
//! **A schema is its declaration's.** The item that declares a structure or an inductive registers its row, and a fragment that imports the row is linked after that item's rows are, its schema resolved to the one the item registered. Only the rows: a declaration a program constructs or matches values of, and names nowhere else, contributes its schemas and none of what its own item erased to ([`curios_ersd::Module::link_rows`]). An anonymous row is declared by no item: the first fragment linked that holds one of a width registers it, and every later one is resolved to it.

use {
    super::{Binding, ErasedItem, ErasedNames, ErasedProgram, ErasedUnit, Erasures},
    crate::Error,
    curios_core::Global,
    std::collections::{BTreeMap, BTreeSet},
};

/// A program under construction: the module, and where each name and each schema linked so far landed.
#[derive(Default)]
pub(super) struct Linking {
    pub(super) module: curios_ersd::Module,
    bound: BTreeMap<Global, curios_ersd::Atom>,
    /// The name each value and each function a linked item bound is declared under: the first to bind it, which is the item that defines it, since an item that hands another's name on is linked after it.
    pub(super) values: BTreeMap<curios_ersd::ValueId, Global>,
    pub(super) functions: BTreeMap<curios_ersd::FunctionId, Global>,
    /// The functions a linked item bound a name to as what its evaluation computed: closures, each bound under a name and defined by no declaration.
    computed: BTreeSet<curios_ersd::FunctionId>,
    pub(super) structs: BTreeMap<Global, curios_ersd::ProductId>,
    pub(super) families: BTreeMap<Global, curios_ersd::FamilyId>,
    pub(super) tuples: BTreeMap<usize, curios_ersd::ProductId>,
}

impl Linking {
    /// Append what `reached` takes of `units`, in the order of the units and, of each, in the order it stores its items, which puts a declaration ahead of whatever imports its rows.
    pub(super) fn add_reached(&mut self, units: &[&ErasedUnit], reached: &Reached) {
        for (unit, (items, rows)) in units.iter().zip(reached.items.iter().zip(&reached.rows)) {
            for &place in items.union(rows) {
                let erased = unit.item(place);
                match items.contains(&place) {
                    true => {
                        self.add(erased);
                    }
                    false => self.add_rows(erased),
                }
            }
        }
    }

    /// Append one item's fragment, its imports standing for where the items that bind them landed, and hand back where its identities landed.
    pub(super) fn add(&mut self, erased: &ErasedItem) -> curios_ersd::Linked {
        let bound = |name: &Global| {
            *self.bound.get(name).unwrap_or_else(|| {
                panic!(
                    "{} is imported before the item that binds it is linked",
                    name.faithful()
                )
            })
        };
        let resolution = curios_ersd::Resolution {
            values: erased.values().iter().map(bound).collect(),
            functions: erased
                .functions()
                .iter()
                .map(|name| match bound(name) {
                    curios_ersd::Atom::Function(function) => function,
                    atom => panic!(
                        "{} is imported as a function and bound to {atom:?}",
                        name.faithful()
                    ),
                })
                .collect(),
            ..self.held(erased)
        };

        let linked = self.module.link(erased.module(), &resolution);

        self.hold(erased, &linked);
        for (name, binding) in erased.environment().bindings() {
            if let Binding::Atom(atom) = binding {
                let atom = linked.atom(atom);
                self.bound.insert(*name, atom);
                match atom {
                    curios_ersd::Atom::Value(value) => {
                        self.values.entry(value).or_insert(*name);
                    }
                    curios_ersd::Atom::Function(function) => {
                        self.functions.entry(function).or_insert(*name);
                        if erased.computed(name) {
                            self.computed.insert(function);
                        }
                    }
                    curios_ersd::Atom::Constant(_) => {}
                }
            }
        }

        linked
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
                "the row of {} is imported before the rows of the item that declares it are linked",
                name.faithful()
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

/// What a program takes of each unit it is linked from, as the places of its items, ascending.
pub(super) struct Reached {
    /// The items it links.
    pub(super) items: Vec<BTreeSet<usize>>,
    /// The items whose rows it links: the declarations of the rows a linked item imports, with theirs.
    rows: Vec<BTreeSet<usize>>,
}

/// The unit and the place of the item declaring each of `names` that one of `units` declares. A name none declares is the program's own, and every item of the program is linked.
fn places<'n>(
    units: &[&ErasedUnit],
    names: impl Iterator<Item = &'n Global>,
) -> Vec<(usize, usize)> {
    names
        .filter_map(|name| {
            units
                .iter()
                .enumerate()
                .find_map(|(unit, erased)| Some((unit, erased.place(name)?)))
        })
        .collect()
}

/// What `roots` take of each of `units`: the items they import, transitively, and, where `computing`, every item that computes at initialization with what it imports; and, of the declarations whose rows any of those import, the rows.
pub(super) fn reached<'a>(
    units: &[&ErasedUnit],
    roots: impl Iterator<Item = &'a ErasedItem>,
    computing: bool,
) -> Reached {
    let mut reached = Reached {
        items: vec![BTreeSet::new(); units.len()],
        rows: vec![BTreeSet::new(); units.len()],
    };
    let mut items = Vec::new();
    let mut rows = Vec::new();

    for root in roots {
        items.extend(places(units, root.values().iter().chain(root.functions())));
        rows.extend(places(units, root.environment().imported_rows()));
    }
    if computing {
        for (unit, erased) in units.iter().enumerate() {
            items.extend(erased.computing().map(|place| (unit, place)));
        }
    }
    while let Some((unit, place)) = items.pop() {
        if reached.items[unit].insert(place) {
            let erased = units[unit].item(place);
            items.extend(places(
                units,
                erased.values().iter().chain(erased.functions()),
            ));
            rows.extend(places(units, erased.environment().imported_rows()));
        }
    }
    // A row's fields name other rows, which the declaring fragment imports in its turn.
    while let Some((unit, place)) = rows.pop() {
        if reached.rows[unit].insert(place) {
            let erased = units[unit].item(place);
            rows.extend(places(units, erased.environment().imported_rows()));
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
    let reached = reached(scope.units(), own.forms().chain([entry]), true);
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
    linking.add_reached(scope.units(), &reached);
    for erased in own.forms() {
        linking.add(erased);
    }
    linking.add(entry);

    let Linking {
        module,
        functions,
        computed,
        ..
    } = linking;
    // A function is its definition's where a declaration defines it: the first name bound to it, and none where an item computed it.
    let defined = functions
        .into_iter()
        .filter(|(function, _)| !computed.contains(function))
        .map(|(function, name)| (name, function))
        .collect();
    // Every fragment was held to the recursion rules where it was erased: its own groups by its verification, what it applies of another through that function's interface, and a read of a value whose initializer reads itself where the read was erased. So nothing here is expected to be refused, and a refusal would name nothing an extent was kept for.
    module.verify().map_err(|error| {
        Error::refused_by_verifier(error, &BTreeMap::new(), &ErasedNames::default())
    })?;

    Ok(ErasedProgram::new(module, defined))
}
