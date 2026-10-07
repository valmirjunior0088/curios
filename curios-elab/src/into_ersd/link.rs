//! Linking a program: the erased forms it reaches, appended to one module in the order their items evaluate.
//!
//! **A program links what it reaches.** Its own items, its entry, and of its scope the items those import, transitively. An item of the scope the program never names is no part of the module the back half lowers, so a program is not handed a library to drop.
//!
//! **And what computes at initialization, whether or not it is named.** A top-level value whose evaluation is observable runs before the entry in every program its unit is in scope of, which is the language's meaning and not this pass's to change. Whether one is observable is read off the linked program by the optimizer's prune, which holds the summaries; here every item whose top-level statement computes at all is linked with what it reaches, and the prune keeps or drops each exactly as it does an item of the program's own.
//!
//! **The order is the scope's.** Units in dependency order, each one's items in the order it stores them, then the program's own and its entry: every item after the items it imports from, which is the order their statements evaluate in.
//!
//! **A schema is one in a program however many fragments hold it.** A fragment's copy of a structure's or an inductive's row is known by the declaration it is the row of, and an anonymous row by its width; the first fragment linked that holds one registers it, and every later one is resolved to it.

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
        let environment = erased.environment();
        let mut resolution = curios_ersd::Resolution {
            values: erased.values().iter().map(bound).collect(),
            functions: erased
                .functions()
                .iter()
                .map(|name| match bound(name) {
                    curios_ersd::Atom::Function(function) => function,
                    atom => panic!("{name} is imported as a function and bound to {atom:?}"),
                })
                .collect(),
            ..Default::default()
        };
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

        let linked = self.module.link(erased.module(), &resolution);

        for (name, schema) in environment.struct_schemas() {
            self.structs.entry(*name).or_insert(linked.product(schema));
        }
        for (width, schema) in environment.tuple_schemas() {
            self.tuples.entry(width).or_insert(linked.product(schema));
        }
        for (name, family) in environment.families() {
            self.families.entry(*name).or_insert(linked.family(family));
        }
        for (name, binding) in environment.bindings() {
            if let Binding::Atom(atom) = binding {
                self.bound.insert(*name, linked.atom(atom));
            }
        }
    }
}

/// The items of each unit in scope a program links: what `roots` import, transitively, and every item that computes at initialization with what it imports — each unit's as the places of its items, ascending.
fn reached<'a>(
    scope: Erasures<'_>,
    roots: impl Iterator<Item = &'a ErasedItem>,
) -> Vec<BTreeSet<usize>> {
    let units = scope.units();
    let mut reached = vec![BTreeSet::new(); units.len()];
    let mut pending = Vec::new();

    let imports = |erased: &ErasedItem, pending: &mut Vec<(usize, usize)>| {
        for name in erased.values().iter().chain(erased.functions()) {
            // A name no unit in scope declares is the program's own, and every item of the program is linked.
            if let Some(place) = scope.place(name) {
                pending.push(place);
            }
        }
    };
    for root in roots {
        imports(root, &mut pending);
    }
    for (unit, erased) in units.iter().enumerate() {
        pending.extend(erased.computing().map(|place| (unit, place)));
    }
    while let Some((unit, place)) = pending.pop() {
        if reached[unit].insert(place) {
            imports(&units[unit].items()[place], &mut pending);
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
        reached.iter().map(BTreeSet::len).sum::<usize>()
    );

    let mut linking = Linking::default();
    for (unit, places) in scope.units().iter().zip(&reached) {
        for &place in places {
            linking.add(&unit.items()[place]);
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
