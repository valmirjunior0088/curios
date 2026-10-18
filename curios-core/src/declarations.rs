//! The declarations a module's items make, as a walk asks for them: what elaboration finds a declaration by, from a name it is asked for, visiting none it is not asked for.
//!
//! **A declaration is a written item with the items generated from it.** A type former's constructors and a concept's method wrappers name the declaration they were generated from (`DefinitionKind::InductiveConstructor`, `DefinitionKind::ConceptMethod`); each is that declaration's, wherever it sits among the items, and every other item is a declaration of its own. An item generated from a declaration the module does not hold is one of its own too.
//!
//! **Made where the module is made**, and kept with whatever holds the module, so a walk asking for one declaration reads its items and no other's. Its tables are persistent, so the declarations of a version of a module are its module's with those of what the version changed made again.

#[cfg(test)]
mod tests;

use {
    super::{DefinitionKind, Global, Item, Module},
    curios_utilities::{Persistent, Sorted},
    std::{collections::BTreeMap, rc::Rc},
};

/// The declarations a module's items make. See the module documentation.
#[derive(Debug, Clone, Default)]
pub struct Declarations {
    /// For each name an item declares, the item's place, and the declaration's: its written item's place.
    names: Persistent<Global, (usize, usize)>,
    /// Each declaration's items, by its written item's place: the written item first, then those generated from it in the order the items stand.
    items: Sorted<usize, Rc<[usize]>>,
}

/// The declaration `item` was generated from, by the name it declares: a constructor's former, a method wrapper's concept. `None` for a written item.
fn generated_from(item: &Item) -> Option<Global> {
    let kind = match item {
        Item::Let(definition) => &definition.kind,
        Item::Rec(rec) => &rec.definitions[0].kind,
    };

    match kind {
        DefinitionKind::InductiveConstructor { owner, .. }
        | DefinitionKind::ConceptMethod { owner } => Some(Global::Authored(*owner)),
        _ => None,
    }
}

impl Declarations {
    /// The declarations `module`'s items make.
    pub fn of(module: &Module) -> Self {
        let items = &module.items;
        let mut written = Persistent::<Global, usize>::new();
        for place in items.places() {
            if generated_from(&items[place]).is_none() {
                for name in items.names(place) {
                    written.insert(*name, place);
                }
            }
        }
        let mut grouped = BTreeMap::<usize, Vec<usize>>::new();
        for place in items.places() {
            let declaration = generated_from(&items[place])
                .and_then(|owner| written.get(&owner).copied())
                .unwrap_or(place);
            grouped.entry(declaration).or_default().push(place);
        }

        let mut declarations = Self::default();
        for (declaration, mut places) in grouped {
            // The written item first, then the generated ones in the order they stand.
            places.sort_by_key(|place| (*place != declaration, *place));
            for place in &places {
                for name in items.names(*place) {
                    declarations.names.insert(*name, (*place, declaration));
                }
            }
            declarations.items.insert(declaration, places.into());
        }

        declarations
    }

    /// The place of the item declaring `name`, where the module declares it.
    pub fn item(&self, name: &Global) -> Option<usize> {
        self.names.get(name).map(|(item, _)| *item)
    }

    /// The declaration declaring `name`, by its written item's place, where the module declares it.
    pub fn declaration(&self, name: &Global) -> Option<usize> {
        self.names.get(name).map(|(_, declaration)| *declaration)
    }

    /// The places of `declaration`'s items, its written item first and the items generated from it after, in the order they stand.
    pub fn items(&self, declaration: usize) -> &Rc<[usize]> {
        self.items
            .get(&declaration)
            .expect("a declaration is the module's")
    }

    /// Every declaration, by its written item's place, in the order those items stand.
    pub fn each(&self) -> impl Iterator<Item = usize> + '_ {
        self.items.iter().map(|(declaration, _)| *declaration)
    }

    /// Every name the module declares, in no stated order.
    pub fn names(&self) -> impl Iterator<Item = &Global> {
        self.names.iter().map(|(name, _)| name)
    }
}
