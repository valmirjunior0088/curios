//! The declarations a module's items make, as a walk asks for them: what elaboration finds a declaration by, from a name it is asked for, visiting none it is not asked for.
//!
//! **A declaration is a written item with the items generated from it.** A type former's constructors and a concept's method wrappers name the declaration they were generated from (`DefinitionKind::InductiveConstructor`, `DefinitionKind::ConceptMethod`); each is that declaration's, wherever it sits among the items, and every other item is a declaration of its own. An item generated from a declaration the module does not hold is one of its own too.
//!
//! **Made where the module is made**, and kept with whatever holds the module, so a walk asking for one declaration reads its items and no other's. Its tables are persistent, so the declarations of a version of a module are its module's with those of what the version changed made again.

#[cfg(test)]
mod tests;

use {
    super::{Global, Items, Module},
    curios_utilities::{Persistent, Sorted},
    std::{
        collections::{BTreeMap, BTreeSet, HashSet},
        rc::Rc,
    },
};

/// The declarations a module's items make. See the module documentation.
#[derive(Debug, Clone, Default)]
pub struct Declarations {
    /// For each name an item declares, the item's place, and the declaration's: its written item's place.
    names: Persistent<Global, (usize, usize)>,
    /// Each declaration's items, by its written item's place: the written item first, then those generated from it in the order the items stand.
    items: Sorted<usize, Rc<[usize]>>,
}

impl Declarations {
    /// The declarations `module`'s items make, each item's read off what its place declares: of a stored module, with nothing restored.
    pub fn of(module: &Module) -> Self {
        let items = &module.items;
        let mut written = Persistent::<Global, usize>::new();
        for place in items.places() {
            if items.generated_from(place).is_none() {
                for name in items.names(place) {
                    written.insert(*name, place);
                }
            }
        }
        let mut grouped = BTreeMap::<usize, Vec<usize>>::new();
        for place in items.places() {
            let declaration = items
                .generated_from(place)
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

    /// These declarations, of a module's items, made those of `items`, a version of them: the items at the places `removed` names taken out, each with the names it declared, and those at `placed` put in, each read now. A place an item was replaced at is in both. Every declaration neither touches keeps its entries unread.
    pub fn revise(&mut self, items: &Items, removed: &[(usize, Vec<Global>)], placed: &[usize]) {
        let gone = removed
            .iter()
            .map(|(place, _)| *place)
            .collect::<HashSet<_>>();
        // The items of every declaration an item is taken out of or put into are placed again, but for the ones taken out.
        let mut again = BTreeSet::new();
        let mut taken = Vec::new();
        for (_, names) in removed {
            for name in names {
                if let Some((_, declaration)) = self.names.remove(name) {
                    taken.push(declaration);
                }
            }
        }
        for place in placed {
            again.insert(*place);
            if let Some(owner) = items.generated_from(*place)
                && let Some((_, declaration)) = self.names.get(&owner)
            {
                taken.push(*declaration);
            }
        }
        for declaration in taken {
            let Some(held) = self.items.remove(&declaration) else {
                continue;
            };
            for place in held.iter().filter(|place| !gone.contains(place)) {
                for name in items.names(*place) {
                    self.names.remove(name);
                }
                again.insert(*place);
            }
        }
        // Written items first, so a generated item finds the declaration it was generated from.
        let (generated, written) = again
            .into_iter()
            .partition::<Vec<_>, _>(|place| items.generated_from(*place).is_some());
        for place in written {
            for name in items.names(place) {
                self.names.insert(*name, (place, place));
            }
            self.items.insert(place, Rc::from([place]));
        }
        for place in generated {
            let declaration = items
                .generated_from(place)
                .and_then(|owner| self.names.get(&owner).copied())
                .filter(|(item, declaration)| item == declaration)
                .map_or(place, |(_, declaration)| declaration);
            let mut held = self
                .items
                .get(&declaration)
                .map(|held| held.to_vec())
                .unwrap_or_default();
            held.push(place);
            // The written item first, then the generated ones in the order they stand.
            held.sort_by_key(|held| (*held != declaration, *held));
            held.dedup();
            for name in items.names(place) {
                self.names.insert(*name, (place, declaration));
            }
            self.items.insert(declaration, held.into());
        }
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
