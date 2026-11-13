//! The declarations a module's items make, as a walk asks for them: what elaboration finds a declaration by, from a name it is asked for, visiting none it is not asked for.
//!
//! **A declaration is one item, with every name it declares.** A family's constructors are names its item declares and none of its definitions holds ([`Item::declared_names`](crate::Item::declared_names)), so they are their family's declaration's with no item of their own, and every item is a declaration of its own.
//!
//! **Made where the module is made**, and kept with whatever holds the module, so a walk asking for one declaration reads its items and no other's. Its tables are persistent, so the declarations of a version of a module are its module's with those of what the version changed made again.

#[cfg(test)]
mod tests;

use {
    super::{Definitions, Global, Items},
    curios_common::{Persistent, Sorted},
    std::rc::Rc,
};

/// The declarations a module's items make. See the module documentation.
#[derive(Debug, Clone, Default)]
pub struct Declarations {
    /// For each name an item declares, the item's place, which is its declaration's.
    names: Persistent<Global, usize>,
    /// Each declaration's items, by its item's place: that item alone.
    items: Sorted<usize, Rc<[usize]>>,
}

impl Declarations {
    /// The declarations `module`'s items make, each item's read off what its place declares: of a stored module, with nothing restored.
    pub fn of(definitions: &Definitions) -> Self {
        let mut declarations = Self::default();
        for place in definitions.items.places() {
            declarations.place(&definitions.items, place);
        }

        declarations
    }

    /// These declarations, of a module's items, made those of `items`, a version of them: the items at the places `removed` names taken out, each with the names it declared, and those at `placed` put in, each read now. A place an item was replaced at is in both. Every declaration neither touches keeps its entries unread.
    pub fn revise(&mut self, items: &Items, removed: &[(usize, Vec<Global>)], placed: &[usize]) {
        for (place, names) in removed {
            for name in names {
                self.names.remove(name);
            }
            self.items.remove(place);
        }
        for place in placed {
            self.place(items, *place);
        }
    }

    /// The declaration the item at `place` of `items` is, with every name it declares.
    fn place(&mut self, items: &Items, place: usize) {
        for name in items.names(place) {
            self.names.insert(name, place);
        }
        self.items.insert(place, Rc::from([place]));
    }

    /// The place of the item declaring `name`, where the module declares it.
    pub fn item(&self, name: &Global) -> Option<usize> {
        self.names.get(name).copied()
    }

    /// The declaration declaring `name`, by its item's place, where the module declares it.
    pub fn declaration(&self, name: &Global) -> Option<usize> {
        self.names.get(name).copied()
    }

    /// The places of `declaration`'s items: its item.
    pub fn items(&self, declaration: usize) -> &Rc<[usize]> {
        self.items
            .get(&declaration)
            .expect("a declaration is the module's")
    }

    /// Every declaration, by its item's place, in the order the items stand.
    pub fn each(&self) -> impl Iterator<Item = usize> + '_ {
        self.items.iter().map(|(declaration, _)| *declaration)
    }

    /// Every name the module declares, in no stated order.
    pub fn names(&self) -> impl Iterator<Item = &Global> {
        self.names.iter().map(|(name, _)| name)
    }
}
