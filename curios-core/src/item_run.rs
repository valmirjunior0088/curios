//! Elaborated items stored together: what a pack keeps of the declarations it holds, one table of nodes for all of them, and what each is named by, its form consed alone.
//!
//! **One table for the items stored together.** A structure two declarations spell is one node of the run, restored once for both ([`ItemRun::of`]): consed alone, `/std`'s elaborated items take 16.72 MB against 9.27 MB in one table, and hold 2.18 times the distinct nodes.
//!
//! **Named by what it is alone.** Which items a run holds follows how they were filed, so an item is named by the digest of its form consed in a table of its own ([`ItemRun::identity`]), which says nothing of its neighbours: a unit compiled whole and the same unit compiled over a baseline name each item alike, wherever each was stored.

#[cfg(test)]
mod tests;

use {
    crate::{Item, Sharing},
    curios_utilities::Digest,
};

/// Elaborated items consed in one table, as a pack keeps them: each a member of the run, by its place in it.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct ItemRun {
    pub items: Vec<Item>,
}

impl ItemRun {
    /// The bytes of a run of `items`, in their order, consed in one table: one node for each structure as it is spelled, under no position ([`Item::shared`]).
    pub fn of<'i>(items: impl IntoIterator<Item = &'i Item>) -> Result<Vec<u8>, String> {
        let sharing = Sharing::new();
        let run = ItemRun {
            items: items
                .into_iter()
                .map(|item| item.shared(&sharing))
                .collect(),
        };

        curios_archive::to_bytes(&run).map(|bytes| bytes.to_vec())
    }

    /// What `item` is named by among the objects of a store: the digest of its form consed in a table of its own and archived alone.
    pub fn identity(item: &Item) -> Result<Digest, String> {
        curios_archive::to_bytes(&item.shared(&Sharing::new())).map(|bytes| Digest::of(&bytes))
    }
}
