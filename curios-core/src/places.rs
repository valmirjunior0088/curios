//! Where a module holds each name it defines, made once for the module.
//!
//! A finished module is read by name: a later unit's elaboration asks for a definition's type, the certifier for what a name unfolds to, erasure for how one is bound. [`Places`] says where a name is held — the item that declares it and, for a group's member, which member — and is made once for a module and kept with whatever holds the module, so no reader keys a module again.
//!
//! **Looked up, never walked.** The table is a hash's, and nothing reads its order.
//!
//! **One place for a name.** A finished module declares each name once: lowering refuses a second declaration and the certifier a module that carries one. A table made of a module that did would hold the later declaration and say nothing of the earlier, to every reader alike, so such a module is its builder's fault and has no places — asserted, where a walk reports.

#[cfg(test)]
mod tests;

use {
    super::{Global, Item, Module, RecGroup, RecItem, Term},
    std::{cell::OnceCell, collections::HashMap},
};

/// Where a module holds one name: an item that is a definition, or a member of an item that is a group.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Place {
    Let(u32),
    /// `slot` counts the module's group members in item order, which is where [`Places::member`] keeps what a member is read by.
    Member {
        item: u32,
        member: u32,
        slot: u32,
    },
}

impl Place {
    /// The item that declares the name, as its place among the module's items.
    pub fn item(&self) -> usize {
        match self {
            Place::Let(item) | Place::Member { item, .. } => *item as usize,
        }
    }
}

/// Where a module holds each name it defines.
#[derive(Debug, Default)]
pub struct Places {
    places: HashMap<Global, Place>,
    /// A group's member is read by its type and by its projection out of the group, which are terms the group does not hold as such: each is made the first time its member is read.
    members: Vec<OnceCell<(Term, Term)>>,
    /// The item each of the module's groups is, made the first time a report asks which names a group's members go by.
    groups: OnceCell<HashMap<RecGroup, u32>>,
}

impl Places {
    /// The places of `module`'s names: one for each definition and each member of a group, and nothing copied.
    pub fn of(module: &Module) -> Self {
        const TWICE: &str = "a module declares one name twice";
        let mut places = HashMap::with_capacity(module.items.len());
        let mut slots = 0;
        for (item, declared) in module.items.iter().enumerate() {
            let item = u32::try_from(item).expect("a unit's items are counted in a u32");
            match declared {
                Item::Let(definition) => {
                    let earlier = places.insert(definition.name, Place::Let(item));
                    assert!(earlier.is_none(), "{TWICE}");
                }
                Item::Rec(rec) => {
                    for (member, definition) in rec.definitions.iter().enumerate() {
                        let member =
                            u32::try_from(member).expect("a group's members are counted in a u32");
                        let earlier = places.insert(
                            definition.name,
                            Place::Member {
                                item,
                                member,
                                slot: slots,
                            },
                        );
                        assert!(earlier.is_none(), "{TWICE}");
                        slots += 1;
                    }
                }
            }
        }

        Self {
            places,
            members: (0..slots).map(|_| OnceCell::new()).collect(),
            groups: OnceCell::new(),
        }
    }

    /// Where the module holds `name`, where it defines it.
    pub fn place(&self, name: &Global) -> Option<Place> {
        self.places.get(name).copied()
    }

    /// The type and the projection the member at `member` of `rec` is read by, kept at `slot` from the first time it is asked for.
    pub fn member(&self, rec: &RecItem, member: u32, slot: u32) -> &(Term, Term) {
        self.members[slot as usize].get_or_init(|| {
            let index = member as usize;

            (
                rec.group.member_type(index),
                Term::rec_proj(rec.group.clone(), index),
            )
        })
    }

    /// The item of `module` that is `group`, where it holds it.
    pub fn group(&self, module: &Module, group: &RecGroup) -> Option<usize> {
        let groups = self.groups.get_or_init(|| {
            module
                .items
                .iter()
                .enumerate()
                .filter_map(|(item, declared)| match declared {
                    Item::Rec(rec) => Some((
                        rec.group.clone(),
                        u32::try_from(item).expect("a unit's items are counted in a u32"),
                    )),
                    Item::Let(_) => None,
                })
                .collect()
        });

        groups.get(group).map(|item| *item as usize)
    }
}
