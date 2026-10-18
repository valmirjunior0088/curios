//! Where a module holds each name it defines, made once for the module.
//!
//! A finished module is read by name: a later unit's elaboration asks for a definition's type, the certifier for what a name unfolds to, erasure for how one is bound. [`Places`] says where a name is held — the item that declares it and, for a group's member, which member — and is made once for a module and kept with whatever holds the module, so no reader keys a module again.
//!
//! **Looked up, never walked.** The table is a hash's, and nothing reads its order. It is persistent, so the places of a version of a module are its module's with the names of what the version changed placed again ([`Places::place_item`], [`Places::forget`]).
//!
//! **One place for a name.** A finished module declares each name once: lowering refuses a second declaration and the certifier a module that carries one. A table made of a module that did would hold the later declaration and say nothing of the earlier, to every reader alike, so such a module is its builder's fault and has no places — asserted, where a walk reports.

#[cfg(test)]
mod tests;

use {
    super::{Global, Item, Items, Module, RecGroup, RecItem, Term},
    curios_utilities::{Keyed, Persistent},
    std::{cell::OnceCell, collections::HashMap, rc::Rc},
};

/// Where a module holds one name: an item that is a definition, or a member of an item that is a group, each by the item's place ([`crate::Items::places`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Place {
    Let(u64),
    Member { item: u64, member: u32 },
}

impl Place {
    /// The item that declares the name, as its place among the module's items.
    pub fn item(&self) -> usize {
        match self {
            Place::Let(item) | Place::Member { item, .. } => {
                usize::try_from(*item).expect("a place is counted in a usize")
            }
        }
    }
}

/// Where a module holds a name, and, for a group's member, what it is read by: its type and its projection out of the group, terms the group does not hold as such, each made the first time the member is read.
#[derive(Debug, Clone)]
struct Held {
    place: Place,
    member: Option<Rc<OnceCell<(Term, Term)>>>,
}

/// Where a module holds each name it defines.
#[derive(Debug, Clone, Default)]
pub struct Places {
    places: Persistent<Global, Held>,
    /// The item each of the module's groups is, made the first time a report asks which names a group's members go by.
    groups: OnceCell<HashMap<Keyed<RecGroup>, usize>>,
}

impl Places {
    /// The places of `module`'s names: one for each definition and each member of a group, and nothing copied.
    pub fn of(module: &Module) -> Self {
        let mut places = Self::default();
        // Asked of what each place declares, which a stored module knows without its item: placing a unit's names reads none of its declarations.
        for place in module.items.places() {
            places.place_item(&module.items, place);
        }

        places
    }

    /// Place the names the item at `place` of `items` declares there: what a version of a module's places is made with, of the items the version changed.
    pub fn place_item(&mut self, items: &Items, place: usize) {
        const TWICE: &str = "a module declares one name twice";
        let item = u64::try_from(place).expect("a place is counted in a u64");
        let group = items.is_group(place);
        for (member, name) in items.names(place).into_iter().enumerate() {
            let held = match group {
                false => Held {
                    place: Place::Let(item),
                    member: None,
                },
                true => Held {
                    place: Place::Member {
                        item,
                        member: u32::try_from(member)
                            .expect("a group's members are counted in a u32"),
                    },
                    member: Some(Rc::new(OnceCell::new())),
                },
            };
            let earlier = self.places.get(name).map(|earlier| earlier.place.item());
            assert!(earlier.is_none_or(|earlier| earlier == place), "{TWICE}");
            self.places.insert(*name, held);
        }
        self.groups = OnceCell::new();
    }

    /// Hold `name` nowhere: a version of a module that no longer declares it, or declares it elsewhere.
    pub fn forget(&mut self, name: &Global) {
        self.places.remove(name);
        self.groups = OnceCell::new();
    }

    /// Where the module holds `name`, where it defines it.
    pub fn place(&self, name: &Global) -> Option<Place> {
        self.places.get(name).map(|held| held.place)
    }

    /// The type and the projection `name`, the member at `member` of `rec`, is read by, kept from the first time it is asked for.
    pub fn member(&self, name: &Global, rec: &RecItem, member: u32) -> &(Term, Term) {
        let cell = self
            .places
            .get(name)
            .and_then(|held| held.member.as_deref())
            .expect("a member of a group the module holds");

        cell.get_or_init(|| {
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
                .places()
                .filter(|place| module.items.is_group(*place))
                .filter_map(|place| match &module.items[place] {
                    Item::Rec(rec) => Some((Keyed::new(rec.group.clone()), place)),
                    Item::Let(_) => None,
                })
                .collect()
        });

        groups.get(&Keyed::new(group.clone())).copied()
    }
}
