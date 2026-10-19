//! The order a module's items are stored in: each item at its depth, one more than the deepest item of the module it reaches, and among items of one depth by its first name ([`Module::ordered`]), so every item after the items it reaches.
//!
//! **An item's place is a facet of what it reaches.** Its key, its depth with its name, is read off the keys of what it reaches, so a unit compiled over a baseline gives the order again from the items it made: each takes the depth what it reaches gives it, and an item whose depth moves passes the change on to the items that reach it, in increasing depth, stopping where a depth comes out as it was ([`Order::revised`]). Only an item whose key moved is placed again; every other stands where the baseline holds it.
//!
//! **What an item reaches is taken where it is made**, off its terms and the registry entries it declares ([`Module::reaches`]), and kept with the unit ([`Order`]), so no walk reads an item's terms to place it again.
//!
//! **Stored with each name written once.** An item reaches the few names its neighbours do, so the stored form holds a table of the names, each once, and each item holds places in it ([`OrderTable`]), as a unit's traces do.
//!
//! **A cycle has no depth.** The elaborator refuses declarations that reach one another before a module is ordered, so a module the order is given to holds none; one handed in anyway is broken at the least-named item left, placed at the depth of what it reaches that has one, and a revision that meets one leaves the module to be ordered whole.

#[cfg(test)]
mod tests;

use {
    crate::{Arranged, Global, Item, Items, Module, Places},
    curios_utilities::{Persistent, Sorted},
    std::{
        borrow::Borrow,
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        mem,
        rc::Rc,
    },
};

/// What one item reaches of its module's names, and the depth that gives it: one more than the deepest item that declares one of them, zero where it reaches none.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reach {
    pub depth: u32,
    pub names: Vec<Global>,
}

/// Where each item of a module stands in the order it is stored in, by the item's first name: what it reaches and its depth ([`Reach`]). Made where a module is ordered whole ([`Module::in_order`]) and revised by a recompile ([`Order::revised`]); a unit keeps it in its elaborator's record.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Order {
    #[archived_with(OrderTabled)]
    items: Sorted<Global, Rc<Reach>>,
    /// Every item by its key, its depth then its first name: the order itself, read off `items` the first time a revision asks and handed on by one. Never stored.
    #[archived_with(curios_archive::Skip)]
    keys: OnceCell<Sorted<(u32, Global), ()>>,
    /// The items that reach each name, by their first names: `items` read backwards the first time a revision asks, and handed on by one. Looked up, never walked. Never stored.
    #[archived_with(curios_archive::Skip)]
    readers: OnceCell<Persistent<Global, Rc<BTreeSet<Global>>>>,
}

/// Two orders are one where they place every item alike, whatever each has read off itself.
impl PartialEq for Order {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}

impl Eq for Order {}

/// What [`Order::revised`] makes of a module compiled over a baseline: its items in the order, where it holds each name where the baseline's places were handed on and no item had to be placed apart again, and the order.
pub struct Revised {
    pub items: Items,
    pub places: Option<Places>,
    pub order: Order,
}

impl Order {
    /// What the item declaring `first` first reaches, and its depth, where the module holds one.
    pub fn reach(&self, first: &Global) -> Option<&Reach> {
        self.items.get(first).map(Rc::as_ref)
    }

    /// Every item by its key. See the field.
    fn keys(&self) -> &Sorted<(u32, Global), ()> {
        self.keys.get_or_init(|| {
            let mut keys = Sorted::new();
            for (first, reach) in self.items.iter() {
                keys.insert((reach.depth, *first), ());
            }
            keys
        })
    }

    /// The items that reach each name. See the field.
    fn readers(&self) -> &Persistent<Global, Rc<BTreeSet<Global>>> {
        self.readers.get_or_init(|| {
            // Gathered whole before any is put in the persistent table, so no reader's set is copied as it grows.
            let mut gathered = HashMap::<Global, BTreeSet<Global>>::new();
            for (first, reach) in self.items.iter() {
                for name in &reach.names {
                    gathered.entry(*name).or_default().insert(*first);
                }
            }
            let mut readers = Persistent::new();
            #[expect(
                clippy::iter_over_hash_type,
                reason = "a persistent table built of the entries reads the same whatever order they come in"
            )]
            for (name, reading) in gathered {
                readers.insert(name, Rc::new(reading));
            }
            readers
        })
    }

    /// The order of a module made of `baseline`, the module this is the order of, whose names `places` holds: each item of `made` in place of the baseline's items that declare one of its names, every name of `dropped` taken away, and every other item the baseline's own.
    ///
    /// **Given again from what was made.** Each item made takes the depth what it reaches gives it, and an item whose depth moves passes the change on to the items that reach it, in increasing depth, stopping where a depth comes out as it was. An item made at the key the baseline's item of its first name held stands where that one stood; an item made at another key, and a kept one whose depth moved, is placed after the item whose key comes before its own, in key order, and every other item stands where the baseline holds it, read no more than it was. Where no place is left between two items, the module's items are placed apart again in the order, and where it holds each name is left to be made.
    ///
    /// `None` where an item made declares no name, the baseline holds an item this order does not place, an item taken out declares a name neither made nor dropped, a kept item reaches a name taken away, or the items reach one another: the module is then ordered whole ([`Module::in_order`]).
    pub fn revised(
        &self,
        baseline: &Module,
        places: &Places,
        made: &[Item],
        dropped: &BTreeSet<Global>,
    ) -> Option<Revised> {
        let held = &baseline.items;
        if held.len() != self.items.len() {
            return None;
        }
        // Each item made by its first name, and the first name of the item made that declares each name.
        let mut made_first = BTreeMap::<Global, usize>::new();
        let mut made_owner = BTreeMap::<Global, Global>::new();
        for (index, item) in made.iter().enumerate() {
            let names = item.declared_names();
            let first = **names.first()?;
            made_first.insert(first, index);
            made_owner.extend(names.into_iter().map(|name| (*name, first)));
        }
        // The baseline's items taken out, by their first names: each declaring a name made or dropped.
        let mut taken = BTreeMap::<Global, usize>::new();
        for name in made_owner.keys().chain(dropped) {
            if let Some(place) = places.place(name) {
                taken.insert(*held.first_name(place.item())?, place.item());
            }
        }
        if taken.values().any(|place| {
            held.names(*place)
                .into_iter()
                .any(|name| !made_owner.contains_key(name) && !dropped.contains(name))
        }) {
            return None;
        }
        // The first name of the item that declares `name` in the module made, and the names the item first named `first` declares.
        let owner = |name: &Global| -> Option<Global> {
            if let Some(first) = made_owner.get(name) {
                return Some(*first);
            }
            if dropped.contains(name) {
                return None;
            }
            held.first_name(places.place(name)?.item()).copied()
        };
        let names_of = |first: &Global| -> Vec<Global> {
            match made_first.get(first) {
                Some(index) => made[*index].declared_names().into_iter().copied().collect(),
                None => places
                    .place(first)
                    .map(|place| held.names(place.item()).into_iter().copied().collect())
                    .unwrap_or_default(),
            }
        };

        let mut items = self.items.clone();
        let mut keys = self.keys().clone();
        let mut readers = self.readers().clone();
        let kept = |first: &Global| !made_first.contains_key(first) && !taken.contains_key(first);
        if dropped.iter().any(|name| {
            readers
                .get(name)
                .is_some_and(|reading| reading.iter().any(kept))
        }) {
            return None;
        }

        // What the items taken out reached is taken out, what each item made reaches put in, and every item that reaches a name either declares is asked its depth again.
        let mut was = BTreeMap::<Global, u32>::new();
        let mut asked = BTreeSet::<Global>::new();
        for (first, place) in &taken {
            if let Some(reach) = items.remove(first) {
                keys.remove(&(reach.depth, *first));
                unread(&mut readers, first, &reach.names);
                was.insert(*first, reach.depth);
            }
            for name in held.names(*place) {
                asked.extend(
                    readers
                        .get(name)
                        .into_iter()
                        .flat_map(|reading| reading.iter()),
                );
            }
        }
        let holds = |name: &Global| {
            made_owner.contains_key(name)
                || (places.place(name).is_some() && !dropped.contains(name))
        };
        for (first, index) in &made_first {
            let item = &made[*index];
            let own = item.declared_names();
            let names = baseline
                .reaches(item)
                .into_iter()
                .filter(|name| holds(name) && !own.contains(&name))
                .collect::<Vec<_>>();
            read(&mut readers, first, &names);
            items.insert(*first, Rc::new(Reach { depth: 0, names }));
            keys.insert((0, *first), ());
            asked.insert(*first);
        }

        // In increasing depth, so an item is asked once what it reaches has settled, wherever that can be.
        let mut queue = asked
            .iter()
            .filter_map(|first| Some((items.get(first)?.depth, *first)))
            .collect::<BTreeSet<_>>();
        let deepest = u32::try_from(items.len()).ok()?;
        let mut moved = BTreeSet::<Global>::new();
        while let Some((_, first)) = queue.pop_first() {
            let Some(reach) = items.get(&first).cloned() else {
                continue;
            };
            let depth = reach
                .names
                .iter()
                .filter_map(&owner)
                .filter_map(|reached| items.get(&reached))
                .map(|reached| reached.depth + 1)
                .max()
                .unwrap_or(0);
            // A depth past the module's size is a cycle, which no recompile resolves alone.
            if depth > deepest {
                return None;
            }
            if depth == reach.depth {
                continue;
            }
            keys.remove(&(reach.depth, first));
            keys.insert((depth, first), ());
            items.insert(
                first,
                Rc::new(Reach {
                    depth,
                    names: reach.names.clone(),
                }),
            );
            if kept(&first) {
                moved.insert(first);
            }
            for name in names_of(&first) {
                for reader in readers
                    .get(&name)
                    .into_iter()
                    .flat_map(|reading| reading.iter())
                {
                    if let Some(read) = items.get(reader) {
                        queue.insert((read.depth, *reader));
                    }
                }
            }
        }

        let order = Order {
            items,
            keys: OnceCell::from(keys),
            readers: OnceCell::from(readers),
        };
        let edit = Edit {
            held,
            places,
            made,
            made_first,
            taken,
            was,
            moved,
        };
        let (items, places) = order.place(&edit).unwrap_or_else(|| order.arrange(&edit));

        Some(Revised {
            items,
            places,
            order,
        })
    }

    /// The items of the module `edit` made of the baseline's, in this order: each item made placed at its key, each kept item whose depth moved placed again, every item taken out taken away, and every other where the baseline holds it. With where the module holds each name, where the baseline's places were a version's. `None` where no place is left between two items.
    fn place(&self, edit: &Edit<'_>) -> Option<(Items, Option<Places>)> {
        let Edit {
            held,
            places,
            made,
            made_first,
            taken,
            was,
            moved,
        } = edit;
        let in_version = |place: usize| held.place_in_version(place);
        let mut items = held.version();
        let mut placing = held.is_version().then(|| Places::clone(places));
        let mut placed = BTreeMap::<Global, usize>::new();
        // Every name of an item taken out or placed again is held nowhere until it is placed.
        if let Some(placing) = &mut placing {
            for place in taken.values() {
                for name in held.names(*place) {
                    placing.forget(name);
                }
            }
            for first in moved {
                if let Some(place) = places.place(first) {
                    for name in held.names(place.item()) {
                        placing.forget(name);
                    }
                }
            }
        }
        // An item made at the key the baseline's item of its first name held stands where that one stood; every other item taken out is taken away.
        let mut pending = Vec::new();
        for (first, index) in made_first {
            let depth = self.items.get(first)?.depth;
            match (taken.get(first), was.get(first)) {
                (Some(place), Some(earlier)) if *earlier == depth => {
                    let at = in_version(*place);
                    items.replace(at, made[*index].clone());
                    placed.insert(*first, at);
                }
                _ => pending.push((depth, *first)),
            }
        }
        for (first, place) in taken {
            if !placed.contains_key(first) {
                items.remove(in_version(*place));
            }
        }
        pending.extend(
            moved
                .iter()
                .filter_map(|first| Some((self.items.get(first)?.depth, *first))),
        );
        // In key order, so the item whose key comes before one's own already stands where it stays.
        pending.sort();
        let keys = self.keys();
        for key in pending {
            let before = keys.before(&key).map(|((_, name), ())| *name);
            let after = match before {
                Some(name) => Some(match placed.get(&name) {
                    Some(at) => *at,
                    None => in_version(places.place(&name)?.item()),
                }),
                None => None,
            };
            let first = key.1;
            let at = match made_first.get(&first) {
                Some(index) => items.insert_after(after, made[*index].clone())?,
                None => items.relocate(in_version(places.place(&first)?.item()), after)?,
            };
            placed.insert(first, at);
        }
        if let Some(placing) = &mut placing {
            for at in placed.values() {
                placing.place_item(&items, *at);
            }
        }

        Some((items, placing))
    }

    /// The items of the module `edit` made of the baseline's, in this order, placed apart again: each item made in hand, and every other the baseline's own, read no more than it was. Where the module holds each name is left to be made.
    fn arrange(&self, edit: &Edit<'_>) -> (Items, Option<Places>) {
        let Edit {
            held,
            places,
            made,
            made_first,
            ..
        } = edit;
        let arranged = self
            .keys()
            .iter()
            .map(|((_, first), ())| match made_first.get(first) {
                Some(index) => Arranged::New(Rc::new(made[*index].clone())),
                None => Arranged::Held(
                    held.place_in_version(
                        places
                            .place(first)
                            .expect("a kept item's first name is held where the baseline holds it")
                            .item(),
                    ),
                ),
            });

        (held.version().arranged(arranged), None)
    }
}

/// An order as it is stored: every name it holds, each once, and each item's reach holding places among them, in the order of the items' first names.
#[curios_archive::archived]
pub struct OrderTable {
    names: Vec<Global>,
    items: Vec<TabledReach>,
}

/// One item's reach as an [`OrderTable`] holds it: the place of its first name, its depth, and the places of the names it reaches.
#[curios_archive::archived]
pub struct TabledReach {
    first: u32,
    depth: u32,
    names: Vec<u32>,
}

/// How an order is archived: as an [`OrderTable`].
pub struct OrderTabling;

/// The field adapter: `#[archived_with(OrderTabled)]`.
pub type OrderTabled = curios_archive::Via<OrderTabling>;

impl curios_archive::Proxy<Sorted<Global, Rc<Reach>>> for OrderTabling {
    type Archivable = OrderTable;

    fn to_archivable(items: &Sorted<Global, Rc<Reach>>) -> impl Borrow<OrderTable> {
        let mut names = Vec::new();
        let mut places = HashMap::new();
        let mut place = |name: &Global| {
            *places.entry(*name).or_insert_with(|| {
                let place = u32::try_from(names.len()).expect("a table is counted in a u32");
                names.push(*name);
                place
            })
        };
        let items = items
            .iter()
            .map(|(first, reach)| TabledReach {
                first: place(first),
                depth: reach.depth,
                names: reach.names.iter().map(&mut place).collect(),
            })
            .collect();

        OrderTable { names, items }
    }

    fn from_archivable(table: OrderTable) -> Result<Sorted<Global, Rc<Reach>>, String> {
        let placed = |place: u32| {
            usize::try_from(place)
                .ok()
                .and_then(|place| table.names.get(place))
                .copied()
                .ok_or_else(|| "a stored order holds a name its table does not".to_string())
        };
        let mut items = Sorted::new();
        for TabledReach {
            first,
            depth,
            names,
        } in &table.items
        {
            let names = names
                .iter()
                .map(|place| placed(*place))
                .collect::<Result<Vec<_>, _>>()?;
            items.insert(
                placed(*first)?,
                Rc::new(Reach {
                    depth: *depth,
                    names,
                }),
            );
        }

        Ok(items)
    }
}

/// What a revision made of a baseline's module, as placing its items reads it: the baseline's items and where it holds each name, the items made by their first names, the baseline's items taken out by theirs with the depth each had, and the kept items whose depth moved.
struct Edit<'e> {
    held: &'e Items,
    places: &'e Places,
    made: &'e [Item],
    made_first: BTreeMap<Global, usize>,
    taken: BTreeMap<Global, usize>,
    was: BTreeMap<Global, u32>,
    moved: BTreeSet<Global>,
}

/// `first` among the readers of each of `names`.
fn read(readers: &mut Persistent<Global, Rc<BTreeSet<Global>>>, first: &Global, names: &[Global]) {
    for name in names {
        let mut reading = readers
            .get(name)
            .map(|held| (**held).clone())
            .unwrap_or_default();
        reading.insert(*first);
        readers.insert(*name, Rc::new(reading));
    }
}

/// `first` taken out of the readers of each of `names`.
fn unread(
    readers: &mut Persistent<Global, Rc<BTreeSet<Global>>>,
    first: &Global,
    names: &[Global],
) {
    for name in names {
        let Some(held) = readers.get(name) else {
            continue;
        };
        let mut reading = (**held).clone();
        reading.remove(first);
        match reading.is_empty() {
            true => {
                readers.remove(name);
            }
            false => readers.insert(*name, Rc::new(reading)),
        }
    }
}

impl Module {
    /// This module with its items in the order a unit is stored in: each item at its depth, one more than the deepest item it [reaches](Self::reaches), and among items of one depth, the one whose first declared name is least.
    ///
    /// The order is a function of what the items declare and name, and of nowhere they were written: a unit holds the same module wherever its declarations sit in its sources, and whether it was compiled whole or over a baseline. It is given once, where a module is made, and every walk after that takes the items as they come: the certifier, which refuses a name an item reaches before it is defined, erasure, and a program's link.
    ///
    /// Items that reach one another have no depth. The least-named of what is left then takes the depth of what it reaches that has one, so the walk ends, and the certifier refuses the name it finds unbound.
    pub fn ordered(self) -> Self {
        self.in_order().0
    }

    /// [`Module::ordered`], with the order it puts the items in: what each reaches and its depth.
    pub fn in_order(mut self) -> (Self, Order) {
        let items = mem::take(&mut self.items).into_iter().collect::<Vec<_>>();
        let count = items.len();
        let owner = items
            .iter()
            .enumerate()
            .flat_map(|(index, item)| {
                item.declared_names()
                    .into_iter()
                    .map(move |name| (*name, index))
            })
            .collect::<BTreeMap<_, _>>();
        let reached = items
            .iter()
            .enumerate()
            .map(|(index, item)| {
                self.reaches(item)
                    .into_iter()
                    .filter(|name| owner.get(name).is_some_and(|held| *held != index))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        let mut waiting = vec![0usize; count];
        let mut dependents = vec![Vec::new(); count];
        for (index, names) in reached.iter().enumerate() {
            let dependencies = names
                .iter()
                .map(|name| owner[name])
                .collect::<BTreeSet<_>>();
            waiting[index] = dependencies.len();
            for dependency in dependencies {
                dependents[dependency].push(index);
            }
        }

        // By first name, then by place for items that declare none or share one, which no stored unit holds.
        let rank = |index: usize| {
            (
                items[index].declared_names().first().map(|name| **name),
                index,
            )
        };
        // Each item's depth, found once everything it reaches has one: where nothing is ready with items left, a cycle is broken at the least-named of them.
        let mut depth = vec![None::<u32>; count];
        let mut left = (0..count).map(rank).collect::<BTreeSet<_>>();
        let mut ready = (0..count)
            .filter(|index| waiting[*index] == 0)
            .collect::<Vec<_>>();
        while !left.is_empty() {
            let next = match ready.pop() {
                Some(next) => next,
                None => left.first().expect("an item is left").1,
            };
            if !left.remove(&rank(next)) {
                continue;
            }
            depth[next] = Some(
                reached[next]
                    .iter()
                    .filter_map(|name| depth[owner[name]])
                    .map(|reached| reached + 1)
                    .max()
                    .unwrap_or(0),
            );
            for dependent in &dependents[next] {
                waiting[*dependent] -= 1;
                if waiting[*dependent] == 0 {
                    ready.push(*dependent);
                }
            }
        }
        let depth = depth
            .into_iter()
            .map(|depth| depth.expect("every item is given a depth"))
            .collect::<Vec<_>>();

        let mut order = Order::default();
        for (index, names) in reached.into_iter().enumerate() {
            if let Some(first) = rank(index).0 {
                order.items.insert(
                    first,
                    Rc::new(Reach {
                        depth: depth[index],
                        names,
                    }),
                );
            }
        }
        let mut sequence = (0..count).collect::<Vec<_>>();
        sequence.sort_by_key(|index| (depth[*index], rank(*index)));
        let mut slots = items.into_iter().map(Some).collect::<Vec<_>>();
        self.items = sequence
            .into_iter()
            .map(|index| slots[index].take().expect("an item is ordered once"))
            .collect();

        (self, order)
    }
}
