//! A module's items, each in hand or restored the first time it is read.
//!
//! **A stored module is read by item.** A compilation reads of a unit in scope the declarations it names, and a library holds thousands a program names a few of; restored whole, every process pays for all of them before its first declaration. So a module that was stored holds a cell for each of its items and a restorer its unit hands it, and reading the item at a place fills that cell and no other.
//!
//! **What a place declares is known without its item.** Where a module holds each name, which of its items are groups and whether two units declare one name are asked of every unit in scope by every compilation, and none of them reads a term: a stored module keeps the names each place declares beside its cells.
//!
//! **Reading it is indexing it.** Every reader that goes by place is written as it would be over a vector, and a reader that walks every item restores every item: slow where a walk was not looked for, and never another answer.
//!
//! **A version holds items by place.** A unit compiled over a baseline holds the baseline's items and replaces the ones it made: a version shares every item it did not change with the module it was made from, each at the place it had, and holds an item new to it at a place between two others ([`Items::version`]). A module made whole or restored places its items at their indices; a version made of one spaces them apart first, once, and every version made of a version keeps its places. So a reader goes by the places a module gives ([`Items::places`]), never by counting.

use {
    crate::{Declared, Global, Item},
    curios_utilities::Ordered,
    std::{
        cell::OnceCell,
        fmt,
        ops::{Index, IndexMut, Range},
        rc::Rc,
    },
};

/// How far apart a version first places the items of a module that placed them at their indices: room for an item new to a later version between any two, many times over.
const SPACING: usize = 1 << 20;

/// An item's outline: the names it declares, in the order it declares them, what it declares each as, and whether it is a group. What is known of a place of a stored module without its item.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Outline {
    pub names: Vec<Global>,
    /// What each of `names` is declared as, in their order: a former by which kind of declaration it is the former of, with nothing of its entry.
    pub declared: Vec<Declared>,
    pub group: bool,
}

impl Outline {
    /// `item`'s outline.
    pub fn of(item: &Item) -> Self {
        Self {
            names: item.declared_names().into_iter().copied().collect(),
            declared: declared_by(item),
            group: matches!(item, Item::Rec(_)),
        }
    }
}

/// The first name `item` declares.
fn first_name(item: &Item) -> Option<&Global> {
    match item {
        Item::Let(definition) => Some(&definition.name),
        Item::Rec(rec) => rec.definitions.first().map(|definition| &definition.name),
    }
}

/// What `item` declares each of its names as, in the order it declares them.
fn declared_by(item: &Item) -> Vec<Declared> {
    match item {
        Item::Let(definition) => vec![definition.kind.declared()],
        Item::Rec(rec) => rec
            .definitions
            .iter()
            .map(|definition| definition.kind.declared())
            .collect(),
    }
}

/// A stored module's items: a cell for each, what each declares, and what restores one.
struct Stored {
    cells: Vec<OnceCell<Item>>,
    declared: Vec<Outline>,
    restore: Box<dyn Fn(usize) -> Item>,
}

/// An item a version holds: one in hand, or a stored module's, read where it is first asked for.
#[derive(Clone)]
enum Entry {
    Made(Rc<Item>),
    Stored(Rc<Stored>, usize),
}

impl Entry {
    fn first_name(&self) -> Option<&Global> {
        match self {
            Entry::Made(item) => first_name(item),
            Entry::Stored(stored, index) => stored.declared[*index].names.first(),
        }
    }

    fn item(&self) -> &Item {
        match self {
            Entry::Made(item) => item,
            Entry::Stored(stored, index) => {
                stored.cells[*index].get_or_init(|| (stored.restore)(*index))
            }
        }
    }

    fn names(&self) -> Vec<&Global> {
        match self {
            Entry::Made(item) => item.declared_names(),
            Entry::Stored(stored, index) => stored.declared[*index].names.iter().collect(),
        }
    }

    fn declared(&self) -> Vec<Declared> {
        match self {
            Entry::Made(item) => declared_by(item),
            Entry::Stored(stored, index) => stored.declared[*index].declared.clone(),
        }
    }

    fn is_group(&self) -> bool {
        match self {
            Entry::Made(item) => matches!(**item, Item::Rec(_)),
            Entry::Stored(stored, index) => stored.declared[*index].group,
        }
    }
}

#[derive(Clone)]
enum Held {
    /// Every item, as a module made in this process holds them.
    Made(Vec<Item>),
    /// The items of a module that was stored, shared by every clone of it, so one reading serves them all.
    Stored(Rc<Stored>),
    /// The items of a version, each at its place ([`Items::version`]).
    Placed(Ordered<Entry>),
}

/// The places of a module's items, in order: a version's taken as the walk begins.
#[derive(Debug, Clone)]
pub enum ItemPlaces {
    Indices(Range<usize>),
    Placed(std::vec::IntoIter<usize>),
}

impl Iterator for ItemPlaces {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        match self {
            ItemPlaces::Indices(indices) => indices.next(),
            ItemPlaces::Placed(places) => places.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            ItemPlaces::Indices(indices) => indices.size_hint(),
            ItemPlaces::Placed(places) => places.size_hint(),
        }
    }
}

impl DoubleEndedIterator for ItemPlaces {
    fn next_back(&mut self) -> Option<usize> {
        match self {
            ItemPlaces::Indices(indices) => indices.next_back(),
            ItemPlaces::Placed(places) => places.next_back(),
        }
    }
}

impl ExactSizeIterator for ItemPlaces {}

/// A module's items, in the order it stores them — see the module documentation.
#[derive(Clone)]
pub struct Items(Held);

impl Items {
    /// The items of a stored module: one for each entry of `declared`, the item at a place being what `restore` answers for it, asked once.
    pub fn stored(declared: Vec<Outline>, restore: impl Fn(usize) -> Item + 'static) -> Self {
        Self(Held::Stored(Rc::new(Stored {
            cells: declared.iter().map(|_| OnceCell::new()).collect(),
            declared,
            restore: Box::new(restore),
        })))
    }

    pub fn len(&self) -> usize {
        match &self.0 {
            Held::Made(items) => items.len(),
            Held::Stored(stored) => stored.cells.len(),
            Held::Placed(placed) => placed.len(),
        }
    }

    /// The places of the items, in order: their indices, where the module is no version.
    pub fn places(&self) -> ItemPlaces {
        match &self.0 {
            Held::Made(items) => ItemPlaces::Indices(0..items.len()),
            Held::Stored(stored) => ItemPlaces::Indices(0..stored.cells.len()),
            Held::Placed(placed) => ItemPlaces::Placed(
                placed
                    .iter()
                    .map(|(place, _)| place_of(place))
                    .collect::<Vec<_>>()
                    .into_iter(),
            ),
        }
    }

    /// Whether these items are a version's, held at places a version made of them keeps.
    pub fn is_version(&self) -> bool {
        matches!(self.0, Held::Placed(_))
    }

    /// The place a version made of these items ([`Items::version`]) holds the item at `place` at.
    pub fn place_in_version(&self, place: usize) -> usize {
        match &self.0 {
            Held::Placed(_) => place,
            Held::Made(_) | Held::Stored(_) => spaced(place),
        }
    }

    /// The place of the item after the one at `place`, or of the first where it is `None`.
    pub fn place_after(&self, place: Option<usize>) -> Option<usize> {
        match &self.0 {
            Held::Placed(placed) => placed
                .after(place.map_or(-1, position))
                .map(|(next, _)| place_of(next)),
            Held::Made(_) | Held::Stored(_) => {
                let next = place.map_or(0, |place| place + 1);
                (next < self.len()).then_some(next)
            }
        }
    }

    /// The first name the item at `place` declares, with nothing restored.
    pub fn first_name(&self, place: usize) -> Option<&Global> {
        match &self.0 {
            Held::Made(items) => first_name(&items[place]),
            Held::Stored(stored) => stored.declared[place].names.first(),
            Held::Placed(placed) => entry(placed, place).first_name(),
        }
    }

    /// These items as a version to change, sharing every item with them: their own places where they are a version, and their indices spaced apart otherwise, so an item new to the version has a place between any two.
    pub fn version(&self) -> Items {
        let placed = match &self.0 {
            Held::Placed(placed) => placed.clone(),
            Held::Made(items) => {
                let mut placed = Ordered::new();
                for (index, item) in items.iter().enumerate() {
                    placed.insert(position(spaced(index)), Entry::Made(Rc::new(item.clone())));
                }
                placed
            }
            Held::Stored(stored) => {
                let mut placed = Ordered::new();
                for index in 0..stored.cells.len() {
                    placed.insert(
                        position(spaced(index)),
                        Entry::Stored(Rc::clone(stored), index),
                    );
                }
                placed
            }
        };

        Self(Held::Placed(placed))
    }

    /// `item` at `place`, in place of what this version held there.
    pub fn replace(&mut self, place: usize, item: Item) {
        self.placed()
            .insert(position(place), Entry::Made(Rc::new(item)));
    }

    /// `item` at a place after `after`, or first where it is `None`, and before the place that follows it: the place given, or `None` where no place lies between the two, and the version is as it was.
    pub fn insert_after(&mut self, after: Option<usize>, item: Item) -> Option<usize> {
        let placed = self.placed();
        let low = after.map_or(-1, position);
        let high = placed
            .after(low)
            .map_or(position(usize::MAX / 2), |(next, _)| next);
        let place = low + (high - low) / 2;
        if place <= low {
            return None;
        }
        placed.insert(place, Entry::Made(Rc::new(item)));

        Some(place_of(place))
    }

    /// The item at `place`, taken out of this version.
    pub fn remove(&mut self, place: usize) {
        self.placed().remove(position(place));
    }

    /// The items as a version's.
    fn placed(&mut self) -> &mut Ordered<Entry> {
        if !matches!(self.0, Held::Placed(_)) {
            *self = self.version();
        }
        match &mut self.0 {
            Held::Placed(placed) => placed,
            _ => unreachable!("a version above"),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Every item in order, each restored where it was not in hand.
    pub fn iter(&self) -> Iter<'_> {
        Iter {
            items: self,
            places: self.places(),
        }
    }

    /// The names the item at `place` declares, in the order it declares them, with nothing restored.
    pub fn names(&self, place: usize) -> Vec<&Global> {
        match &self.0 {
            Held::Made(items) => items[place].declared_names(),
            Held::Stored(stored) => stored.declared[place].names.iter().collect(),
            Held::Placed(placed) => entry(placed, place).names(),
        }
    }

    /// What the item at `place` declares each of its names as, in the order of [`Items::names`], with nothing restored.
    pub fn declared(&self, place: usize) -> Vec<Declared> {
        match &self.0 {
            Held::Made(items) => declared_by(&items[place]),
            Held::Stored(stored) => stored.declared[place].declared.clone(),
            Held::Placed(placed) => entry(placed, place).declared(),
        }
    }

    /// Whether the item at `place` is a group, with nothing restored.
    pub fn is_group(&self, place: usize) -> bool {
        match &self.0 {
            Held::Made(items) => matches!(items[place], Item::Rec(_)),
            Held::Stored(stored) => stored.declared[place].group,
            Held::Placed(placed) => entry(placed, place).is_group(),
        }
    }

    /// The items as a vector of their own, every one restored where the module was stored.
    pub fn to_vec(&self) -> Vec<Item> {
        self.iter().cloned().collect()
    }

    /// Every item in order, to change: a stored module's are all restored first, as [`Items::push`] restores them.
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Item> {
        self.made().iter_mut()
    }

    /// Add `item` after the others. A stored module's items are all restored first: a module that grows is one being made.
    pub fn push(&mut self, item: Item) {
        self.made().push(item);
    }

    /// The items as a vector to change, every one restored where the module was stored.
    fn made(&mut self) -> &mut Vec<Item> {
        if !matches!(self.0, Held::Made(_)) {
            self.0 = Held::Made(self.iter().cloned().collect());
        }
        match &mut self.0 {
            Held::Made(items) => items,
            _ => unreachable!("made above"),
        }
    }
}

/// The place a version made of a module that placed its items at their indices holds the item at `index` at: spaced apart, with room before the first.
fn spaced(index: usize) -> usize {
    (index + 1) * SPACING
}

/// A place as a version's position.
fn position(place: usize) -> i64 {
    i64::try_from(place).expect("a place is counted in an i64")
}

/// The place a version's position is.
fn place_of(position: i64) -> usize {
    usize::try_from(position).expect("a version's places are not negative")
}

/// The entry a version holds at `place`, which it must hold.
fn entry(placed: &Ordered<Entry>, place: usize) -> &Entry {
    placed
        .get(position(place))
        .expect("a place the version holds")
}

impl Default for Items {
    fn default() -> Self {
        Self(Held::Made(Vec::new()))
    }
}

impl From<Vec<Item>> for Items {
    fn from(items: Vec<Item>) -> Self {
        Self(Held::Made(items))
    }
}

/// Items added after the others, as [`Items::push`] adds one.
impl Extend<Item> for Items {
    fn extend<I: IntoIterator<Item = Item>>(&mut self, items: I) {
        self.made().extend(items);
    }
}

impl FromIterator<Item> for Items {
    fn from_iter<I: IntoIterator<Item = Item>>(items: I) -> Self {
        Self(Held::Made(items.into_iter().collect()))
    }
}

impl Index<usize> for Items {
    type Output = Item;

    fn index(&self, place: usize) -> &Item {
        match &self.0 {
            Held::Made(items) => &items[place],
            Held::Stored(stored) => stored.cells[place].get_or_init(|| (stored.restore)(place)),
            Held::Placed(placed) => entry(placed, place).item(),
        }
    }
}

/// The item at a place, to change: a stored module's are all restored first, as [`Items::push`] restores them.
impl IndexMut<usize> for Items {
    fn index_mut(&mut self, place: usize) -> &mut Item {
        &mut self.made()[place]
    }
}

/// The items by value, every one restored where the module was stored.
impl IntoIterator for Items {
    type Item = Item;
    type IntoIter = std::vec::IntoIter<Item>;

    fn into_iter(mut self) -> Self::IntoIter {
        std::mem::take(self.made()).into_iter()
    }
}

impl<'a> IntoIterator for &'a Items {
    type Item = &'a Item;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// A module's items in order ([`Items::iter`]), each restored as it is reached.
pub struct Iter<'a> {
    items: &'a Items,
    places: ItemPlaces,
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Item;

    fn next(&mut self) -> Option<&'a Item> {
        self.places.next().map(|place| &self.items[place])
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.places.size_hint()
    }
}

impl DoubleEndedIterator for Iter<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.places.next_back().map(|place| &self.items[place])
    }
}

impl ExactSizeIterator for Iter<'_> {}

/// Two modules hold the same items where each place holds an equal one, however either came by them.
impl PartialEq for Items {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().eq(other.iter())
    }
}

/// The items in hand, and how many a stored module has yet to read: printing a module restores nothing.
impl fmt::Debug for Items {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Held::Made(items) => items.fmt(formatter),
            Held::Placed(placed) => formatter
                .debug_map()
                .entries(placed.iter().map(|(place, entry)| (place, entry.item())))
                .finish(),
            Held::Stored(stored) => {
                let read = stored.cells.iter().filter_map(OnceCell::get);
                let unread = stored
                    .cells
                    .iter()
                    .filter(|cell| cell.get().is_none())
                    .count();

                formatter
                    .debug_struct("Items")
                    .field("read", &read.collect::<Vec<_>>())
                    .field("unread", &unread)
                    .finish()
            }
        }
    }
}

/// A module's items archived as the vector they are: the ones in hand by reference, and a stored module's each restored first.
pub struct ItemVector;

pub type AsVector = curios_archive::Via<ItemVector>;

impl curios_archive::Proxy<Items> for ItemVector {
    type Archivable = Vec<Item>;

    fn to_archivable(items: &Items) -> impl std::borrow::Borrow<Vec<Item>> {
        match &items.0 {
            Held::Made(items) => std::borrow::Cow::Borrowed(items),
            Held::Stored(_) | Held::Placed(_) => {
                std::borrow::Cow::Owned(items.iter().cloned().collect())
            }
        }
    }

    fn from_archivable(items: Vec<Item>) -> Result<Items, String> {
        Ok(items.into())
    }
}
