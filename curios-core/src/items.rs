//! A module's items, each in hand or restored the first time it is read.
//!
//! **A stored module is read by item.** A compilation reads of a unit in scope the declarations it names, and a library holds thousands a program names a few of; restored whole, every process pays for all of them before its first declaration. So a module that was stored holds a cell for each of its items and a restorer its unit hands it, and reading the item at a place fills that cell and no other.
//!
//! **What a place declares is known without its item.** Where a module holds each name, which of its items are groups and whether two units declare one name are asked of every unit in scope by every compilation, and none of them reads a term: a stored module keeps the names each place declares beside its cells.
//!
//! **Reading it is indexing it.** Every reader that goes by place is written as it would be over a vector, and a reader that walks every item restores every item: slow where a walk was not looked for, and never another answer.

use {
    crate::{Declared, Global, Item},
    std::{
        cell::OnceCell,
        fmt,
        ops::{Index, IndexMut, Range},
        rc::Rc,
    },
};

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

#[derive(Clone)]
enum Held {
    /// Every item, as a module made in this process holds them.
    Made(Vec<Item>),
    /// The items of a module that was stored, shared by every clone of it, so one reading serves them all.
    Stored(Rc<Stored>),
}

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
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Every item in order, each restored where it was not in hand.
    pub fn iter(&self) -> Iter<'_> {
        Iter {
            items: self,
            places: 0..self.len(),
        }
    }

    /// The names the item at `place` declares, in the order it declares them, with nothing restored.
    pub fn names(&self, place: usize) -> Vec<&Global> {
        match &self.0 {
            Held::Made(items) => items[place].declared_names(),
            Held::Stored(stored) => stored.declared[place].names.iter().collect(),
        }
    }

    /// What the item at `place` declares each of its names as, in the order of [`Items::names`], with nothing restored.
    pub fn declared(&self, place: usize) -> Vec<Declared> {
        match &self.0 {
            Held::Made(items) => declared_by(&items[place]),
            Held::Stored(stored) => stored.declared[place].declared.clone(),
        }
    }

    /// Whether the item at `place` is a group, with nothing restored.
    pub fn is_group(&self, place: usize) -> bool {
        match &self.0 {
            Held::Made(items) => matches!(items[place], Item::Rec(_)),
            Held::Stored(stored) => stored.declared[place].group,
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
        if let Held::Stored(_) = &self.0 {
            self.0 = Held::Made(self.iter().cloned().collect());
        }
        match &mut self.0 {
            Held::Made(items) => items,
            Held::Stored(_) => unreachable!("made above"),
        }
    }
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
#[derive(Debug, Clone)]
pub struct Iter<'a> {
    items: &'a Items,
    places: Range<usize>,
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
#[cfg(feature = "archive")]
pub struct ItemVector;

#[cfg(feature = "archive")]
pub type AsVector = curios_archive::Via<ItemVector>;

#[cfg(feature = "archive")]
impl curios_archive::Proxy<Items> for ItemVector {
    type Archivable = Vec<Item>;

    fn to_archivable(items: &Items) -> impl std::borrow::Borrow<Vec<Item>> {
        match &items.0 {
            Held::Made(items) => std::borrow::Cow::Borrowed(items),
            Held::Stored(_) => std::borrow::Cow::Owned(items.iter().cloned().collect()),
        }
    }

    fn from_archivable(items: Vec<Item>) -> Result<Items, String> {
        Ok(items.into())
    }
}
