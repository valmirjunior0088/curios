//! A module's items, each in hand or restored the first time it is read.
//!
//! **A stored module is read by item.** A compilation reads of a unit in scope the declarations it names, and a library holds thousands a program names a few of; restored whole, every process pays for all of them before its first declaration. So a module that was stored holds a cell for each of its items and a restorer its unit hands it, and reading the item at a place fills that cell and no other.
//!
//! **What a place declares is known without its item.** Where a module holds each name, which of its items are groups and whether two units declare one name are asked of every unit in scope by every compilation, and none of them reads a term: a stored module keeps the names each place declares beside its cells ([`Outlines`]).
//!
//! **An item stored on its own is stored again as it was read.** A module whose items were each archived apart is restored from those parts, and a version that shares one of its items hands back the part it was read from ([`Items::part`]), so a unit filed again writes the bytes of what it did not change as they were.
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

/// What each of a module's items declares, flat: every name in the order of the items and of each item's own, what each is declared as — a former by which kind of declaration it is the former of, with nothing of its entry — where each item's names end, which items are groups, and the declaration each item generated from one was generated from. What is known of the places of a stored module without its items.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Outlines {
    names: Vec<Global>,
    declared: Vec<Declared>,
    /// The end of each item's names in `names`.
    ends: Vec<u32>,
    groups: Vec<bool>,
    /// The declaration each item was generated from, by the name its first definition says it was ([`Items::generated_from`]).
    generated: Vec<Option<Global>>,
}

impl Outlines {
    /// The outlines of `items`, each read off what its place declares: of a stored module, with nothing restored.
    pub fn of(items: &Items) -> Self {
        let mut outlines = Self::default();
        for place in items.places() {
            outlines.push(
                items.names(place).into_iter().copied(),
                items.declared(place),
                items.is_group(place),
                items.generated_from(place),
            );
        }

        outlines
    }

    /// The outline of an item after the others: the names it declares, what each is declared as, in their order, whether it is a group, and the declaration it was generated from.
    pub fn push(
        &mut self,
        names: impl IntoIterator<Item = Global>,
        declared: impl IntoIterator<Item = Declared>,
        group: bool,
        generated: Option<Global>,
    ) {
        self.names.extend(names);
        self.declared.extend(declared);
        debug_assert_eq!(self.names.len(), self.declared.len());
        self.ends
            .push(u32::try_from(self.names.len()).expect("a module's names are counted in a u32"));
        self.groups.push(group);
        self.generated.push(generated);
    }

    /// How many items are outlined.
    pub fn len(&self) -> usize {
        self.ends.len()
    }

    pub fn is_empty(&self) -> bool {
        self.ends.is_empty()
    }

    /// Where the names of the item at `index` lie among every item's.
    fn run(&self, index: usize) -> Range<usize> {
        let start = match index {
            0 => 0,
            _ => self.ends[index - 1] as usize,
        };

        start..self.ends[index] as usize
    }

    fn names(&self, index: usize) -> &[Global] {
        &self.names[self.run(index)]
    }

    fn declared(&self, index: usize) -> &[Declared] {
        &self.declared[self.run(index)]
    }

    fn group(&self, index: usize) -> bool {
        self.groups[index]
    }

    fn generated(&self, index: usize) -> Option<Global> {
        self.generated[index]
    }
}

/// The first name `item` declares.
fn first_name(item: &Item) -> Option<&Global> {
    match item {
        Item::Let(definition) => Some(&definition.name),
        Item::Rec(rec) => rec.definitions.first().map(|definition| &definition.name),
    }
}

/// The declaration `item` was generated from, by the name it declares: a constructor's former, a method wrapper's concept. `None` for a written item.
fn generated_by(item: &Item) -> Option<Global> {
    let kind = match item {
        Item::Let(definition) => &definition.kind,
        Item::Rec(rec) => &rec.definitions[0].kind,
    };

    kind.generated_from().map(Global::Authored)
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

/// A stored module's items: a cell for each, what each declares, and where each is restored from.
struct Stored {
    cells: Vec<OnceCell<Item>>,
    outlines: Outlines,
    source: Source,
}

/// Where a stored module's items are restored from.
enum Source {
    /// What the module's unit hands it, asked for one item at a time.
    Restorer(Box<dyn Fn(usize) -> Item>),
    /// The bytes each item was archived as on its own: none where they were not handed over, and no item can be read.
    Parts(Vec<curios_archive::Bytes>),
}

impl Stored {
    /// The item at `index`, restored the first time it is asked for.
    fn item(&self, index: usize) -> &Item {
        self.cells[index].get_or_init(|| match &self.source {
            Source::Restorer(restore) => restore(index),
            Source::Parts(parts) => {
                let part = parts
                    .get(index)
                    .expect("a stored module's items are handed over before one is read");
                curios_archive::from_bytes::<Item>(part.as_slice())
                    .unwrap_or_else(|error| panic!("a stored item failed to restore: {error}"))
            }
        })
    }

    /// The bytes the item at `index` was archived as on its own, where it was.
    fn part(&self, index: usize) -> Option<&curios_archive::Bytes> {
        match &self.source {
            Source::Parts(parts) => parts.get(index),
            Source::Restorer(_) => None,
        }
    }
}

/// One item of a version [`Items::arranged`] puts in order: one the items hold, at its place among them, or one new to them.
pub enum Arranged {
    Held(usize),
    New(Rc<Item>),
}

/// An item a version holds: one in hand, or a stored module's, read where it is first asked for.
#[derive(Clone)]
enum Entry {
    Made(Rc<Item>),
    Stored(Rc<Stored>, usize),
}

impl Entry {
    /// Whether this is `other`, the one item: an item in hand shared between the two, or one stored module's item at one place. Two items alike in every term but made apart are two.
    fn is(&self, other: &Entry) -> bool {
        match (self, other) {
            (Entry::Made(this), Entry::Made(that)) => Rc::ptr_eq(this, that),
            (Entry::Stored(this, at), Entry::Stored(that, there)) => {
                Rc::ptr_eq(this, that) && at == there
            }
            _ => false,
        }
    }

    fn first_name(&self) -> Option<&Global> {
        match self {
            Entry::Made(item) => first_name(item),
            Entry::Stored(stored, index) => stored.outlines.names(*index).first(),
        }
    }

    fn item(&self) -> &Item {
        match self {
            Entry::Made(item) => item,
            Entry::Stored(stored, index) => stored.item(*index),
        }
    }

    fn names(&self) -> Vec<&Global> {
        match self {
            Entry::Made(item) => item.declared_names(),
            Entry::Stored(stored, index) => stored.outlines.names(*index).iter().collect(),
        }
    }

    fn declared(&self) -> Vec<Declared> {
        match self {
            Entry::Made(item) => declared_by(item),
            Entry::Stored(stored, index) => stored.outlines.declared(*index).to_vec(),
        }
    }

    fn is_group(&self) -> bool {
        match self {
            Entry::Made(item) => matches!(**item, Item::Rec(_)),
            Entry::Stored(stored, index) => stored.outlines.group(*index),
        }
    }

    fn part(&self) -> Option<&curios_archive::Bytes> {
        match self {
            Entry::Made(_) => None,
            Entry::Stored(stored, index) => stored.part(*index),
        }
    }

    fn generated_from(&self) -> Option<Global> {
        match self {
            Entry::Made(item) => generated_by(item),
            Entry::Stored(stored, index) => stored.outlines.generated(*index),
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

/// What a version changed of the items it was made from, by place ([`Items::changed_since`]).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Changes {
    /// The places the version holds another item at than the earlier items hold there, in order.
    pub replaced: Vec<usize>,
    /// The places the version holds an item at and the earlier items hold none at, in order.
    pub inserted: Vec<usize>,
    /// The places the earlier items hold an item at and the version holds none at, in order.
    pub removed: Vec<usize>,
}

/// A module's items, in the order it stores them — see the module documentation.
#[derive(Clone)]
pub struct Items(Held);

impl Items {
    /// The items of a stored module: one for each of `outlines`, the item at a place being what `restore` answers for it, asked once.
    pub fn stored(outlines: Outlines, restore: impl Fn(usize) -> Item + 'static) -> Self {
        Self(Held::Stored(Rc::new(Stored {
            cells: (0..outlines.len()).map(|_| OnceCell::new()).collect(),
            outlines,
            source: Source::Restorer(Box::new(restore)),
        })))
    }

    /// The items of a stored module whose items were each archived on its own: one for each of `outlines`, the item at a place read out of its part the first time it is asked for. No part is handed over yet where `parts` is empty, and reading an item is then a fault; `Err` is parts that are neither none nor one for each item.
    pub fn parted(outlines: Outlines, parts: Vec<curios_archive::Bytes>) -> Result<Self, String> {
        if !parts.is_empty() && parts.len() != outlines.len() {
            return Err(format!(
                "a module outlines {} items and stores {}",
                outlines.len(),
                parts.len()
            ));
        }

        Ok(Self(Held::Stored(Rc::new(Stored {
            cells: (0..outlines.len()).map(|_| OnceCell::new()).collect(),
            outlines,
            source: Source::Parts(parts),
        }))))
    }

    /// These items, a stored module's archived on their own ([`Items::parted`]), with `parts` handed over as their bytes.
    pub fn with_parts(&self, parts: Vec<curios_archive::Bytes>) -> Result<Self, String> {
        match &self.0 {
            Held::Stored(stored) if matches!(stored.source, Source::Parts(_)) => {
                Self::parted(stored.outlines.clone(), parts)
            }
            _ => Err("items not archived on their own are handed no parts".to_string()),
        }
    }

    /// The bytes the item at `place` was archived as on its own, where it was read from them and these items hold it as it was read: what it is archived as again.
    pub fn part(&self, place: usize) -> Option<&curios_archive::Bytes> {
        match &self.0 {
            Held::Made(_) => None,
            Held::Stored(stored) => stored.part(place),
            Held::Placed(placed) => entry(placed, place).part(),
        }
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

    /// What these items changed of `earlier`, where both are versions and so hold each item by place: every place at which they hold another item than `earlier` holds there, or one where it holds none, and every place `earlier` holds an item at and these do not. An item both share is passed over unread, so two versions are compared at the cost of what they do not share. `None` where either is no version, and nothing says which items they share.
    pub fn changed_since(&self, earlier: &Items) -> Option<Changes> {
        let (Held::Placed(now), Held::Placed(then)) = (&self.0, &earlier.0) else {
            return None;
        };
        let mut changes = Changes::default();
        now.diff(then, Entry::is, |position, earlier, held| {
            let place = place_of(position);
            match (earlier, held) {
                (Some(_), Some(_)) => changes.replaced.push(place),
                (None, _) => changes.inserted.push(place),
                (Some(_), None) => changes.removed.push(place),
            }
        });

        Some(changes)
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
            Held::Stored(stored) => stored.outlines.names(place).first(),
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

    /// These items as a version placed apart again, each at its rank as a version made of a module that placed its items at their indices holds it, sharing every item: what a version with no place left between two items is made again as.
    pub fn respaced(&self) -> Items {
        let Held::Placed(placed) = &self.0 else {
            return self.version();
        };
        let mut respaced = Ordered::new();
        for (rank, (_, entry)) in placed.iter().enumerate() {
            respaced.insert(position(spaced(rank)), entry.clone());
        }

        Self(Held::Placed(respaced))
    }

    /// `item` at `place`, in place of what this version held there.
    pub fn replace(&mut self, place: usize, item: Item) {
        self.placed()
            .insert(position(place), Entry::Made(Rc::new(item)));
    }

    /// `item` at a place after `after`, or first where it is `None`, and before the place that follows it, or a version's spacing after `after` where none follows: the place given, or `None` where no place lies between the two, and the version is as it was.
    pub fn insert_after(&mut self, after: Option<usize>, item: Item) -> Option<usize> {
        let placed = self.placed();
        let low = after.map_or(-1, position);
        let place = between(placed, low)?;
        placed.insert(place, Entry::Made(Rc::new(item)));

        Some(place_of(place))
    }

    /// `items`, in their order, at places after `after`, or first where it is `None`, spread evenly up to the place that follows it, or spaced as a version first spaces its items where none follows: their places, or `None` where fewer places lie between the two than there are items, and the version is as it was. One item stands midway, so one put between the same two again stands where it stood.
    pub fn insert_run(&mut self, after: Option<usize>, items: Vec<Item>) -> Option<Vec<usize>> {
        let placed = self.placed();
        let low = after.map_or(-1, position);
        let count = i64::try_from(items.len()).ok()?;
        let step = match placed.after(low) {
            Some((high, _)) => (high - low) / (count + 1),
            None => position(SPACING),
        };
        if step < 1 || low.checked_add(step.checked_mul(count)?).is_none() {
            return None;
        }
        let mut places = Vec::with_capacity(items.len());
        let mut at = low;
        for item in items {
            at += step;
            placed.insert(at, Entry::Made(Rc::new(item)));
            places.push(place_of(at));
        }

        Some(places)
    }

    /// The item at `place`, taken out of this version.
    pub fn remove(&mut self, place: usize) {
        self.placed().remove(position(place));
    }

    /// The item at `place` moved to a place after `after`, or first where it is `None`, and before the place that follows it, read no more than it was: the place it is moved to, or `None` where no place lies between the two, and the version is as it was.
    pub fn relocate(&mut self, place: usize, after: Option<usize>) -> Option<usize> {
        let placed = self.placed();
        let entry = placed.remove(position(place))?;
        let low = after.map_or(-1, position);
        let Some(moved) = between(placed, low) else {
            placed.insert(position(place), entry);
            return None;
        };
        placed.insert(moved, entry);

        Some(place_of(moved))
    }

    /// These items, as a version, in the order `arranged` gives — each one held at a place of these items, or one new to them — placed apart as a version first places its items, sharing every item held: what a version with no place left between two items is made again as.
    pub fn arranged(&self, arranged: impl IntoIterator<Item = Arranged>) -> Items {
        let version = self.version();
        let Held::Placed(placed) = &version.0 else {
            unreachable!("a version is placed");
        };
        let mut respaced = Ordered::new();
        for (rank, item) in arranged.into_iter().enumerate() {
            let entry = match item {
                Arranged::Held(place) => placed
                    .get(position(place))
                    .expect("an item held is at a place of the items")
                    .clone(),
                Arranged::New(item) => Entry::Made(item),
            };
            respaced.insert(position(spaced(rank)), entry);
        }

        Self(Held::Placed(respaced))
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
            Held::Stored(stored) => stored.outlines.names(place).iter().collect(),
            Held::Placed(placed) => entry(placed, place).names(),
        }
    }

    /// What the item at `place` declares each of its names as, in the order of [`Items::names`], with nothing restored.
    pub fn declared(&self, place: usize) -> Vec<Declared> {
        match &self.0 {
            Held::Made(items) => declared_by(&items[place]),
            Held::Stored(stored) => stored.outlines.declared(place).to_vec(),
            Held::Placed(placed) => entry(placed, place).declared(),
        }
    }

    /// The declaration the item at `place` was generated from, by the name it declares, with nothing restored: a constructor's former, a method wrapper's concept, and `None` for a written item.
    pub fn generated_from(&self, place: usize) -> Option<Global> {
        match &self.0 {
            Held::Made(items) => generated_by(&items[place]),
            Held::Stored(stored) => stored.outlines.generated(place),
            Held::Placed(placed) => entry(placed, place).generated_from(),
        }
    }

    /// Whether the item at `place` is a group, with nothing restored.
    pub fn is_group(&self, place: usize) -> bool {
        match &self.0 {
            Held::Made(items) => matches!(items[place], Item::Rec(_)),
            Held::Stored(stored) => stored.outlines.group(place),
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
/// The position midway between `low` and the position after it in `placed`, or a version's spacing after `low` where none follows: `None` where no position lies between the two.
fn between(placed: &Ordered<Entry>, low: i64) -> Option<i64> {
    let place = match placed.after(low) {
        Some((high, _)) => low + (high - low) / 2,
        None => low.checked_add(position(SPACING))?,
    };

    (place > low).then_some(place)
}

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
            Held::Stored(stored) => stored.item(place),
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
