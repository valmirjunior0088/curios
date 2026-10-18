//! One unit erased over a baseline: which of the baseline's erased forms it keeps.
//!
//! **An erased form is kept where its declaration is the baseline's and everything its erasure asked is answered as it was.** An item's erased form is a function of its elaborated declaration and of what its erasure read of the declarations around it. The first is the baseline's where the elaborator kept the declaration; the second is the item's trace, which the baseline's erasure filed (`ErasedUnit`), asked again here. So an item is erased again where it was elaborated again, or where something its erasure read came out another way, and nowhere else: a body respelled to the same binding moves no item that calls it.
//!
//! **Asked only where an edit could have reached.** The baseline's traces read backwards (`Readers`) say which items read each name, so the walk finds from the edit which items it could reach: an item elaborated again or new, one whose trace erasure cannot keep by, a name the baseline declared and the unit does not, and, of a unit in scope that moved, a part read that digests otherwise now. An item that read one of them is reached, and so is one that read an item reached; every other item keeps its erased form unasked, nothing it read having moved.
//!
//! **Asked in the order the items are stored in, which is the order they are read in.** A module's items stand each after the items it reaches, and erasing one reads the erased form of another only where the other is already erased, so what a question is about is decided by the time it is asked: no item waits for another, and none is asked for.
//!
//! **A part is the elaborated declaration's, a binding the erased item's.** A question about a name's type, body or registry entry is answered as it was where the elaborator kept the declaration, and for one elaborated again where that part came out equal as it is stored ([`Same`]). A question about how a name is bound is answered as it was where the item declaring it kept its erased form, and for one erased again where an item naming it reads the two alike (`ErasedItem::read_alike`). A question about an erased form whole, which an item evaluated where it is erased asks of each item it ran (`evaluate`), is answered as it was where the item kept its erased form, and for one erased again where the two forms are stored as the same bytes. A name neither the unit nor its baseline declares is another unit's or no one's, and what was read of it stands where the unit that holds it is the one the baseline was erased after, or where it digests now to what the baseline recorded (`Foreign`).

use {
    super::{ErasedUnit, Erasures, Held},
    crate::{
        Answered, Question, Same, concepts_alike, entries_alike, inducts_alike, structs_alike,
    },
    curios_core::{Global, Item, Module, Nominal, Places, Trace},
    std::{
        collections::{BTreeSet, HashMap},
        rc::Rc,
    },
};

/// The baseline a unit is erased over: an earlier text of the unit as it elaborated and as it erased, with every name of a declaration the new text's elaboration made again: every other the unit declares it kept as the baseline holds it.
#[derive(Clone, Copy)]
pub struct ErasedBaseline<'a> {
    pub core: &'a Module,
    pub places: &'a Places,
    pub erased: &'a ErasedUnit,
    pub made: &'a BTreeSet<Global>,
}

/// An erased form the unit keeps of its baseline, with what erasing it asked.
pub(super) type Kept = (Held, Rc<Trace<Question, Answered>>);

/// What an erasure over a baseline has decided, as it walks the unit's items in the order they are stored in.
pub(super) struct ErasedOver<'a> {
    baseline: ErasedBaseline<'a>,
    module: &'a Module,
    /// Where the unit declares each name: its item, and its place among the item's definitions.
    declared: HashMap<Global, (usize, usize)>,
    /// Whether each item walked so far holds the baseline's own erased form.
    kept: Vec<bool>,
    /// What each name elaborated again left as the baseline holds it, compared the first time a trace asks.
    same: HashMap<Global, Same>,
    /// The units in scope, which say whether each is the one the baseline was erased after.
    scope: Erasures<'a>,
    /// Whether each part or binding a kept form's erasure read of a declaration the unit does not hold, in a scope that moved, is the one the baseline read: asked again once.
    read: HashMap<Global, Vec<(Question, bool)>>,
    /// Whether an edit could have reached each item, by the baseline's traces read backwards: an item not reached keeps its erased form unasked.
    reached: Vec<bool>,
    /// The place each item walked is at, in the order the walk takes them: what an item numbered by the walk is read at.
    places: Vec<usize>,
    /// The first names of the baseline's items the unit no longer declares.
    lost: Vec<Global>,
}

impl<'a> ErasedOver<'a> {
    pub(super) fn new(
        baseline: ErasedBaseline<'a>,
        module: &'a Module,
        scope: Erasures<'a>,
    ) -> Self {
        let declared = module
            .items
            .iter()
            .enumerate()
            .flat_map(|(item, declared)| {
                declared
                    .declared_names()
                    .into_iter()
                    .enumerate()
                    .map(move |(member, name)| (*name, (item, member)))
            })
            .collect();

        let mut over = Self {
            baseline,
            module,
            declared,
            places: module.items.places().collect(),
            kept: Vec::with_capacity(module.items.len()),
            same: HashMap::new(),
            scope,
            read: HashMap::new(),
            reached: vec![false; module.items.len()],
            lost: Vec::new(),
        };
        over.reach();

        over
    }

    /// Mark every item an edit could have reached: each item the walk cannot keep by its trace, each that read a part of another unit that moved, and every item that read an item marked or a name the unit no longer declares.
    fn reach(&mut self) {
        let (module, baseline) = (self.module, self.baseline);
        let readers = baseline.erased.readers();
        let mut frontier = Vec::new();
        let places = std::mem::take(&mut self.places);
        let mark = |item: usize, reached: &mut Vec<bool>, frontier: &mut Vec<Global>| {
            if !reached[item] {
                reached[item] = true;
                frontier.extend(
                    module.items[places[item]]
                        .declared_names()
                        .into_iter()
                        .copied(),
                );
            }
        };

        // An item elaborated again or new, or one the baseline holds no erased form or trace of.
        for (item, declared) in module.items.iter().enumerate() {
            let names = declared.declared_names();
            let keepable = names.first().is_some_and(|first| {
                names.iter().all(|name| !baseline.made.contains(*name))
                    && baseline.erased.held(first).is_some()
                    && baseline.erased.asked(first).is_some()
            });
            if !keepable {
                mark(item, &mut self.reached, &mut frontier);
            }
        }
        // An item whose trace asked something of the unit as a whole, which no erased form is kept by.
        let whole = readers
            .asked_whole()
            .filter_map(|reader| self.declared.get(reader).map(|(item, _)| *item))
            .collect::<Vec<_>>();
        for item in whole {
            mark(item, &mut self.reached, &mut frontier);
        }
        // A name the baseline declared and the unit does not.
        for name in baseline.erased.names() {
            if !self.declared.contains_key(name) {
                frontier.push(*name);
                if baseline.erased.asked(name).is_some() {
                    self.lost.push(*name);
                }
            }
        }
        // A part of a declaration of another unit, where that unit moved and the part digests otherwise now, or the baseline recorded nothing of it.
        if !self.scope.settled() {
            let mut foreign = readers
                .names()
                .filter(|name| {
                    !self.declared.contains_key(*name)
                        && baseline.places.place(name).is_none()
                        && baseline.erased.place(name).is_none()
                })
                .copied()
                .collect::<Vec<_>>();
            foreign.sort_unstable();
            for name in foreign {
                for (reader, read) in readers.of_name(&name) {
                    let moved = read
                        .questions(name)
                        .any(|question| !self.read_stands(&name, &question));
                    if moved && let Some(&(item, _)) = self.declared.get(reader) {
                        mark(item, &mut self.reached, &mut frontier);
                    }
                }
            }
        }
        while let Some(name) = frontier.pop() {
            for (reader, _) in readers.of_name(&name) {
                if let Some(&(item, _)) = self.declared.get(reader) {
                    mark(item, &mut self.reached, &mut frontier);
                }
            }
        }
        self.places = places;
    }

    /// The first names of the baseline's items the unit no longer declares.
    pub(super) fn lost(&self) -> &[Global] {
        &self.lost
    }

    /// Whether every item walked so far holds the baseline's own erased form.
    pub(super) fn kept_all(&self) -> bool {
        self.kept.iter().all(|kept| *kept)
    }

    /// What the baseline's erasure read of declarations its unit did not hold.
    pub(super) fn earlier(&self) -> &'a ErasedUnit {
        self.baseline.erased
    }

    /// Whether what `question` asks of `name`, a declaration the unit does not hold, is what the baseline's erasure read: the unit that holds it is the one the baseline was erased after, or it digests now to what the baseline recorded of it. A read the baseline recorded nothing of does not stand.
    fn read_stands(&mut self, name: &Global, question: &Question) -> bool {
        if self.scope.unmoved(name) {
            return true;
        }
        let asked = self
            .read
            .get(name)
            .and_then(|asked| asked.iter().find(|(asked, _)| asked == question))
            .map(|(_, stands)| *stands);
        if let Some(stands) = asked {
            return stands;
        }

        let stands = match (
            self.baseline.erased.foreign().digest(question),
            self.scope.part_digest(question),
        ) {
            (Some(then), Ok(now)) => *then == now,
            _ => false,
        };
        self.read
            .entry(*name)
            .or_default()
            .push((*question, stands));

        stands
    }

    /// The baseline's erased form of `item`, the next of the unit's, with what erasing it asked, where the unit keeps them: `unit` holds the erased forms of the items before it. What is answered is noted for the items after it.
    pub(super) fn keep(&mut self, item: &Item, unit: &ErasedUnit) -> Option<Kept> {
        let kept = self.standing(item, unit);
        self.kept.push(kept.is_some());

        kept
    }

    fn standing(&mut self, item: &Item, unit: &ErasedUnit) -> Option<Kept> {
        let names = item.declared_names();
        let first = names.first()?;
        if names.iter().any(|name| self.baseline.made.contains(*name)) {
            return None;
        }
        let erased = self.baseline.erased.held(first)?;
        let trace = self.baseline.erased.asked(first)?;
        // Nothing an item no edit reached read can have moved.
        if !self.reached[self.kept.len()] {
            return Some((erased, Rc::clone(trace)));
        }
        for (question, answered) in trace.asked() {
            if !self.answered(question, answered, unit) {
                return None;
            }
        }

        Some((erased, Rc::clone(trace)))
    }

    /// Whether `question` is answered `answered` now.
    fn answered(&mut self, question: &Question, answered: &Answered, unit: &ErasedUnit) -> bool {
        // An erasure resolves no goal and writes no proof, so it asks nothing of the unit as a whole: a trace that holds such a question is not one to keep an erased form by.
        if *answered != Answered::Part {
            return false;
        }
        match question {
            Question::Erased(name) => self.read_alike(name, unit),
            Question::Ran(name) => self.ran_alike(name, unit),
            part => self.held(part),
        }
    }

    /// Whether the erased form of the item declaring `name` is, whole, the one an evaluation ran.
    fn ran_alike(&mut self, name: &Global, unit: &ErasedUnit) -> bool {
        match self.declared.get(name) {
            None => {
                self.baseline.erased.place(name).is_none()
                    && self.read_stands(name, &Question::Ran(*name))
            }
            Some(&(item, _)) => match self.kept.get(item) {
                Some(true) => true,
                Some(false) => unit.form_alike(self.baseline.erased, name),
                // An item after this one, which nothing before it reads.
                None => false,
            },
        }
    }

    /// Whether an item naming `name` reads the erased form of the item declaring it as it did.
    fn read_alike(&mut self, name: &Global, unit: &ErasedUnit) -> bool {
        match self.declared.get(name) {
            None => {
                self.baseline.erased.place(name).is_none()
                    && self.read_stands(name, &Question::Erased(*name))
            }
            Some(&(item, _)) => match self.kept.get(item) {
                Some(true) => true,
                Some(false) => match (unit.find(name), self.baseline.erased.find(name)) {
                    (Some(now), Some(then)) => now.read_alike(then, name),
                    _ => false,
                },
                // An item after this one, which nothing before it reads.
                None => false,
            },
        }
    }

    /// Whether the part of a declaration that `question` asks is as the baseline holds it.
    fn held(&mut self, question: &Question) -> bool {
        let Some(name) = question.subject() else {
            return false;
        };
        let Some(&(item, member)) = self.declared.get(name) else {
            return self.baseline.places.place(name).is_none() && self.read_stands(name, question);
        };
        if !self.baseline.made.contains(name) {
            return true;
        }
        let (module, baseline) = (self.module, self.baseline.core);
        let places = self.baseline.places;
        let same = *self.same.entry(*name).or_insert_with(|| {
            let Some(place) = places.place(name) else {
                return Same::default();
            };

            let (made, held) = (
                &module.items[self.places[item]],
                &baseline.items[place.item()],
            );
            // An entry rides in the former that declares it, so the two items hold what is compared.
            let (this, that) = (made.nominal(name), held.nominal(name));

            Same::of(made, held, member).keying(
                entries_alike(
                    this.and_then(Nominal::inductive),
                    that.and_then(Nominal::inductive),
                    inducts_alike,
                ),
                entries_alike(
                    this.and_then(Nominal::record),
                    that.and_then(Nominal::record),
                    structs_alike,
                ),
                entries_alike(
                    this.and_then(Nominal::concept),
                    that.and_then(Nominal::concept),
                    concepts_alike,
                ),
            )
        });

        same.answers(question)
    }
}
