//! One unit erased over a baseline: which of the baseline's erased forms it keeps.
//!
//! **An erased form is kept where its declaration is the baseline's and everything its erasure asked is answered as it was.** An item's erased form is a function of its elaborated declaration and of what its erasure read of the declarations around it. The first is the baseline's where the elaborator kept the declaration; the second is the item's trace, which the baseline's erasure filed (`ErasedUnit`), asked again here. So an item is erased again where it was elaborated again, or where something its erasure read came out another way, and nowhere else: a body respelled to the same binding moves no item that calls it.
//!
//! **Asked in the order the items are stored in, which is the order they are read in.** A module's items stand each after the items it reaches, and erasing one reads the erased form of another only where the other is already erased, so what a question is about is decided by the time it is asked: no item waits for another, and none is asked for.
//!
//! **A part is the elaborated declaration's, a binding the erased item's.** A question about a name's type, body or registry entry is answered as it was where the elaborator kept the declaration, and for one elaborated again where that part came out equal as it is stored ([`Same`]). A question about how a name is bound is answered as it was where the item declaring it kept its erased form, and for one erased again where an item naming it reads the two alike (`ErasedItem::read_alike`). A name neither the unit nor its baseline declares is another unit's, which the baseline was erased after as this unit is.

use {
    super::{ErasedUnit, Held},
    crate::{
        Answered, Question, Same, concepts_alike, entries_alike, inducts_alike, structs_alike,
    },
    curios_core::{Global, Item, Module, Nominal, Places, Trace},
    std::{
        collections::{BTreeSet, HashMap},
        rc::Rc,
    },
};

/// The baseline a unit is erased over: an earlier text of the unit as it elaborated and as it erased, with every name of a declaration the new text's elaboration kept as the baseline holds it.
#[derive(Clone, Copy)]
pub struct ErasedBaseline<'a> {
    pub core: &'a Module,
    pub places: &'a Places,
    pub erased: &'a ErasedUnit,
    pub kept: &'a BTreeSet<Global>,
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
}

impl<'a> ErasedOver<'a> {
    pub(super) fn new(baseline: ErasedBaseline<'a>, module: &'a Module) -> Self {
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

        Self {
            baseline,
            module,
            declared,
            kept: Vec::with_capacity(module.items.len()),
            same: HashMap::new(),
        }
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
        if !names.iter().all(|name| self.baseline.kept.contains(*name)) {
            return None;
        }
        let erased = self.baseline.erased.held(first)?;
        let trace = self.baseline.erased.asked(first)?;
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
            part => self.held(part),
        }
    }

    /// Whether an item naming `name` reads the erased form of the item declaring it as it did.
    fn read_alike(&self, name: &Global, unit: &ErasedUnit) -> bool {
        match self.declared.get(name) {
            None => self.baseline.erased.place(name).is_none(),
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
            return self.baseline.places.place(name).is_none();
        };
        if self.baseline.kept.contains(name) {
            return true;
        }
        let (module, baseline) = (self.module, self.baseline.core);
        let places = self.baseline.places;
        let same = *self.same.entry(*name).or_insert_with(|| {
            let Some(place) = places.place(name) else {
                return Same::default();
            };

            let (made, held) = (&module.items[item], &baseline.items[place.item()]);
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
