//! One unit erased over a baseline: which of the baseline's erased forms it keeps.
//!
//! **An erased form is kept where its declaration is the baseline's and everything its erasure asked is answered as it was.** An item's erased form is a function of its elaborated declaration and of what its erasure read of the declarations around it. The first is the baseline's where the elaborator kept the declaration; the second is the item's trace, which the baseline's erasure filed (`ErasedUnit`), asked again here. So an item is erased again where it was elaborated again, or where something its erasure read came out another way, and nowhere else: a body respelled to the same binding moves no item that calls it.
//!
//! **Asked only where an edit could have reached.** The baseline's traces read backwards (`Readers`) say which items read each name, so the walk finds from the edit which items it could reach: an item elaborated again or new, one whose trace erasure cannot keep by, a name the baseline declared and the unit does not, and, of a unit in scope that moved, a part read that digests otherwise now. An item that read one of them is reached, and so is one that read an item reached; the walk visits those alone, and every other item keeps its erased form unasked, nothing it read having moved: where the module is a version of the baseline's, the unit erased is a version of the baseline's unit, holding every such form where the baseline holds it with nothing visited.
//!
//! **Asked in the order the items are stored in, which is the order they are read in.** A module's items stand each after the items it reaches, and erasing one reads the erased form of another only where the other is already erased, so what a question is about is decided by the time it is asked: no item waits for another, and none is asked for.
//!
//! **An item to be erased is first looked for in a store.** Where the baseline is handed what a store holds of items erased before ([`ErasedBaseline::candidates`]), an item the walk does not keep is taken from a candidate filed for the item it is, where every part and binding the candidate read digests as it does now ([`ErasedCandidate`]); it is erased otherwise.
//!
//! **A part is the elaborated declaration's, a binding the erased item's.** A question about a name's type, body or registry entry is answered as it was where the elaborator kept the declaration, and for one elaborated again where that part came out equal as it is stored ([`Same`]). A question about how a name is bound is answered as it was where the item declaring it kept its erased form, and for one erased again where an item naming it reads the two alike (`ErasedItem::read_alike`). A question about an erased form whole, which an item evaluated where it is erased asks of each item it ran (`evaluate`), is answered as it was where the item kept its erased form, and for one erased again where the two forms are stored as the same bytes. A name neither the unit nor its baseline declares is another unit's or no one's, and what was read of it stands where the unit that holds it is the one the baseline was erased after, or where it digests now to what the baseline recorded (`Foreign`).

use {
    super::{ErasedCandidate, ErasedUnit, Erasures, Held, Projection},
    crate::{
        Answered, HeldTrace, Question, Read, Same, bytes_of, concepts_alike, entries_alike,
        inducts_alike, part_digest, structs_alike,
    },
    curios_core::{Changes, Global, Item, ItemRun, Module, Nominal, Place, Places, Trace},
    curios_utilities::{Candidates, Digest, Facet},
    std::{
        collections::{BTreeSet, HashMap},
        rc::Rc,
    },
};

/// The baseline a unit is erased over: an earlier text of the unit as it elaborated and as it erased, with every name of a declaration the new text's elaboration made again — every other the unit declares it kept as the baseline holds it — and every name the new text no longer declares.
#[derive(Clone, Copy)]
pub struct ErasedBaseline<'a> {
    pub core: &'a Module,
    pub places: &'a Places,
    pub erased: &'a ErasedUnit,
    /// What erasure read of the baseline's module out of its universes, kept with its unit.
    pub projection: &'a Projection,
    pub made: &'a BTreeSet<Global>,
    pub dropped: &'a BTreeSet<Global>,
    /// Where the module being erased holds each name.
    pub now: &'a Places,
    /// What a store holds of items erased before, where an item to be erased may be taken from it ([`ErasedCandidate`]).
    pub candidates: Option<&'a dyn Candidates>,
}

/// An erased form the unit keeps of its baseline, with what erasing it asked.
pub(super) type Kept = (Held, HeldTrace);

/// What an erasure over a baseline has decided, as it walks the unit's items in the order they are stored in.
pub(super) struct ErasedOver<'a> {
    baseline: ErasedBaseline<'a>,
    module: &'a Module,
    /// What the module changed of the baseline's items, where it is a version of the baseline's module, so the two hold an unchanged item at one place.
    changes: Option<Changes>,
    /// Whether each item the walk has visited, by place, holds the baseline's own erased form.
    kept: HashMap<usize, bool>,
    /// What each name elaborated again left as the baseline holds it, compared the first time a trace asks.
    same: HashMap<Global, Same>,
    /// The units in scope, which say whether each is the one the baseline was erased after.
    scope: Erasures<'a>,
    /// Whether each part or binding a kept form's erasure read of a declaration the unit does not hold, in a scope that moved, is the one the baseline read: asked again once.
    read: HashMap<Global, Vec<(Question, bool)>>,
    /// The places of the items an edit could have reached, by the baseline's traces read backwards: the walk visits these, and every other item keeps its erased form unasked.
    visited: BTreeSet<usize>,
    /// The first names of the baseline's items the unit no longer declares, each one's trace the baseline holds.
    lost: Vec<Global>,
}

impl<'a> ErasedOver<'a> {
    pub(super) fn new(
        baseline: ErasedBaseline<'a>,
        module: &'a Module,
        scope: Erasures<'a>,
    ) -> Self {
        let mut over = Self {
            baseline,
            module,
            changes: module.items.changed_since(&baseline.core.items),
            kept: HashMap::new(),
            same: HashMap::new(),
            scope,
            read: HashMap::new(),
            visited: BTreeSet::new(),
            lost: Vec::new(),
        };
        over.reach();

        over
    }

    /// Mark every item an edit could have reached: each the elaborator made again, each whose trace asked something of the unit as a whole, each that read a part of another unit that moved, and every item that read an item marked or a name the unit no longer declares. Where the module is no version of the baseline's, nothing says which items they share, and each the walk cannot keep by its trace is marked too.
    fn reach(&mut self) {
        let (module, baseline) = (self.module, self.baseline);
        let readers = baseline.erased.readers();
        let mut frontier = Vec::new();

        for name in baseline.made {
            if let Some(place) = baseline.now.place(name) {
                self.mark(place.item(), &mut frontier);
            }
        }
        if self.changes.is_none() {
            for place in module.items.places() {
                let names = module.items.names(place);
                let keepable = names.first().is_some_and(|first| {
                    baseline.erased.held(first).is_some() && baseline.erased.asked(first).is_some()
                });
                if !keepable {
                    self.mark(place, &mut frontier);
                }
            }
        }
        // An item whose trace asked something of the unit as a whole, which no erased form is kept by.
        let whole = readers
            .asked_whole()
            .filter_map(|reader| baseline.now.place(reader))
            .map(|place| place.item())
            .collect::<Vec<_>>();
        for place in whole {
            self.mark(place, &mut frontier);
        }
        // A name the baseline declared and the unit does not.
        for name in baseline.dropped {
            frontier.push(*name);
            if baseline.erased.asked(name).is_some() {
                self.lost.push(*name);
            }
        }
        // A part of a declaration of another unit, where that unit moved and the part digests otherwise now, or the baseline recorded nothing of it.
        if !self.scope.settled() {
            let mut foreign = readers
                .names()
                .filter(|name| {
                    baseline.now.place(name).is_none()
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
                    if moved && let Some(place) = baseline.now.place(reader) {
                        self.mark(place.item(), &mut frontier);
                    }
                }
            }
        }
        while let Some(name) = frontier.pop() {
            for (reader, _) in readers.of_name(&name) {
                if let Some(place) = baseline.now.place(reader) {
                    self.mark(place.item(), &mut frontier);
                }
            }
        }
    }

    /// Mark the item at `place`, and hand each of its names on to `frontier`, where it is not marked yet.
    fn mark(&mut self, place: usize, frontier: &mut Vec<Global>) {
        if self.visited.insert(place) {
            frontier.extend(self.module.items.names(place).into_iter().copied());
        }
    }

    /// The places of the items the walk visits, in the order the module stores them.
    pub(super) fn visited(&self) -> impl Iterator<Item = usize> + '_ {
        self.visited.iter().copied()
    }

    /// What the module changed of the baseline's items, where it is a version of the baseline's module.
    pub(super) fn changes(&self) -> Option<&Changes> {
        self.changes.as_ref()
    }

    /// The first names of the baseline's items the unit no longer declares.
    pub(super) fn lost(&self) -> &[Global] {
        &self.lost
    }

    /// The baseline being erased over.
    pub(super) fn baseline(&self) -> ErasedBaseline<'a> {
        self.baseline
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

    /// The baseline's erased form of `item`, at `place`, with what erasing it asked, where the unit keeps them: `unit` holds the erased forms of the items before it. What is answered is noted for the items after it.
    pub(super) fn keep(&mut self, place: usize, item: &Item, unit: &ErasedUnit) -> Option<Kept> {
        let kept = self.standing(place, item, unit);
        self.kept.insert(place, kept.is_some());

        kept
    }

    fn standing(&mut self, place: usize, item: &Item, unit: &ErasedUnit) -> Option<Kept> {
        let names = item.declared_names();
        let first = names.first()?;
        if names.iter().any(|name| self.baseline.made.contains(*name)) {
            return None;
        }
        let erased = self.baseline.erased.held(first)?;
        let trace = self.baseline.erased.asked(first)?;
        // Nothing an item no edit reached read can have moved.
        if !self.visited.contains(&place) {
            return Some((erased, Rc::clone(trace)));
        }
        for (question, answered) in trace.asked() {
            if !self.answered(question, answered, unit) {
                return None;
            }
        }

        Some((erased, Rc::clone(trace)))
    }

    /// The erased form of `item`, at `place`, taken from a candidate a store holds for the item, where one stands ([`ErasedCandidate`]), with what its erasure asked and the candidate's name: `unit` holds the erased forms of the items before it. Asked of an item the walk does not keep.
    pub(super) fn taken(
        &self,
        place: usize,
        item: &Item,
        unit: &ErasedUnit,
    ) -> Option<(Held, Trace<Question, Answered>, Digest)> {
        let candidates = self.baseline.candidates?;
        let names = item.declared_names();
        let first = names.first()?;
        curios_profile::profile!("erase::candidates");
        let identity = match self.module.items.part(place) {
            Some(stored) => stored.digest,
            None => ItemRun::identity(item).ok()?,
        };

        candidates
            .candidates(Facet::Erasure, &identity)
            .into_iter()
            .find_map(|object| {
                let candidate = ErasedCandidate::read(object.bytes.as_slice(), &identity, first)?;
                if !candidate.stands(self, &names, unit) {
                    return None;
                }
                let form = candidates
                    .object(&candidate.form)
                    .filter(|form| form.member.is_none())?;

                Some((Held::stored(form).ok()?, candidate.trace, object.digest))
            })
    }

    /// What the part or binding `question` asks reads now, as a candidate of the erasure of the item declaring `names` records it ([`Read`]): `unit` holding the erased forms of the items before the one asking. `None` where it cannot be told: a binding of an item the walk has not come to, or a part or a binding that will not digest.
    ///
    /// A binding of an item of the unit is read off the form `unit` holds, and a part off the module being erased, which is final; one of another unit is the record the baseline holds of it where that unit did not move, and digested again otherwise; and a fixed unit's is recorded nowhere.
    pub(super) fn read_now(
        &self,
        names: &[&Global],
        question: &Question,
        unit: &ErasedUnit,
    ) -> Option<Read> {
        let Some(subject) = question
            .subject()
            .filter(|subject| !names.contains(subject))
        else {
            return Some(None);
        };
        let digest = match (question, self.baseline.now.place(subject)) {
            (Question::Erased(_) | Question::Ran(_), Some(place)) => {
                self.kept_form(place.item())?;
                let at = unit.place(subject)?;
                match question {
                    Question::Erased(_) => unit.item(at).read_digest(subject),
                    _ => unit.form_digest(at),
                }
                .map(Some)
            }
            (part, Some(_)) => part_digest(self.module, self.baseline.now, part),
            _ if !self.scope.recorded(subject) => return Some(None),
            _ => match self.baseline.erased.foreign().digest(question) {
                Some(then) if self.scope.unmoved(subject) => Ok(then.clone()),
                _ => self.scope.part_digest(question),
            },
        };

        digest
            .ok()
            .map(|digest| Some(digest.as_deref().and_then(bytes_of)))
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

    /// Whether the item declaring `name`, of the unit, holds the baseline's erased form: one the walk does not visit does, and one it visited where it kept it. `None` for one it visits and has not come to, which nothing before it reads.
    fn kept_form(&self, place: usize) -> Option<bool> {
        match self.visited.contains(&place) {
            true => self.kept.get(&place).copied(),
            false => Some(true),
        }
    }

    /// Whether the erased form of the item declaring `name` is, whole, the one an evaluation ran.
    fn ran_alike(&mut self, name: &Global, unit: &ErasedUnit) -> bool {
        match self.baseline.now.place(name) {
            None => {
                self.baseline.erased.place(name).is_none()
                    && self.read_stands(name, &Question::Ran(*name))
            }
            Some(place) => match self.kept_form(place.item()) {
                Some(true) => true,
                Some(false) => unit.form_alike(self.baseline.erased, name),
                None => false,
            },
        }
    }

    /// Whether an item naming `name` reads the erased form of the item declaring it as it did.
    fn read_alike(&mut self, name: &Global, unit: &ErasedUnit) -> bool {
        match self.baseline.now.place(name) {
            None => {
                self.baseline.erased.place(name).is_none()
                    && self.read_stands(name, &Question::Erased(*name))
            }
            Some(place) => match self.kept_form(place.item()) {
                Some(true) => true,
                Some(false) => match (unit.find(name), self.baseline.erased.find(name)) {
                    (Some(now), Some(then)) => now.read_alike(then, name),
                    _ => false,
                },
                None => false,
            },
        }
    }

    /// Whether the part of a declaration that `question` asks is as the baseline holds it.
    fn held(&mut self, question: &Question) -> bool {
        let Some(name) = question.subject() else {
            return false;
        };
        let Some(at) = self.baseline.now.place(name) else {
            return self.baseline.places.place(name).is_none() && self.read_stands(name, question);
        };
        if !self.baseline.made.contains(name) {
            return true;
        }
        let member = match at {
            Place::Let(_) => 0,
            Place::Member { member, .. } => member as usize,
        };
        let (module, baseline) = (self.module, self.baseline.core);
        let places = self.baseline.places;
        let same = *self.same.entry(*name).or_insert_with(|| {
            let Some(place) = places.place(name) else {
                return Same::default();
            };

            let (made, held) = (&module.items[at.item()], &baseline.items[place.item()]);
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
