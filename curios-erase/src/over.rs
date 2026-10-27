//! One unit erased over a baseline: which of the baseline's erased forms it keeps.
//!
//! **An erased form is kept where its declaration is the baseline's and everything its erasure read digests as it did.** An item's erased form is a function of its certified declaration and of what its erasure read of the declarations around it. The first is the baseline's where the elaborator kept the declaration; the second is the item's record, which the baseline's erasure filed (`ErasedUnit`), asked again here. So an item is erased again where it was elaborated again, or where something its erasure read came out another way, and nowhere else: a body respelled to the same binding moves no item that calls it.
//!
//! **Asked only where an edit could have reached.** The baseline's records read backwards ([`AskedBy`](crate::AskedBy)) say which items read each name, so the walk finds from the edit which items it could reach: an item elaborated again or new, one the baseline holds no record of, a name the baseline declared and the unit does not, and, of a unit in scope that moved, a part read that digests otherwise now. An item that read one of them is reached, and so is one that read an item reached; the walk visits those alone, and every other item keeps its erased form unasked, nothing it read having moved: where the module is a version of the baseline's, the unit erased is a version of the baseline's unit, holding every such form where the baseline holds it with nothing visited.
//!
//! **Asked in the order the items are stored in, which is the order they are read in.** A module's items stand each after the items it reaches, and erasing one reads the erased form of another only where the other is already erased, so what a question is about is decided by the time it is asked: no item waits for another, and none is asked for.
//!
//! **An item to be erased is first looked for in a store.** Where the baseline is handed what a store holds of items erased before ([`ErasedBaseline::candidates`]), an item the walk does not keep is taken from a record filed of an erasure of the item it is, where every part the record read digests as it does now ([`ErasedUnit::complete`]); it is erased otherwise.
//!
//! **A part is the certified declaration's, a binding the erased item's.** A signature, a body or a kind read of a declaration of the unit stands where the elaborator kept the declaration, and for one elaborated again where the part digests as it did: a signature and a body by the certifier's own digest, of the module against the record its walk filed and of the baseline against its own ([`curios_cert::part_digest`]), and a kind by erasure's. A binding read stands where the item declaring it kept its erased form, and for one erased again where an item naming it reads the two alike (`ErasedItem::read_alike`). An erased form read whole, which an item evaluated where it is erased reads of each item it ran (`evaluate`), stands where the item kept its erased form, and for one erased again where the two forms are stored as the same bytes. A name neither the unit nor its baseline declares is another unit's or no one's, and what was read of it stands where the unit that holds it is the one the baseline was erased after, or where it digests now to what the baseline recorded.

use {
    crate::{
        ErasedUnit, Erasures, Held, HeldAsked, Part, Read, bytes_of, declared_digest,
        erased_digest, found,
    },
    curios_utilities::Candidates,
    std::{
        collections::{BTreeSet, HashMap},
        rc::Rc,
    },
};

/// The baseline a unit is erased over: an earlier text of the unit as it was certified and as it erased, with every name of a declaration the new text's elaboration made again — every other the unit declares it kept as the baseline holds it — and every name the new text no longer declares.
#[derive(Clone, Copy)]
pub struct ErasedBaseline<'a> {
    pub core: &'a curios_core::Module,
    pub places: &'a curios_core::Places,
    /// The record the baseline's certification filed: what a part read of it is digested under.
    pub certification: &'a curios_core::Certification,
    pub erased: &'a ErasedUnit,
    pub made: &'a BTreeSet<curios_core::Global>,
    pub dropped: &'a BTreeSet<curios_core::Global>,
    /// Where the module being erased holds each name.
    pub now: &'a curios_core::Places,
    /// The record the kernel filed of the module being erased: what a part read of it is digested under.
    pub recorded: &'a curios_core::Certification,
    /// What a store holds of items erased before, where an item to be erased may be taken from one of its records ([`ErasedUnit::complete`]).
    pub candidates: Option<&'a dyn Candidates>,
}

/// An erased form the unit keeps of its baseline, with what erasing it read.
pub type Kept = (Held, HeldAsked);

/// What an erasure over a baseline has decided, as it walks the unit's items in the order they are stored in.
pub struct ErasedOver<'a> {
    baseline: ErasedBaseline<'a>,
    module: &'a curios_core::Module,
    /// What the module changed of the baseline's items, where it is a version of the baseline's module, so the two hold an unchanged item at one place.
    changes: Option<curios_core::Changes>,
    /// Whether each item the walk has visited, by place, holds the baseline's own erased form.
    kept: HashMap<usize, bool>,
    /// Whether each part of a name elaborated again digests as the baseline's, compared the first time a record asks.
    same: HashMap<(curios_core::Global, Part), bool>,
    /// The units in scope, which say whether each is the one the baseline was erased after.
    scope: Erasures<'a>,
    /// Whether each part a kept form's erasure read of a declaration the unit does not hold, in a scope that moved, is the one the baseline read: asked again once.
    read: HashMap<(curios_core::Global, Part), bool>,
    /// The places of the items an edit could have reached, by the baseline's records read backwards: the walk visits these, and every other item keeps its erased form unasked.
    visited: BTreeSet<usize>,
    /// The first names of the baseline's items the unit no longer declares, each one's record the baseline holds.
    lost: Vec<curios_core::Global>,
}

impl<'a> ErasedOver<'a> {
    pub fn new(
        baseline: ErasedBaseline<'a>,
        module: &'a curios_core::Module,
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

    /// Mark every item an edit could have reached: each the elaborator made again, each that read a part of another unit that moved, and every item that read an item marked or a name the unit no longer declares. Where the module is no version of the baseline's, nothing says which items they share, and each the walk cannot keep by its record is marked too.
    fn reach(&mut self) {
        let (module, baseline) = (self.module, self.baseline);
        let readers = baseline.erased.readers();
        let mut frontier = Vec::new();

        for name in baseline.made {
            if let Some(place) = baseline.now.place(name) {
                self.mark(place.item(), &mut frontier);
            }
        }
        // An item held where the baseline held another or none, the baseline's own moved there: visited, so its form is kept at the place the module holds it at, every form the baseline held where the module holds another being taken out.
        if let Some(changes) = &self.changes {
            self.visited
                .extend(changes.inserted.iter().chain(&changes.replaced).copied());
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
                    let moved = read.parts().any(|part| !self.read_stands(&name, part));
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
    fn mark(&mut self, place: usize, frontier: &mut Vec<curios_core::Global>) {
        if self.visited.insert(place) {
            frontier.extend(self.module.items.names(place).into_iter().copied());
        }
    }

    /// The places of the items the walk visits, in the order the module stores them.
    pub fn visited(&self) -> impl Iterator<Item = usize> + '_ {
        self.visited.iter().copied()
    }

    /// What the module changed of the baseline's items, where it is a version of the baseline's module.
    pub fn changes(&self) -> Option<&curios_core::Changes> {
        self.changes.as_ref()
    }

    /// The first names of the baseline's items the unit no longer declares.
    pub fn lost(&self) -> &[curios_core::Global] {
        &self.lost
    }

    /// The baseline being erased over.
    pub fn baseline(&self) -> ErasedBaseline<'a> {
        self.baseline
    }

    /// What the baseline's erasure read of declarations its unit did not hold.
    pub fn earlier(&self) -> &'a ErasedUnit {
        self.baseline.erased
    }

    /// Whether `part` of `name`, a declaration the unit does not hold, is what the baseline's erasure read: the unit that holds it is the one the baseline was erased after, or it digests now to what the baseline recorded of it. A read the baseline recorded nothing of does not stand.
    fn read_stands(&mut self, name: &curios_core::Global, part: Part) -> bool {
        if self.scope.unmoved(name) {
            return true;
        }
        if let Some(stands) = self.read.get(&(*name, part)) {
            return *stands;
        }

        let stands = match (
            self.baseline.erased.asked_items().abroad(name, part),
            self.scope.part_digest((*name, part)),
        ) {
            (Some(then), Ok(now)) => *then == now,
            _ => false,
        };
        self.read.insert((*name, part), stands);

        stands
    }

    /// The baseline's erased form of `item`, at `place`, with what erasing it read, where the unit keeps them: `unit` holds the erased forms of the items before it. What is answered is noted for the items after it.
    pub fn keep(
        &mut self,
        place: usize,
        item: &curios_core::Item,
        unit: &ErasedUnit,
    ) -> Option<Kept> {
        let kept = self.standing(place, item, unit);
        self.kept.insert(place, kept.is_some());

        kept
    }

    fn standing(
        &mut self,
        place: usize,
        item: &curios_core::Item,
        unit: &ErasedUnit,
    ) -> Option<Kept> {
        let names = item.declared_names();
        let first = names.first()?;
        if names.iter().any(|name| self.baseline.made.contains(*name)) {
            return None;
        }
        let erased = self.baseline.erased.held(first)?;
        let record = self.baseline.erased.asked(first)?;
        // Nothing an item no edit reached read can have moved.
        if !self.visited.contains(&place) {
            return Some((erased, Rc::clone(record)));
        }
        for (name, part) in record.reads() {
            if !self.stands_now(&name, part, unit) {
                return None;
            }
        }

        Some((erased, Rc::clone(record)))
    }

    /// The erased form of `item`, at `place`, taken from a record a store holds of an erasure of the item, where one stands ([`ErasedUnit::complete`]), with the record: `unit` holds the erased forms of the items before it. Asked of an item the walk does not keep.
    pub fn taken(
        &self,
        place: usize,
        item: &curios_core::Item,
        unit: &ErasedUnit,
    ) -> Option<(Held, HeldAsked)> {
        let candidates = self.baseline.candidates?;
        let names = item.declared_names();
        let first = names.first()?;
        curios_profile::profile!("erase::candidates");
        let identity = match self.module.items.part(place) {
            Some(stored) => stored.digest,
            None => curios_core::ItemRun::identity(item).ok()?,
        };

        found(candidates, &identity, first)
            .into_iter()
            .find_map(|record| {
                let made = record.made()?;
                if !self.stands(&names, made, &record, unit) {
                    return None;
                }
                let form = candidates
                    .object(&made.forms)
                    .filter(|form| form.member.is_none())?;

                Some((Held::stored(form).ok()?, Rc::clone(&record)))
            })
    }

    /// What `part` of `name` reads now, as a completed record of the erasure of the item declaring `names` holds it ([`Read`]): `unit` holding the erased forms of the items before the one asking. `None` where it cannot be told: a binding or forms of an item the walk has not come to, or a part that will not digest.
    ///
    /// A binding or forms of an item of the unit are read off what `unit` holds, and a part of a declaration of the unit off the module being erased under the record its walk filed, which are final; one of another unit is the record the baseline holds of it where that unit did not move, and digested again otherwise; and a fixed unit's is recorded nowhere.
    pub fn read_now(
        &self,
        names: &[&curios_core::Global],
        (name, part): (curios_core::Global, Part),
        unit: &ErasedUnit,
    ) -> Option<Read> {
        if names.contains(&&name) {
            return Some(None);
        }
        let digest = match (part, self.baseline.now.place(&name)) {
            (Part::Binding | Part::Forms, Some(place)) => {
                self.kept_form(place.item())?;
                erased_digest(unit, unit.place(&name)?, (name, part))
            }
            (_, Some(_)) => declared_digest(
                self.module,
                self.baseline.now,
                self.baseline.recorded,
                (name, part),
            ),
            _ if !self.scope.recorded(&name) => return Some(None),
            _ => match self.baseline.erased.asked_items().abroad(&name, part) {
                Some(then) if self.scope.unmoved(&name) => Ok(then.clone()),
                _ => self.scope.part_digest((name, part)),
            },
        };

        digest
            .ok()
            .map(|digest| Some(digest.as_deref().and_then(bytes_of)))
    }

    /// Whether `part` of `name` reads now as the baseline's erasure read it.
    fn stands_now(&mut self, name: &curios_core::Global, part: Part, unit: &ErasedUnit) -> bool {
        match part {
            Part::Binding => self.read_alike(name, unit),
            Part::Forms => self.ran_alike(name, unit),
            Part::Signature | Part::Body | Part::Kind => self.held(name, part),
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
    fn ran_alike(&mut self, name: &curios_core::Global, unit: &ErasedUnit) -> bool {
        match self.baseline.now.place(name) {
            None => {
                self.baseline.erased.place(name).is_none() && self.read_stands(name, Part::Forms)
            }
            Some(place) => match self.kept_form(place.item()) {
                Some(true) => true,
                Some(false) => unit.form_alike(self.baseline.erased, name),
                None => false,
            },
        }
    }

    /// Whether an item naming `name` reads the erased form of the item declaring it as it did.
    fn read_alike(&mut self, name: &curios_core::Global, unit: &ErasedUnit) -> bool {
        match self.baseline.now.place(name) {
            None => {
                self.baseline.erased.place(name).is_none() && self.read_stands(name, Part::Binding)
            }
            Some(place) => match self.kept_form(place.item()) {
                Some(true) => true,
                Some(false) => match (
                    unit.place(name).map(|at| unit.item(at)),
                    self.baseline
                        .erased
                        .place(name)
                        .map(|at| self.baseline.erased.item(at)),
                ) {
                    (Some(now), Some(then)) => now.read_alike(then, name),
                    _ => false,
                },
                None => false,
            },
        }
    }

    /// Whether `part` of `name`, a declaration the unit or another unit holds, is as the baseline's erasure read it: of the unit, a declaration the elaborator kept is the baseline's, and one it made again stands where the part digests as it did, under the record each walk filed of it.
    fn held(&mut self, name: &curios_core::Global, part: Part) -> bool {
        if self.baseline.now.place(name).is_none() {
            return self.baseline.places.place(name).is_none() && self.read_stands(name, part);
        }
        if !self.baseline.made.contains(name) {
            return true;
        }
        let (module, baseline) = (self.module, self.baseline);

        *self.same.entry((*name, part)).or_insert_with(|| {
            let now = declared_digest(module, baseline.now, baseline.recorded, (*name, part));
            let then = declared_digest(
                baseline.core,
                baseline.places,
                baseline.certification,
                (*name, part),
            );

            matches!((now, then), (Ok(now), Ok(then)) if now == then)
        })
    }
}
