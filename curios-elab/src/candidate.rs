//! A declaration's elaboration as a store keeps it to be found by what the declaration was lowered to: its trace, completed where it is filed with what the declaration was made from, the digest of every part it read and what it made ([`Candidating`], [`Made`]).
//!
//! **Found by what it was made from.** A completed record is named by the digest of the declaration's lowered items ([`declaration_input`]) and then by what it holds ([`curios_utilities::candidate_name`]), so a store holds every record filed for one lowering together, whichever unit filed each ([`found`]).
//!
//! **Taken where every part it read digests as it did.** A declaration is [a function of what it reads](../../documentation/design/compilation/a-declaration-is-a-function-of-what-it-reads.md), so one lowered to the items a record was made from, every answer it was given standing and every part it read digesting as it did, elaborates to the record's items, and a walk over a baseline takes them in place of elaborating. A part of the unit's own is answered in a trace by the unit the trace is kept with, so a completed record holds a digest of each, which no later unit moves; a part of a fixed unit is the compiler's, which a store's address names, and is recorded nowhere.
//!
//! **What a filing completes again is what it completed.** A part's digest is of the part as it is compared, under no position ([`part_digest`]), so a declaration kept over a baseline, which read each part as it was, holds the record its baseline filed, and a unit compiled whole completes it again alike.
//!
//! **A witness is elaborated, never taken.** It registers at its key as its signature elaborates, which nothing here replays.

use {
    crate::{
        Answered, HeldTrace, Made, Question, Read, TraceRecord, TraceTable, Traces, part_digest,
    },
    curios_core::{Declarations, Global, Item, ItemRun, Module, Places},
    curios_utilities::{Candidates, Digest, Facet, Object},
    std::{cell::RefCell, collections::HashMap, rc::Rc},
};

/// What the declaration whose lowered items digest to `items`, in their order, was made from: the digest its elaboration's record is found by.
pub fn declaration_input(items: impl IntoIterator<Item = Digest>) -> Digest {
    let mut bytes = Vec::new();
    for item in items {
        bytes.extend_from_slice(item.as_bytes());
    }

    Digest::of(&bytes)
}

/// The digest of the lowered item at `position` of `lowered`, as a store keeps it: the object it was read from, where it was, and its archive otherwise.
pub fn lowered_digest(lowered: &Module, position: usize) -> Result<Digest, String> {
    match lowered.items.part(position) {
        Some(stored) => Ok(stored.digest),
        None => curios_archive::to_bytes(&lowered.items[position]).map(|bytes| Digest::of(&bytes)),
    }
}

/// What a filing completes a unit's elaboration records with: its lowered module and the declarations its items make, the digest of each lowered item and of each elaborated one by its place, its elaborated module with where it holds each name, and its traces.
pub struct Candidating<'a> {
    lowered: &'a Module,
    declarations: &'a Declarations,
    /// The digest of each lowered item, by its place.
    inputs: HashMap<usize, Digest>,
    module: &'a Module,
    places: &'a Places,
    /// The identity of each elaborated item, by its place.
    items: HashMap<usize, Digest>,
    traces: &'a Traces,
    /// What each part asked digests to, made once however many declarations read it: `None` where it will not digest, and `None` within where the name declared no such part.
    read: RefCell<HashMap<Question, Option<Option<[u8; 32]>>>>,
}

impl<'a> Candidating<'a> {
    /// What a filing completes the records of the unit lowered to `lowered`, whose items make `declarations` and digest to `inputs`, and elaborated to `module`, held where `places` says, whose items are named `items`, and whose declarations asked `traces`: `inputs` and `items` each by the place of its item.
    pub fn new(
        lowered: &'a Module,
        declarations: &'a Declarations,
        inputs: impl IntoIterator<Item = (usize, Digest)>,
        module: &'a Module,
        places: &'a Places,
        items: impl IntoIterator<Item = (usize, Digest)>,
        traces: &'a Traces,
    ) -> Self {
        Self {
            lowered,
            declarations,
            inputs: inputs.into_iter().collect(),
            module,
            places,
            items: items.into_iter().collect(),
            traces,
            read: RefCell::default(),
        }
    }

    /// Complete the record of each declaration that no filing has, `credited` saying which written binders the proofs of the declaration declaring its names read: what the declaration made and read, or nothing for one no record can be completed of — a witness, or one that read a part that will not digest, or of another unit that the unit records nothing of.
    pub fn complete(
        &self,
        credited: impl Fn(&[Global]) -> Vec<(Global, u32)>,
    ) -> Result<(), String> {
        curios_profile::profile!("elaborations::complete");
        for declaration in self.declarations.each() {
            // Asked of what each place declares, which a stored module knows without its item: a filing over a store completes what its edit made and restores nothing else.
            let Some(first) = self.lowered.items.names(declaration).first().copied() else {
                continue;
            };
            let Some(held) = self.traces.held(first) else {
                continue;
            };
            if held.completed() {
                continue;
            }
            held.complete(self.made(declaration, held, &credited)?);
        }

        Ok(())
    }

    /// What `declaration`, whose trace is `held`, made and read: `None` where no record of it can be completed.
    fn made(
        &self,
        declaration: usize,
        held: &TraceRecord,
        credited: &impl Fn(&[Global]) -> Vec<(Global, u32)>,
    ) -> Result<Option<Made>, String> {
        let positions = self.declarations.items(declaration);
        let names = positions
            .iter()
            .flat_map(|position| self.lowered.items.names(*position))
            .copied()
            .collect::<Vec<_>>();
        if names
            .iter()
            .any(|name| self.module.witnesses.contains(name))
        {
            return Ok(None);
        }
        let mut outputs = Vec::with_capacity(positions.len());
        for position in positions.iter() {
            let made = self
                .lowered
                .items
                .names(*position)
                .first()
                .and_then(|name| self.places.place(name));
            let Some(made) = made.and_then(|made| self.items.get(&made.item())) else {
                return Ok(None);
            };
            outputs.push(*made);
        }

        let mut reads = Vec::with_capacity(held.len());
        for (question, answered) in held.asked() {
            let read = match (question, answered) {
                (Question::Witness(..), Answered::Witness(Some(found))) => {
                    self.read(&names, &Question::Signature(*found))
                }
                (Question::Witness(..) | Question::WitnessDeclared(..) | Question::Lemmas, _) => {
                    Some(None)
                }
                (part, _) => self.read(&names, part),
            };
            let Some(read) = read else {
                return Ok(None);
            };
            reads.push(read);
        }
        let Some(inputs) = positions
            .iter()
            .map(|position| self.inputs.get(position).copied())
            .collect::<Option<Vec<_>>>()
        else {
            return Ok(None);
        };

        Ok(Some(Made {
            input: declaration_input(inputs),
            reads,
            outputs,
            credited: credited(&names),
        }))
    }

    /// What the declaration declaring `names` read of the part `question` asks, as a completed record holds it ([`Read`]): nothing of a part of its own, or of a fixed unit's, which the unit records nothing of; and `None` where the part will not digest.
    fn read(&self, names: &[Global], question: &Question) -> Option<Read> {
        let Some(subject) = question
            .subject()
            .filter(|subject| !names.contains(subject))
        else {
            return Some(None);
        };
        if let Some(read) = self.read.borrow().get(question) {
            return read.map(Some);
        }

        let read = match self.places.place(subject) {
            Some(_) => part_digest(self.module, self.places, question)
                .ok()
                .map(|digest| digest.and_then(|digest| bytes_of(&digest))),
            // What a unit records of a read of another is every read but a fixed unit's.
            None => match self.traces.foreign().digest(question) {
                Some(digest) => Some(digest.as_deref().and_then(bytes_of)),
                None => return Some(None),
            },
        };
        self.read.borrow_mut().insert(*question, read);

        read.map(Some)
    }
}

/// The thirty-two bytes a part's digest spells.
pub fn bytes_of(digest: &str) -> Option<[u8; 32]> {
    Digest::from_hex(digest).map(|digest| *digest.as_bytes())
}

/// The completed records of the declaration written as `name` that `facet` made from `input`, as `candidates` holds them, each with the digest it is stored under: what a walk looks among before it makes the declaration.
pub fn found(
    candidates: &dyn Candidates,
    facet: Facet,
    input: &Digest,
    name: &Global,
) -> Vec<HeldTrace> {
    candidates
        .candidates(facet, input)
        .into_iter()
        .filter_map(|object| {
            let member = object
                .member
                .filter(|member| member.kind == TraceTable::KIND)?;
            let (traced, trace, made) =
                TraceTable::member(object.bytes.as_slice(), member.index).ok()?;
            let made =
                made.filter(|made| made.input == *input && made.reads.len() == trace.len())?;

            (traced == *name)
                .then(|| Rc::new(TraceRecord::restored(trace, Some(made), object.digest)))
        })
        .collect()
}

/// The items `made` names, read out of the objects `object` finds by their identities, each run checked once: `None` where one is not found.
pub fn items_of(made: &Made, object: impl Fn(&Digest) -> Option<Object>) -> Option<Vec<Item>> {
    let mut runs = Vec::<(Object, Vec<(usize, u32)>)>::new();
    for (at, identity) in made.outputs.iter().enumerate() {
        let found = object(identity)?;
        let member = found.member.filter(|member| member.kind == ItemRun::KIND)?;
        match runs
            .iter_mut()
            .find(|(run, _)| run.bytes.as_slice().as_ptr() == found.bytes.as_slice().as_ptr())
        {
            Some((_, members)) => members.push((at, member.index)),
            None => runs.push((found, vec![(at, member.index)])),
        }
    }
    let mut items = vec![None; made.outputs.len()];
    for (run, members) in runs {
        let indices = members.iter().map(|(_, index)| *index).collect::<Vec<_>>();
        let read = ItemRun::members(run.bytes.as_slice(), &indices).ok()?;
        for ((at, _), item) in members.into_iter().zip(read) {
            items[at] = Some(item);
        }
    }

    items.into_iter().collect()
}
