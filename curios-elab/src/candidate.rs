//! What elaborating a declaration made, filed for a later compile that lowers it alike: a candidate, holding the items it made, what it asked with the digest of every part it read, and the binders its proofs read ([`Candidating`]).
//!
//! **Found by what it was made from.** A candidate is named by the digest of the declaration's lowered items ([`declaration_input`]) and then by what it holds ([`curios_utilities::candidate`]), so a store holds every candidate filed for one lowering together, whichever unit filed each.
//!
//! **Taken where every part it read digests as it did.** A declaration is [a function of what it reads](../../documentation/design/compilation/a-declaration-is-a-function-of-what-it-reads.md), so one lowered to the items a candidate was made from, every answer it was given standing and every part it read digesting as it did, elaborates to the candidate's items, and a walk over a baseline takes them in place of elaborating. A part of the unit's own is answered in a trace by the unit the trace is kept with, so a candidate holds a digest of each, which no later unit moves; a part of a fixed unit is the compiler's, which a store's address names, and is recorded nowhere.
//!
//! **What a filing makes again is what it made.** A part's digest is of the part as a unit stores it, under no position ([`part_digest`]), so a declaration kept over a baseline, which read each part as it was, files the candidate its baseline filed: a recompile carries it ([`Traces::candidate`]), and a unit compiled whole makes it again, alike.
//!
//! **A witness is elaborated, never taken.** It registers at its key as its signature elaborates, which nothing here replays.

use {
    crate::{Answered, Question, TraceTable, Traces, part_digest},
    curios_core::{Declarations, Global, Item, Module, Places, Trace},
    curios_utilities::{Digest, Facet, Object, candidate},
    std::{cell::RefCell, collections::HashMap},
};

/// What a question read, as a candidate records it: `None` where it read no part a candidate records, and otherwise the digest of the part, `None` within where the name declared no such part.
pub(crate) type Read = Option<Option<[u8; 32]>>;

/// A declaration's elaboration as a store keeps it. See the module documentation.
#[curios_archive::archived]
struct ElaborationCandidate {
    /// What it was made from ([`declaration_input`]), whole, of which its name holds a prefix.
    input: [u8; 32],
    /// The items it made, each by its identity ([`curios_core::ItemRun::identity`]), one for each of its lowered items, in their order.
    items: Vec<[u8; 32]>,
    /// What it asked, as a run of one trace ([`TraceTable::run`]).
    trace: Vec<u8>,
    /// What each question read, in the trace's order: `None` where it read no part — the unit's lemmas, whether a witness is declared, a key no witness answers, a part of the declaration's own or of a fixed unit — and otherwise the digest of the part, `None` within where the name declared no such part. A witness found is read by its signature.
    reads: Vec<Read>,
    /// The written binders its proofs read, each by the name of its definition and its place among that definition's written binders, that a lowering lints unless a proof reads it.
    credited: Vec<(Global, u32)>,
}

/// What the declaration whose lowered items digest to `items`, in their order, was made from: the digest a candidate of its elaboration is found by.
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

/// What a filing makes a unit's candidates of: its lowered module and the declarations its items make, the digest of each lowered item and of each elaborated one by its place, its elaborated module with where it holds each name, and its traces.
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
    /// What a filing makes the candidates of the unit lowered to `lowered`, whose items make `declarations` and digest to `inputs`, and elaborated to `module`, held where `places` says, whose items are named `items`, and whose declarations asked `traces`: `inputs` and `items` each by the place of its item.
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

    /// The candidate of `declaration`, whose proofs read `credited`, as a store keeps it: `None` where none is made of it — a witness, one the unit holds no trace of, or one that read a part that will not digest, or of another unit that the unit records nothing of.
    pub fn candidate(
        &self,
        declaration: usize,
        credited: Vec<(Global, u32)>,
    ) -> Result<Option<Object>, String> {
        let positions = self.declarations.items(declaration);
        let names = positions
            .iter()
            .flat_map(|position| self.lowered.items[*position].declared_names())
            .copied()
            .collect::<Vec<_>>();
        if names
            .iter()
            .any(|name| self.module.witnesses.contains(name))
        {
            return Ok(None);
        }
        let Some(trace) = self.traces.of(&names[0]) else {
            return Ok(None);
        };
        let mut items = Vec::with_capacity(positions.len());
        for position in positions.iter() {
            let made = self.lowered.items[*position]
                .declared_names()
                .first()
                .and_then(|name| self.places.place(name));
            let Some(made) = made.and_then(|made| self.items.get(&made.item())) else {
                return Ok(None);
            };
            items.push(*made.as_bytes());
        }

        let mut reads = Vec::with_capacity(trace.len());
        for (question, answered) in trace.asked() {
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
        let input = declaration_input(inputs);
        let made = ElaborationCandidate {
            input: *input.as_bytes(),
            items,
            trace: TraceTable::run([(&names[0], trace)])?,
            reads,
            credited,
        };

        let bytes = curios_archive::to_bytes(&made)?.to_vec();
        Ok(Some(candidate(Facet::Elaboration, &input, bytes)))
    }

    /// What the declaration declaring `names` read of the part `question` asks, as a candidate records it ([`Read`]): nothing of a part of its own, or of a fixed unit's, which the unit records nothing of; and `None` where the part will not digest.
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
pub(crate) fn bytes_of(digest: &str) -> Option<[u8; 32]> {
    Digest::from_hex(digest).map(|digest| *digest.as_bytes())
}

/// A candidate read back: what it was made from, its items' identities, its trace, what each question read and the binders its proofs read.
pub(crate) struct Candidate {
    pub(crate) items: Vec<Digest>,
    pub(crate) trace: Trace<Question, Answered>,
    pub(crate) reads: Vec<Read>,
    pub(crate) credited: Vec<(Global, u32)>,
}

impl Candidate {
    /// The candidate `bytes` hold, made from `input` by the declaration written as `name` and holding an item for each of its `items` lowered items: `None` for bytes that hold no such candidate.
    pub(crate) fn read(bytes: &[u8], input: &Digest, name: &Global, items: usize) -> Option<Self> {
        let candidate = curios_archive::from_bytes::<ElaborationCandidate>(bytes).ok()?;
        if candidate.input != *input.as_bytes() || candidate.items.len() != items {
            return None;
        }
        let [(traced, trace)] =
            <[_; 1]>::try_from(TraceTable::read(&candidate.trace).ok()?).ok()?;
        if traced != *name || trace.len() != candidate.reads.len() {
            return None;
        }

        Some(Self {
            items: candidate
                .items
                .into_iter()
                .map(Digest::from_bytes)
                .collect(),
            trace,
            reads: candidate.reads,
            credited: candidate.credited,
        })
    }

    /// The items this candidate made, read out of the objects `object` finds by their identities, each run checked once: `None` where one is not found.
    pub(crate) fn made(&self, object: impl Fn(&Digest) -> Option<Object>) -> Option<Vec<Item>> {
        let mut runs = Vec::<(Object, Vec<(usize, u32)>)>::new();
        for (at, identity) in self.items.iter().enumerate() {
            let found = object(identity)?;
            let member = found
                .member
                .filter(|member| member.kind == curios_core::ItemRun::KIND)?;
            match runs
                .iter_mut()
                .find(|(run, _)| run.bytes.as_slice().as_ptr() == found.bytes.as_slice().as_ptr())
            {
                Some((_, members)) => members.push((at, member.index)),
                None => runs.push((found, vec![(at, member.index)])),
            }
        }
        let mut items = vec![None; self.items.len()];
        for (run, members) in runs {
            let indices = members.iter().map(|(_, index)| *index).collect::<Vec<_>>();
            let read = curios_core::ItemRun::members(run.bytes.as_slice(), &indices).ok()?;
            for ((at, _), item) in members.into_iter().zip(read) {
                items[at] = Some(item);
            }
        }

        items.into_iter().collect()
    }
}
