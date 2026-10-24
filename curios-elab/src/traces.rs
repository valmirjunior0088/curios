//! What each declaration of a unit asked of the declarations around it, kept with the unit: the record a later compile of the same unit keeps a declaration's elaboration by.
//!
//! **One trace for each declaration, under the name it is written as.** A declaration is an item with every item the lowering generated from it, attempted together in one state, so what the attempt that stood asked is the declaration's. It is filed under the first name its written item declares, which a later lowering of the same text gives it again.
//!
//! **A version of the baseline's.** A unit compiled over a baseline holds its baseline's traces with the trace of each declaration it made filed in place of the one it replaces and each declaration it dropped taken out: the table is persistent and read back in the order of its names ([`Sorted`]), so a trace kept is shared, never copied, and nothing is visited to keep it.
//!
//! **Each declaration's trace an object, stored with each name written once.** A trace is stored as an object of its own, and the traces a filing writes are the members of one run ([`TraceTable`]). A trace names the same few declarations its neighbours do: the library's traces hold some forty names each, drawn from the few thousand the library and its scope declare. A name is archived as its segments, and read back through the process's table at an allocation a segment, so a run holds a table of the names and of the witness keys, each once, and a question holds its place there. What the unit reads of other units, the declarations settled before the offer and the stored order are the unit's own, stored with it.
//!
//! **Completed where it is filed, and found by what its declaration was made from.** A store's filing completes each trace with what the declaration was made from, what each question read and what it made ([`Made`]), and names it by those ([`TraceRecord::digest`]): the one record a stage keeps of a declaration, which a later compile keeps the declaration by where it holds the baseline and takes the declaration from where it finds the record among those a store holds for the same input ([`curios_utilities::Candidates`]). A constructive trace, which "additionally stores the resulting value", where "it makes sense to record many constructive traces per key" (Mokhov, Mitchell and Peyton Jones, [Build Systems à la Carte](https://www.microsoft.com/en-us/research/wp-content/uploads/2018/03/build-systems.pdf), section 4.2.3), the value being held by its identity in the store beside it.

#[cfg(test)]
mod tests;

use {
    crate::{Answered, Question},
    curios_core::{Global, Order, Trace, WitnessKey},
    curios_utilities::{
        Digest, Facet, InKeyOrder, Object, Persistent, Records, Sorted, candidate_name,
    },
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        convert::Infallible,
        hash::Hash,
        ops::Deref,
        rc::Rc,
    },
};

/// What a question read, as a completed record holds it: `None` where it read no part a record holds — whether lemmas are on offer, whether a witness is declared, a key no witness answers, a part of the declaration's own or of a fixed unit's, which a store's address names — and otherwise the digest of the part, `None` within where the name declared no such part.
pub type Read = Option<Option<[u8; 32]>>;

/// What a filing completes a declaration's trace with: what the declaration was made from, what each of its questions read and what it made. See the module documentation.
#[derive(Debug, Clone, PartialEq)]
pub struct Made {
    /// What the declaration was made from: the digest of its lowered items for its elaboration ([`declaration_input`](crate::declaration_input)), the identity of its elaborated item for its erasure.
    pub input: Digest,
    /// What each question read, in the trace's order.
    pub reads: Vec<Read>,
    /// What it made, each by its identity: its elaborated items in the order of its lowered ones, or its erased form.
    pub outputs: Vec<Digest>,
    /// The written binders its proofs read, each by the name of its definition and its place among that definition's written binders.
    pub credited: Vec<(Global, u32)>,
}

/// A declaration's trace as a unit holds it: what it asked, and once a filing completed it, what it was made from and made ([`Made`]). Shared with every version that keeps it, beside the digest it is stored under.
#[derive(Debug)]
pub struct TraceRecord {
    asked: Trace<Question, Answered>,
    /// What a filing completed it with: unset until one did, `None` where nothing completes it.
    made: OnceCell<Option<Made>>,
    /// The digest it is stored under, kept once it is complete: what a record not yet completed is stored under moves where a filing completes it.
    digest: OnceCell<Digest>,
}

impl TraceRecord {
    /// The record of a declaration that asked `asked`, which no filing has completed.
    pub(crate) fn new(asked: Trace<Question, Answered>) -> Self {
        Self {
            asked,
            made: OnceCell::new(),
            digest: OnceCell::new(),
        }
    }

    /// The record a store held under `digest`: complete where it was filed with `made`, and otherwise left for a filing to complete.
    pub(crate) fn restored(
        asked: Trace<Question, Answered>,
        made: Option<Made>,
        digest: Digest,
    ) -> Self {
        let traced = Self::new(asked);
        if let Some(made) = made {
            let _ = traced.made.set(Some(made));
            let _ = traced.digest.set(digest);
        }

        traced
    }

    /// What the declaration made and read, where a filing completed the record with it.
    pub fn made(&self) -> Option<&Made> {
        self.made.get()?.as_ref()
    }

    /// Whether a filing has completed the record, with what the declaration made or with nothing.
    pub fn completed(&self) -> bool {
        self.made.get().is_some()
    }

    /// Complete the record with `made`, where nothing has: the record of every version that holds it. `None` is a record nothing completes.
    pub fn complete(&self, made: Option<Made>) {
        let _ = self.made.set(made);
    }

    /// The digest a store names this record by, the record of the declaration written as `name` that `facet` made: where it is complete, the candidate's name, which a store finds it among the records filed for its input by ([`candidate_name`]), and otherwise the digest of its run alone.
    pub fn digest(&self, facet: Facet, name: &Global) -> Result<Digest, String> {
        if let Some(digest) = self.digest.get() {
            return Ok(*digest);
        }
        let run = TraceTable::run([(name, &self.asked, self.made())])?;
        let digest = match self.made() {
            Some(made) => candidate_name(facet, &made.input, &run),
            None => Digest::of(&run),
        };
        if self.completed() {
            let _ = self.digest.set(digest);
        }

        Ok(digest)
    }
}

/// A record reads as what its declaration asked.
impl Deref for TraceRecord {
    type Target = Trace<Question, Answered>;

    fn deref(&self) -> &Self::Target {
        &self.asked
    }
}

/// A declaration's record as a unit holds it: shared with every version that keeps it.
pub type HeldTrace = Rc<TraceRecord>;

/// A declaration's record as a run holds it: the name the declaration is written as, its trace, and what a filing completed it with.
pub type TracedEntry = (Global, Trace<Question, Answered>, Option<Made>);

/// A declaration's record as a run is written from: the name the declaration is written as, its trace, and what a filing completed it with, each borrowed.
pub type Entry<'t> = (&'t Global, &'t Trace<Question, Answered>, Option<&'t Made>);

/// Each declaration's trace, by the name it is written as. See the module documentation.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Traces {
    /// **Stored apart, an object for each**, and handed back where the traces are restored ([`Records`]): what is archived with the unit is the rest.
    #[archived_with(curios_archive::Skip)]
    declarations: Sorted<Global, HeldTrace>,
    /// What the declarations read of declarations the unit does not hold. See [`Foreign`].
    foreign: Rc<Foreign>,
    /// The declarations settled before the unit's lemmas were offered to a proof the elaborator writes: the prover's vocabulary and everything it reads, by the names they are written as.
    ///
    /// **What a recompile keeps the offer by.** Where an edit reaches none of them, the vocabulary reads what it read, so each declaration stands on the side of the offer it was elaborated on, and the walk over the unit asks none of them again to learn it.
    #[archived_with(curios_archive::Shared)]
    before: Rc<BTreeSet<Global>>,
    /// The order the unit's items are stored in, with what each reaches of the unit and its depth: what a unit compiled over this one gives the order again from the items it made by ([`Order::revised`]).
    order: Order,
}

/// What a unit's declarations read of declarations the unit does not hold, another unit's or no one's: each part asked of one, with the digest it had (`part_digest`), none where the name declared no such part.
///
/// **What a read of another unit stands on.** A trace's question holds no answer for a part, the declaration being the one to hold it; of a declaration in the unit that is the baseline's own, and of one in another unit it is this digest. A unit compiled over a baseline asks nothing of a unit that is the one the baseline was compiled after, and of any other asks each part again by what it digests to now.
///
/// **One table for a unit**, every declaration of one compile having read one scope: a part is digested once however many declarations asked it, and the table is read back in the order of its questions, so one made by reading every trace and one kept by the changes of a recompile's are one table.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Foreign {
    #[archived_with(InKeyOrder)]
    read: Sorted<Question, Option<String>>,
}

impl Foreign {
    /// The digest recorded of the part `question` asks: `None` where nothing asked it, `Some(None)` where the name declared no such part.
    pub(crate) fn digest(&self, question: &Question) -> Option<&Option<String>> {
        self.read.get(question)
    }

    /// Record `digest` as what the part `question` asks had, in place of what was recorded of it.
    pub(crate) fn record(&mut self, question: Question, digest: Option<String>) {
        self.read.insert(question, digest);
    }

    /// Take out what was recorded of the part `question` asks: no declaration reads it now.
    pub(crate) fn forget(&mut self, question: &Question) {
        self.read.remove(question);
    }

    /// This table as a walk revises it that made again or dropped some of the declarations it was read off, the table of every other standing: each part of `read` — one a declaration made again reads now — recorded, digested again where nothing was recorded of it or `moved` says the unit declaring it moved, and taken out where it will not digest; each part of `dropped` — one a declaration made again or dropped read before — taken out where `read_now` says no declaration reads it now; and each part of `declared` — one of a name the unit declares now — taken out.
    pub(crate) fn revised(
        &self,
        read: impl IntoIterator<Item = Question>,
        dropped: impl IntoIterator<Item = Question>,
        declared: impl IntoIterator<Item = Question>,
        moved: impl Fn(&Question) -> bool,
        digest: impl Fn(&Question) -> Result<Option<String>, String>,
        read_now: impl Fn(&Question) -> bool,
    ) -> Foreign {
        let mut revised = self.clone();
        for part in read {
            if revised.digest(&part).is_some() && !moved(&part) {
                continue;
            }
            match digest(&part) {
                Ok(digest) => revised.record(part, digest),
                Err(_) => revised.forget(&part),
            }
        }
        for part in dropped {
            if !read_now(&part) {
                revised.forget(&part);
            }
        }
        for part in declared {
            revised.forget(&part);
        }

        revised
    }

    /// The name each part recorded here was asked of.
    pub fn subjects(&self) -> impl Iterator<Item = &Global> {
        self.read.iter().filter_map(|(asked, _)| asked.subject())
    }
}

impl Traces {
    /// File `record` as the declaration written as `name`'s: another unit's record of it, held where that unit holds it, or one a store held.
    pub(crate) fn share(&mut self, name: Global, record: HeldTrace) {
        self.declarations.insert(name, record);
    }

    /// Take out the trace held under `name`, where one is: a declaration no longer written as it.
    pub(crate) fn forget(&mut self, name: &Global) {
        self.declarations.remove(name);
    }

    /// What the declaration written as `name` asked, where the unit holds one.
    pub fn of(&self, name: &Global) -> Option<&Trace<Question, Answered>> {
        self.held(name).map(|held| &***held)
    }

    /// The trace of the declaration written as `name`, as the unit holds it.
    pub(crate) fn held(&self, name: &Global) -> Option<&HeldTrace> {
        self.declarations.get(name)
    }

    /// Each declaration's trace as the unit holds it, in the order of the names the declarations are written as: what the unit stores an object for.
    pub fn declarations(&self) -> impl Iterator<Item = (&Global, &HeldTrace)> {
        self.declarations.iter()
    }

    /// What the declarations read of declarations the unit does not hold.
    pub fn foreign(&self) -> &Rc<Foreign> {
        &self.foreign
    }

    /// Hold `foreign` as what the declarations read of declarations the unit does not hold.
    pub(crate) fn read(&mut self, foreign: Rc<Foreign>) {
        self.foreign = foreign;
    }

    /// The declarations settled before the unit's lemmas were offered. See the field.
    pub(crate) fn before(&self) -> &BTreeSet<Global> {
        &self.before
    }

    /// Hold `before` as the declarations settled before the unit's lemmas were offered.
    pub(crate) fn settle_before(&mut self, before: BTreeSet<Global>) {
        self.before = Rc::new(before);
    }

    /// The order the unit's items are stored in. See the field.
    pub fn order(&self) -> &Order {
        &self.order
    }

    /// Hold `order` as the order the unit's items are stored in.
    pub(crate) fn ordered(&mut self, order: Order) {
        self.order = order;
    }

    /// Every declaration's trace, in the order of the names the declarations are written as.
    pub(crate) fn each(&self) -> impl Iterator<Item = &Trace<Question, Answered>> {
        self.declarations.iter().map(|(_, trace)| &***trace)
    }

    /// The declarations holding a trace, by the name each is written as, with how many questions each asked.
    pub fn sizes(&self) -> impl Iterator<Item = (&Global, usize)> {
        self.declarations
            .iter()
            .map(|(name, trace)| (name, trace.len()))
    }
}

/// Which parts of a name a declaration's questions read: a set over the parts a [`Question`] can ask.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Parts(u16);

impl Parts {
    pub(crate) const SIGNATURE: Parts = Parts(1);
    pub(crate) const BODY: Parts = Parts(1 << 1);
    pub(crate) const KIND: Parts = Parts(1 << 2);
    pub(crate) const TOTALITY: Parts = Parts(1 << 3);
    pub(crate) const VARIANCES: Parts = Parts(1 << 4);
    pub(crate) const INDUCT: Parts = Parts(1 << 5);
    pub(crate) const STRUCT: Parts = Parts(1 << 6);
    pub(crate) const CONCEPT: Parts = Parts(1 << 7);
    pub(crate) const ERASED: Parts = Parts(1 << 8);
    pub(crate) const RAN: Parts = Parts(1 << 9);
    pub(crate) const SORT: Parts = Parts(1 << 10);
    /// Every part: what a declaration made or taken away moves of what it declares.
    pub(crate) const ALL: Parts = Parts((1 << 11) - 1);

    /// The part `question` asks of the name it is about, where it asks one: a witness's key, whether one is declared and whether lemmas are on offer ask none.
    pub(crate) fn of(question: &Question) -> Option<Parts> {
        Some(match question {
            Question::Signature(_) => Parts::SIGNATURE,
            Question::Body(_) => Parts::BODY,
            Question::Kind(_) => Parts::KIND,
            Question::Totality(_) => Parts::TOTALITY,
            Question::Variances(_) => Parts::VARIANCES,
            Question::Sort(_) => Parts::SORT,
            Question::Induct(_) => Parts::INDUCT,
            Question::Struct(_) => Parts::STRUCT,
            Question::Concept(_) => Parts::CONCEPT,
            Question::Erased(_) => Parts::ERASED,
            Question::Ran(_) => Parts::RAN,
            Question::Witness(..) | Question::WitnessDeclared(..) | Question::Lemmas => {
                return None;
            }
        })
    }

    /// The question asking each part of `name` in this set.
    pub(crate) fn questions(self, name: Global) -> impl Iterator<Item = Question> {
        [
            (Parts::SIGNATURE, Question::Signature(name)),
            (Parts::BODY, Question::Body(name)),
            (Parts::KIND, Question::Kind(name)),
            (Parts::TOTALITY, Question::Totality(name)),
            (Parts::VARIANCES, Question::Variances(name)),
            (Parts::SORT, Question::Sort(name)),
            (Parts::INDUCT, Question::Induct(name)),
            (Parts::STRUCT, Question::Struct(name)),
            (Parts::CONCEPT, Question::Concept(name)),
            (Parts::ERASED, Question::Erased(name)),
            (Parts::RAN, Question::Ran(name)),
        ]
        .into_iter()
        .filter(move |(part, _)| self.meets(*part))
        .map(|(_, question)| question)
    }

    /// Both sets of parts.
    pub(crate) fn with(self, other: Parts) -> Parts {
        Parts(self.0 | other.0)
    }

    /// Whether the two sets share a part.
    pub(crate) fn meets(self, other: Parts) -> bool {
        self.0 & other.0 != 0
    }
}

/// Who asked what, of one stage's traces read backwards: what a recompile over the unit finds the declarations an edit could reach by, from the edit rather than by asking every declaration whether it was reached ([A compile costs what it reads](../../documentation/design/compilation/a-compile-costs-what-it-reads.md)).
///
/// **Read off the traces, never stored.** Made the first time a recompile over the unit asks, and kept with the unit as where its module holds each name is; a unit compiled over a baseline holds the baseline's readers with the edges of every declaration it made or dropped replaced, so a session inverts no trace twice.
///
/// **Looked up, never walked.** Its maps are persistent and keyed by hash, so their order follows the process, and what reads them asks after one name, one key or one declaration at a time.
#[derive(Debug, Clone, Default)]
pub struct Readers {
    /// For each name a question was about, the declarations that asked it, by the name each is written as, with the parts each asked. A witness a goal found is read by its signature.
    parts: Persistent<Global, Vec<(Global, Parts)>>,
    /// For each witness key a goal asked at, or asked whether a witness is declared at, the declarations that asked, with what the key was answered.
    keys: Persistent<(Global, WitnessKey), Keyed>,
    /// Each declaration that asked whether the unit's lemmas were on offer, with what it was answered.
    lemmas: Persistent<Global, bool>,
}

/// The declarations that asked at one witness key, with what it was answered: the witness found standing there, where a goal asked, and whether one was declared there, where that was asked.
///
/// **One answer for the key.** A key no declaration of the unit is declared at is answered by the units in scope alone, the same to every declaration that asks; a key one of the unit's declarations is declared at is told apart by which witnesses the unit declares there.
#[derive(Debug, Clone, Default)]
pub(crate) struct Keyed {
    pub(crate) readers: Vec<Global>,
    pub(crate) answered: KeyAnswers,
}

/// What one witness key was answered: the witness found standing there, where a goal asked, and whether one was declared there, where that was asked.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct KeyAnswers {
    pub(crate) found: Option<Option<Global>>,
    pub(crate) declared: Option<bool>,
}

/// The edges one declaration's trace adds: each name it asked about with the parts it asked, each key it asked at with what it was answered there, and what it was answered about the unit's lemmas.
struct Edges {
    parts: Vec<(Global, Parts)>,
    keys: Vec<((Global, WitnessKey), KeyAnswers)>,
    lemmas: Option<bool>,
}

impl Edges {
    fn of(trace: &Trace<Question, Answered>) -> Self {
        let mut parts = Vec::<(Global, Parts)>::new();
        let mut keys = Vec::<((Global, WitnessKey), KeyAnswers)>::new();
        let mut lemmas = None;
        let mut asked =
            |name: Global, part: Parts| match parts.iter_mut().find(|(held, _)| *held == name) {
                Some((_, held)) => *held = held.with(part),
                None => parts.push((name, part)),
            };
        for (question, answered) in trace.asked() {
            match question {
                Question::Witness(concept, key) | Question::WitnessDeclared(concept, key) => {
                    let at = match keys.iter().position(|(held, _)| *held == (*concept, *key)) {
                        Some(at) => at,
                        None => {
                            keys.push(((*concept, *key), KeyAnswers::default()));
                            keys.len() - 1
                        }
                    };
                    match (question, answered) {
                        (Question::Witness(..), Answered::Witness(found)) => {
                            keys[at].1.found = Some(*found);
                            if let Some(found) = found {
                                asked(*found, Parts::SIGNATURE);
                            }
                        }
                        (_, Answered::Whether(declared)) => keys[at].1.declared = Some(*declared),
                        _ => {}
                    }
                }
                Question::Lemmas => lemmas = Some(*answered == Answered::Whether(true)),
                part => {
                    if let (Some(name), Some(read)) = (part.subject(), Parts::of(part)) {
                        asked(*name, read);
                    }
                }
            }
        }

        Self {
            parts,
            keys,
            lemmas,
        }
    }
}

impl Keyed {
    /// This key with `reader` among those that asked, and the answers it was given held as the key's: a declaration made again was answered as the key answers now, and one that read an answer since moved was made again.
    fn asked(&mut self, reader: Global, answered: KeyAnswers) {
        if !self.readers.contains(&reader) {
            self.readers.push(reader);
        }
        if answered.found.is_some() {
            self.answered.found = answered.found;
        }
        if answered.declared.is_some() {
            self.answered.declared = answered.declared;
        }
    }
}

impl Readers {
    /// Who asked what, of `traces`.
    pub fn of(traces: &Traces) -> Self {
        curios_profile::profile!("readers::of", declarations = traces.declarations.len());
        // Gathered whole first, so each name's list is built once and not copied at every reader a persistent insert would add.
        let mut parts = BTreeMap::<Global, Vec<(Global, Parts)>>::new();
        let mut keys = BTreeMap::<(Global, WitnessKey), Keyed>::new();
        let mut readers = Readers::default();
        for (reader, trace) in traces.declarations.iter() {
            let edges = Edges::of(trace);
            for (name, read) in edges.parts {
                parts.entry(name).or_default().push((*reader, read));
            }
            for (key, answered) in edges.keys {
                keys.entry(key).or_default().asked(*reader, answered);
            }
            if let Some(offered) = edges.lemmas {
                readers.lemmas.insert(*reader, offered);
            }
        }
        for (name, asked) in parts {
            readers.parts.insert(name, asked);
        }
        for (key, asked) in keys {
            readers.keys.insert(key, asked);
        }

        readers
    }

    /// Replace the edges of the declaration written as `reader`: those `earlier`, the trace it held, added are taken out, and those `trace`, the one it holds now, adds are put in. `None` on either side is a declaration that held, or holds, no trace.
    pub(crate) fn replace(
        &mut self,
        reader: Global,
        earlier: Option<&Trace<Question, Answered>>,
        trace: Option<&Trace<Question, Answered>>,
    ) {
        if let Some(earlier) = earlier {
            let edges = Edges::of(earlier);
            for (name, _) in edges.parts {
                Self::drop_from(&mut self.parts, name, |(held, _)| *held == reader);
            }
            for (key, _) in edges.keys {
                let Some(held) = self.keys.get(&key) else {
                    continue;
                };
                let mut held = held.clone();
                held.readers.retain(|held| *held != reader);
                match held.readers.is_empty() {
                    true => {
                        self.keys.remove(&key);
                    }
                    false => self.keys.insert(key, held),
                }
            }
            self.lemmas.remove(&reader);
        }
        if let Some(trace) = trace {
            let edges = Edges::of(trace);
            for (name, read) in edges.parts {
                let mut asked = self.parts.get(&name).cloned().unwrap_or_default();
                asked.push((reader, read));
                self.parts.insert(name, asked);
            }
            for (key, answered) in edges.keys {
                let mut asked = self.keys.get(&key).cloned().unwrap_or_default();
                asked.asked(reader, answered);
                self.keys.insert(key, asked);
            }
            if let Some(offered) = edges.lemmas {
                self.lemmas.insert(reader, offered);
            }
        }
    }

    /// `map` with every entry `drop` holds of under `key` taken out, and the key with them where none is left.
    fn drop_from<K: Hash + Eq + Clone, T: Clone>(
        map: &mut Persistent<K, Vec<T>>,
        key: K,
        drop: impl Fn(&T) -> bool,
    ) {
        let Some(held) = map.get(&key) else {
            return;
        };
        let kept = held
            .iter()
            .filter(|entry| !drop(entry))
            .cloned()
            .collect::<Vec<_>>();
        match kept.is_empty() {
            true => {
                map.remove(&key);
            }
            false => map.insert(key, kept),
        }
    }

    /// The declarations that asked about `name`, each with the parts it asked.
    pub(crate) fn of_name(&self, name: &Global) -> &[(Global, Parts)] {
        self.parts.get(name).map_or(&[], Vec::as_slice)
    }

    /// Whether some declaration asked the part `question` asks of the name it is about.
    pub(crate) fn asks(&self, question: &Question) -> bool {
        let (Some(name), Some(asked)) = (question.subject(), Parts::of(question)) else {
            return false;
        };

        self.of_name(name).iter().any(|(_, read)| read.meets(asked))
    }

    /// The declarations that asked at `concept` and `key`: for the witness standing there, or whether one is declared there.
    pub(crate) fn at_key(&self, concept: Global, key: WitnessKey) -> &[Global] {
        self.keys
            .get(&(concept, key))
            .map_or(&[], |keyed| keyed.readers.as_slice())
    }

    /// What the declaration written as `reader` was answered about the unit's lemmas being on offer, where it asked.
    pub(crate) fn lemmas(&self, reader: &Global) -> Option<bool> {
        self.lemmas.get(reader).copied()
    }

    /// Every name a question was about, each once, in no stated order: what a unit in scope that moved is read again by.
    pub(crate) fn names(&self) -> impl Iterator<Item = &Global> {
        self.parts.iter().map(|(name, _)| name)
    }

    /// Every declaration that asked something of the unit as a whole — the witness at a key, whether one is declared there, whether lemmas are on offer — in no stated order: erasure asks none of them, and keeps no form whose trace holds one.
    pub(crate) fn asked_whole(&self) -> impl Iterator<Item = &Global> {
        self.keys
            .iter()
            .flat_map(|(_, keyed)| keyed.readers.iter())
            .chain(self.lemmas.iter().map(|(reader, _)| reader))
    }

    /// Every witness key asked at, with what it was answered and who asked: what a unit in scope that moved may answer otherwise.
    pub(crate) fn keys(&self) -> impl Iterator<Item = (&(Global, WitnessKey), &Keyed)> {
        self.keys.iter()
    }
}

/// Declarations' traces as a run of the store holds them: every name and every witness key a question holds, once, and each declaration's questions holding their places, a member of the run.
#[curios_archive::archived]
pub struct TraceTable {
    names: Vec<Global>,
    keys: Vec<WitnessKey>,
    /// Each declaration's trace, in the order of the names the declarations are written as.
    declarations: Vec<TabledTrace>,
}

/// One declaration's record as a [`TraceTable`] holds it: the place of the name it is written as, its questions holding places, and what a filing completed it with.
#[curios_archive::archived]
pub struct TabledTrace {
    name: u32,
    asked: Trace<Question<u32, u32>, Answered<u32>>,
    made: Option<TabledMade>,
}

/// What a filing completed a record with ([`Made`]), as a [`TraceTable`] holds it: the names of its binders holding places.
#[curios_archive::archived]
pub struct TabledMade {
    input: [u8; 32],
    reads: Vec<Read>,
    outputs: Vec<[u8; 32]>,
    credited: Vec<(u32, u32)>,
}

impl TabledTrace {
    /// The entry this holds, each place read through `name` and `key`.
    fn entry(
        self,
        name: impl Fn(u32) -> Result<Global, String>,
        key: impl Fn(u32) -> Result<WitnessKey, String>,
    ) -> Result<TracedEntry, String> {
        let mut trace = Trace::default();
        for (question, answered) in self.asked.asked() {
            trace.record(question.spelled(&name, &key)?, answered.spelled(&name)?);
        }
        let made = self
            .made
            .map(|made| {
                Ok::<_, String>(Made {
                    input: Digest::from_bytes(made.input),
                    reads: made.reads,
                    outputs: made.outputs.into_iter().map(Digest::from_bytes).collect(),
                    credited: made
                        .credited
                        .into_iter()
                        .map(|(place, ordinal)| Ok((name(place)?, ordinal)))
                        .collect::<Result<_, String>>()?,
                })
            })
            .transpose()?;

        Ok((name(self.name)?, trace, made))
    }
}

/// The places a table gives its entries, each entry at the place it was first asked for.
struct Placing<T> {
    entries: Vec<T>,
    places: HashMap<T, u32>,
}

impl<T: Copy + Eq + Hash> Placing<T> {
    fn new() -> Self {
        Self {
            entries: Vec::new(),
            places: HashMap::new(),
        }
    }

    fn place(&mut self, entry: T) -> u32 {
        *self.places.entry(entry).or_insert_with(|| {
            let place = u32::try_from(self.entries.len()).expect("a table is counted in a u32");
            self.entries.push(entry);

            place
        })
    }
}

/// The entry `table` holds at `place`, or a refusal naming what the table is `of`.
fn placed<T: Copy>(table: &[T], place: u32, of: &str) -> Result<T, String> {
    usize::try_from(place)
        .ok()
        .and_then(|place| table.get(place))
        .copied()
        .ok_or_else(|| format!("a stored trace holds a {of} its table does not"))
}

impl TraceTable {
    /// The kind a pack's entry names for a member of a run of traces ([`curios_utilities::Member`]).
    pub const KIND: u8 = 2;

    /// The bytes of a run of `entries`, each a declaration's name, its trace and what a filing completed it with, in their order.
    pub fn run<'t>(entries: impl IntoIterator<Item = Entry<'t>>) -> Result<Vec<u8>, String> {
        curios_archive::to_bytes(&Self::of(entries)).map(|bytes| bytes.to_vec())
    }

    /// The entries of the run `bytes` holds ([`TraceTable::run`]), in their order. `Err` is bytes that hold no run of traces.
    pub fn read(bytes: &[u8]) -> Result<Vec<TracedEntry>, String> {
        let TraceTable {
            names,
            keys,
            declarations,
        } = curios_archive::from_bytes::<TraceTable>(bytes)?;

        declarations
            .into_iter()
            .map(|tabled| {
                tabled.entry(
                    |place| placed(&names, place, "name"),
                    |place| placed(&keys, place, "witness key"),
                )
            })
            .collect()
    }

    /// The entry the run `bytes` holds as its member `member`, the run checked once and nothing of it restored but the member and the names and keys it holds: what a compile reads of a record a store found among those of a run.
    pub fn member(bytes: &[u8], member: u32) -> Result<TracedEntry, String> {
        let table = curios_archive::access::<ArchivedTraceTable>(bytes)?;
        let tabled = usize::try_from(member)
            .ok()
            .and_then(|member| table.declarations.get(member))
            .ok_or_else(|| format!("a run of traces holds no member {member}"))?;
        let tabled = curios_archive::deserialize::<TabledTrace>(tabled)?;
        let read = |place: u32, of: &str| {
            usize::try_from(place)
                .ok()
                .ok_or_else(|| format!("a stored trace holds a {of} its table does not"))
        };

        tabled.entry(
            |place| {
                let at = read(place, "name")?;
                let name = table
                    .names
                    .get(at)
                    .ok_or("a stored trace holds a name its table does not")?;

                curios_archive::deserialize::<Global>(name)
            },
            |place| {
                let at = read(place, "witness key")?;
                let key = table
                    .keys
                    .get(at)
                    .ok_or("a stored trace holds a witness key its table does not")?;

                curios_archive::deserialize::<WitnessKey>(key)
            },
        )
    }

    /// The table holding `entries`, every name and witness key once, at the place it was first asked for.
    fn of<'t>(entries: impl IntoIterator<Item = Entry<'t>>) -> TraceTable {
        let mut names = Placing::new();
        let mut keys = Placing::new();
        let declarations = entries
            .into_iter()
            .map(|(name, trace, made)| {
                let mut asked = Trace::default();
                for (question, answered) in trace.asked() {
                    let Ok(question) = question.spelled(
                        |name| Ok::<_, Infallible>(names.place(name)),
                        |key| Ok(keys.place(key)),
                    );
                    let Ok(answered) =
                        answered.spelled(|name| Ok::<_, Infallible>(names.place(name)));
                    asked.record(question, answered);
                }
                let made = made.map(|made| TabledMade {
                    input: *made.input.as_bytes(),
                    reads: made.reads.clone(),
                    outputs: made
                        .outputs
                        .iter()
                        .map(|output| *output.as_bytes())
                        .collect(),
                    credited: made
                        .credited
                        .iter()
                        .map(|(name, place)| (names.place(*name), *place))
                        .collect(),
                });

                TabledTrace {
                    name: names.place(*name),
                    asked,
                    made,
                }
            })
            .collect();

        TraceTable {
            names: names.entries,
            keys: keys.entries,
            declarations,
        }
    }
}

/// A unit's traces restored out of what it stores of them: the rest, archived with the unit, and each declaration's trace a member of a run ([`TraceTable`]), restored beside the digest it was found by. A run several traces lie in is read once.
impl Records for Traces {
    fn restored(table: &[u8], entries: &[Object]) -> Result<Self, String> {
        let mut traces = curios_archive::from_bytes::<Traces>(table)?;
        let mut runs = HashMap::<(usize, usize), Vec<Option<TracedEntry>>>::new();
        for object in entries {
            let member = object
                .member
                .ok_or("a trace is stored as a member of a run")?;
            let bytes = object.bytes.as_slice();
            let run = match runs.entry((bytes.as_ptr() as usize, bytes.len())) {
                std::collections::hash_map::Entry::Occupied(read) => read.into_mut(),
                std::collections::hash_map::Entry::Vacant(unread) => {
                    unread.insert(TraceTable::read(bytes)?.into_iter().map(Some).collect())
                }
            };
            let (name, trace, made) = usize::try_from(member.index)
                .ok()
                .and_then(|index| run.get_mut(index))
                .and_then(Option::take)
                .ok_or_else(|| format!("a run of traces holds no member {}", member.index))?;
            traces.declarations.insert(
                name,
                Rc::new(TraceRecord::restored(trace, made, object.digest)),
            );
        }

        Ok(traces)
    }
}
