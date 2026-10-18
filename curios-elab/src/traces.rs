//! What each declaration of a unit asked of the declarations around it, kept with the unit: the record a later compile of the same unit keeps a declaration's elaboration by.
//!
//! **One trace for each declaration, under the name it is written as.** A declaration is an item with every item the lowering generated from it, attempted together in one state, so what the attempt that stood asked is the declaration's. It is filed under the first name its written item declares, which a later lowering of the same text gives it again.
//!
//! **Held by pointer.** A unit compiled over a baseline keeps the trace of every declaration it keeps, and that is a copy of a handle ([`Traces::share`]).
//!
//! **Stored with each name written once.** A trace names the same few declarations its neighbours do: the library's traces hold some forty names each, drawn from the few thousand the library and its scope declare. A name is archived as its segments, and read back through the process's table at an allocation a segment, so the stored form holds a table of the names and of the witness keys, each once, and a question holds its place there.

#[cfg(test)]
mod tests;

use {
    crate::{Answered, Question},
    curios_core::{Global, Trace, WitnessKey},
    curios_utilities::Persistent,
    std::{
        borrow::Borrow,
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        convert::Infallible,
        hash::Hash,
        rc::Rc,
    },
};

/// Each declaration's trace, by the name it is written as. See the module documentation.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Traces {
    #[archived_with(TraceTabled)]
    declarations: BTreeMap<Global, Rc<Trace<Question, Answered>>>,
    /// What the declarations read of declarations the unit does not hold. See [`Foreign`].
    foreign: Rc<Foreign>,
    /// The declarations settled before the unit's lemmas were offered to a proof the elaborator writes: the prover's vocabulary and everything it reads, by the names they are written as.
    ///
    /// **What a recompile keeps the offer by.** Where an edit reaches none of them, the vocabulary reads what it read, so each declaration stands on the side of the offer it was elaborated on, and the walk over the unit asks none of them again to learn it.
    before: BTreeSet<Global>,
}

/// What a unit's declarations read of declarations the unit does not hold, another unit's or no one's: each part asked of one, with the digest it had (`part_digest`), none where the name declared no such part.
///
/// **What a read of another unit stands on.** A trace's question holds no answer for a part, the declaration being the one to hold it; of a declaration in the unit that is the baseline's own, and of one in another unit it is this digest. A unit compiled over a baseline asks nothing of a unit that is the one the baseline was compiled after, and of any other asks each part again by what it digests to now.
///
/// **One table for a unit**, every declaration of one compile having read one scope: a part is digested once however many declarations asked it, in the order the declarations, by name, first asked.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Foreign {
    read: Vec<(Question, Option<String>)>,
    /// Where `read` holds each subject's parts, made the first time one is asked for.
    #[archived_with(curios_archive::Skip)]
    places: OnceCell<HashMap<Global, Vec<u32>>>,
}

impl Foreign {
    /// The digest recorded of the part `question` asks: `None` where nothing asked it, `Some(None)` where the name declared no such part.
    pub(crate) fn digest(&self, question: &Question) -> Option<&Option<String>> {
        let places = self.places.get_or_init(|| {
            let mut places = HashMap::<Global, Vec<u32>>::new();
            for (place, (asked, _)) in self.read.iter().enumerate() {
                if let Some(subject) = asked.subject() {
                    places.entry(*subject).or_default().push(place as u32);
                }
            }

            places
        });

        places
            .get(question.subject()?)?
            .iter()
            .map(|place| &self.read[*place as usize])
            .find(|(asked, _)| asked == question)
            .map(|(_, digest)| digest)
    }

    /// Record `digest` as what the part `question` asks had. The table's places are made after its last record.
    pub(crate) fn record(&mut self, question: Question, digest: Option<String>) {
        self.read.push((question, digest));
    }

    /// The name each part recorded here was asked of.
    pub fn subjects(&self) -> impl Iterator<Item = &Global> {
        self.read.iter().filter_map(|(asked, _)| asked.subject())
    }
}

impl Traces {
    /// File `trace` as what the declaration written as `name` asked.
    pub(crate) fn file(&mut self, name: Global, trace: Trace<Question, Answered>) {
        self.declarations.insert(name, Rc::new(trace));
    }

    /// File `trace` as what the declaration written as `name` asked: another unit's trace of it, held where that unit holds it.
    pub(crate) fn share(&mut self, name: Global, trace: Rc<Trace<Question, Answered>>) {
        self.declarations.insert(name, trace);
    }

    /// What the declaration written as `name` asked, where the unit holds one.
    pub fn of(&self, name: &Global) -> Option<&Trace<Question, Answered>> {
        self.held(name).map(Rc::as_ref)
    }

    /// The trace of the declaration written as `name`, as the unit holds it.
    pub(crate) fn held(&self, name: &Global) -> Option<&Rc<Trace<Question, Answered>>> {
        self.declarations.get(name)
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
        self.before = before;
    }

    /// Every declaration's trace, in the order of the names the declarations are written as.
    pub(crate) fn each(&self) -> impl Iterator<Item = &Trace<Question, Answered>> {
        self.declarations.values().map(Rc::as_ref)
    }

    /// How many declarations hold a trace.
    pub(crate) fn len(&self) -> usize {
        self.declarations.len()
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
    /// Every part: what a declaration made or taken away moves of what it declares.
    pub(crate) const ALL: Parts = Parts((1 << 10) - 1);

    /// The part `question` asks of the name it is about, where it asks one: a witness's key, whether one is declared and whether lemmas are on offer ask none.
    pub(crate) fn of(question: &Question) -> Option<Parts> {
        Some(match question {
            Question::Signature(_) => Parts::SIGNATURE,
            Question::Body(_) => Parts::BODY,
            Question::Kind(_) => Parts::KIND,
            Question::Totality(_) => Parts::TOTALITY,
            Question::Variances(_) => Parts::VARIANCES,
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
        for (reader, trace) in &traces.declarations {
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

/// A unit's traces as they are stored: every name and every witness key a question holds, once, and each declaration's questions holding their places.
#[curios_archive::archived]
pub struct TraceTable {
    names: Vec<Global>,
    keys: Vec<WitnessKey>,
    /// Each declaration's trace, in the order of the names the declarations are written as.
    declarations: Vec<TabledTrace>,
}

/// One declaration's trace as a [`TraceTable`] holds it: the place of the name it is written as, and its questions holding places.
#[curios_archive::archived]
pub struct TabledTrace {
    name: u32,
    asked: Trace<Question<u32, u32>, Answered<u32>>,
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

/// How a unit's traces are archived: as a [`TraceTable`].
pub struct TraceTabling;

/// The field adapter: `#[archived_with(TraceTabled)]`.
pub type TraceTabled = curios_archive::Via<TraceTabling>;

impl curios_archive::Proxy<BTreeMap<Global, Rc<Trace<Question, Answered>>>> for TraceTabling {
    type Archivable = TraceTable;

    fn to_archivable(
        declarations: &BTreeMap<Global, Rc<Trace<Question, Answered>>>,
    ) -> impl Borrow<TraceTable> {
        let mut names = Placing::new();
        let mut keys = Placing::new();
        let declarations = declarations
            .iter()
            .map(|(name, trace)| {
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

                TabledTrace {
                    name: names.place(*name),
                    asked,
                }
            })
            .collect();

        TraceTable {
            names: names.entries,
            keys: keys.entries,
            declarations,
        }
    }

    fn from_archivable(
        table: TraceTable,
    ) -> Result<BTreeMap<Global, Rc<Trace<Question, Answered>>>, String> {
        let TraceTable {
            names,
            keys,
            declarations,
        } = table;

        declarations
            .into_iter()
            .map(|TabledTrace { name, asked }| {
                let mut trace = Trace::default();
                for (question, answered) in asked.asked() {
                    trace.record(
                        question.spelled(
                            |place| placed(&names, place, "name"),
                            |place| placed(&keys, place, "witness key"),
                        )?,
                        answered.spelled(|place| placed(&names, place, "name"))?,
                    );
                }

                Ok((placed(&names, name, "name")?, Rc::new(trace)))
            })
            .collect()
    }
}
