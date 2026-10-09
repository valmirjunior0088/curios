//! What each declaration of a unit asked of the declarations around it, kept with the unit: the record a later compile of the same unit keeps a declaration's elaboration by.
//!
//! **One trace for each declaration, under the name it is written as.** A declaration is an item with every item the lowering generated from it, attempted together in one state, so what the attempt that stood asked is the declaration's. It is filed under the first name its written item declares, which a later lowering of the same text gives it again.
//!
//! **Held by pointer.** A unit compiled over a baseline keeps the trace of every declaration it keeps, and that is a copy of a handle ([`Traces::share`]).
//!
//! **Stored with each name written once.** A trace names the same few declarations its neighbours do: the library's traces hold some forty names each, drawn from the few thousand the library and its scope declare. A name is archived as its segments, and read back through the process's table at an allocation a segment, so the stored form holds a table of the names and of the witness keys, each once, and a question holds its place there.

use {
    crate::{Answered, Question},
    curios_core::{Global, Trace},
    std::{
        cell::OnceCell,
        collections::{BTreeMap, HashMap},
        rc::Rc,
    },
};

#[cfg(feature = "archive")]
use {
    curios_core::WitnessKey,
    std::{borrow::Borrow, convert::Infallible, hash::Hash},
};

/// Each declaration's trace, by the name it is written as. See the module documentation.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Traces {
    #[archived_with(TraceTabled)]
    declarations: BTreeMap<Global, Rc<Trace<Question, Answered>>>,
    /// What the declarations read of declarations the unit does not hold. See [`Foreign`].
    foreign: Rc<Foreign>,
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
    #[cfg(feature = "archive")]
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
    #[cfg(feature = "archive")]
    pub(crate) fn read(&mut self, foreign: Rc<Foreign>) {
        self.foreign = foreign;
    }

    /// Every declaration's trace, in the order of the names the declarations are written as.
    #[cfg(feature = "archive")]
    pub(crate) fn each(&self) -> impl Iterator<Item = &Trace<Question, Answered>> {
        self.declarations.values().map(Rc::as_ref)
    }

    /// How many declarations hold a trace.
    #[cfg(feature = "archive")]
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

/// A unit's traces as they are stored: every name and every witness key a question holds, once, and each declaration's questions holding their places.
#[cfg(feature = "archive")]
#[curios_archive::archived]
pub struct TraceTable {
    names: Vec<Global>,
    keys: Vec<WitnessKey>,
    /// Each declaration's trace, in the order of the names the declarations are written as.
    declarations: Vec<TabledTrace>,
}

/// One declaration's trace as a [`TraceTable`] holds it: the place of the name it is written as, and its questions holding places.
#[cfg(feature = "archive")]
#[curios_archive::archived]
pub struct TabledTrace {
    name: u32,
    asked: Trace<Question<u32, u32>, Answered<u32>>,
}

/// The places a table gives its entries, each entry at the place it was first asked for.
#[cfg(feature = "archive")]
struct Placing<T> {
    entries: Vec<T>,
    places: HashMap<T, u32>,
}

#[cfg(feature = "archive")]
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
#[cfg(feature = "archive")]
fn placed<T: Copy>(table: &[T], place: u32, of: &str) -> Result<T, String> {
    usize::try_from(place)
        .ok()
        .and_then(|place| table.get(place))
        .copied()
        .ok_or_else(|| format!("a stored trace holds a {of} its table does not"))
}

/// How a unit's traces are archived: as a [`TraceTable`].
#[cfg(feature = "archive")]
pub struct TraceTabling;

/// The field adapter: `#[archived_with(TraceTabled)]`.
#[cfg(feature = "archive")]
pub type TraceTabled = curios_archive::Via<TraceTabling>;

#[cfg(feature = "archive")]
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
