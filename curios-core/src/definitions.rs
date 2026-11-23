//! The finished program: a flat list of top-level [`Item`]s, each nominal declaration's entry held by the definition that is its former.
//!
//! This is what a checker is handed. Elaboration produces it and erasure consumes it, but the shape itself is representation — a [`Definition`] is a name, a universe context, a type, and a body, and a [`RecItem`] is the same for a recursive group whose members reference each other through one shared [`RecGroup`] binder rather than through free names. Both checkers walk this structure, which is why it lives here rather than beside either of them.
//!
//! Items are stored in dependency order, each at its depth and among items of one depth by name, so every item after the items it reaches ([`Order`](crate::Order), [`Definitions::ordered`]), and every walk reads them as stored. The entry an item's types may name ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]) is carried by the type former that declares it ([`Nominal`]), and beside its items a unit's [`Definitions`] carry its mounts, the witness set and its tests. The one compilation with an entrypoint holds a [`Program`], the module beside the entry's type and body.
//!
//! Well-formedness that *judges* rather than describes is not decided here. Whether a universe context is satisfiable is decided by each checker for itself, the elaborator's solver and the certifier's loop check; whether a definition terminates runs the size-change engine in `curios-analysis`, which both checkers drive. [`Totality`] is the classification those judgments record onto a definition, and the enum lives here because the field does.

use {
    super::{
        Applications, Arity, Atom, Bound, ConceptDecl, Cursor, Enter, Free, Func, FuncType, Global,
        InductDecl, Items, Level, Lexicon, Marks, Members, Mentions, Place, Places, Polarity,
        Quality, ReadEntry, ReadTable, RecGroup, Scope, Sharing, StructDecl, StructType, Subterm,
        Symbols, Telescope, Term, UniverseContext, UniverseError, UniverseParam, Universes,
        Variance, WitnessId, foreign_produced, wire_term,
    },
    curios_abi::ForeignFunction,
    curios_common::{
        Blob, Class, Digest, Facet, InKeyOrder, Mount, Persistent, Plicity, Qualifier, Recorded,
        Records, Sorted, SyntaxRegistry, candidate_name,
    },
    std::{
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        fmt,
        ops::Deref,
        rc::Rc,
        sync::Arc,
    },
};

/// Whether a definition is known to terminate on every input, and where it is not, whether its own recursion is why.
///
/// Neither `Partial` nor `Unfounded` is "proven divergent": a productive corecursive definition and a genuine infinite loop are both not proven total, and both remain legal wherever erasure keeps them. The variants are ordered by how much of the reason is the definition's own, so the verdict of a term made of parts is the greatest of theirs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[curios_archive::archived]
pub enum Totality {
    /// Every recursive group this definition contains descends, it calls no diverging host row, and neither does anything it reaches.
    Total,
    /// Not proven total, through what it reaches or a host row it calls, its own recursion descending. The conservative default: a definition whose classification is unknown is `Partial`, never `Total`.
    #[default]
    Partial,
    /// Its item's recursion does not descend: the item's group's calls do not close to a descent, or a definition it holds encloses a group that does not. Partial too, and the verdict a checker withholds a definition's value by.
    Unfounded,
}

impl Totality {
    pub fn is_total(self) -> bool {
        matches!(self, Totality::Total)
    }

    pub fn is_unfounded(self) -> bool {
        matches!(self, Totality::Unfounded)
    }
}

/// What the certifier concluded about the declarations one of its walks judged: each definition's totality, closed over everything it mentions, with what judging it read of other items, and each `induct` and `struct` entry's polarity vector.
///
/// **A version of its baseline's where its walk was over one.** Its tables are persistent and read back in the order of their names, so a walk over the unit as a baseline starts from the record it adopts verdicts from and changes it at the declarations it judges or drops alone ([`Certification::revised`]).
///
/// Only the certifier's walk makes one — `curios_cert::certify_definitions` — and a later walk reads it as the verdicts on the definitions it covers: no [`Definition`] carries elaboration's. It is filed with the unit whose definitions it covers, and that unit's address is its identity: a stored unit is found only under the compiler that judged it, and the fixed prelude's record is a constant of the build that certified it. It answers for every name its unit holds, a totality for each definition and both vectors for each family and record, as every walk's record does, and is read per name where its unit is in scope (`curios_cert::Filed`), a name it misses being a construction bug; no reading walk takes elaboration's word for a unit.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Certification {
    /// Each judged definition's totality: `Unfounded` where its item's recursion does not descend, and otherwise closed over everything it mentions.
    #[archived_with(InKeyOrder)]
    totalities: Sorted<Global, Totality>,
    /// How each judged definition applies its parameters, where it applies one every use of which is seen: what a later walk grades a call in a callback the definition is handed by.
    #[archived_with(InKeyOrder)]
    applications: Sorted<Global, Applications>,
    /// How each `induct` and `struct` entry the walk accepted uses its parameters, as the walk computed it: what a later walk composes through where a family it judges mentions this one. The vector the entry itself carries is elaboration's, which no walk reads.
    #[archived_with(InKeyOrder)]
    polarities: Sorted<Global, Vec<Polarity>>,
    /// How two instances of each `induct` and `struct` entry the walk accepted compare in each of its universe parameters, as the walk computed it: what a later walk compares two instances of the family by. The vector the entry itself carries is elaboration's, which the walk holds to this one and no judgment reads.
    #[archived_with(InKeyOrder)]
    variances: Sorted<Global, Vec<Variance>>,
    /// What judging each definition read of other items: what a later walk over the unit decides by which of these verdicts stand, and what nothing compiled after the unit reads.
    ///
    /// **Of the unit's baseline half**, as its traces are: skipped where the record is stored, stored beside it, each definition's reads a blob ([`Certification::recorded`]), handed back where it is restored ([`Certification::with_reads`]), and read when a walk over the unit first asks.
    #[archived_with(curios_archive::Skip)]
    reads: Recorded<Read>,
    /// What judging each definition read, read backwards. See [`ReadBy`].
    #[archived_with(curios_archive::Skip)]
    read_by: OnceCell<ReadBy>,
}

/// Who read what of a certifier's record, read backwards: for each name, the definitions whose judgment read its signature, and those whose judgment asked for its body. What a walk over the record's unit as a baseline finds the declarations an edit could reach by, from the edit, where asking every definition's reads asks the whole record.
///
/// **Read off the record, never stored.** Made the first time a walk over the unit asks, and held with the record for as long as its unit is held; a walk over a baseline hands the record it files the baseline's, with the edges of every definition it judged or the module no longer holds replaced ([`ReadBy::replace`]), so a session reads no record backwards twice.
///
/// **Looked up, never walked.** Its maps are persistent and keyed by hash, so their order follows the process, and what reads them asks after one name at a time.
#[derive(Debug, Clone, Default)]
pub struct ReadBy {
    signatures: Persistent<Global, Vec<Global>>,
    bodies: Persistent<Global, Vec<Global>>,
}

impl ReadBy {
    /// Who read what, of what judging each definition of `by` read.
    fn of(by: &Sorted<Global, HeldReads>) -> Self {
        // Gathered whole first, so each name's list is built once and not copied at every reader a persistent insert would add.
        let (mut signatures, mut bodies) = (
            BTreeMap::<Global, Vec<Global>>::new(),
            BTreeMap::<Global, Vec<Global>>::new(),
        );
        for (reader, reads) in by.iter() {
            for name in &reads.signatures {
                signatures.entry(*name).or_default().push(*reader);
            }
            for name in &reads.bodies {
                bodies.entry(*name).or_default().push(*reader);
            }
        }
        let mut read_by = ReadBy::default();
        for (name, readers) in signatures {
            read_by.signatures.insert(name, readers);
        }
        for (name, readers) in bodies {
            read_by.bodies.insert(name, readers);
        }

        read_by
    }

    /// The definitions whose judgment read `name`'s signature: its type, universe scheme, registry entry or verdict.
    pub fn signature(&self, name: &Global) -> &[Global] {
        self.signatures.get(name).map_or(&[], Vec::as_slice)
    }

    /// The definitions whose judgment asked for `name`'s body.
    pub fn body(&self, name: &Global) -> &[Global] {
        self.bodies.get(name).map_or(&[], Vec::as_slice)
    }

    /// Every name some judgment read, each once, in no stated order.
    pub fn names(&self) -> impl Iterator<Item = &Global> {
        self.signatures.iter().map(|(name, _)| name).chain(
            self.bodies
                .iter()
                .map(|(name, _)| name)
                .filter(|name| !self.signatures.contains_key(name)),
        )
    }

    /// Replace the edges of the definition `reader`: those `earlier`, what judging it read, added are taken out, and those `now`, what it reads, adds are put in. `None` on either side is a definition that was, or is, not judged.
    pub fn replace(&mut self, reader: Global, earlier: Option<&Reads>, now: Option<&Reads>) {
        if let Some(earlier) = earlier {
            for (names, map) in [
                (&earlier.signatures, &mut self.signatures),
                (&earlier.bodies, &mut self.bodies),
            ] {
                for name in names {
                    let Some(held) = map.get(name) else {
                        continue;
                    };
                    let kept = held
                        .iter()
                        .filter(|held| **held != reader)
                        .copied()
                        .collect::<Vec<_>>();
                    match kept.is_empty() {
                        true => {
                            map.remove(name);
                        }
                        false => map.insert(*name, kept),
                    }
                }
            }
        }
        if let Some(now) = now {
            for (names, map) in [
                (&now.signatures, &mut self.signatures),
                (&now.bodies, &mut self.bodies),
            ] {
                for name in names {
                    let mut held = map.get(name).cloned().unwrap_or_default();
                    held.push(reader);
                    map.insert(*name, held);
                }
            }
        }
    }
}

/// What the certifier completes a definition's reads with where its unit is filed: the identity of the item judged, the certifier's own digest of each part read, and what it concluded of the definition. What a later walk takes the verdict from where it judges an item of that identity and finds every part read digesting as it did.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Concluded {
    /// The identity of the item judged ([`ItemRun::identity`](crate::ItemRun::identity)).
    pub input: Digest,
    /// The digest of what each signature read read, in the order of the reads: `None` where nothing is recorded of it — a fixed unit's, of which no read is recorded, or a name no unit holds — and otherwise the digest, `None` within where the recorded digest does not decode.
    pub signatures: Vec<Option<Option<[u8; 32]>>>,
    /// The same, of each body read.
    pub bodies: Vec<Option<Option<[u8; 32]>>>,
    /// The definition's totality: `Unfounded` where its item's recursion does not descend, and otherwise closed over what it mentions.
    pub totality: Totality,
    /// How it applies its parameters ([`Applications`]).
    pub applications: Applications,
    /// The vectors of the registry entry it is the former of, where it is a former.
    pub vectors: Option<(Vec<Polarity>, Vec<Variance>)>,
}

/// What judging a definition read as a record holds it, and once a filing completed it, what judged it and what it concluded ([`Concluded`]): shared with every version that keeps it, beside the digest it is stored under.
#[derive(Debug)]
pub struct ReadRecord {
    reads: Reads,
    /// What a filing completed it with: unset until one did, `None` where nothing completes it.
    made: OnceCell<Option<Concluded>>,
    /// The digest it is stored under, kept once it is complete.
    digest: OnceCell<Digest>,
}

impl ReadRecord {
    /// The record of a judgment that read `reads`, which no filing has completed.
    pub fn new(reads: Reads) -> Self {
        Self {
            reads,
            made: OnceCell::new(),
            digest: OnceCell::new(),
        }
    }

    /// The record a cabinet held under `digest`: complete where it was filed with `made`, and otherwise left for a filing to complete.
    pub fn restored(reads: Reads, made: Option<Concluded>, digest: Digest) -> Self {
        let record = Self::new(reads);
        if let Some(made) = made {
            let _ = record.made.set(Some(made));
            let _ = record.digest.set(digest);
        }

        record
    }

    /// What judged the definition and what it concluded, where a filing completed the record with it.
    pub fn made(&self) -> Option<&Concluded> {
        self.made.get()?.as_ref()
    }

    /// Whether a filing has completed the record, with what judged the definition or with nothing.
    pub fn completed(&self) -> bool {
        self.made.get().is_some()
    }

    /// Complete the record with `made`, where nothing has: the record of every version that holds it.
    pub fn complete(&self, made: Option<Concluded>) {
        let _ = self.made.set(made);
    }

    /// The digest a cabinet names this record of the definition `name` by: where it is complete, the candidate's name, which a cabinet finds it among the records filed for the item it judged by ([`candidate_name`]), and otherwise the digest of its run alone.
    pub fn digest(&self, name: &Global) -> Result<Digest, String> {
        if let Some(digest) = self.digest.get() {
            return Ok(*digest);
        }
        let run = ReadTable::run([(name, &self.reads, self.made())])?;
        let digest = match self.made() {
            Some(made) => candidate_name(Facet::Judgment, &made.input, &run),
            None => Digest::of(&run),
        };
        if self.completed() {
            let _ = self.digest.set(digest);
        }

        Ok(digest)
    }
}

/// A record reads as what the judgment read.
impl Deref for ReadRecord {
    type Target = Reads;

    fn deref(&self) -> &Reads {
        &self.reads
    }
}

/// Two records are one where they say the same was read: what completed them is a function of that and of the unit.
impl PartialEq for ReadRecord {
    fn eq(&self, other: &Self) -> bool {
        self.reads == other.reads
    }
}

impl Eq for ReadRecord {}

/// What judging a definition read, as a record holds it: shared with every version that keeps it, beside the digest it is stored under.
pub type HeldReads = Rc<ReadRecord>;

/// What a walk's judgments read, held apart from what they concluded: of the items of its own unit, by name, and of the declarations of other units, each by the digest of the part read.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Read {
    /// What judging each definition read of other items: stored apart, a blob for each ([`ReadTable`]), and handed back where the record is restored ([`Records`]), what is archived with it being the rest.
    #[archived_with(curios_archive::Skip)]
    by: Sorted<Global, HeldReads>,
    /// What the walk read of each declaration its unit does not hold, another unit's. See [`Abroad`].
    #[archived_with(InKeyOrder)]
    abroad: Sorted<Global, Abroad>,
}

impl Read {
    /// What judging each definition read, as the record holds it, in the order of the definitions' names: what the record stores a blob for.
    pub fn definitions(&self) -> impl Iterator<Item = (&Global, &HeldReads)> {
        self.by.iter()
    }
}

/// What a walk's judgments read restored out of what a unit stores of them: the rest, archived with the record, and what judging each definition read a member of a run ([`ReadTable`]), restored beside the digest it was found by. A run several lie in is read once.
impl Records for Read {
    fn restored(table: &[u8], entries: &[Blob]) -> Result<Self, String> {
        let mut read = curios_archive::from_bytes::<Read>(table)?;
        let mut runs = HashMap::<(usize, usize), Vec<Option<ReadEntry>>>::new();
        for blob in entries {
            let member = blob
                .member
                .ok_or("what a judgment read is stored as a member of a run")?;
            let bytes = blob.bytes.as_slice();
            let run = match runs.entry((bytes.as_ptr() as usize, bytes.len())) {
                std::collections::hash_map::Entry::Occupied(held) => held.into_mut(),
                std::collections::hash_map::Entry::Vacant(unread) => {
                    unread.insert(ReadTable::read(bytes)?.into_iter().map(Some).collect())
                }
            };
            let (name, reads, made) = usize::try_from(member.index)
                .ok()
                .and_then(|index| run.get_mut(index))
                .and_then(Option::take)
                .ok_or_else(|| format!("a run of reads holds no member {}", member.index))?;
            read.by.insert(
                name,
                Rc::new(ReadRecord::restored(reads, made, blob.digest)),
            );
        }

        Ok(read)
    }
}

/// What a walk read of one declaration of another unit: the digest of what a signature read reads of it, and of what a body read does, each where some judgment read it. The certifier makes both, from the terms in its scope and its own verdict on them, and compares them where a walk over a baseline meets a unit that is not the one the baseline's walk read.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Abroad {
    pub signature: Option<String>,
    pub body: Option<String>,
}

/// Two records are one where they conclude the same of every definition and every entry and judging each read the same: a record's reads are part of what it says, wherever they are held.
impl PartialEq for Certification {
    fn eq(&self, other: &Self) -> bool {
        self.totalities == other.totalities
            && self.applications == other.applications
            && self.polarities == other.polarities
            && self.variances == other.variances
            && self.read() == other.read()
    }
}

impl Eq for Certification {}

/// What the certifier concluded about one definition it judged.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Certified {
    /// Whether the definition terminates: `Unfounded` where its item's recursion does not descend, and otherwise closed over everything it mentions.
    pub totality: Totality,
    /// How it applies its parameters ([`Applications`]).
    pub applications: Applications,
    /// What judging it read of other items. A recursive group is judged as one item, so each of its members holds the group's reads; a declaration's registry entry is accepted as part of its type former, whose entry holds what that acceptance read.
    pub reads: Reads,
}

/// The items one judgment read, and how: its type, or its body.
///
/// Recorded where the certifier consults its environment — a name's type or universe scheme, a declaration's registry entry, a definition's body — so a read counts however the judgment reached it. A remembered reduct included: every memo lives one declaration, so the bodies a remembered reduct unfolds were asked for within the judgment that hits it. A body counts as read whenever it is asked for, unfolded or not, because asking is what makes the answer depend on it.
///
/// A definition's classification also reads the verdict of every name it mentions, which the totality closure runs over, and that read is a signature read of the name: typing a mention reads its type, but a mention in an arm no reduction takes is typed by no rule, so the certifier records the read where it classifies the definition, and a verdict closed over such a name is judged again once the name's verdict moves.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Reads {
    /// The items whose type, universe scheme or registry entry was read.
    pub signatures: BTreeSet<Global>,
    /// The definitions whose body was asked for.
    pub bodies: BTreeSet<Global>,
}

impl Reads {
    /// These reads with `other`'s beside them.
    pub fn extend(&mut self, other: Reads) {
        self.signatures.extend(other.signatures);
        self.bodies.extend(other.bodies);
    }
}

impl Certification {
    /// A record of each of `definitions`, of the vectors of each entry in `polarities` and in `variances`, and of what the walk read of each declaration of another unit, `abroad`.
    pub fn of(
        definitions: impl IntoIterator<Item = (Global, Certified)>,
        polarities: impl IntoIterator<Item = (Global, Vec<Polarity>)>,
        variances: impl IntoIterator<Item = (Global, Vec<Variance>)>,
        abroad: impl IntoIterator<Item = (Global, Abroad)>,
    ) -> Self {
        let mut revision = Certification::default().revised();
        for (name, certified) in definitions {
            revision.judged(name, certified);
        }
        for (name, vector) in polarities {
            revision.polarities.insert(name, vector);
        }
        for (name, vector) in variances {
            revision.variances.insert(name, vector);
        }
        for (name, read) in abroad {
            revision.abroad.insert(name, read);
        }

        revision.filed()
    }

    /// This record as the start of a later walk's over its unit as a baseline: every table shared, to be changed where that walk judges or drops a declaration ([`Revision`]).
    pub fn revised(&self) -> Revision {
        let read = self.read();

        Revision {
            totalities: self.totalities.clone(),
            applications: self.applications.clone(),
            polarities: self.polarities.clone(),
            variances: self.variances.clone(),
            by: read.by.clone(),
            abroad: read.abroad.clone(),
        }
    }

    /// This record holding `read_by` as who read what of it: the index a walk over a baseline made of the baseline's, with the edges of what it judged replaced. See [`ReadBy`].
    pub fn reading(self, read_by: ReadBy) -> Self {
        let held = OnceCell::new();
        let _ = held.set(read_by);

        Self {
            read_by: held,
            ..self
        }
    }

    /// Who read what of this record, read backwards the first time it is asked. See [`ReadBy`].
    pub fn read_by(&self) -> &ReadBy {
        self.read_by.get_or_init(|| {
            curios_common::profile!("read_by::of", definitions = self.read().by.len());
            ReadBy::of(&self.read().by)
        })
    }

    /// What the walk's judgments read, read out of its bytes now where the record was restored and no walk has asked before.
    fn read(&self) -> &Read {
        self.reads
            .get()
            .unwrap_or_else(|error| panic!("a certifier's reads failed to restore: {error}"))
    }

    /// What judging each definition read, as the record holds it: stored apart from the record, each definition's reads a blob, since only a walk over its unit as a baseline reads them.
    pub fn recorded(&self) -> &Recorded<Read> {
        &self.reads
    }

    /// This record with `table` and `entries` as what judging each definition read ([`Certification::recorded`]), read when a walk over its unit first asks: what a restored record is given, since restoring it leaves it with none.
    pub fn with_reads(
        mut self,
        table: impl Into<curios_archive::Bytes>,
        entries: Vec<Blob>,
    ) -> Self {
        self.reads = Recorded::stored(table, entries);
        self
    }

    /// The polarity vector the certifier computed for the registry entry `name`, when this record holds one.
    pub fn polarities(&self, name: &Global) -> Option<&[Polarity]> {
        self.polarities.get(name).map(Vec::as_slice)
    }

    /// The variance vector the certifier computed for the registry entry `name`, when this record holds one.
    pub fn variances(&self, name: &Global) -> Option<&[Variance]> {
        self.variances.get(name).map(Vec::as_slice)
    }

    /// The certifier's classification of `name`, where this record files one.
    pub fn totality(&self, name: &Global) -> Option<Totality> {
        self.totalities.get(name).copied()
    }

    /// How `name` applies its parameters, where this record files it: a definition applying none of them, or using each it applies otherwise as well, has nothing filed.
    pub fn applications(&self, name: &Global) -> Option<&Applications> {
        self.applications.get(name)
    }

    /// What judging `name` read of other items, where this record files it.
    pub fn reads(&self, name: &Global) -> Option<&Reads> {
        self.read().by.get(name).map(|held| &***held)
    }

    /// What the walk read of `name`, a declaration of another unit, where it read it and recorded so.
    pub fn abroad(&self, name: &Global) -> Option<&Abroad> {
        self.read().abroad.get(name)
    }

    /// What judging each definition read, as the record holds it, in the order of the definitions' names: what a filing completes.
    pub fn records(&self) -> impl Iterator<Item = (&Global, &HeldReads)> {
        self.read().definitions()
    }

    /// The declarations of other units the walk recorded a read of.
    pub fn read_abroad(&self) -> impl Iterator<Item = &Global> {
        self.read().abroad.iter().map(|(name, _)| name)
    }

    /// Every classification this record holds.
    pub fn iter(&self) -> impl Iterator<Item = (&Global, Totality)> {
        self.totalities
            .iter()
            .map(|(name, totality)| (name, *totality))
    }
}

/// A certifier's record being revised by a walk over its unit as a baseline: the record's tables, shared with it, changed where the walk judges or drops a declaration ([`Certification::revised`]), and filed as a record of its own.
#[derive(Debug, Clone)]
pub struct Revision {
    totalities: Sorted<Global, Totality>,
    applications: Sorted<Global, Applications>,
    polarities: Sorted<Global, Vec<Polarity>>,
    variances: Sorted<Global, Vec<Variance>>,
    by: Sorted<Global, HeldReads>,
    abroad: Sorted<Global, Abroad>,
}

impl Revision {
    /// Record what the walk concluded of `name`, which it judged.
    pub fn judged(&mut self, name: Global, certified: Certified) {
        self.totalities.insert(name, certified.totality);
        self.applied(name, certified.applications);
        self.by
            .insert(name, Rc::new(ReadRecord::new(certified.reads)));
    }

    /// Record `record`, a record a cabinet held, as what judging `name` read, and its verdict as the walk's: a definition whose verdict the walk took from it.
    pub fn adopted(&mut self, name: Global, record: HeldReads) {
        if let Some(made) = record.made() {
            self.totalities.insert(name, made.totality);
            self.applied(name, made.applications.clone());
        }
        self.by.insert(name, record);
    }

    /// Take out everything recorded of `name`, which the unit no longer declares.
    pub fn dropped(&mut self, name: &Global) {
        self.totalities.remove(name);
        self.applications.remove(name);
        self.by.remove(name);
        self.polarities.remove(name);
        self.variances.remove(name);
    }

    /// File how `name` applies its parameters, or take out what was filed where it applies none ([`Applications::is_empty`]).
    fn applied(&mut self, name: Global, applications: Applications) {
        match applications.is_empty() {
            true => {
                self.applications.remove(&name);
            }
            false => {
                self.applications.insert(name, applications);
            }
        }
    }

    /// File the vectors of the registry entry the walk accepted under `name`, or take out the ones filed under it where it accepted none there.
    pub fn vectors(&mut self, name: Global, vectors: Option<(Vec<Polarity>, Vec<Variance>)>) {
        match vectors {
            Some((polarities, variances)) => {
                self.polarities.insert(name, polarities);
                self.variances.insert(name, variances);
            }
            None => {
                self.polarities.remove(&name);
                self.variances.remove(&name);
            }
        }
    }

    /// What the record holds of what was read of `name`, a declaration of another unit.
    pub fn abroad(&self, name: &Global) -> Option<&Abroad> {
        self.abroad.get(name)
    }

    /// Record `read` as what was read of `name`, a declaration of another unit, or take out what was recorded of it where nothing reads it now.
    pub fn read_abroad(&mut self, name: Global, read: Option<Abroad>) {
        match read {
            Some(read) => self.abroad.insert(name, read),
            None => {
                self.abroad.remove(&name);
            }
        }
    }

    /// The revised record, with who read what of it unmade until something asks.
    pub fn filed(self) -> Certification {
        Certification {
            totalities: self.totalities,
            applications: self.applications,
            polarities: self.polarities,
            variances: self.variances,
            reads: Recorded::held(Read {
                by: self.by,
                abroad: self.abroad,
            }),
            read_by: OnceCell::new(),
        }
    }
}

/// Where a module holds each name it declares, as [`Definitions::lexicon`] finds one: the item's place, and the member's within it.
type Located = HashMap<Global, (usize, usize)>;

/// How a lowered definition was introduced.
///
/// This is elaboration metadata, not a fact inferred from the flattened qualified name. In particular, a module and a nominal type may share a qualifier without turning ordinary module members into generated nominal members. A constructor is no kind of definition: an item makes it of its family's entry where it is read ([`Made`]), and holds none.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum DefinitionKind {
    Authored,
    Witness,
    Test,
}

impl DefinitionKind {
    /// What the definition is declared as, `type_` being the type it states: a written definition is a type, a function or a value by that type ([`Declared::defined`]).
    pub fn declared(&self, type_: &Term) -> Declared {
        match self {
            DefinitionKind::Authored => Declared::defined(type_),
            DefinitionKind::Witness => Declared::Witness,
            DefinitionKind::Test => Declared::Test,
        }
    }
}

/// What a name is declared as: a [`DefinitionKind`], a former or a constructor as its item makes it ([`MadeKind`]), or a concept's method, which is a name its concept declares and no definition; read and compared with no declaration in hand. A written definition is told a type, a function and a value apart by the type it states ([`Declared::defined`]), so what a name is painted as is read here with nothing read either.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Declared {
    /// A written definition whose stated type is a function's, ending in no sort.
    Function,
    /// A written definition whose stated type is neither a function's nor a sort.
    Value,
    /// A written definition whose stated type is a sort, or a function's ending in one: a type or a family of them, an intrinsic type of `/sys` among them.
    Type,
    Inductive,
    Struct,
    Concept,
    Constructor,
    Method,
    Witness,
    Test,
}

impl Term {
    /// Whether this type, read as written and unfolding nothing, ends past every function's telescope in a sort: a type's type, or a type former's — `Endo(Nat)` does not, whatever it unfolds to. What makes a written definition a type ([`Declared::defined`]) and a written parameter a type parameter.
    pub fn ends_in_sort(&self) -> bool {
        let mut result = self;
        while let Subterm::FuncType(FuncType { telescope }) = &**result {
            result = telescope.terminal();
        }
        matches!(&**result, Subterm::Sort(_))
    }
}

impl Declared {
    /// A written definition stating `type_`, read as written and unfolding nothing — `Endo(Nat)` is a value's type, whatever it unfolds to: a type where it ends, past every function's telescope, in a sort; a function where it is a function's otherwise; and a value where it is neither.
    pub fn defined(type_: &Term) -> Declared {
        match (type_.ends_in_sort(), &**type_) {
            (true, _) => Declared::Type,
            (false, Subterm::FuncType(_)) => Declared::Function,
            (false, _) => Declared::Value,
        }
    }

    /// Whether the name is a written definition: a type, a function or a value.
    pub fn is_defined(self) -> bool {
        matches!(self, Declared::Function | Declared::Value | Declared::Type)
    }

    /// What a name of a declaration declared as `self` is painted as: a type former and a written type as a type, a written function and a test as a function, and each other as its own. `None` for a witness, whose name the compiler gives and no reader writes.
    pub fn class(self) -> Option<Class> {
        Some(match self {
            Declared::Type | Declared::Inductive | Declared::Struct => Class::Type,
            Declared::Function | Declared::Test => Class::Function,
            Declared::Value => Class::Value,
            Declared::Concept => Class::Concept,
            Declared::Constructor => Class::Constructor,
            Declared::Method => Class::Method,
            Declared::Witness => return None,
        })
    }

    /// Whether the name is a nominal declaration's former.
    pub fn is_former(self) -> bool {
        matches!(
            self,
            Declared::Inductive | Declared::Struct | Declared::Concept
        )
    }
}

/// A nominal declaration's registry entry, held by the item that declares it ([`Nominals`]), which makes its former of it.
///
/// **One fact, held once.** A former among a module's items and its entry in a registry beside them are two facts under one name with nothing holding one to the other; held by the item, a former is what its entry makes, and a name declares at most one. A concept's record is held with it for the same reason: a concept is the superclass edges its fields reach and the record a witness is a value of, whose scheme and labels it reads, and neither is declared alone. Why is the crate `README.md`'s "A nominal declaration is held as its entries".
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Nominal {
    Inductive(InductDecl),
    Struct(StructDecl),
    Concept {
        concept: ConceptDecl,
        record: StructDecl,
    },
}

impl Nominal {
    /// What the declaration's former is declared as: an inductive, a structure or a concept.
    pub fn declared(&self) -> Declared {
        match self {
            Nominal::Inductive(_) => Declared::Inductive,
            Nominal::Struct(_) => Declared::Struct,
            Nominal::Concept { .. } => Declared::Concept,
        }
    }

    /// The scheme the declaration is generalized under: a family's, or its record's.
    pub fn universe_context(&self) -> &UniverseContext {
        match self {
            Nominal::Inductive(declaration) => declaration.universe_context(),
            Nominal::Struct(record) | Nominal::Concept { record, .. } => &record.universe_context,
        }
    }

    /// The module the declaration sits in.
    pub fn module(&self) -> Qualifier {
        match self {
            Nominal::Inductive(declaration) => declaration.module(),
            Nominal::Struct(record) | Nominal::Concept { record, .. } => record.module,
        }
    }

    /// The inductive family's entry, where the declaration is one.
    pub fn inductive(&self) -> Option<&InductDecl> {
        match self {
            Nominal::Inductive(declaration) => Some(declaration),
            _ => None,
        }
    }

    /// The record's entry: a structure's own, or the record a concept's witnesses are values of.
    pub fn record(&self) -> Option<&StructDecl> {
        match self {
            Nominal::Struct(record) | Nominal::Concept { record, .. } => Some(record),
            Nominal::Inductive(_) => None,
        }
    }

    /// The concept's superclass edges, where the declaration is one.
    pub fn concept(&self) -> Option<&ConceptDecl> {
        match self {
            Nominal::Concept { concept, .. } => Some(concept),
            _ => None,
        }
    }

    /// The domains the declaration's former ranges over: a family's parameters and indices, a record's and a concept's parameters.
    pub fn former_domains(&self) -> Vec<&Term> {
        match self {
            Nominal::Inductive(declaration) => declaration.former_domains(),
            Nominal::Struct(record) | Nominal::Concept { record, .. } => record.former_domains(),
        }
    }

    /// Every top-level name the entry's telescopes reach.
    pub fn reaches(&self) -> BTreeSet<Global> {
        match self {
            Nominal::Inductive(declaration) => declaration.reaches(),
            Nominal::Struct(record) | Nominal::Concept { record, .. } => record.reaches(),
        }
    }

    /// The entry with every term hash-consed against `sharing` — see [`Definitions::shared`].
    pub fn shared(&self, sharing: &Sharing) -> Self {
        match self {
            Nominal::Inductive(declaration) => Nominal::Inductive(declaration.shared(sharing)),
            Nominal::Struct(record) => Nominal::Struct(record.shared(sharing)),
            Nominal::Concept { concept, record } => Nominal::Concept {
                concept: concept.clone(),
                record: record.shared(sharing),
            },
        }
    }
}

/// A single top-level definition: `name` bound to `body` of declared `type_`.
///
/// A standalone top-level `let` uses free `Var`s keyed by `name`. A definition returned by [`RecItem::definitions`] is the opened view of a scoped recursive member and likewise uses the group's export names; the authoritative recursive type and body remain in [`RecItem::group`].
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Definition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub universe_context: UniverseContext,
    /// This definition's declaring module — `name`'s qualifier prefix, precomputed once by `into_core` (before `name` was flattened) rather than re-derived from it later. Stamped into `Context::island` per item by `elaborate_definitions_suffix` for the representation-privacy checks, which test subtree containment against it rather than equality; the same value `InductDecl::module` and `StructDecl::module` carry for type declarations. Islands are surface-elaboration state: erasure re-derives types with privacy suppressed and never stamps them.
    pub island: Qualifier,
    /// Whether this definition is a proof a declaration wrote: one introduced as [`DefinitionKind::Authored`] whose type is a proposition, or a function into one. Its body is then read by its own check and by nothing after it: a judgment that asks what the name unfolds to is answered with nothing, in both checkers, so the name stands for a constant of its type. No judgment reads a proof's value, which is what a binder's mark rests on too ([`Scope::binds_proof`](super::Scope::binds_proof)), so a body withheld refuses nothing a body unfolded would admit, and what a later declaration reads of this one is its type.
    ///
    /// Stated by each checker of its own run, as the definition's item is published, with its binders' marks, and `false` until then: `curios-elab` states it for the module it elaborates, and the certifier states it again by its own reading, reading none a module arrives with, in the module it hands back, which is the one a unit stores. A constructor of a proposition is the compiler's own term and stays what it unfolds to.
    pub proof: bool,
    /// The sort this definition's value inhabits, where it takes no parameter and its type is written as a sort, so that its value is a type: that type's own sort, read off the value and never off the annotation, which may name a larger universe. A sort term under [`Self::universe_context`], read at an occurrence's universes as [`InductDecl::result_sort`](super::InductDecl::result_sort) is. `None` for any other definition, and until the item is published.
    ///
    /// Stated beside [`Self::proof`], by each checker as the proof mark is, and read by every judgment that asks a binder's sort, quality or proof-ness where the binder's type is written as this definition, before anything is reduced: the value is not walked to learn it. The certifier states it of the value it checks, at every assignment of the definition's sorts, and a definition stating none is read by its reduct.
    pub value_sort: Option<Term>,
    pub type_: Term,
    pub body: Term,
}

/// A name an item declares and holds in none of its definitions, as the item makes it of what it does hold: a family's constructor, made of its family's entry ([`Made::constructors`]). Its type and its value are its maker's, at the maker's own levels, so nothing about it is stated anywhere else, and it is read where it is asked for rather than held.
#[derive(Debug, Clone, PartialEq)]
pub struct Made {
    pub name: Global,
    pub made: MadeKind,
    pub universe_context: UniverseContext,
    /// The module its maker sits in, which it shares.
    pub island: Qualifier,
    /// The sort its value inhabits where it takes nothing and its type is a sort ([`Definition::value_sort`]): a former's of a declaration with no parameter, which is the declaration's result sort.
    pub value_sort: Option<Term>,
    pub type_: Term,
    pub body: Term,
}

/// What a made name is, and of what it is made.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum MadeKind {
    /// The former of the declaration it names, which is declared as `declared`.
    Former(Declared),
    /// The constructor `tag` of the family `owner`.
    Constructor { owner: Qualifier, tag: Atom },
}

impl Made {
    /// The former of the nominal declaration `name` whose entry is `nominal`, spelled from the entry ([`former_spelling`]): declared at its parameters, then for a family its indices, ending in its sort, and unfolding to the nominal type over them at the declaration's own levels; where it takes nothing, its value inhabits the declaration's result sort.
    ///
    /// **The one spelling of a former from an entry.** A former says nothing its entry does not: an item holds the entry alone, and every reader of the former's name reads what this makes of it.
    pub fn former(name: Global, island: Qualifier, nominal: &Nominal) -> Self {
        let (universe_context, arity, result_sort) = match nominal {
            Nominal::Inductive(declaration) => (
                declaration.universe_context(),
                declaration.arity(),
                declaration.result_sort(),
            ),
            Nominal::Struct(record) | Nominal::Concept { record, .. } => {
                (&record.universe_context, &record.arity, &record.result_sort)
            }
        };
        let family = matches!(nominal, Nominal::Inductive(_));
        let (type_, body) = former_spelling(name, universe_context, arity, result_sort, family);
        let value_sort = matches!(&*type_, Subterm::Sort(_)).then(|| result_sort.clone());

        Self {
            name,
            made: MadeKind::Former(nominal.declared()),
            universe_context: universe_context.clone(),
            island,
            value_sort,
            type_,
            body,
        }
    }

    /// The value constructors of the family `owner` whose entry is `declaration`, one to a case in declaration order, each spelled from its case's signature: declared over the signature's binders under their marks, each binding a proof where the signature's does, the family's parameters hidden ahead of the payload, ending in the family at the case's indices, and unfolding to the case's injection over them, both at the family's own levels.
    ///
    /// **The one spelling of a constructor from its family's entry.** A constructor says nothing its family's entry does not, so a family's declaration states its constructors: elaboration makes each one by this, from the entry as elaborated, and a reader of Core text makes it by this too.
    pub fn constructors(
        owner: Qualifier,
        island: Qualifier,
        declaration: &InductDecl,
    ) -> Vec<Self> {
        let family = Global::Authored(owner);
        let universes = declaration.universe_context().identity_instance();
        let params = declaration.param_count();
        declaration
            .constructors()
            .iter()
            .map(|(tag, case)| {
                let signature = declaration.signature(case);
                let proofs = signature.proofs();
                let (binders, indices) = opened(signature.cursor());
                let variables = variables(&binders);
                let (leading, payload) = variables.split_at(params.min(variables.len()));
                let type_ =
                    Term::induct_type_at(family, universes.clone(), leading.to_vec(), indices);
                let injection = Term::variant_at(
                    family,
                    universes.clone(),
                    leading.to_vec(),
                    tag.clone(),
                    payload.to_vec(),
                );

                Self {
                    name: Global::Authored(owner.with(tag.as_str())),
                    made: MadeKind::Constructor {
                        owner,
                        tag: tag.clone(),
                    },
                    universe_context: declaration.universe_context().clone(),
                    island,
                    value_sort: None,
                    type_: Term::from(Subterm::FuncType(FuncType {
                        telescope: Telescope::build_marked(binders.clone(), type_)
                            .with_proofs(&proofs),
                    })),
                    body: Term::from(Subterm::Func(Func {
                        telescope: Telescope::build_marked(binders, injection).with_proofs(&proofs),
                    })),
                }
            })
            .collect()
    }

    /// The family this name is a constructor of, where it is a constructor.
    pub fn owner(&self) -> Option<Qualifier> {
        match &self.made {
            MadeKind::Constructor { owner, .. } => Some(*owner),
            MadeKind::Former(_) => None,
        }
    }

    /// What this name is declared as.
    pub fn declared(&self) -> Declared {
        match self.made {
            MadeKind::Former(declared) => declared,
            MadeKind::Constructor { .. } => Declared::Constructor,
        }
    }
}

/// The field of one of its own binders a definition's value is: where its value, beneath its own binders, is a field of one of them, which binder and which slot, as the elaborator's level alignment reads it ([`Families::parameter_field`](crate::Families::parameter_field)).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub struct ParameterField {
    /// The binder of the definition's outer telescope, counted from the first.
    pub parameter: usize,
    pub slot: usize,
}

/// Export metadata for one member of a flat top-level recursive group. The member's type and body live only in [`RecItem::group`], scoped over every export in the group.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct RecDefinition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub island: Qualifier,
    /// Per member, as [`Definition::proof`] is: a group may hold a lemma beside the function it is about, and the function still unfolds.
    pub proof: bool,
}

/// A flat top-level recursive item backed by the same structural fixed-point representation as a local [`super::Rec`]. Keeping the export metadata separate preserves the module's flat architecture without retaining a second, free-name copy of each recursive type and body.
///
/// One export per member of the group, held by the doors that build one: [`RecItem::new`] closes the group over the members it exports, [`RecItem::with_group`] asserts the count, and [`RecItem::exports_mut`] hands out the exports as a slice, whose length it cannot change.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct RecItem {
    definitions: Vec<RecDefinition>,
    group: RecGroup,
    /// The universe context every definition of this item was declared under, which is the one its group's members' level and quality parameters mean.
    ///
    /// Held by the item and not by the group, because it is the *declaration's*: a group is a term, its members' parameters are positionally the parameters of whatever declaration holds it, and an instance of that declaration substitutes them with every other level it binds. A context on the group would be a second binder over the same indices, which is the reading `xboard/src/formation/2026_11_02_a_group_met_as_a_bare_projection_is_checked_under_the_enclosing_declarations_constraints_and_read_under_the_readers.rs` admitted Hurkens' paradox through.
    universe_context: UniverseContext,
}

impl RecItem {
    /// Each member's export, in the group's order.
    pub fn exports(&self) -> &[RecDefinition] {
        &self.definitions
    }

    /// Each member's export, to restate in place.
    pub fn exports_mut(&mut self) -> &mut [RecDefinition] {
        &mut self.definitions
    }

    pub fn group(&self) -> &RecGroup {
        &self.group
    }

    pub fn universe_context(&self) -> &UniverseContext {
        &self.universe_context
    }

    /// The item's universe context, to restate in place: no count of the item reads it.
    pub fn universe_context_mut(&mut self) -> &mut UniverseContext {
        &mut self.universe_context
    }

    /// This item over `group`, a rewrite of its own that keeps a member for each export.
    pub fn with_group(&self, group: RecGroup) -> Self {
        assert_eq!(
            group.length(),
            self.definitions.len(),
            "a group item exports one name per member"
        );
        Self {
            group,
            ..self.clone()
        }
    }

    /// This group with universe data projected out of every member, in place.
    ///
    /// The universe context is cleared here as it is on a [`Definition`]: the projection's whole purpose is that no universe data survives into ANF.
    pub fn projected(&self) -> Self {
        Self {
            definitions: self.definitions.clone(),
            group: self.group.projected(),
            universe_context: UniverseContext::empty(),
        }
    }

    pub fn new(members: Vec<(Free, Definition)>) -> Self {
        Self::try_new(members).expect("a recursive group has one valid universe context")
    }

    /// The group of `members`, each a definition beside the local it and its siblings name it by: closed over those locals, so every reference a member makes to the group is the group's own binder.
    pub fn try_new(members: Vec<(Free, Definition)>) -> Result<Self, UniverseError> {
        let universe_context = members
            .first()
            .map(|(_, definition)| definition.universe_context.clone())
            .unwrap_or_default();
        if !members
            .iter()
            .all(|(_, definition)| definition.universe_context == universe_context)
        {
            return Err(UniverseError::MismatchedRecursiveContexts);
        }
        let group = RecGroup::closing(
            members
                .iter()
                .map(|(local, definition)| {
                    (*local, definition.type_.clone(), definition.body.clone())
                })
                .collect(),
        );
        let definitions = members
            .into_iter()
            .map(|(_, definition)| RecDefinition {
                name: definition.name,
                kind: definition.kind,
                island: definition.island,
                proof: definition.proof,
            })
            .collect();

        Ok(Self {
            definitions,
            group,
            universe_context,
        })
    }

    /// Open the recursive scopes against their exported names: each member's references to the group read as the declarations they are, at the group's own instance.
    ///
    /// The returned definitions are a read-only projection; the authoritative types and bodies remain structurally shared in the group's scheme.
    pub fn definitions(&self) -> Vec<Definition> {
        let names = self
            .definitions
            .iter()
            .map(|definition| {
                Term::instance(definition.name, self.universe_context.identity_instance())
            })
            .collect::<Vec<_>>();

        self.opened(&names)
    }

    /// The recursive scopes opened at `members`, one for each member in order, standing for it wherever the group names it: what a reader that names the members its own way reads, as a lowering reads its members' written names and an elaboration its locals.
    pub fn opened(&self, members: &[Term]) -> Vec<Definition> {
        let name_refs = members.iter().collect::<Vec<_>>();

        self.definitions
            .iter()
            .zip(self.group.iter())
            .map(|(definition, member)| Definition {
                name: definition.name,
                kind: definition.kind.clone(),
                universe_context: self.universe_context.clone(),
                island: definition.island,
                proof: definition.proof,
                value_sort: None,
                type_: member.type_.open(&name_refs),
                body: member.body.open(&name_refs),
            })
            .collect()
    }

    pub fn island(&self) -> Qualifier {
        self.definitions
            .first()
            .map(|definition| definition.island)
            .unwrap_or_default()
    }
}

/// A nominal declaration's former spelled from its entry's parts, its type and its body: declared at its parameters, then for a family its indices, ending in its sort, and unfolding to the nominal type over them at the declaration's own levels — the lambdas over the node, one to a call, which are the former's η-form ([`former_eta`](crate::former_eta)), or the node itself where the former takes nothing. `family` says whether the arity's inner telescope is a family's indices, part of its former, or a record's fields, no part of it.
///
/// **The one spelling of a former from an entry**, read by the former its entry makes ([`Made::former`]) and by an occurrence of the former's name, which elaboration spells as this body at the occurrence's levels.
pub fn former_spelling(
    name: Global,
    universe_context: &UniverseContext,
    arity: &Telescope<Telescope<()>>,
    result_sort: &Term,
    family: bool,
) -> (Term, Term) {
    let universes = universe_context.identity_instance();

    // The entry's telescopes are closed, so any binders open them: counted from zero, under the hints the entry keeps.
    let mut params = Vec::new();
    let mut cursor = arity.cursor();
    while let (Some((hint, domain)), Some(mark)) = (cursor.entry(), cursor.mark()) {
        let binder = Free::local(params.len() as u64, hint);
        cursor.advance(Term::free_var(&binder));
        params.push((mark, binder, domain));
    }
    // A family's indices follow its parameters, each explicit; a record's fields are no part of its former, so its field telescope is never opened.
    let mut indices = Vec::new();
    if family {
        let within = cursor
            .body()
            .expect("a cursor past every entry holds its payload");
        let mut cursor = within.cursor();
        while let Some((hint, domain)) = cursor.entry() {
            let binder = Free::local((params.len() + indices.len()) as u64, hint);
            cursor.advance(Term::free_var(&binder));
            indices.push((Plicity::Explicit, binder, domain));
        }
    }

    let variables = |binders: &[(Plicity, Free, Term)]| {
        binders
            .iter()
            .map(|(_, binder, _)| Term::free_var(binder))
            .collect::<Vec<_>>()
    };
    let applied = match family {
        true => Term::induct_type_at(name, universes, variables(&params), variables(&indices)),
        false => Term::from(Subterm::StructType(StructType {
            name,
            universes,
            params: variables(&params),
        })),
    };
    // One call for the parameters and one for the indices, the indices innermost: `Vec : (T : Type) -> (n : Nat) -> Type`.
    [params, indices]
        .into_iter()
        .filter(|group| !group.is_empty())
        .rev()
        .fold((result_sort.clone(), applied), |(type_, body), group| {
            (
                Term::func_type_marked(group.clone(), type_),
                Term::func_marked(group, body),
            )
        })
}

/// A telescope's entries opened over fresh locals, each under its mark, counted from zero under the hints the entries keep, with what the telescope ends in opened over them.
fn opened<B: Bound>(mut cursor: Cursor<'_, B>) -> (Vec<(Plicity, Free, Term)>, B) {
    let mut binders = Vec::new();
    while let (Some((hint, domain)), Some(mark)) = (cursor.entry(), cursor.mark()) {
        let binder = Free::local(binders.len() as u64, hint);
        cursor.advance(Term::free_var(&binder));
        binders.push((mark, binder, domain));
    }
    let body = cursor
        .body()
        .expect("a cursor past every entry holds its payload");

    (binders, body)
}

/// Each binder's variable, in order.
fn variables(binders: &[(Plicity, Free, Term)]) -> Vec<Term> {
    binders
        .iter()
        .map(|(_, binder, _)| Term::free_var(binder))
        .collect()
}

impl Definition {
    /// What a finished module publishes of this definition: what its name unfolds to, and the sort that value inhabits where it states one.
    ///
    /// The two go together because [`Self::proof`] withholds both — a judgment that asks what a proof's name unfolds to is answered with nothing, and the sort of a value no judgment may read is nothing a judgment may read either. Stated here, where the mark is, so that the readers of a finished module ask one question and no two of them can pair the fields differently: `curios-cert`'s environment and its read-digest, and `curios-elab`'s scope, its publication and the comparison that decides whether a part came out as it was.
    pub fn published(&self) -> (Option<&Term>, Option<&Term>) {
        match self.proof {
            true => (None, None),
            false => (Some(&self.body), self.value_sort.as_ref()),
        }
    }

    /// The definition a `foreign` row declares under `name`, made from the row: a function of its wire parameters, each explicit and hintless, ahead of which a row that does not return takes the type it yields, hidden, at a level of the definition's own; answering the row's call over them, at the type the row produces ([`foreign_produced`]).
    ///
    /// **The one spelling of a foreign row's definition.** The lowering writes a `foreign` declaration's definition so, and a reader of Core text makes it so from the row a `foreign` form states.
    pub fn foreign(
        name: Global,
        island: Qualifier,
        function: Arc<ForeignFunction>,
        syntax: &SyntaxRegistry,
    ) -> Self {
        let yields = function.diverges();
        let universe_context = UniverseContext {
            parameter_count: usize::from(yields),
            ..UniverseContext::empty()
        };
        let mut minted = 0u64;
        let mut fresh = |hint: Option<&str>| {
            minted += 1;
            Free::local(minted - 1, hint)
        };
        let yielded = yields.then(|| {
            (
                Plicity::Implicit,
                fresh(None),
                Term::type_at(Level::param(UniverseParam(0))),
            )
        });
        let telescope =
            yielded
                .into_iter()
                .chain(function.signature().params.iter().map(|wire_type| {
                    (Plicity::Explicit, fresh(None), wire_term(wire_type, syntax))
                }))
                .collect::<Vec<_>>();
        let operands = telescope
            .iter()
            .map(|(_, binder, _)| Term::free_var(binder))
            .collect::<Vec<_>>();
        let produced = foreign_produced(&function, &operands, syntax, |label| fresh(Some(label)));

        Self {
            name,
            kind: DefinitionKind::Authored,
            universe_context,
            island,
            proof: false,
            value_sort: None,
            type_: Term::func_type_marked(telescope.clone(), produced),
            body: Term::func_marked(telescope, Term::foreign(function, name, operands)),
        }
    }

    /// The definition a `test` declares under `name` with `value` its body: run where it is selected, so a function of nothing answering `value`, at the test family `family`'s type over the unit type at the least instance its entry `declaration` takes, `() -> Test({})`.
    ///
    /// **The one spelling of a test's definition.** Elaboration closes a test's type at that instance, every level the unit type sets at its least and every sort a type, and a reader of Core text makes a `test` form's definition so.
    pub fn test(
        name: Global,
        island: Qualifier,
        value: Term,
        family: Global,
        declaration: &InductDecl,
    ) -> Self {
        let context = declaration.universe_context();
        let universes = Universes {
            levels: vec![Level::zero(); context.parameter_count],
            qualities: vec![Quality::Type; context.quality_count],
        };
        let tested = Term::induct_type_at(
            family,
            universes,
            [Term::tuple_type_unit()],
            Vec::<Term>::new(),
        );
        let none = Vec::<(Plicity, Free, Term)>::new();

        Self {
            name,
            kind: DefinitionKind::Test,
            universe_context: UniverseContext::empty(),
            island,
            proof: false,
            value_sort: None,
            type_: Term::func_type_marked(none.clone(), tested),
            body: Term::func_marked(none, value),
        }
    }

    /// Every declaration this definition references.
    pub fn mentions(&self) -> BTreeSet<Global> {
        let (body, type_) = (self.body.mentions(), self.type_.mentions());
        body.declarations()
            .chain(type_.declarations())
            .copied()
            .collect()
    }

    /// Every top-level name this definition's elaboration consumed: what it [`mentions`](Self::mentions), plus the nominal heads its constructions and type-former normal forms name. Those live in the registry rather than in a reference, so `mentions` cannot see them — and an item that builds a `Struct` or matches a `Variant` depends on that declaration exactly as one that names it does.
    ///
    /// This is the edge set an invalidation is closed over, a refusal is poisoned along and, with what an item's registry entries reach ([`Definitions::reaches`]), the certifier's order follows. [`mentions`](Self::mentions) is what the totality closure needs, which the reference edges alone serve.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = self.mentions();
        names.extend(construction_heads(&self.type_));
        names.extend(construction_heads(&self.body));
        names
    }
}

/// A top-level item: a single `let` definition, a `rec` group of mutually-recursive definitions (which may reference each other by `name`), or a nominal declaration held as its entries. A definition is held by a box: it carries what its declaration publishes beside its terms, and a group carries its members' behind the group it shares, so held inline it would size every item of a module by the widest.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum Item {
    Let(Box<Definition>),
    Rec(RecItem),
    Nominal(Nominals),
}

/// A nominal declaration as an item holds it: each name it declares a former under, with that name's entry, in declaration order — one for a lone declaration, several for a group, of one kind. It holds nothing else: its formers and their constructors are made of the entries where they are read ([`Made`]), its island is its entries' module, and its scheme the one its entries share, as a group's members share theirs. Why is the crate `README.md`'s "A nominal declaration is held as its entries".
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Nominals {
    entries: Vec<(Global, Rc<Nominal>)>,
}

impl Nominals {
    /// The declaration whose names and entries are `entries`, in declaration order: at least one, all of one kind and under one scheme.
    pub fn new(entries: Vec<(Global, Nominal)>) -> Self {
        assert!(!entries.is_empty(), "a declaration declares a name");
        assert!(
            entries
                .iter()
                .all(|(_, entry)| entry.declared() == entries[0].1.declared()),
            "a declaration's entries are of one kind"
        );
        assert!(
            entries
                .iter()
                .all(|(_, entry)| entry.universe_context() == entries[0].1.universe_context()),
            "a declaration's entries share one scheme"
        );

        Self {
            entries: entries
                .into_iter()
                .map(|(name, entry)| (name, Rc::new(entry)))
                .collect(),
        }
    }

    /// Each name the declaration declares a former under, with its entry, in declaration order.
    pub fn entries(&self) -> &[(Global, Rc<Nominal>)] {
        &self.entries
    }

    /// The module the declaration sits in: its entries'.
    pub fn island(&self) -> Qualifier {
        self.entries[0].1.module()
    }

    /// The scheme the declaration is generalized under, which its entries share.
    pub fn universe_context(&self) -> &UniverseContext {
        self.entries[0].1.universe_context()
    }

    /// The declaration with every term hash-consed against `sharing` — see [`Definitions::shared`].
    pub fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            entries: self
                .entries
                .iter()
                .map(|(name, entry)| (*name, Rc::new(entry.shared(sharing))))
                .collect(),
        }
    }

    /// The declaration with each entry replaced by what `entry` answers for its name and the entry it holds now, where it answers one.
    pub fn with_entries(
        mut self,
        mut entry: impl FnMut(&Global, &Nominal) -> Option<Nominal>,
    ) -> Self {
        for (name, held) in &mut self.entries {
            if let Some(replaced) = entry(name, held) {
                *held = Rc::new(replaced);
            }
        }

        self
    }
}

impl Item {
    /// This item with every term hash-consed against `sharing` — see [`Definitions::shared`].
    pub fn shared(&self, sharing: &Sharing) -> Item {
        match self {
            Item::Let(definition) => Item::Let(Box::new(Definition {
                name: definition.name,
                kind: definition.kind.clone(),
                universe_context: definition.universe_context.clone(),
                island: definition.island,
                proof: definition.proof,
                value_sort: definition
                    .value_sort
                    .as_ref()
                    .map(|sort| sharing.share(sort)),
                type_: sharing.share(&definition.type_),
                body: sharing.share(&definition.body),
            })),
            Item::Rec(rec) => Item::Rec(RecItem {
                definitions: rec
                    .definitions
                    .iter()
                    .map(|member| RecDefinition {
                        name: member.name,
                        kind: member.kind.clone(),
                        island: member.island,
                        proof: member.proof,
                    })
                    .collect(),
                // Mapped in place rather than opened and re-closed: the round trip rebuilds every node twice and drops every memoized derivation with it, and the rebuilt nodes would escape this very pass.
                group: rec.group.map_members(|term| sharing.share(term)),
                universe_context: rec.universe_context.clone(),
            }),
            Item::Nominal(nominals) => Item::Nominal(nominals.shared(sharing)),
        }
    }

    /// How a diagnostic names this item.
    ///
    /// An authored declaration is named by its path. A witness has no authored path — that is the point of `satisfy` — so it is named by the module it was declared in, which is the coordinate a reader can actually act on.
    pub fn describe(&self) -> String {
        let described = |definition: &Definition| match definition.name.qualifier() {
            Some(path) => path.join(),
            None => match definition.island.is_root() {
                true => "the witness in the entry module".to_string(),
                false => format!("the witness in '{}'", definition.island.join()),
            },
        };
        match self {
            Item::Let(definition) => described(definition),
            Item::Rec(rec) => rec
                .definitions()
                .iter()
                .map(described)
                .collect::<Vec<_>>()
                .join(", "),
            Item::Nominal(nominals) => nominals
                .entries()
                .iter()
                .map(|(name, _)| name.faithful().to_string())
                .collect::<Vec<_>>()
                .join(", "),
        }
    }

    /// The names this top-level item declares, in declaration order: each of its definitions' or each name its declaration declares a former under, then each constructor of each family it declares, which the family's entry names ([`Item::constructors`]).
    pub fn declared_names(&self) -> Vec<Global> {
        let own = match self {
            Item::Let(definition) => vec![definition.name],
            Item::Rec(rec) => rec
                .definitions
                .iter()
                .map(|definition| definition.name)
                .collect(),
            Item::Nominal(nominals) => nominals.entries().iter().map(|(name, _)| *name).collect(),
        };

        own.into_iter().chain(self.constructor_names()).collect()
    }

    /// What this item declares each of its names as, in the order of [`Item::declared_names`]: each definition it holds as it is written, or each former as its entry's kind, then each constructor of a family it declares.
    pub fn declared(&self) -> Vec<Declared> {
        let own = match self {
            Item::Nominal(nominals) => nominals
                .entries()
                .iter()
                .map(|(_, entry)| entry.declared())
                .collect(),
            Item::Let(definition) => vec![definition.kind.declared(&definition.type_)],
            Item::Rec(rec) => rec
                .definitions
                .iter()
                .enumerate()
                .map(|(member, definition)| {
                    definition.kind.declared(&rec.group.member_type(member))
                })
                .collect(),
        };
        let constructors = self.constructor_names().len();

        own.into_iter()
            .chain(std::iter::repeat_n(Declared::Constructor, constructors))
            .collect()
    }

    /// Each family this item declares: the path its former is named by, the module it sits in, and its entry.
    fn families(&self) -> Vec<(Qualifier, Qualifier, &InductDecl)> {
        let Item::Nominal(nominals) = self else {
            return Vec::new();
        };
        let island = nominals.island();
        nominals
            .entries()
            .iter()
            .filter_map(|(name, entry)| match name {
                Global::Authored(owner) => Some((*owner, island, entry.inductive()?)),
                Global::Witness(_) => None,
            })
            .collect()
    }

    /// The names of the constructors of each family this item declares, one to a case in declaration order.
    fn constructor_names(&self) -> Vec<Global> {
        self.families()
            .into_iter()
            .flat_map(|(owner, _, declaration)| {
                declaration
                    .constructors()
                    .iter()
                    .map(move |(tag, _)| Global::Authored(owner.with(tag.as_str())))
            })
            .collect()
    }

    /// The value constructors of each family this item declares, as each family's entry makes them ([`Made::constructors`]): what the item declares beside its definitions and holds in none of them, so a constructor is its family's by construction.
    pub fn constructors(&self) -> Vec<Made> {
        self.families()
            .into_iter()
            .flat_map(|(owner, island, declaration)| Made::constructors(owner, island, declaration))
            .collect()
    }

    /// The formers of the declaration this item holds, as each entry makes it ([`Made::former`]): none for an item of definitions.
    pub fn formers(&self) -> Vec<Made> {
        let Item::Nominal(nominals) = self else {
            return Vec::new();
        };
        let island = nominals.island();
        nominals
            .entries()
            .iter()
            .map(|(name, entry)| Made::former(*name, island, entry))
            .collect()
    }

    /// The name `name` as this item makes it of an entry it holds: a former, or a constructor of a family it declares. `None` for a name the item holds a definition under, or declares not at all.
    pub fn made(&self, name: &Global) -> Option<Made> {
        if let Item::Nominal(nominals) = self
            && let Some((_, entry)) = nominals.entries().iter().find(|(held, _)| held == name)
        {
            return Some(Made::former(*name, nominals.island(), entry));
        }
        self.constructor(name)
    }

    /// The constructor `name` of a family this item declares, as its family's entry makes it.
    pub fn constructor(&self, name: &Global) -> Option<Made> {
        let Global::Authored(path) = name else {
            return None;
        };
        let owner = path.without_last();
        let (_, island, declaration) = self
            .families()
            .into_iter()
            .find(|(family, _, _)| *family == owner)?;

        Made::constructors(owner, island, declaration)
            .into_iter()
            .find(|constructor| constructor.name == *name)
    }

    /// The entries this item holds, each under the name that declares it, in declaration order: none for an item of definitions.
    pub fn nominals(&self) -> Vec<(&Global, &Nominal)> {
        match self {
            Item::Nominal(nominals) => nominals
                .entries()
                .iter()
                .map(|(name, entry)| (name, &**entry))
                .collect(),
            Item::Let(_) | Item::Rec(_) => Vec::new(),
        }
    }

    /// The entry this item holds under `name`, where it holds one.
    pub fn nominal(&self, name: &Global) -> Option<&Nominal> {
        self.nominals()
            .into_iter()
            .find_map(|(held, nominal)| (held == name).then_some(nominal))
    }

    /// The definitions this top-level item holds, in the same order as [`Item::declared_names`] — one for a `let`, one per member for a `rec`, and none for a nominal declaration, whose names are made ([`Item::made`]).
    ///
    /// Written out at each caller, the fan-out would be one more place a new `Item` variant could be missed. It belongs here beside `declared_names` for the same reason that one does: what an item declares is the item's own question.
    ///
    /// Owned rather than borrowed, because a `rec` member's [`Definition`] is *materialized* from the group rather than stored — there is nothing to hand a reference to.
    pub fn definitions(&self) -> Vec<Definition> {
        match self {
            Item::Let(definition) => vec![Definition::clone(definition)],
            Item::Rec(rec) => rec.definitions(),
            Item::Nominal(_) => Vec::new(),
        }
    }

    /// Every term this item holds, offered to `visit`: the positions a module is validated over (`item_positions`), each read as the terms it is made of. What a read of the item as it was written goes over, a term holding where it was written.
    pub fn each_term(&self, mut visit: impl FnMut(&Term)) {
        item_positions(self, &|_| false, &mut |_, carried: &dyn Carried| {
            carried.terms(&mut visit)
        });
    }
}

/// A program's entrypoint: the expression it closes with, with the type it is judged at. One type rather than two fields, so a type without an entry — a type for no expression — is unspellable rather than asserted away.
#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub body: Term,
    /// The type the body is judged at. Before elaboration it is what whoever compiles the entry states — the program contract, or the empty proposition for an entry put as a proof — and absent only where nothing is stated, for elaboration to infer. Elaboration writes the type the body was judged at either way, so every stage after it reads the entry's type off the entry, and the certifier refuses a program whose entry states none.
    pub type_: Option<Term>,
}

/// A program: the unit it is compiled from, and the term it closes with.
///
/// The one compilation that has an entrypoint is the one that holds a `Program`; every other unit — a library, a prelude root, a recompile's reused items — is its [`Definitions`] alone. An optional entry on every module would be `None` for all of them but one, and each stage reading a module would carry a case for the entry it almost never has. Carried beside the module instead, the entry exists exactly where a program is compiled, and a stored unit cannot hold one.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub definitions: Definitions,
    pub entrypoint: Entrypoint,
}

/// The whole of one unit as a *flat* list of top-level `items`. A nominal declaration's registry entry is held by the definition that is its former ([`Nominal`]), so the registries a stage checks against are read off the items and a module holds none beside them.
///
/// Flat rather than one N-deep nested `Subterm::Let`/`Rec` term: folding a unit into one would make its construction (`Scope::close` over the whole accumulator at each step) and every pass that recursed along its `.tail` spine O(N) in stack, overflowing at prelude depth. `Subterm::Let`/`Rec` remain for genuine *local*, in-expression bindings, which are shallow.
///
/// **The module holds its definitions and nothing beside them.** Which of them are witnesses and which are tests is what each is declared as, read off its place with no item restored ([`Definitions::witnesses`], [`Definitions::tests`]), so no table can say otherwise; and the prefixes its unit claims are the unit's, held beside it ([`Mount`]). The default is the empty unit, and a module built by hand names only what it holds.
#[derive(Debug, Clone, Default, PartialEq)]
#[curios_archive::archived]
pub struct Definitions {
    /// The module's items, in the order it stores them: every one in hand where the module was made in this process, and each restored the first time it is read where the module was stored ([`Items`]).
    #[archived_with(crate::AsVector)]
    pub items: Items,
}

impl Definitions {
    /// The entry the former `name` is made of, read at the place `places` keeps for it: one item read, and of a stored module one item restored.
    pub fn nominal(&self, places: &Places, name: &Global) -> Option<&Nominal> {
        let place = places.place(name)?;
        match place {
            Place::Made { .. } => self.items[place.item()].nominal(name),
            Place::Let(_) | Place::Member { .. } => None,
        }
    }

    /// The inductive family `name` declares, read where `places` holds it.
    pub fn induct_decl(&self, places: &Places, name: &Global) -> Option<&InductDecl> {
        self.nominal(places, name)?.inductive()
    }

    /// The record `name` declares, a structure's or the one a concept's witnesses are values of, read where `places` holds it.
    pub fn struct_decl(&self, places: &Places, name: &Global) -> Option<&StructDecl> {
        self.nominal(places, name)?.record()
    }

    /// The concept `name` declares, read where `places` holds it.
    pub fn concept(&self, places: &Places, name: &Global) -> Option<&ConceptDecl> {
        self.nominal(places, name)?.concept()
    }

    /// The names the module declares a nominal under, each with which kind of declaration it is the former of, in the order the module stores them: read off what each place says it declares, with no item read.
    pub fn formers(&self) -> Vec<(Global, Declared)> {
        self.items
            .places()
            .flat_map(|place| {
                let names = self.items.names(place).into_iter();

                names.zip(self.items.declared(place))
            })
            .filter(|(_, declared)| declared.is_former())
            .collect()
    }

    /// Every entry the module's items hold, each under the name that declares it, in the order the module stores its items. Reads every item: for a module in hand, never for a unit in scope, which is asked by name.
    pub fn nominals(&self) -> impl Iterator<Item = (&Global, &Nominal)> {
        self.items.iter().flat_map(Item::nominals)
    }

    /// The names the module declares as witnesses, in the order it stores them. A unit in scope is asked for its witnesses at a key by the places it is read by ([`Places::witnesses_at`]).
    pub fn witnesses(&self) -> Vec<Global> {
        self.declared_as(|declared| declared == Declared::Witness)
    }

    /// The names the module declares as tests, in the order it stores them: a unit's lowered module stores its items in the order its sources declare them, which a test program schedules its tests by, module by module.
    pub fn tests(&self) -> Vec<Global> {
        self.declared_as(|declared| declared == Declared::Test)
    }

    /// Every inductive family the module declares, by name — see [`Definitions::nominals`].
    pub fn induct_decls(&self) -> BTreeMap<Global, &InductDecl> {
        self.nominals()
            .filter_map(|(name, nominal)| Some((*name, nominal.inductive()?)))
            .collect()
    }

    /// Every record the module declares, a structure's or a concept's, by name — see [`Definitions::nominals`].
    pub fn struct_decls(&self) -> BTreeMap<Global, &StructDecl> {
        self.nominals()
            .filter_map(|(name, nominal)| Some((*name, nominal.record()?)))
            .collect()
    }

    /// Every concept the module declares, by name — see [`Definitions::nominals`].
    pub fn concepts(&self) -> BTreeMap<Global, &ConceptDecl> {
        self.nominals()
            .filter_map(|(name, nominal)| Some((*name, nominal.concept()?)))
            .collect()
    }

    /// This module with every term hash-consed against `sharing` — one shared allocation for each structure as it is spelled, under no position — and with no universe constraint saying where it was raised.
    ///
    /// Built for an elaborated module about to be archived. Elaboration constructs the same types, telescopes, and proof spines independently in definition after definition, and nothing deduplicates them, because `Rc` sharing only ever arises from *cloning* a value: two definitions that build the same type build it twice. Unshared, the prelude's nodes outnumber its distinct structures several times over (the prelude build reports each root's distinct count), and the archive would store that expansion in full and every restored traversal walk it in full.
    ///
    /// Not for a lowered module, which a lint and a report are located in: a consed term says nothing of where it was written.
    pub fn shared(&self, sharing: &Sharing) -> Definitions {
        Definitions {
            items: self.items.iter().map(|item| item.shared(sharing)).collect(),
        }
    }

    /// This module, archived without its items ([`WithoutItems`](crate::WithoutItems)), with `blobs` handed over as what each item was archived as, one for each, in their order: what a restored unit is given, since restoring it leaves its module without one. `Err` is not one for each item the module outlines.
    pub fn with_items(mut self, blobs: Vec<Blob>) -> Result<Self, String> {
        self.items = self.items.with_parts(blobs)?;

        Ok(self)
    }

    /// Each nominal declaration's argument plicities, keyed by the family's name — parameters then indices, in the order a use site supplies them.
    ///
    /// Read off the type constructor's own definition, whose declared type is the one or two `FuncType`s lowering nests — the parameters', then an indexed family's indices' — ending in the sort: the parameters keep their declared marks and the indices are always explicit. That type is the marks' one home a checker establishes: `InductType` carries none (for a fixed name they are a function of the name, so storing them per-occurrence would be derived data that conversion must then either compare pointlessly or exclude from `Hash`, and excluding them lets hash-consing collapse differently-marked equal nodes), and the marks a registry entry's arity states for the elaborator ([`InductDecl::arity`], [`StructDecl::arity`]) are checked by no one, so a report does not read them.
    ///
    /// The formers are the definitions declared as one, which each place of the module says of its names with no item read ([`Items::declared`]), each then read where the module holds it and no other item with it: a member of a `rec` item for an inductive, which refers to itself, and a plain `let` for a structure or a concept. A nullary declaration has no `FuncType` wrapper at all and contributes nothing.
    fn nominal_plicities(&self, located: &Located) -> Marks {
        let mut marks = Marks::new();
        // Which definitions are type formers is what each place says it declares, so the formers are read and no other item is.
        for name in &self.declared_as(Declared::is_former) {
            if let Some(former) = self.item_at(located, name).and_then(|item| item.made(name))
                && let Some(collected) = former_marks(&former.type_)
            {
                marks.insert(former.name, Rc::new(collected));
            }
        }

        marks
    }

    /// Where the module holds each name it declares, read off what each place declares with no item read: what lets a lexicon be made of the declarations it is about and no other.
    ///
    /// **Its own index, where [`Places`] would be the table.** A table of places is made of no module that declares a name twice, and such a module is refused in words that still have to be spelled; here the later declaration answers, as it does for every reader of a report.
    fn located(&self) -> Located {
        let mut located = HashMap::new();
        for place in self.items.places() {
            for (member, name) in self.items.names(place).into_iter().enumerate() {
                located.insert(name, (place, member));
            }
        }

        located
    }

    /// The names the module declares as a kind `wanted` holds of, in the order it stores them: read off what each place says it declares ([`Items::declared`]), with no item read.
    fn declared_as(&self, wanted: impl Fn(Declared) -> bool) -> Vec<Global> {
        self.items
            .places()
            .flat_map(|place| {
                let names = self.items.names(place).into_iter();

                names.zip(self.items.declared(place))
            })
            .filter(|(_, declared)| wanted(*declared))
            .map(|(name, _)| name)
            .collect()
    }

    /// The item `located` holds `name` in: the one item read, and where the module was stored the one item restored.
    fn item_at(&self, located: &Located, name: &Global) -> Option<&Item> {
        let &(place, _) = located.get(name)?;

        Some(&self.items[place])
    }

    /// What a report spells this unit's names by — see [`Lexicon`].
    pub fn lexicon(&self) -> Lexicon {
        let located = self.located();

        Lexicon {
            symbols: Rc::new(Symbols::of(self.symbols())),
            plicities: Rc::new(self.nominal_plicities(&located)),
            members: Rc::new(self.members(&located)),
        }
    }

    /// This module's lexicon made of `earlier`, the lexicon of a module that declares otherwise than this one only the names of `changed`, where `places` holds this module's names: each of those read again of this module, and every other entry `earlier`'s. The items that declare them are read and no other.
    pub fn lexicon_over(
        &self,
        earlier: &Lexicon,
        places: &Places,
        changed: &BTreeSet<Global>,
    ) -> Lexicon {
        let held = |name: &&Global| places.place(name).is_some();
        let symbols = earlier.symbols.revised(
            changed.iter().filter(|name| !held(name)).copied(),
            changed.iter().filter(held).copied(),
        );
        let mut plicities = (*earlier.plicities).clone();
        let mut members = (*earlier.members).clone();
        for name in changed {
            plicities.remove(name);
            members.fields.remove(name);
            members.payloads.remove(name);
            // A former is a name its item holds an entry under: one read, the item restored where the module was stored.
            let Some(item) = places.place(name).map(|place| &self.items[place.item()]) else {
                continue;
            };
            let Some(entry) = item.nominal(name) else {
                continue;
            };
            if let Some(marks) = item
                .made(name)
                .and_then(|former| former_marks(&former.type_))
            {
                plicities.insert(*name, Rc::new(marks));
            }
            put_members(&mut members, name, entry);
        }

        Lexicon {
            symbols: Rc::new(symbols),
            plicities: Rc::new(plicities),
            members: Rc::new(members),
        }
    }

    /// The module the definition `name` names sits in ([`Definition::island`]), where `places` holds it: the one item is read.
    pub fn island_of(&self, places: &Places, name: &Global) -> Option<Qualifier> {
        let place = places.place(name)?;

        let item = &self.items[place.item()];

        item.definitions()
            .into_iter()
            .find(|definition| definition.name == *name)
            .map(|definition| definition.island)
            .or_else(|| item.made(name).map(|made| made.island))
    }

    /// What this unit's structures and constructors are written by ([`Members`]): each structure's fields under their marks and labels, a concept's record among them, and the marks each constructor is called by.
    fn members(&self, located: &Located) -> Members {
        let mut members = Members::default();
        for name in &self.declared_as(Declared::is_former) {
            if let Some(entry) = self
                .item_at(located, name)
                .and_then(|item| item.nominal(name))
            {
                put_members(&mut members, name, entry);
            }
        }

        members
    }

    /// Every top-level name `item`'s elaboration consumed: what the item [reaches](Item::reaches) through its definitions, plus what the registry entries it declares reach — a struct's field types, an inductive's constructor payloads and a concept's parameters live only in the entry, and an item whose entry names another declaration depends on it as its body would.
    pub fn reaches(&self, item: &Item) -> BTreeSet<Global> {
        let mut names = item.reaches();

        for (_, nominal) in item.nominals() {
            names.extend(nominal.reaches());
        }

        names
    }

    /// Every global qualified name in `self`: each definition (`let`/`rec`), each inductive type, each struct type. The universe a global is shortened *against*.
    pub fn symbols(&self) -> Vec<Global> {
        let mut symbols = Vec::new();
        // Asked of what each place declares, which a stored module knows without its item.
        for place in self.items.places() {
            symbols.extend(self.items.names(place));
        }
        symbols
    }
}

impl Term {
    /// The top-level names this term reaches: the declarations it references, and the nominal heads of its constructions and type-former normal forms, which live in the registry rather than in a reference.
    pub fn reaches(&self) -> BTreeSet<Global> {
        term_reaches(self)
    }
}

impl Entrypoint {
    /// Every free local the entry's body or stated type mentions — what a walk judging the entry beside its module refuses, as [`free_locals_of`] is for the module.
    pub fn free_locals(&self) -> BTreeSet<Free> {
        let (body, type_) = (
            self.body.mentions(),
            self.type_.as_ref().map(Term::mentions),
        );
        body.locals()
            .chain(type_.iter().flat_map(Mentions::locals))
            .copied()
            .collect()
    }

    /// How far the entry's body or stated type reaches past the binders above them — none, as a position has none — so zero for a finished entry and the reach of its open position otherwise. The sibling of [`Entrypoint::free_locals`], as [`open_positions_of`] is of [`free_locals_of`].
    pub fn reach(&self) -> usize {
        let stated = self.type_.as_ref().map_or(0, Bound::reach);

        Bound::reach(&self.body).max(stated)
    }

    /// The top-level names the entry reaches, through its body and the type it states.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = term_reaches(&self.body);
        names.extend(self.type_.iter().flat_map(term_reaches));
        names
    }
}

impl Item {
    /// The top-level names this item reaches — [`Definition::reaches`] for a `let`; for a `rec`, its members read under the group's own binders rather than opened, so a member's reference to a sibling is not an edge to itself; for a nominal declaration, its entries'.
    pub fn reaches(&self) -> BTreeSet<Global> {
        match self {
            Item::Let(definition) => definition.reaches(),
            Item::Rec(rec) => {
                let mut names = BTreeSet::new();
                for member in rec.group.iter() {
                    names.extend(term_reaches(member.type_.body()));
                    names.extend(term_reaches(member.body.body()));
                }
                names
            }
            Item::Nominal(nominals) => nominals
                .entries()
                .iter()
                .flat_map(|(_, entry)| entry.reaches())
                .collect(),
        }
    }
}

impl InductDecl {
    /// The top-level names this declaration reaches: through its arity, its result sort and its constructors' signatures.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = arity_reaches(self.arity());
        names.extend(term_reaches(self.result_sort()));
        for (_, constructor) in self.constructors() {
            names.extend(payload_reaches(constructor.payload().body()));
        }
        names
    }

    /// The domains its former ranges over: its parameters' types, then its indices'. What the family is indexed by, as against what its constructors hold.
    pub fn former_domains(&self) -> Vec<&Term> {
        let (params, indices) = entries(self.arity());
        params.into_iter().chain(entries(indices).0).collect()
    }
}

impl StructDecl {
    /// The top-level names this declaration reaches: through its arity, fields included, and its result sort.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = arity_reaches(&self.arity);
        names.extend(term_reaches(&self.result_sort));
        names
    }

    /// The domains its former ranges over: its parameters' types. What the record is indexed by, as against what its fields hold.
    pub fn former_domains(&self) -> Vec<&Term> {
        entries(&self.arity).0
    }
}

/// The names `term` reaches: the declarations it references and its construction heads.
fn term_reaches(term: &Term) -> BTreeSet<Global> {
    let mut names = term
        .mentions()
        .declarations()
        .copied()
        .collect::<BTreeSet<_>>();
    names.extend(construction_heads(term));

    names
}

/// The nominal heads `term`'s constructions and type-former normal forms name.
///
/// On the shared walk driver, deduplicated on node identity, for the reason every walk over shared structure is: a string literal's scan chain is linear in nodes and quadratic in paths, and a walk that revisits shared nodes pays the square.
fn construction_heads(term: &Term) -> BTreeSet<Global> {
    let mut state: (HashSet<Term>, BTreeSet<Global>) = (HashSet::new(), BTreeSet::new());
    term.walk(
        &mut state,
        |state, term| {
            if !state.0.insert(term.clone()) {
                return Enter::Skip(());
            }
            match &**term {
                Subterm::InductType(node) => {
                    state.1.insert(node.name);
                }
                Subterm::Variant(node) => {
                    state.1.insert(node.name);
                }
                Subterm::StructType(node) => {
                    state.1.insert(node.name);
                }
                Subterm::Struct(node) => {
                    state.1.insert(node.name);
                }
                _ => {}
            }
            Enter::Descend
        },
        |_, _, _| (),
    );

    state.1
}

/// A telescope's entry types in order, and what it ends in.
fn entries<B: Bound>(telescope: &Telescope<B>) -> (Vec<&Term>, &B) {
    let mut types = Vec::new();
    let mut rest = telescope;
    loop {
        match rest {
            Telescope::Cons(_, type_, scope, ..) => {
                types.push(type_);
                rest = scope.body();
            }
            Telescope::Done(done) => return (types, done),
        }
    }
}

/// The names a field telescope reaches.
fn fields_reaches(fields: &Telescope<()>) -> BTreeSet<Global> {
    entries(fields)
        .0
        .into_iter()
        .flat_map(term_reaches)
        .collect()
}

/// The names a declaration's arity reaches: its parameters, then the telescope they end in.
fn arity_reaches(arity: &Telescope<Telescope<()>>) -> BTreeSet<Global> {
    let (params, fields) = entries(arity);
    params
        .into_iter()
        .flat_map(term_reaches)
        .chain(fields_reaches(fields))
        .collect()
}

/// The names a constructor's signature reaches: its payloads, then the index targets they end in.
fn payload_reaches(signature: &Telescope<Vec<Term>>) -> BTreeSet<Global> {
    let (payloads, targets) = entries(signature);
    payloads
        .into_iter()
        .chain(targets.iter())
        .flat_map(term_reaches)
        .collect()
}

/// What sits at a module position, read as the terms it is made of: a term is itself, a list each of its own, and a telescope each entry's type and then what it ends in.
trait Terms {
    fn terms(&self, visit: &mut dyn FnMut(&Term));
}

impl Terms for Term {
    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        visit(self);
    }
}

impl Terms for Vec<Term> {
    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        self.iter().for_each(visit);
    }
}

impl Terms for () {
    fn terms(&self, _: &mut dyn FnMut(&Term)) {}
}

impl<A: Arity, B: Bound + Terms> Terms for Scope<A, B> {
    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        self.body().terms(visit);
    }
}

impl<B: Bound + Terms> Terms for Telescope<B> {
    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        let (types, done) = entries(self);
        types.into_iter().for_each(&mut *visit);
        done.terms(visit);
    }
}

/// One question a walk asks of whatever sits at a module position.
///
/// A [`Bound`] behind a trait object rather than a generic parameter, so [`item_positions`] can offer one list of positions to more than one collector. Three reads: the two identities findable by looking at a term, which are the local binder indices it mentions and whether a metavariable node survives in it, and the terms themselves, for a read that asks where each was written.
trait Carried {
    fn mentions(&self) -> Mentions;
    fn has_metavar(&self) -> bool;
    fn reach(&self) -> usize;
    fn terms(&self, visit: &mut dyn FnMut(&Term));
}

impl<B: Bound + Terms> Carried for B {
    fn mentions(&self) -> Mentions {
        Bound::mentions(self)
    }

    fn has_metavar(&self) -> bool {
        Bound::has_metavar(self)
    }

    fn reach(&self) -> usize {
        Bound::reach(self)
    }

    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        Terms::terms(self, visit);
    }
}

/// Every position of `item` that can hold a bound value, offered to `visit` with the top-level name owning it, skipping whatever `in_scope` already answers for: each definition's type, body and the sort it states its value at, a recursive group's opened against its members' names, then what the entry of each former it declares carries.
///
/// One list, read by every walk that asks what a module or one item of it carries — the certifier's free-local refusal below it, the storage refusal beside that, and the read of an item's terms as they were written ([`Item::each_term`]). A second copy is how a position quietly stops being covered, which is the failure the enumeration exists to prevent, so a new question about an item's contents is a new `visit` and never a new walk.
fn item_positions(
    item: &Item,
    in_scope: &impl Fn(&Global) -> bool,
    visit: &mut impl FnMut(Option<&Global>, &dyn Carried),
) {
    let names = item.declared_names();
    if names.is_empty() || !names.iter().all(in_scope) {
        for definition in item.definitions() {
            visit(Some(&definition.name), &definition.type_);
            visit(Some(&definition.name), &definition.body);
            if let Some(sort) = &definition.value_sort {
                visit(Some(&definition.name), sort);
            }
        }
    }

    for (name, nominal) in item
        .nominals()
        .into_iter()
        .filter(|(name, _)| !in_scope(name))
    {
        if let Some(declaration) = nominal.inductive() {
            visit(Some(name), declaration.arity());
            visit(Some(name), declaration.result_sort());
            for (_, constructor) in declaration.constructors() {
                visit(Some(name), constructor.payload());
            }
        }
        if let Some(record) = nominal.record() {
            visit(Some(name), &record.arity);
            visit(Some(name), &record.result_sort);
        }
    }
}

/// Every position that can hold a bound value in the items of `module` it holds as other than a blob a cabinet holds, item by item — see [`item_positions`]: an item read out of the blob it was stored as was walked where it was filed, by the compiler that files this one, since a cabinet's address names its compiler.
fn made_positions(definitions: &Definitions, mut visit: impl FnMut(Option<&Global>, &dyn Carried)) {
    let items = &definitions.items;
    for place in items.places().filter(|place| items.part(*place).is_none()) {
        item_positions(&items[place], &|_| false, &mut visit);
    }
}

/// What a stored unit may not carry at a position: an identity meaningful only in the compilation that assigned it, and, carrying none, a bound variable no scope in the unit binds, which is no identity at all and is read at whatever depth a later splice gives it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Positional {
    /// A free local binder. Its index came from one compilation's binder counter, and every walk mints from a counter of its own that starts at zero — so a local surviving into stored output is an index two compilations can both hand out.
    FreeLocal { owner: Option<Global>, index: u64 },
    /// A term metavariable. Zonking is contracted to substitute every solution and to refuse an unsolved hole, so one reaching here is that contract broken rather than a hole still to be solved.
    Metavar { owner: Option<Global> },
    /// A bound variable no scope in the position binds, `reach` binders out. Not an identity a counter assigned but an index with no binder, which [`Mentions`] answers nothing for and the free local above therefore misses: stored, it is read at whatever depth the term it is spliced under gives it, which is a different term with an error nowhere.
    LooseIndex { owner: Option<Global>, reach: usize },
    /// A witness this module declares, scoped to a mount it does not own. Its name holds the module that declares it, so one carrying somebody else's is a name two units can both declare — the aliasing that would silently rebind a coherence-table entry.
    UnscopedWitness { witness: Global },
    /// A witness this module declares under its place among its module's witnesses ([`WitnessId::Unspelled`]): its signature spelled no key, which registration refuses, so one reaching a cabinet is a name the same text written in another order gives to another declaration.
    PlacedWitness { witness: Global },
}

impl fmt::Display for Positional {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (owner, carried) = match self {
            Positional::FreeLocal { owner, index } => (owner, format!("free local binder {index}")),
            Positional::Metavar { owner } => (owner, "an unsolved metavariable".to_string()),
            Positional::LooseIndex { owner, reach } => (
                owner,
                format!("a bound variable no scope binds, reaching {reach} binders out"),
            ),
            Positional::UnscopedWitness { witness } => {
                return write!(
                    formatter,
                    "{} is declared here and scoped to a mount this module does not own",
                    witness.faithful()
                );
            }
            Positional::PlacedWitness { witness } => {
                return write!(
                    formatter,
                    "{} is declared here under its place in its module, its signature spelling no key",
                    witness.faithful()
                );
            }
        };

        match owner {
            Some(name) => write!(formatter, "{} carries {carried}", name.faithful()),
            None => write!(formatter, "the entrypoint carries {carried}"),
        }
    }
}

/// Refuse `module` if it carries an identity meaningful only in the compilation that produced it.
///
/// **A unit may be stored only if it carries no positional identity, and only if every position it carries is closed.** Storing an identity is how rustc came to need `cnum_map` — an index another compilation reads and then has to remap — and it is the property deciding whether a stored unit is portable at all. A bound variable no scope binds is the other half of the same question asked of a position, and it has no remapping even in principle: it denotes the binder at its depth wherever it is read, so a unit carrying one stores a term that is not the term it was written from.
///
/// Five of the classes are refused here. The remaining one, an unsolved universe metavariable, is refused at the same seam by `curios-elab`'s `validate_universes`, which names it in as many words; restating it would be a second implementation of one predicate rather than a second opinion about it, which is the standing [`UniverseContext::is_closed`] holds for the same reason.
///
/// The two witness classes are asked differently from the other two, and deliberately. Those are read off the module's *terms*, because a metavariable or a free local anywhere in one is disqualifying. A witness reference is not: a stored unit legitimately mentions witnesses its predecessors declared, scoped to *their* mounts. What must hold is that every witness this module **declares** is scoped to a mount it owns and named by what it witnesses, which are questions about `Definitions::witnesses` rather than about any position — so they are asked over the declarations and not through the walk.
///
/// The terms are those of the items the module holds as other than a blob a cabinet holds, made by the compilation filing it: an item read out of the blob it was stored as was refused or admitted by the filing that wrote it, the prelude's by its build, so a filing walks what its edit made and not the unit (`made_positions`).
///
/// It refuses what [`free_locals_of`] and [`open_positions_of`] report, over the same positions, at the other seam: the certifier refuses one where it walks a module, and this refuses one where a module is stored. Neither has a safe direction to degrade in — a local reaching either aliases silently with a binder some later walk mints, which admits rather than crashes. It still *describes* rather than judges by this module's rule — whether a node is a metavariable, and whether a variable is local, are properties of the representation, taking no reduction, no conversion and no `Driver`.
pub fn validate_stored_identities(
    definitions: &Definitions,
    mounts: &[Mount],
) -> Result<(), Positional> {
    let mut found: Option<Positional> = None;

    made_positions(definitions, |owner, carried| {
        if found.is_some() {
            return;
        }

        if carried.has_metavar() {
            found = Some(Positional::Metavar {
                owner: owner.cloned(),
            });
            return;
        }

        if let Some(index) = carried.mentions().locals().next().map(Free::index) {
            found = Some(Positional::FreeLocal {
                owner: owner.cloned(),
                index,
            });
            return;
        }

        let reach = carried.reach();
        if reach != 0 {
            found = Some(Positional::LooseIndex {
                owner: owner.cloned(),
                reach,
            });
        }
    });

    let witnesses = definitions.witnesses();
    if let Some(witness) = witnesses.iter().find(|witness| match witness {
        // Containment rather than equality: the identity names the *module* that declares it, which lies within one of this unit's mounts rather than being one.
        Global::Witness(id) => Mount::owning(mounts, id.module()).is_none(),
        Global::Authored(_) => false,
    }) {
        return Err(Positional::UnscopedWitness { witness: *witness });
    }
    if let Some(witness) = witnesses
        .iter()
        .find(|witness| matches!(witness, Global::Witness(WitnessId::Unspelled { .. })))
    {
        return Err(Positional::PlacedWitness { witness: *witness });
    }

    found.map_or(Ok(()), Err)
}

/// Every free local the terms of `items` mention, each with the top-level name owning the position it was found in.
///
/// **What a checker refuses before it mints a binder of its own.** Both checkers mint from a counter no predecessor raised — the certifier's from zero, the elaborator's from its own unit's lowering count — because no term a walk is handed from elsewhere carries a local ([`validate_stored_identities`]). A free local reaching a walk is therefore one its counter could hand out again, and the binder it mints while comparing under a telescope or eta-contracting would silently identify two terms that differ. The judgment could not see it: by the time it looks the local up, the local is bound. So the refusal is taken at the boundary, over every position a term can sit in, rather than where a judgment meets it.
///
/// It lives here by this module's own rule, the one stated at the top: it *describes* rather than judges. Whether a variable is local asks nothing of a certifier — no reduction, no conversion, no `Driver` — so it is a property of the data, and a second implementation would be a second run of the same function rather than a second opinion. That is the standing `UniverseContext::is_closed` has for the same reason.
///
/// A walk hands in the items it judges the terms of: over a baseline, the ones whose terms are not the baseline's, the walk that filed the baseline having refused a local of its own in each of the others.
pub fn free_locals_of<'a>(
    items: impl IntoIterator<Item = &'a Item>,
) -> BTreeSet<(Option<Global>, Free)> {
    let mut found = BTreeSet::new();
    let mut visit = |owner: Option<&Global>, carried: &dyn Carried| {
        found.extend(
            carried
                .mentions()
                .locals()
                .map(|local| (owner.copied(), *local)),
        );
    };
    for item in items {
        item_positions(item, &|_| false, &mut visit);
    }

    found
}

/// Every position of `items` whose term is not closed, each with how far it reaches past the binders above it — which are none, a position being where a term starts — and the top-level name owning it.
///
/// **A bound variable no scope binds is refused where the free locals are, and for a reason of its own.** It is not an identity a counter assigned: [`Mentions`] is built from `var.as_free()` and answers nothing for a bound occurrence, so [`free_locals_of`]'s question misses it entirely. What meets one instead is a rule, where one reads the position at all: `infer`'s refuses it by its depth, a reader past that rule asks it for the identity it has not got ([`Var::as_free`](crate::Var::as_free)) and reads it as no variable, and a position no rule types — a stated value sort — is met by nothing. Nor could a judgment answer where the boundary does not: an index with no binder denotes whatever the depth it is read at gives it, so the term a rule sees is not the term the module carries. The refusal is therefore taken here, over the same positions, where it costs each position's memoized [`Bound::reach`].
///
/// A scope body legitimately carries the indices its own binders bind, and every term of every module holds some, so what is read is a *position* — a definition's type, body or stated sort, a registry entry's telescopes, a group's members opened over their own names — where no binder stands above the term and `reach` is therefore zero for everything well formed.
pub fn open_positions_of<'a>(
    items: impl IntoIterator<Item = &'a Item>,
) -> BTreeSet<(Option<Global>, usize)> {
    let mut found = BTreeSet::new();
    let mut visit = |owner: Option<&Global>, carried: &dyn Carried| {
        let reach = carried.reach();
        if reach != 0 {
            found.insert((owner.copied(), reach));
        }
    };
    for item in items {
        item_positions(item, &|_| false, &mut visit);
    }

    found
}

/// The plicities a type former's type, `type_`, states, parameters then indices: `None` for a nullary declaration, which has no `FuncType` wrapper and contributes nothing. A former's result is a sort, so the walk ends at the first node that is not a function type.
fn former_marks(type_: &Term) -> Option<Vec<Plicity>> {
    let mut collected = Vec::new();
    let mut type_ = type_;
    while let Subterm::FuncType(FuncType { telescope }) = &**type_ {
        collected.extend(telescope.marks());
        type_ = telescope.terminal();
    }

    (!collected.is_empty()).then_some(collected)
}

/// What the declaration `nominal`, the entry of `name`, declares of structures and constructors, put in `members`: a structure's fields under their marks and labels, a concept's record among them, and the marks each constructor is called by.
fn put_members(members: &mut Members, name: &Global, nominal: &Nominal) {
    if let Some(record) = nominal.record() {
        let fields = record.fields();
        let written = fields
            .marks()
            .into_iter()
            .zip(fields.labels())
            .map(|(mark, label)| (mark, (!label.is_empty()).then(|| label.to_string())))
            .collect();
        members.fields.insert(*name, Rc::new(written));
    }
    if let Some(family) = nominal.inductive() {
        let constructors = family
            .constructors()
            .iter()
            .map(|(tag, case)| {
                let marks = family.parameter_marks().into_iter().chain(case.plicities());
                (tag.clone(), marks.collect())
            })
            .collect();
        members.payloads.insert(*name, Rc::new(constructors));
    }
}

#[cfg(test)]
mod tests;
