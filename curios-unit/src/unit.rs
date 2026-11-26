//! What one unit provides to its successors: what each stage produced for it, and the certifier's record of it.

use {
    crate::{
        Indexed, Kind, anchors_indexed, apart_parts, asked_indexed, digests, found, halved, halves,
        imports_indexed, listed, lowering_indexed, reads_indexed, traces_indexed,
    },
    curios_abi::ForeignStore,
    curios_archive::{Bytes, joined, parted},
    curios_cert::{Accepted, Filed},
    curios_common::{Blob, Digest, Mount, Recorded},
    curios_core::{Anchors, Certification, Definitions, ItemRun, Lexicon, Lint, Places},
    curios_elab::{Candidating, Classifications, Observed, Readers, Traces},
    curios_erase::ErasedUnit,
    curios_module::{PackageRoot, PreparedText},
    std::{borrow::Cow, cell::OnceCell, collections::BTreeSet, rc::Rc},
};

/// What a unit was written in: a program's sources, whose lowering it holds, or Curios/Core, read straight to the module a lowering and an elaboration would have made, which leaves it nothing of a lowering's but the prefixes it claims and the rows it declares.
///
/// **A lowering is a program's.** What reads one — a later lowering resolving a name, a recompile keeping what its digests say stands, a lint's findings, a report's anchors, the documentation, the tests a program schedules — reads it of a unit written as a program, and of a unit read from Curios/Core reads nothing, so a later program names nothing such a unit declares.
#[derive(Clone)]
#[curios_archive::archived]
enum Written {
    /// A program's lowering, and the lints the lint owner decided of the unit where it was made: stored with it, so a unit restored or compiled over reports what it was compiled to report, and a unit read from Curios/Core holds none.
    Program {
        text: Rc<PreparedText>,
        lints: Rc<Vec<Lint>>,
    },
    /// What a unit read from Curios/Core claims and declares, and where its text writes each declaration: anchored as a lowered unit is, so a report about it is placed in its text. Never stored: a `.crsc` is loose and filed nowhere, and a soundness board's history names what its units declare rather than placing it, so a restored unit holds none.
    CoreText {
        mounts: Vec<Mount>,
        foreigns: ForeignStore,
        #[archived_with(curios_archive::Skip)]
        anchors: Rc<Anchors>,
    },
}

/// What each stage produced for a unit: everything a [`Unit`] holds beside the certifier's record.
///
/// Composed of one opaque artifact per stage rather than flattened into their fields. `curios-module`'s resolution tables and `curios-erase`'s erased forms are that crate's business, and widening them to `pub` so this struct could hold them directly would export a resolver's internals for no consumer. What this type adds is the pairing: the artifacts describe *one* unit, and nothing else says so.
///
/// **Each artifact is held by pointer, so a unit is handed over and never copied.** A fold hands a unit to its successors, a session keeps one as the next question's baseline, and a cabinet offers one, and each of those is a clone of the handles and of nothing they hold.
#[derive(Clone)]
#[curios_archive::archived]
struct Produced {
    written: Written,
    /// Elaborated and zonked, as the certifier handed it back: elaboration's items, with every proof mark and value sort the certifier stated of them. This is what a successor's elaboration and the certifier's walk over it read where it stands.
    ///
    /// **Stored without its items, which are read by item.** A compilation after the unit reads the declarations it names, so the unit stores the module's outline — each item's names and what each is declared as — and each item as a blob of the cabinet, a member of a run consed with the items filed beside it, and a unit restored holds a module whose items are each read when something first asks ([`curios_core::WithoutItems`], [`Definitions::with_items`]).
    #[archived_with(curios_core::WithoutItems)]
    core: Rc<Definitions>,
    /// Where `core` holds each name it defines, made the first time the unit is read by name and kept for as long as it is held: no compilation keys the unit again. Never stored, being read off `core`.
    #[archived_with(curios_archive::Skip)]
    places: Rc<OnceCell<Places>>,
    /// What a report spells this unit's names by, made the first time a report is made over the unit and kept beside its places: no report builds a table over the unit again. Never stored, being read off `core`.
    #[archived_with(curios_archive::Skip)]
    lexicon: Rc<OnceCell<Lexicon>>,
    /// What each of the unit's items erased to, in identities of its own and naming what another item binds by its name: this unit's alone, saying nothing of the units before it. Why is the crate `README.md`'s "An erased form is its item's".
    erased: Rc<ErasedUnit>,
    /// What each of the unit's declarations asked of the declarations around it as it elaborated: what a later compile of the same sources keeps a declaration's elaboration by.
    ///
    /// **Of the unit's baseline half, with the text's** ([`PreparedText::baseline_digests`]): nothing compiled after the unit reads it, so it is stored beside what the unit is for its successors, each declaration's trace a blob, and read when a compile over the unit first asks.
    #[archived_with(curios_archive::Skip)]
    traces: Rc<Recorded<Traces>>,
    /// What elaboration concluded of each of the unit's definitions besides their terms ([`Classifications`]): all that the elaboration of a unit compiled after this one reads of it. Gathered off the traces where the unit is made and stored with the unit, of its successors' half, so a compile after the unit restores none of its declarations' records.
    classifications: Rc<Classifications>,
    /// Who asked what, of the unit's traces read backwards: what a recompile over the unit finds what an edit could reach by. Made the first time a recompile over the unit asks, or handed over by the recompile that made the unit, and kept beside its places; never stored, being read off the traces.
    #[archived_with(curios_archive::Skip)]
    readers: Rc<OnceCell<Readers>>,
    /// Whether the unit is the compiler's own: restored out of bytes that are a constant of the executable, so one thing under one compiler. Never stored, being where the unit came from.
    ///
    /// **Nothing records a read of a fixed unit.** A baseline is offered by the compiler that made it — an object's header names the compiler that filed it, and a session lives in one process — so a fixed unit is the one every baseline was compiled after, and what a unit read of it is never asked again (`curios_elab::Established::fixed`).
    #[archived_with(curios_archive::Skip)]
    fixed: bool,
}

impl Produced {
    fn new(written: Written, core: Definitions, erased: ErasedUnit, traces: Traces) -> Self {
        Self {
            written,
            core: Rc::new(core),
            places: Rc::default(),
            lexicon: Rc::default(),
            erased: Rc::new(erased),
            classifications: Rc::new(traces.classifications()),
            traces: Rc::new(Recorded::held(traces)),
            readers: Rc::default(),
            fixed: false,
        }
    }

    /// This unit with what its declarations are written in read for `packages` ([`PreparedText::read_for`]), and every other artifact the handle it was.
    fn read_for(self, packages: Vec<PackageRoot>) -> Self {
        let written = match &self.written {
            Written::Program { text, lints } => Written::Program {
                text: Rc::new((**text).clone().read_for(packages)),
                lints: Rc::clone(lints),
            },
            Written::CoreText { .. } => return self,
        };

        Self { written, ..self }
    }

    /// See [`Unit::mounts`].
    pub fn mounts(&self) -> &[Mount] {
        match &self.written {
            Written::Program { text, .. } => text.mounts(),
            Written::CoreText { mounts, .. } => mounts,
        }
    }

    /// See [`Unit::text`].
    pub fn text(&self) -> Option<&PreparedText> {
        match &self.written {
            Written::Program { text, .. } => Some(text),
            Written::CoreText { .. } => None,
        }
    }

    /// See [`Unit::anchors`].
    pub fn anchors(&self) -> &Anchors {
        match &self.written {
            Written::Program { text, .. } => text.anchors(),
            Written::CoreText { anchors, .. } => anchors,
        }
    }

    /// See [`Unit::lints`].
    pub fn lints(&self) -> &[Lint] {
        match &self.written {
            Written::Program { lints, .. } => lints,
            Written::CoreText { .. } => &[],
        }
    }

    /// See [`Unit::core`].
    pub fn core(&self) -> &Definitions {
        &self.core
    }

    /// See [`Unit::places`].
    pub fn places(&self) -> &Places {
        self.places.get_or_init(|| Places::of(&self.core))
    }

    /// See [`Unit::lexicon`].
    pub fn lexicon(&self) -> &Lexicon {
        self.lexicon.get_or_init(|| self.core.lexicon())
    }

    /// See [`Unit::erased`].
    pub fn erased(&self) -> &ErasedUnit {
        &self.erased
    }

    /// See [`Unit::foreigns`].
    pub fn foreigns(&self) -> &ForeignStore {
        match &self.written {
            Written::Program { text, .. } => text.foreigns(),
            Written::CoreText { foreigns, .. } => foreigns,
        }
    }

    /// See [`Unit::traces`].
    pub fn traces(&self) -> &Traces {
        self.traces
            .get()
            .unwrap_or_else(|error| panic!("a unit's traces failed to restore: {error}"))
    }

    /// See [`Unit::classifications`].
    pub fn classifications(&self) -> &Classifications {
        &self.classifications
    }

    /// See [`Unit::readers`].
    pub fn readers(&self) -> &Readers {
        self.readers.get_or_init(|| Readers::of(self.traces()))
    }

    /// This unit as it is stored, `unit` being its own archive and `reads`, of a certified unit, what judging each definition read, its own part and its entries: its index, the unit and then what it stores apart from itself ([`halved`]), and the blobs the index names. What it stores apart is each part behind its length ([`joined`]) and read by whoever asks for it: the digests of its lowered items, its erased forms, its elaborated items, what each declaration asked as it elaborated and what erasing each item read ([`digests`]), then where its declarations lie, its import scopes and its baseline half, a part for each stage that keeps a record, the text's, the elaborator's and erasure's, and `reads` behind them.
    ///
    /// **With `candidates`, what a cabinet files: each stage's records completed** ([`Candidating::complete`], [`ErasedUnit::complete`]), where a filing has not completed them, with what each declaration was made from, read and made, and named by those, so a cabinet finds them by what each was made from.
    fn indexed(
        &self,
        unit: &[u8],
        (reads, certification): (Option<(&[u8], Kind)>, &Certification),
        candidates: bool,
    ) -> Result<Indexed, String> {
        let items = match &self.written {
            Written::Program { text, .. } => text.items_blobs()?,
            Written::CoreText { .. } => Vec::new(),
        };
        let forms = self.erased.items_blobs()?;
        let named = self.elaborated()?;
        // A unit whose records are as it restored them files them as it restored them, and one read from Curios/Core was elaborated from nothing a candidate could be found by.
        if let (true, None, Written::Program { text, .. }) =
            (candidates, self.traces.as_stored(), &self.written)
        {
            self.complete_elaborations(text, &items, &named)?;
        }
        if candidates && self.erased.recorded().as_stored().is_none() {
            self.erased.complete(
                &self.core,
                self.places(),
                certification,
                &named.iter().copied().collect(),
                &self
                    .core
                    .items
                    .places()
                    .zip(forms.iter().map(|blob| blob.digest))
                    .collect(),
            );
        }
        let elaborated = Kind::Items {
            core: Rc::clone(&self.core),
            named,
        };
        let (traces_table, traces) = traces_indexed(&self.traces)?;
        let (asked_table, asked) = asked_indexed(self.erased.recorded())?;
        let ((text, lowering), (imports, scopes), (anchors, places)) = match &self.written {
            Written::Program { text, .. } => (
                lowering_indexed(text)?,
                imports_indexed(text)?,
                anchors_indexed(text)?,
            ),
            // The parts a lowering stores, each empty: the parts a unit stores apart from itself are the same whatever it was written in.
            Written::CoreText { .. } => {
                let empty = || (Cow::Borrowed(&[][..]), Kind::Stored(Vec::new()));
                (empty(), empty(), empty())
            }
        };
        let mut kinds = vec![elaborated, traces, asked, lowering, scopes, places];

        let mut parts = vec![
            digests(items.iter().map(|blob| &blob.digest)),
            digests(forms.iter().map(|blob| &blob.digest)),
        ];
        parts.extend(kinds.iter().map(|kind| digests(kind.digests())));
        let apart = joined(
            &parts
                .iter()
                .map(Vec::as_slice)
                .chain([
                    &anchors[..],
                    &imports[..],
                    &text[..],
                    &traces_table[..],
                    &asked_table[..],
                ])
                .collect::<Vec<_>>(),
        );
        let apart = match reads {
            Some((table, reads)) => {
                let named = digests(reads.digests());
                kinds.push(reads);

                joined(&[&apart[..], &named[..], table])
            }
            None => apart,
        };

        Ok(Indexed::new(
            halved(unit, &apart),
            items.into_iter().chain(forms).collect(),
            kinds,
        ))
    }

    /// Complete the record of each declaration's elaboration a filing has not completed ([`Candidating::complete`]): `items` being the blobs the unit's lowered items are stored as, and `named` the digest of each elaborated item ([`Produced::elaborated`]).
    fn complete_elaborations(
        &self,
        text: &PreparedText,
        items: &[Blob],
        named: &[(usize, Digest)],
    ) -> Result<(), String> {
        let lowered = text.core();
        let candidating = Candidating::new(
            lowered,
            text.declarations(),
            lowered
                .items
                .places()
                .zip(items.iter().map(|blob| blob.digest)),
            &self.core,
            self.places(),
            named.iter().copied(),
            self.traces.get()?,
        );

        candidating.complete()
    }

    /// The digest of the elaborated item at each place: the one it was stored under where the module holds it as it was read, and what it is named by alone otherwise ([`ItemRun::identity`]), which a whole compile and a recompile name it by alike.
    fn elaborated(&self) -> Result<Vec<(usize, Digest)>, String> {
        let items = &self.core.items;
        items
            .places()
            .map(|place| {
                let digest = match items.part(place) {
                    Some(part) => part.digest,
                    None => ItemRun::identity(&items[place])?,
                };

                Ok((place, digest))
            })
            .collect()
    }

    /// This unit with `bytes` as what it stores apart from itself ([`Unit::stored`]), each part a run of the buffer they are in and read when something first asks for it, and each blob they name found by `find` now — see [`PreparedText::with_items`] and [`PreparedText::with_baseline`]. `Err` is bytes that are no such thing, or name a blob `find` holds none for.
    pub fn with_apart(
        self,
        bytes: impl Into<Bytes>,
        find: &dyn Fn(&Digest) -> Option<Blob>,
    ) -> Result<Self, String> {
        let bytes = bytes.into();
        let fixed = matches!(bytes, Bytes::Constant(_));
        let [
            items,
            forms,
            elaborated,
            traces,
            asked,
            lowering,
            scopes,
            places,
            anchors,
            imports,
            text,
            traces_table,
            asked_table,
        ] = apart_parts(&bytes).ok_or("not what a unit stores apart from itself")?;

        Ok(Self {
            fixed,
            written: match self.written {
                Written::Program {
                    text: written,
                    lints,
                } => Written::Program {
                    text: Rc::new(
                        Rc::unwrap_or_clone(written)
                            .with_baseline(text, found(&lowering, find)?)
                            .with_reported(
                                (imports, found(&scopes, find)?),
                                (anchors, found(&places, find)?),
                            )
                            .with_items(found(&items, find)?)?,
                    ),
                    lints,
                },
                read @ Written::CoreText { .. } => read,
            },
            erased: Rc::new(
                Rc::unwrap_or_clone(self.erased)
                    .with_baseline(asked_table, found(&asked, find)?)
                    .with_items(found(&forms, find)?)?,
            ),
            traces: Rc::new(Recorded::stored(traces_table, found(&traces, find)?)),
            classifications: self.classifications,
            readers: Rc::default(),
            core: Rc::new(Rc::unwrap_or_clone(self.core).with_items(found(&elaborated, find)?)?),
            places: Rc::default(),
            lexicon: Rc::default(),
        })
    }
}

/// One compiled and certified unit: everything a later unit needs in order to be compiled against it.
///
/// What a compilation produced, and the record the certifier's walk over it left — so a predecessor always carries the certifier's own verdicts on its definitions, and a later walk never has to ask whether it does. Made only by [`Unit::new`] and [`Unit::read`], which take the module the walk accepted with its record as one value, or restored from the bytes a cabinet or the prelude's image holds, and cloned as a handle is: each stage's artifact is held by pointer.
///
/// The serialized form of this is what a cabinet files under an address covering its mounts, its predecessors and the compiler that judged them, beside a [`Record`](crate::Record) of the files it was compiled from.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Unit {
    unit: Produced,
    /// What the certifier concluded about the unit's definitions: each one's totality, closed over what it mentions, and what judging it read. A later walk reads this for the unit's verdicts rather than what elaboration concluded, which the unit's traces hold for the elaborator alone.
    certification: Rc<Certification>,
}

impl Unit {
    /// Assemble a unit from what each stage produced for it, its module the one the certifier's walk accepted beside the record that walk filed of it — with [`Unit::read`], the only way a unit is made, so none is without one and none holds a record of another module. `observed` is the binder lints the elaboration that ran holds: its declarations' records' and the program's final term's.
    pub fn new(
        text: PreparedText,
        accepted: Accepted<Definitions>,
        erased: ErasedUnit,
        traces: Traces,
        observed: &Observed,
    ) -> Self {
        let (core, certification) = accepted.into_parts();
        // Gathered once, here, where every unit written as a program is made, from what each stage decided: the lowering's lints, and the binder lints elaboration filed with each declaration's record — elaborated now, kept from a baseline or taken from a cabinet — and decided of the program's final term, placed through the lowering's anchors.
        let lints = Lint::ordered(
            text.lints()
                .iter()
                .chain(&observed.lints)
                .cloned()
                .collect(),
            text.anchors(),
        );
        Self {
            unit: Produced::new(
                Written::Program {
                    text: Rc::new(text),
                    lints: Rc::new(lints),
                },
                core,
                erased,
                traces,
            ),
            certification: Rc::new(certification),
        }
    }

    /// Assemble a unit read from Curios/Core: claiming `mounts`, declaring the rows `foreigns` holds, its module the one the certifier's walk accepted beside the record that walk filed of it, erased to `erased`, and its text writing each declaration where `anchors` say. Nothing elaborated it, so it filed no traces, and it holds no lowering ([`Unit::text`]).
    pub fn read(
        mounts: Vec<Mount>,
        foreigns: ForeignStore,
        accepted: Accepted<Definitions>,
        erased: ErasedUnit,
        anchors: Anchors,
    ) -> Self {
        let (core, certification) = accepted.into_parts();
        Self {
            unit: Produced::new(
                Written::CoreText {
                    mounts,
                    foreigns,
                    anchors: Rc::new(anchors),
                },
                core,
                erased,
                Traces::default(),
            ),
            certification: Rc::new(certification),
        }
    }

    /// This unit, holding `readers` as who asked what of its traces: what the recompile that made it carries over from its baseline, in place of reading its traces backwards again.
    pub fn reading(mut self, readers: Readers) -> Self {
        self.unit.readers = Rc::new(OnceCell::from(readers));
        self
    }

    /// This unit, holding `places` as where its module holds each name, where they were handed on: what the recompile that made it carries over from its baseline, with the names of what it changed placed again, in place of placing every name again.
    pub fn placing(mut self, places: Option<Places>) -> Self {
        if let Some(places) = places {
            self.unit.places = Rc::new(OnceCell::from(places));
        }
        self
    }

    /// What the certifier concluded about this unit's definitions. See the field.
    pub fn certification(&self) -> &Certification {
        &self.certification
    }

    /// This unit as its walk filed it, as the certifier reads a unit it does not judge again: its module, where the module holds each name, and its record.
    pub fn filed(&self) -> Filed<'_> {
        Filed {
            definitions: self.core(),
            places: self.places(),
            certification: self.certification(),
        }
    }

    /// The prefixes this unit claims, and whether each is a root only the compiler supplies.
    pub fn mounts(&self) -> &[Mount] {
        self.unit.mounts()
    }

    /// This unit's resolution state, as what a later unit's names resolve against: a program's lowering, and nothing for a unit read from Curios/Core ([`Unit::read`]).
    pub fn text(&self) -> Option<&PreparedText> {
        self.unit.text()
    }

    /// Where this unit's text writes each of its declarations: a program's lowering's anchors, or those a unit read from Curios/Core was read with, which a unit restored from a cabinet holds none of ([`Unit::read`]).
    pub fn anchors(&self) -> &Anchors {
        self.unit.anchors()
    }

    /// This unit's lints, decided where it was made: none for a unit read from Curios/Core, which no lowering made.
    pub fn lints(&self) -> &[Lint] {
        self.unit.lints()
    }

    /// This unit's elaborated module as the certifier handed it back — what a successor's elaboration and judgment read rather than re-check.
    pub fn core(&self) -> &Definitions {
        self.unit.core()
    }

    /// Where this unit's elaborated module holds each name it defines. See the field.
    pub fn places(&self) -> &Places {
        self.unit.places()
    }

    /// What a report spells this unit's names by. See the field.
    pub fn lexicon(&self) -> &Lexicon {
        self.unit.lexicon()
    }

    /// This unit's items' erased forms — what a later unit's erasure reads how a name is bound from, what a program links, and what a recompile of this unit keeps for the items it does not redo.
    pub fn erased(&self) -> &ErasedUnit {
        self.unit.erased()
    }

    /// The `foreign` rows this unit declares. Disjoint from every other unit's by mount, because an `ffi` import name is the declaration's fully qualified name — so the fold unions them without a collision to resolve.
    pub fn foreigns(&self) -> &ForeignStore {
        self.unit.foreigns()
    }

    /// What each of this unit's declarations asked as it elaborated. See the field. Read out of the unit's baseline half now, where the unit was restored and no compile over it has asked before.
    pub fn traces(&self) -> &Traces {
        self.unit.traces()
    }

    /// What elaboration concluded of each of this unit's definitions besides their terms: what a unit compiled after this one reads of its elaboration, with no record of it restored. See the field.
    pub fn classifications(&self) -> &Classifications {
        self.unit.classifications()
    }

    /// Who asked what, of this unit's traces. See the field.
    pub fn readers(&self) -> &Readers {
        self.unit.readers()
    }

    /// Whether the unit is the compiler's own: restored out of bytes that are a constant of the executable, so one thing under one compiler.
    pub fn fixed(&self) -> bool {
        self.unit.fixed
    }

    /// Whether this and `other` are one unit in hand: handles of one compilation's result, or of one restoration's. Two units restored apart from the same bytes are two, which whoever asks tells by their bytes.
    pub fn same(&self, other: &Unit) -> bool {
        Rc::ptr_eq(&self.unit.core, &other.unit.core)
    }

    /// This unit as it is stored: its index, itself, its elaborated module without its items, then what it stores apart from itself and what the certifier's record does, what judging each definition read ([`Certification::recorded`]), each behind its length; and the blobs the index names ([`Indexed::blobs`]).
    ///
    /// **A stored unit says what its items mean, not where they were written or how they were come by.** An elaborated item is named by its form consed in a table of its own, one node for each structure as it is spelled, under no position, so an item elaborated now and the same item reused from a baseline are named alike, and one moved by an edit above it is stored as it was; the items a filing writes are consed in one table, so a node two of them spell is stored once.
    ///
    /// Consed here and not where a unit is assembled, and only the items the unit did not restore: a unit that is neither filed nor followed — a keystroke's — is never serialized.
    pub fn stored(&self) -> Result<Indexed, String> {
        let unit = curios_archive::to_bytes(self)?;
        let (table, reads) = reads_indexed(self.certification.recorded())?;

        self.unit
            .indexed(&unit, (Some((&table, reads)), &self.certification), true)
    }

    /// This unit as the fixed prelude images it: [`Unit::stored`]'s form, with no stage's records completed, since no cabinet files the compiler's own units and finds their declarations by what each was made from.
    pub fn imaged(&self) -> Result<Indexed, String> {
        let unit = curios_archive::to_bytes(self)?;
        let (table, reads) = reads_indexed(self.certification.recorded())?;

        self.unit
            .indexed(&unit, (Some((&table, reads)), &self.certification), false)
    }

    /// The unit whose index is `index` ([`Unit::stored`]), each blob it names found by `find`: decoded, what it stores apart from itself kept as the bytes it is, for whoever asks. `Err` is bytes that hold no unit, or a blob `find` holds none for.
    pub fn restored(index: &[u8], find: impl Fn(&Digest) -> Option<Blob>) -> Result<Self, String> {
        let (unit, apart) = halves(index).ok_or("not a unit's index")?;

        curios_archive::from_bytes::<Unit>(unit)?.with_apart(Bytes::from(apart.to_vec()), &find)
    }

    /// Every blob the index `index` names ([`Unit::stored`]), by its digest: the lists [`Unit::restored`] finds blobs by, read with no unit decoded and no blob found. What a cabinet holds a blob to where it is offered by its name alone. `Err` is bytes that hold no index.
    pub fn named(index: &[u8]) -> Result<BTreeSet<Digest>, String> {
        let (_, apart) = halves(index).ok_or("not a unit's index")?;
        let apart = Bytes::from(apart.to_vec());
        let [own, reads, _]: [Bytes; 3] = parted(&apart)
            .and_then(|parts| parts.try_into().ok())
            .ok_or("not what a certified unit stores apart from itself")?;
        let [
            items,
            forms,
            elaborated,
            traces,
            asked,
            lowering,
            scopes,
            places,
            ..,
        ] = apart_parts(&own).ok_or("not what a unit stores apart from itself")?;

        let mut named = BTreeSet::new();
        for list in [
            items, forms, elaborated, traces, asked, lowering, scopes, places, reads,
        ] {
            named.extend(listed(&list)?);
        }

        Ok(named)
    }

    /// This unit, restored for a source whose files are read for `packages`: each source of what its declarations are written in holds the file its name resolves to on this machine, where the package it is named by stands ([`PreparedText::read_for`]). A unit restored for no source holds its sources' names alone.
    pub fn read_for(self, packages: Vec<PackageRoot>) -> Self {
        Self {
            unit: self.unit.read_for(packages),
            ..self
        }
    }

    /// This unit, decoded from its archive alone, with `bytes` as what it and its record store apart from themselves ([`Unit::stored`], [`Unit::imaged`]), each part a run of the buffer and each blob it names found by `find` now. A unit whose `bytes` are a constant of the executable is the compiler's own ([`Unit::fixed`]). `Err` is bytes that are no such thing, or name a blob `find` holds none for.
    pub fn with_apart(
        self,
        bytes: Bytes,
        find: &dyn Fn(&Digest) -> Option<Blob>,
    ) -> Result<Self, String> {
        let [apart, reads, table]: [Bytes; 3] = parted(&bytes)
            .and_then(|parts| parts.try_into().ok())
            .ok_or("not what a certified unit stores apart from itself")?;
        let reads = found(&reads, find)?;

        Ok(Self {
            unit: self.unit.with_apart(apart, find)?,
            certification: Rc::new(
                Rc::unwrap_or_clone(self.certification).with_reads(table, reads),
            ),
        })
    }
}
