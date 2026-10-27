//! What one unit provides to its successors: what each stage produced for it, and the certifier's record of it.

use {
    crate::{
        Indexed, Kind, anchors_indexed, apart_parts, asked_indexed, digests, found, halved, halves,
        imports_indexed, lowering_indexed, reads_indexed, traces_indexed,
    },
    curios_abi::ForeignStore,
    curios_archive::{Bytes, joined, parted},
    curios_core::{Certification, ItemRun, Lexicon, Module, Places},
    curios_elab::{Candidating, Readers, Traces},
    curios_erase::ErasedUnit,
    curios_text::PreparedText,
    curios_utilities::{Digest, Mount, Object, Recorded},
    std::{cell::OnceCell, rc::Rc},
};

/// What each stage produced for a unit: everything a [`Unit`] holds beside the certifier's record.
///
/// Composed of one opaque artifact per stage rather than flattened into their fields. `curios-text`'s resolution tables and `curios-erase`'s erased forms are that crate's business, and widening them to `pub` so this struct could hold them directly would export a resolver's internals for no consumer. What this type adds is the pairing: the artifacts describe *one* unit, and nothing else says so.
///
/// **Each artifact is held by pointer, so a unit is handed over and never copied.** A fold hands a unit to its successors, a session keeps one as the next question's baseline, and a store offers one, and each of those is a clone of the handles and of nothing they hold.
#[derive(Clone)]
#[curios_archive::archived]
struct Produced {
    text: Rc<PreparedText>,
    /// Elaborated and zonked. This is what the kernel judges and what a successor's elaboration reads where it stands.
    ///
    /// **Stored without its items, which are read by item.** A compilation after the unit reads the declarations it names, so the unit stores the module's outline — each item's names and what each is declared as — and each item as an object of the store, a member of a run consed with the items filed beside it, and a unit restored holds a module whose items are each read when something first asks ([`curios_core::WithoutItems`], [`Module::with_items`]).
    #[archived_with(curios_core::WithoutItems)]
    core: Rc<Module>,
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
    /// **Of the unit's baseline half, with the text's** ([`PreparedText::baseline_digests`]): nothing compiled after the unit reads it, so it is stored beside what the unit is for its successors, each declaration's trace an object, and read when a compile over the unit first asks.
    #[archived_with(curios_archive::Skip)]
    traces: Rc<Recorded<Traces>>,
    /// Who asked what, of the unit's traces read backwards: what a recompile over the unit finds what an edit could reach by. Made the first time a recompile over the unit asks, or handed over by the recompile that made the unit, and kept beside its places; never stored, being read off the traces.
    #[archived_with(curios_archive::Skip)]
    readers: Rc<OnceCell<Readers>>,
    /// Whether the unit is the compiler's own: restored out of bytes that are a constant of the executable, so one thing under one compiler. Never stored, being where the unit came from.
    ///
    /// **Nothing records a read of a fixed unit.** A baseline is offered by the compiler that made it — a slot's address names the compiler, and a session lives in one process — so a fixed unit is the one every baseline was compiled after, and what a unit read of it is never asked again (`curios_elab::Established::fixed`).
    #[archived_with(curios_archive::Skip)]
    fixed: bool,
}

impl Produced {
    fn new(text: PreparedText, core: Module, erased: ErasedUnit, traces: Traces) -> Self {
        Self {
            text: Rc::new(text),
            core: Rc::new(core),
            places: Rc::default(),
            lexicon: Rc::default(),
            erased: Rc::new(erased),
            traces: Rc::new(Recorded::held(traces)),
            readers: Rc::default(),
            fixed: false,
        }
    }

    /// See [`Unit::mounts`].
    pub fn mounts(&self) -> &[Mount] {
        &self.core.mounts
    }

    /// See [`Unit::text`].
    pub fn text(&self) -> &PreparedText {
        &self.text
    }

    /// See [`Unit::core`].
    pub fn core(&self) -> &Module {
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
        self.text.foreigns()
    }

    /// See [`Unit::traces`].
    pub fn traces(&self) -> &Traces {
        self.traces
            .get()
            .unwrap_or_else(|error| panic!("a unit's traces failed to restore: {error}"))
    }

    /// See [`Unit::readers`].
    pub fn readers(&self) -> &Readers {
        self.readers.get_or_init(|| Readers::of(self.traces()))
    }

    /// This unit as it is stored, `unit` being its own archive and `reads`, of a certified unit, what judging each definition read, its own part and its entries: its index, the unit and then what it stores apart from itself ([`halved`]), and the objects the index names. What it stores apart is each part behind its length ([`joined`]) and read by whoever asks for it: the digests of its lowered items, its erased forms, its elaborated items, what each declaration asked as it elaborated and what erasing each item read ([`digests`]), then where its declarations lie, its import scopes and its baseline half, a part for each stage that keeps a record, the text's, the elaborator's and erasure's, and `reads` behind them.
    ///
    /// **With `candidates`, what a store files: each stage's records completed** ([`Candidating::complete`], [`ErasedUnit::complete`]), where a filing has not completed them, with what each declaration was made from, read and made, and named by those, so a store finds them by what each was made from.
    fn indexed(
        &self,
        unit: &[u8],
        (reads, certification): (Option<(&[u8], Kind)>, &Certification),
        candidates: bool,
    ) -> Result<Indexed, String> {
        let items = self.text.items_objects()?;
        let forms = self.erased.items_objects()?;
        let named = self.elaborated()?;
        // A unit whose records are as it restored them files them as it restored them.
        if candidates && self.traces.as_stored().is_none() {
            self.complete_elaborations(&items, &named)?;
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
                    .zip(forms.iter().map(|object| object.digest))
                    .collect(),
            );
        }
        let elaborated = Kind::Items {
            core: Rc::clone(&self.core),
            named,
        };
        let (traces_table, traces) = traces_indexed(&self.traces)?;
        let (asked_table, asked) = asked_indexed(self.erased.recorded())?;
        let (text, lowering) = lowering_indexed(&self.text)?;
        let (imports, scopes) = imports_indexed(&self.text)?;
        let (anchors, places) = anchors_indexed(&self.text)?;
        let mut kinds = vec![elaborated, traces, asked, lowering, scopes, places];

        let mut parts = vec![
            digests(items.iter().map(|object| &object.digest)),
            digests(forms.iter().map(|object| &object.digest)),
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

        Ok(Indexed {
            index: halved(unit, &apart),
            alone: items.into_iter().chain(forms).collect(),
            kinds,
        })
    }

    /// Complete the record of each declaration's elaboration a filing has not completed ([`Candidating::complete`]): `items` being the objects the unit's lowered items are stored as, and `named` the digest of each elaborated item ([`Produced::elaborated`]).
    fn complete_elaborations(
        &self,
        items: &[Object],
        named: &[(usize, Digest)],
    ) -> Result<(), String> {
        let lowered = self.text.core();
        let candidating = Candidating::new(
            lowered,
            self.text.declarations(),
            lowered
                .items
                .places()
                .zip(items.iter().map(|object| object.digest)),
            &self.core,
            self.places(),
            named.iter().copied(),
            self.traces.get()?,
        );

        candidating.complete(|names| self.text.credited_in(names))
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

    /// This unit with `bytes` as what it stores apart from itself ([`Unit::stored`]), each part a run of the buffer they are in and read when something first asks for it, and each object they name found by `find` now — see [`PreparedText::with_items`] and [`PreparedText::with_baseline`]. `Err` is bytes that are no such thing, or name an object `find` holds none for.
    pub fn with_apart(
        self,
        bytes: impl Into<Bytes>,
        find: &dyn Fn(&Digest) -> Option<Object>,
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
            text: Rc::new(
                Rc::unwrap_or_clone(self.text)
                    .with_baseline(text, found(&lowering, find)?)
                    .with_reported(
                        (imports, found(&scopes, find)?),
                        (anchors, found(&places, find)?),
                    )
                    .with_items(found(&items, find)?)?,
            ),
            erased: Rc::new(
                Rc::unwrap_or_clone(self.erased)
                    .with_baseline(asked_table, found(&asked, find)?)
                    .with_items(found(&forms, find)?)?,
            ),
            traces: Rc::new(Recorded::stored(traces_table, found(&traces, find)?)),
            readers: Rc::default(),
            core: Rc::new(Rc::unwrap_or_clone(self.core).with_items(found(&elaborated, find)?)?),
            places: Rc::default(),
            lexicon: Rc::default(),
        })
    }
}

/// One compiled and certified unit: everything a later unit needs in order to be compiled against it.
///
/// What a compilation produced, and the record the certifier's walk over it left — so a predecessor always carries the kernel's own verdicts on its definitions, and a later walk never has to ask whether it does. Made only by [`Unit::new`], which takes the record, and cloned as a handle is: each stage's artifact is held by pointer.
///
/// The serialized form of this is what a store files under an address covering its mounts, its predecessors and the compiler that judged them, beside a [`Record`](crate::Record) of the files it was compiled from.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Unit {
    unit: Produced,
    /// What the certifier concluded about the unit's definitions: each one's totality, closed over what it mentions, and what judging it read. A later walk reads this for the unit's verdicts rather than the stamps elaboration wrote.
    certification: Rc<Certification>,
}

impl Unit {
    /// Assemble a unit from what each stage produced for it and the record the certifier's walk over it left — the only way a unit is made, so none is without one.
    pub fn new(
        text: PreparedText,
        core: Module,
        erased: ErasedUnit,
        traces: Traces,
        certification: Certification,
    ) -> Self {
        Self {
            unit: Produced::new(text, core, erased, traces),
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

    /// The prefixes this unit claims, and whether each is a root only the compiler supplies.
    pub fn mounts(&self) -> &[Mount] {
        self.unit.mounts()
    }

    /// This unit's resolution state, as what a later unit's names resolve against.
    pub fn text(&self) -> &PreparedText {
        self.unit.text()
    }

    /// This unit's elaborated module — what the kernel judges, and what a successor's elaboration reads rather than re-checks.
    pub fn core(&self) -> &Module {
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

    /// This unit as it is stored: its index, itself, its elaborated module without its items, then what it stores apart from itself and what the certifier's record does, what judging each definition read ([`Certification::recorded`]), each behind its length; and the objects the index names ([`Indexed::objects`]).
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

    /// This unit as the fixed prelude images it: [`Unit::stored`]'s form, with no stage's records completed, since no store files the compiler's own units and finds their declarations by what each was made from.
    pub fn imaged(&self) -> Result<Indexed, String> {
        let unit = curios_archive::to_bytes(self)?;
        let (table, reads) = reads_indexed(self.certification.recorded())?;

        self.unit
            .indexed(&unit, (Some((&table, reads)), &self.certification), false)
    }

    /// The unit whose index is `index` ([`Unit::stored`]), each object it names found by `find`: decoded, what it stores apart from itself kept as the bytes it is, for whoever asks. `Err` is bytes that hold no unit, or an object `find` holds none for.
    pub fn restored(
        index: &[u8],
        find: impl Fn(&Digest) -> Option<Object>,
    ) -> Result<Self, String> {
        let (unit, apart) = halves(index).ok_or("not a unit's index")?;

        curios_archive::from_bytes::<Unit>(unit)?.with_apart(Bytes::from(apart.to_vec()), &find)
    }

    /// This unit, decoded from its archive alone, with `bytes` as what it and its record store apart from themselves ([`Unit::stored`], [`Unit::imaged`]), each part a run of the buffer and each object it names found by `find` now. A unit whose `bytes` are a constant of the executable is the compiler's own ([`Unit::fixed`]). `Err` is bytes that are no such thing, or name an object `find` holds none for.
    pub fn with_apart(
        self,
        bytes: Bytes,
        find: &dyn Fn(&Digest) -> Option<Object>,
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
