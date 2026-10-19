//! What one unit provides to its successors, and what it is before a certifier has walked it.

use {
    crate::{apart_parts, digests, found, halved, halves},
    curios_abi::ForeignStore,
    curios_archive::{Bytes, Deferred, joined, parted},
    curios_core::{Certification, Lexicon, Module, Places, Sharing},
    curios_elab::{ErasedUnit, Projection, Readers, Traces},
    curios_text::PreparedText,
    curios_utilities::{Digest, Mount, Object},
    std::{cell::OnceCell, rc::Rc},
};

/// A unit as it is stored: its index, which every compilation after the unit restores and which names each object the unit stores on its own by its digest alone, and those objects.
///
/// **The index is a function of what was compiled**, since it names an object by what the object holds and never by where it is kept: a recompile files the index a whole compile files, and a record digests the index alone and so vouches for every object it names.
pub struct Indexed {
    pub index: Vec<u8>,
    pub objects: Vec<Object>,
}

/// A unit as its compilation produced it, before the kernel has judged it: everything a [`Unit`] holds except the certifier's record.
///
/// Composed of one opaque artifact per stage rather than flattened into their fields. `curios-text`'s resolution tables and `curios-elab`'s erased forms are that crate's business, and widening them to `pub` so this struct could hold them directly would export a resolver's internals for no consumer. What this type adds is the pairing: the three artifacts describe *one* unit, and nothing else says so.
///
/// **Each artifact is held by pointer, so a unit is handed over and never copied.** A fold hands a unit to its successors, a session keeps one as the next question's baseline, and a store offers one, and each of those is a clone of the handles and of nothing they hold.
///
/// A compilation holds one of these only between elaborating a unit and certifying it, and [`Uncertified::certified`] is the one way out. The fixed prelude's images are the one place one is kept: `curios-prelude-archive`'s build script writes them below the certifier by design, framed as a store slot is framed, and `curios-prelude` certifies what they hold and attaches the record its own build filed.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Uncertified {
    text: Rc<PreparedText>,
    /// Elaborated and zonked. This is what the kernel judges and what a successor's elaboration reads where it stands.
    ///
    /// **Stored apart, and read by item.** A compilation after the unit reads the declarations it names, so the module's archive is kept beside the unit, and a unit restored holds a module whose outline is restored — each item's names and what each is declared as — and whose items are each read when something first asks ([`Module::stored`]).
    #[archived_with(curios_archive::Skip)]
    core: Rc<Module>,
    /// Where `core` holds each name it defines, made the first time the unit is read by name and kept for as long as it is held: no compilation keys the unit again. Never stored, being read off `core`.
    #[archived_with(curios_archive::Skip)]
    places: Rc<OnceCell<Places>>,
    /// What erasure has read of `core` out of its universes, made the first time an erasure reads the unit and kept beside its places: no erasure keys the unit again, and a declaration one projected the next finds. Never stored, being read off `core`.
    #[archived_with(curios_archive::Skip)]
    projection: Rc<OnceCell<Projection>>,
    /// What a report spells this unit's names by, made the first time a report is made over the unit and kept beside its places: no report builds a table over the unit again. Never stored, being read off `core`.
    #[archived_with(curios_archive::Skip)]
    lexicon: Rc<OnceCell<Lexicon>>,
    /// What each of the unit's items erased to, in identities of its own and naming what another item binds by its name: this unit's alone, saying nothing of the units before it. Why is the crate `README.md`'s "An erased form is its item's".
    erased: Rc<ErasedUnit>,
    /// What each of the unit's declarations asked of the declarations around it as it elaborated: what a later compile of the same sources keeps a declaration's elaboration by.
    ///
    /// **Of the unit's baseline half, with the text's** ([`PreparedText::baseline_bytes`]): nothing compiled after the unit reads it, so it is stored beside what the unit is for its successors and read out of its bytes when a compile over the unit first asks.
    #[archived_with(curios_archive::Skip)]
    traces: Rc<Deferred<Traces>>,
    /// Who asked what, of the unit's traces read backwards: what a recompile over the unit finds what an edit could reach by. Made the first time a recompile over the unit asks, or handed over by the recompile that made the unit, and kept beside its places; never stored, being read off the traces.
    #[archived_with(curios_archive::Skip)]
    readers: Rc<OnceCell<Readers>>,
    /// Whether the unit is the compiler's own: restored out of bytes that are a constant of the executable, so one thing under one compiler. Never stored, being where the unit came from.
    ///
    /// **Nothing records a read of a fixed unit.** A baseline is offered by the compiler that made it — a slot's address names the compiler, and a session lives in one process — so a fixed unit is the one every baseline was compiled after, and what a unit read of it is never asked again (`curios_elab::Established::fixed`).
    #[archived_with(curios_archive::Skip)]
    fixed: bool,
}

impl Uncertified {
    /// Assemble a unit from what each stage produced for it.
    pub fn new(text: PreparedText, core: Module, erased: ErasedUnit, traces: Traces) -> Self {
        Self {
            text: Rc::new(text),
            core: Rc::new(core),
            places: Rc::default(),
            projection: Rc::default(),
            lexicon: Rc::default(),
            erased: Rc::new(erased),
            traces: Rc::new(Deferred::held(traces)),
            readers: Rc::default(),
            fixed: false,
        }
    }

    /// This unit, holding `readers` as who asked what of its traces: what the recompile that made it carries over from its baseline, in place of reading its traces backwards again.
    pub fn reading(mut self, readers: Readers) -> Self {
        self.readers = Rc::new(OnceCell::from(readers));
        self
    }

    /// This unit, holding `places` as where its module holds each name, where they were handed on: what the recompile that made it carries over from its baseline, with the names of what it changed placed again, in place of placing every name again.
    pub fn placing(mut self, places: Option<Places>) -> Self {
        if let Some(places) = places {
            self.places = Rc::new(OnceCell::from(places));
        }
        self
    }

    /// This unit, holding `projection` as what erasure has read of its module out of its universes: what the erasure over a baseline that made the unit carries over from the baseline's, in place of making a cell for every item again.
    pub fn projecting(mut self, projection: Projection) -> Self {
        self.projection = Rc::new(OnceCell::from(projection));
        self
    }

    /// Whether the unit is the compiler's own. See the field.
    pub fn fixed(&self) -> bool {
        self.fixed
    }

    /// This unit with the record the certifier's walk over it left — the only way a [`Unit`] is made.
    pub fn certified(self, certification: Certification) -> Unit {
        Unit {
            unit: self,
            certification: Rc::new(certification),
        }
    }

    /// The prefixes this unit claims, and whether each is a root only the compiler supplies.
    pub fn mounts(&self) -> &[Mount] {
        &self.core.mounts
    }

    /// This unit's resolution state, as what a later unit's names resolve against.
    pub fn text(&self) -> &PreparedText {
        &self.text
    }

    /// This unit's elaborated module — what the kernel judges, and what a successor's elaboration reads rather than re-checks.
    pub fn core(&self) -> &Module {
        &self.core
    }

    /// Where this unit's elaborated module holds each name it defines. See the field.
    pub fn places(&self) -> &Places {
        self.places.get_or_init(|| Places::of(&self.core))
    }

    /// What a report spells this unit's names by. See the field.
    pub fn lexicon(&self) -> &Lexicon {
        self.lexicon.get_or_init(|| self.core.lexicon())
    }

    /// What erasure has read of this unit's elaborated module out of its universes. See the field.
    pub fn projection(&self) -> &Projection {
        self.projection.get_or_init(|| Projection::of(&self.core))
    }

    /// This unit's items' erased forms — what a later unit's erasure reads how a name is bound from, what a program links, and what a recompile of this unit keeps for the items it does not redo.
    pub fn erased(&self) -> &ErasedUnit {
        &self.erased
    }

    /// The `foreign` rows this unit declares. Disjoint from every other unit's by mount, because an `ffi` import name is the declaration's fully qualified name — so the fold unions them without a collision to resolve.
    pub fn foreigns(&self) -> &ForeignStore {
        self.text.foreigns()
    }

    /// What each of this unit's declarations asked as it elaborated. See the field. Read out of the unit's baseline half now, where the unit was restored and no compile over it has asked before.
    pub fn traces(&self) -> &Traces {
        self.traces
            .get()
            .unwrap_or_else(|error| panic!("a unit's traces failed to restore: {error}"))
    }

    /// Who asked what, of this unit's traces. See the field.
    pub fn readers(&self) -> &Readers {
        self.readers.get_or_init(|| Readers::of(self.traces()))
    }

    /// What this unit stores apart from itself, each part behind its length ([`joined`]) and read by whoever asks for it, with the objects it names: its lowered items and its erased forms, each an object of its own named by its digest ([`PreparedText::items_objects`], [`ErasedUnit::items_objects`]), its elaborated module's archive, read by item ([`Module::stored`]), then its baseline half, a part for each stage that keeps a record, the text's, the elaborator's and erasure's.
    pub fn apart(&self) -> Result<(Vec<u8>, Vec<Object>), String> {
        let items = self.text.items_objects()?;
        let forms = self.erased.items_objects()?;
        let module = curios_archive::to_bytes(&*self.core)?;
        let anchors = self.text.anchors_bytes()?;
        let imports = self.text.imports_bytes()?;
        let text = self.text.baseline_bytes()?;
        let traces = self.traces.bytes()?;
        let erased = self.erased.baseline_bytes()?;

        let apart = joined(&[
            &digests(&items)[..],
            &digests(&forms)[..],
            &module[..],
            &anchors[..],
            &imports[..],
            &text[..],
            &traces[..],
            &erased[..],
        ]);

        Ok((apart, items.into_iter().chain(forms).collect()))
    }

    /// This unit as it is imaged: its index, itself and then what it stores apart from itself ([`halved`]), and the objects the index names.
    pub fn stored(&self) -> Result<Indexed, String> {
        let unit = curios_archive::to_bytes(self)?;
        let (apart, objects) = self.apart()?;

        Ok(Indexed {
            index: halved(&unit, &apart),
            objects,
        })
    }

    /// This unit with `bytes` as what it stores apart from itself ([`Uncertified::apart`]), each part a run of the buffer they are in and read when something first asks for it, and each object they name found by `find` now — see [`PreparedText::with_items`] and [`PreparedText::with_baseline`]. `Err` is bytes that are no such thing, or name an object `find` holds none for.
    pub fn with_apart(
        self,
        bytes: impl Into<Bytes>,
        find: &dyn Fn(&Digest) -> Option<Bytes>,
    ) -> Result<Self, String> {
        let bytes = bytes.into();
        let fixed = matches!(bytes, Bytes::Constant(_));
        let [items, forms, module, anchors, imports, text, traces, erased] =
            apart_parts(&bytes).ok_or("not what a unit stores apart from itself")?;

        Ok(Self {
            fixed,
            text: Rc::new(
                Rc::unwrap_or_clone(self.text)
                    .with_baseline(text)
                    .with_reported(imports, anchors)
                    .with_items(found(&items, find)?)?,
            ),
            erased: Rc::new(
                Rc::unwrap_or_clone(self.erased)
                    .with_baseline(erased)
                    .with_items(found(&forms, find)?)?,
            ),
            traces: Rc::new(Deferred::stored(traces)),
            readers: Rc::default(),
            core: Rc::new(Module::stored(module)?),
            places: Rc::default(),
            projection: Rc::default(),
            lexicon: Rc::default(),
        })
    }

    /// This unit with its elaborated module hash-consed in a table of its own — what [`Unit::stored`] serializes. The lowered module is left as it was built, since a lint and a report are located in it.
    fn consed(&self) -> Self {
        Self {
            text: Rc::clone(&self.text),
            core: Rc::new(self.core.shared(&Sharing::new())),
            places: Rc::default(),
            projection: Rc::default(),
            lexicon: Rc::default(),
            erased: Rc::clone(&self.erased),
            traces: Rc::clone(&self.traces),
            readers: Rc::clone(&self.readers),
            fixed: self.fixed,
        }
    }
}

/// One compiled and certified unit: everything a later unit needs in order to be compiled against it.
///
/// What a compilation produced, and the record the certifier's walk over it left — so a predecessor always carries the kernel's own verdicts on its definitions, and a later walk never has to ask whether it does. Made only by [`Uncertified::certified`], and cloned as a handle is: see [`Uncertified`].
///
/// The serialized form of this is what a store files under an address covering its mounts, its predecessors and the compiler that judged them, beside a [`Record`](crate::Record) of the files it was compiled from.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Unit {
    unit: Uncertified,
    /// What the certifier concluded about the unit's definitions: each one's totality, closed over what it mentions, and what judging it read. A later walk reads this for the unit's verdicts rather than the stamps elaboration wrote.
    certification: Rc<Certification>,
}

impl Unit {
    /// What the certifier concluded about this unit's definitions. See the field.
    pub fn certification(&self) -> &Certification {
        &self.certification
    }

    /// See [`Uncertified::mounts`].
    pub fn mounts(&self) -> &[Mount] {
        self.unit.mounts()
    }

    /// See [`Uncertified::text`].
    pub fn text(&self) -> &PreparedText {
        self.unit.text()
    }

    /// See [`Uncertified::core`].
    pub fn core(&self) -> &Module {
        self.unit.core()
    }

    /// See [`Uncertified::places`].
    pub fn places(&self) -> &Places {
        self.unit.places()
    }

    /// See [`Uncertified::lexicon`].
    pub fn lexicon(&self) -> &Lexicon {
        self.unit.lexicon()
    }

    /// See [`Uncertified::projection`].
    pub fn projection(&self) -> &Projection {
        self.unit.projection()
    }

    /// See [`Uncertified::erased`].
    pub fn erased(&self) -> &ErasedUnit {
        self.unit.erased()
    }

    /// See [`Uncertified::foreigns`].
    pub fn foreigns(&self) -> &ForeignStore {
        self.unit.foreigns()
    }

    /// See [`Uncertified::traces`].
    pub fn traces(&self) -> &Traces {
        self.unit.traces()
    }

    /// See [`Uncertified::readers`].
    pub fn readers(&self) -> &Readers {
        self.unit.readers()
    }

    /// See [`Uncertified::fixed`].
    pub fn fixed(&self) -> bool {
        self.unit.fixed()
    }

    /// Whether this and `other` are one unit in hand: handles of one compilation's result, or of one restoration's. Two units restored apart from the same bytes are two, which whoever asks tells by their bytes.
    pub fn same(&self, other: &Unit) -> bool {
        Rc::ptr_eq(&self.unit.core, &other.unit.core)
    }

    /// This unit as it is stored: its index, itself, its elaborated module hash-consed in a table of its own ([`Module::shared`]), then what it stores apart from itself ([`Uncertified::apart`]) and what the certifier's record does, what judging each definition read ([`Certification::reads_bytes`]), each behind its length; and the objects the index names.
    ///
    /// **A stored unit says what its items mean, not where they were written or how they were come by.** A consed module is one node for each structure as it is spelled, under no position, so an item elaborated now and the same item reused from a baseline are the same nodes, and one moved by an edit above it is stored as it was. The fixed prelude's images are consed by the same pass, where they are built.
    ///
    /// Run here and not where a unit is assembled: the pass builds a node for every node of the module, and a unit that is neither filed nor followed — a keystroke's — is never serialized.
    pub fn stored(&self) -> Result<Indexed, String> {
        let consed = self.unit.consed();
        let (apart, objects) = consed.apart()?;
        let unit = curios_archive::to_bytes(&Unit {
            unit: consed,
            certification: Rc::clone(&self.certification),
        })?;

        let reads = self.certification.reads_bytes()?;

        Ok(Indexed {
            index: halved(&unit, &joined(&[&apart[..], &reads[..]])),
            objects,
        })
    }

    /// The unit whose index is `index` ([`Unit::stored`]), each object it names found by `find`: decoded, what it stores apart from itself kept as the bytes it is, for whoever asks. `Err` is bytes that hold no unit, or an object `find` holds none for.
    pub fn restored(index: &[u8], find: impl Fn(&Digest) -> Option<Bytes>) -> Result<Self, String> {
        let (unit, apart) = halves(index).ok_or("not a unit's index")?;
        let restored = curios_archive::from_bytes::<Unit>(unit)?;
        let [apart, reads]: [Bytes; 2] = parted(&Bytes::from(apart.to_vec()))
            .and_then(|parts| parts.try_into().ok())
            .ok_or("not what a certified unit stores apart from itself")?;

        Ok(Self {
            unit: restored.unit.with_apart(apart, &find)?,
            certification: Rc::new(Rc::unwrap_or_clone(restored.certification).with_reads(reads)),
        })
    }
}
