//! What one unit provides to its successors, and what it is before a certifier has walked it.

use {
    curios_abi::ForeignStore,
    curios_core::{Certification, Module, Places},
    curios_elab::ErasedUnit,
    curios_text::PreparedText,
    curios_utilities::Mount,
    std::{cell::OnceCell, rc::Rc},
};

#[cfg(feature = "archive")]
use curios_core::Sharing;

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
    core: Rc<Module>,
    /// Where `core` holds each name it defines, made the first time the unit is read by name and kept for as long as it is held: no compilation keys the unit again. Never stored, being read off `core`.
    #[archived_with(curios_archive::Skip)]
    places: Rc<OnceCell<Places>>,
    /// What each of the unit's items erased to, in identities of its own and naming what another item binds by its name: this unit's alone, saying nothing of the units before it. Why is the crate `README.md`'s "An erased form is its item's".
    erased: Rc<ErasedUnit>,
}

impl Uncertified {
    /// Assemble a unit from what each stage produced for it.
    pub fn new(text: PreparedText, core: Module, erased: ErasedUnit) -> Self {
        Self {
            text: Rc::new(text),
            core: Rc::new(core),
            places: Rc::default(),
            erased: Rc::new(erased),
        }
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

    /// This unit's items' erased forms — what a later unit's erasure reads how a name is bound from, what a program links, and what a recompile of this unit keeps for the items it does not redo.
    pub fn erased(&self) -> &ErasedUnit {
        &self.erased
    }

    /// The `foreign` rows this unit declares. Disjoint from every other unit's by mount, because an `ffi` import name is the declaration's fully qualified name — so the fold unions them without a collision to resolve.
    pub fn foreigns(&self) -> &ForeignStore {
        self.text.foreigns()
    }

    /// This unit with its elaborated module hash-consed in a table of its own — what [`Unit::stored`] serializes. The lowered module is left as it was built, since a lint and a report are located in it.
    #[cfg(feature = "archive")]
    fn consed(&self) -> Self {
        Self {
            text: Rc::clone(&self.text),
            core: Rc::new(self.core.shared(&Sharing::new())),
            places: Rc::default(),
            erased: Rc::clone(&self.erased),
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

    /// See [`Uncertified::erased`].
    pub fn erased(&self) -> &ErasedUnit {
        self.unit.erased()
    }

    /// See [`Uncertified::foreigns`].
    pub fn foreigns(&self) -> &ForeignStore {
        self.unit.foreigns()
    }

    /// The bytes this unit is stored as: itself, its elaborated module hash-consed in a table of its own ([`Module::shared`]).
    ///
    /// **A stored unit says what its items mean, not where they were written or how they were come by.** A consed module is one node for each structure as it is spelled, under no position, so an item elaborated now and the same item reused from a baseline are the same nodes, and one moved by an edit above it is stored as it was. The fixed prelude's images are consed by the same pass, where they are built.
    ///
    /// Run here and not where a unit is assembled: the pass builds a node for every node of the module, and a unit that is neither filed nor followed — a keystroke's — is never serialized.
    #[cfg(feature = "archive")]
    pub fn stored(&self) -> Result<curios_archive::Serialized, String> {
        curios_archive::to_bytes(&Unit {
            unit: self.unit.consed(),
            certification: Rc::clone(&self.certification),
        })
    }
}
