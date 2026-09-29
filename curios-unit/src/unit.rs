//! What one unit provides to its successors, and what it is before a certifier has walked it.

use {
    curios_abi::ForeignStore,
    curios_core::{Certification, Module},
    curios_elab::ErasedArena,
    curios_text::PreparedText,
    curios_utilities::Mount,
};

/// A unit as its compilation produced it, before the kernel has judged it: everything a [`Unit`] holds except the certifier's record.
///
/// Composed of one opaque artifact per stage rather than flattened into their fields. `curios-text`'s resolution tables and `curios-elab`'s erased arena are that crate's business, and widening them to `pub` so this struct could hold them directly would export a resolver's internals for no consumer. What this type adds is the pairing: the four halves describe *one* unit, and nothing else says so.
///
/// A compilation holds one of these only between elaborating a unit and certifying it, and [`Uncertified::certified`] is the one way out. The fixed prelude's images are the one place one is kept: `curios-prelude-archive`'s build script writes them below the certifier by design, framed as a store slot is framed, and `curios-prelude` certifies what they hold and attaches the record its own build filed.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Uncertified {
    text: PreparedText,
    /// Elaborated and zonked. This is what the kernel judges and what a successor's elaboration replays.
    core: Module,
    /// **Not per-unit, despite sitting on a unit** — which is why it is named for what it is rather than for the stage that made it. Each unit's erasure resumes over the previous one's arena — see [`Prefix::arena`](crate::Prefix::arena) — so what this holds is the whole prefix's artifact, cumulative from the first unit forward, and never an independent arena numbered from zero.
    ///
    /// That is what lets a unit be stored whole. The worry it answers was real: two *independently* erased arenas both start at zero, so per-unit artifacts would need a relocation pass, which is `cnum_map` again. They are not independent, and a stored unit's key names its exact ordered predecessors, so the arena a restored unit carries always matches the prefix it is restored into.
    arena: ErasedArena,
    /// `curios_core::derived_binder_floor` over `core`, computed by the walk that established this unit.
    ///
    /// Carried rather than re-derived because it is a constant of that walk, and re-deriving it means traversing every term in scope on every later one. A floor is a bound rather than a verdict, so a consumer combines it with its own by maximum and can only ever widen.
    binder_floor: usize,
}

impl Uncertified {
    /// Assemble a unit from what each stage produced for it.
    pub fn new(text: PreparedText, core: Module, arena: ErasedArena, binder_floor: usize) -> Self {
        Self {
            text,
            core,
            arena,
            binder_floor,
        }
    }

    /// This unit with the record the certifier's walk over it left — the only way a [`Unit`] is made.
    pub fn certified(self, certification: Certification) -> Unit {
        Unit {
            unit: self,
            certification,
        }
    }

    /// The floor below which every binder identity in this unit was minted. See the field.
    pub fn binder_floor(&self) -> usize {
        self.binder_floor
    }

    /// The prefixes this unit claims, and the privilege tier each carries.
    pub fn mounts(&self) -> &[Mount] {
        &self.core.mounts
    }

    /// This unit's resolution state, as the scope a later unit's names resolve against.
    pub fn text(&self) -> &PreparedText {
        &self.text
    }

    /// This unit's elaborated module — what the kernel judges, and what a successor's elaboration replays rather than re-checks.
    pub fn core(&self) -> &Module {
        &self.core
    }

    /// This unit's erased arena and environment, as an owned clone: replay consumes it by value, so one compile's mutation of its copy can never poison a later one.
    pub fn arena(&self) -> ErasedArena {
        self.arena.clone()
    }

    /// The `foreign` rows this unit declares. Disjoint from every other unit's by mount, because an `ffi` import name is the declaration's fully qualified name — so the fold unions them without a collision to resolve.
    pub fn foreigns(&self) -> &ForeignStore {
        self.text.foreigns()
    }
}

/// One compiled and certified unit: everything a later unit needs in order to be compiled against it.
///
/// What a compilation produced, and the record the certifier's walk over it left — so a unit in scope always carries the kernel's own verdicts on its definitions, and a later walk never has to ask whether it does. Made only by [`Uncertified::certified`].
///
/// The serialized form of this is what a store files under an address covering its mounts, its predecessors and the compiler that judged them, beside a [`Record`](crate::Record) of the files it was compiled from.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Unit {
    unit: Uncertified,
    /// What the certifier concluded about the unit's definitions: each one's totality, closed over what it mentions. A later walk reads this for the unit's verdicts rather than the stamps elaboration wrote.
    certification: Certification,
}

impl Unit {
    /// What the certifier concluded about this unit's definitions. See the field.
    pub fn certification(&self) -> &Certification {
        &self.certification
    }

    /// See [`Uncertified::binder_floor`].
    pub fn binder_floor(&self) -> usize {
        self.unit.binder_floor()
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

    /// See [`Uncertified::arena`].
    pub fn arena(&self) -> ErasedArena {
        self.unit.arena()
    }

    /// See [`Uncertified::foreigns`].
    pub fn foreigns(&self) -> &ForeignStore {
        self.unit.foreigns()
    }
}
