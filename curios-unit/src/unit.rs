//! What one unit provides to its successors.

use {
    curios_abi::ForeignStore,
    curios_core::{Certification, Module},
    curios_elab::ErasedArena,
    curios_text::PreparedText,
    curios_utilities::Mount,
};

/// One compiled unit: everything a later unit needs in order to be compiled against it.
///
/// Composed of one opaque artifact per stage rather than flattened into their fields. `curios-text`'s resolution tables and `curios-elab`'s erased arena are that crate's business, and widening them to `pub` so this struct could hold them directly would export a resolver's internals for no consumer. What this type adds is the pairing: the four halves describe *one* unit, and nothing else says so.
///
/// The serialized form of this is a stored unit — what `curios-prelude-archive` writes and every consumer restores, and what a store files under an address covering its mounts, its predecessors and the compiler that judged them, beside a [`Record`](crate::Record) of the files it was compiled from.
#[derive(Clone)]
#[curios_archive::archived]
pub struct Unit {
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
    /// What the certifier concluded about `core`'s definitions, filed with them — `None` for a unit no certifier has walked yet.
    ///
    /// Only one producer builds a unit before the kernel has seen it: the prelude archive's build script, which sits below the certifier by design, so its images carry none and `curios-prelude` attaches the record its own build filed. A later walk reads this for the unit's verdicts, and one reading a unit without it classifies the unit's definitions for itself.
    certification: Option<Certification>,
}

impl Unit {
    /// Assemble a unit from what each stage produced for it, `certification` included where the certifier has walked it.
    pub fn new(
        text: PreparedText,
        core: Module,
        arena: ErasedArena,
        binder_floor: usize,
        certification: Option<Certification>,
    ) -> Self {
        Self {
            text,
            core,
            arena,
            binder_floor,
            certification,
        }
    }

    /// What the certifier concluded about this unit's definitions, where it has walked them. See the field.
    pub fn certification(&self) -> Option<&Certification> {
        self.certification.as_ref()
    }

    /// This unit with `certification` filed beside it — for a unit built before any certifier walked it, whose record arrives afterwards.
    pub fn certified(mut self, certification: Certification) -> Self {
        self.certification = Some(certification);
        self
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
