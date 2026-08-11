//! What one unit provides to its successors.

use {
    curios_abi::ForeignStore, curios_base::Mount, curios_core::Module, curios_elab::ErasedArena,
    curios_text::PreparedText,
};

/// One compiled unit: everything a later unit needs in order to be compiled against it.
///
/// Composed of one opaque artifact per stage rather than flattened into their fields. `curios-text`'s resolution tables and `curios-elab`'s erased arena are that crate's business, and widening them to `pub` so this struct could hold them directly would export a resolver's internals for no consumer. What this type adds is the pairing: the four halves describe *one* unit, and nothing else says so.
///
/// The serialized form of this is a stored unit — what `curios-prelude-archive` writes and every consumer restores, and what a store files under a key covering its terms and the compiler that judged them.
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
}

impl Unit {
    /// Assemble a unit from what each stage produced for it.
    pub fn new(text: PreparedText, core: Module, arena: ErasedArena, binder_floor: usize) -> Self {
        Self {
            text,
            core,
            arena,
            binder_floor,
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
