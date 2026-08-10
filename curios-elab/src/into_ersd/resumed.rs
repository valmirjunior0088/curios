//! What erasure starts from: an earlier erasure's arena, and the Core it was produced from.
//!
//! Erasure of a compilation unit resumes over what earlier erasures established, and that scope has *two* halves which must describe the same thing. The [`ErasedPrelude`] is the arena the builder resumes over and the environment mapping names to operands; the [`Module`]s beside it are the Core those operands were erased from, re-seeded so the unit's own re-derived types reduce through them.
//!
//! Pairing them in one value is the point. They were two parameters, and nothing said they corresponded — hand `erase_module_with_prelude` the Core of one prelude and the arena of another and every name resolves to an operand from a different program, silently. It held only because the single caller took both from one `Prelude`, which is a property of that caller rather than of this signature. Now there is one construction site to get right instead of one per call.
//!
//! The two halves scale differently, and deliberately. The cores are borrowed **one per unit**, because merging them would copy every predecessor's items into every compilation. The arena is **one, threaded**: each unit's erasure resumes over the arena the previous one produced, so what it accumulates is already the whole scope and there is nothing to combine.

use {
    super::{ErasedPrelude, lower::UniverseErased},
    curios_core::Module,
};

/// Earlier erasures' output, as scope for erasing another unit.
pub struct Resumed<'a> {
    /// The Core each arena contribution was erased from, in dependency order. Universes are taken as already validated — `curios-prelude` validates the archive as it restores, which is where untrusted bytes become a `Module`, and the value is immutable from then on. Re-validating per compilation walked the whole standard library again inside the erasure context's step budget.
    cores: &'a [&'a Module],
    arena: ErasedPrelude,
}

impl<'a> Resumed<'a> {
    /// The arena earlier erasures produced from `cores`, in the order they ran.
    ///
    /// The two must describe the same units; that is the whole reason this type exists, and it is why the pairing happens once at the boundary that holds both rather than at every erasure.
    pub fn of(cores: &'a [&'a Module], arena: ErasedPrelude) -> Self {
        Self { cores, arena }
    }

    /// Whether the arena holds any erased items — the freshness probe the archive tests use.
    pub fn is_empty(&self) -> bool {
        self.arena.is_empty()
    }

    pub(super) fn projected_cores(&self) -> Vec<Module> {
        self.cores
            .iter()
            .map(|core| UniverseErased::<Module>::project_validated(core).into_inner())
            .collect()
    }

    pub(super) fn into_arena(self) -> ErasedPrelude {
        self.arena
    }
}
