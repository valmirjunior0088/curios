//! The build-script/library contract for Curios's compiler-local prelude image.
//!
//! This is deliberately not a stable interchange format. The schema only distinguishes incompatible compiler builds; Cargo regenerates the image in this crate's `OUT_DIR` whenever its inputs change.

use {curios_core::Module, curios_elab::ErasedArena, curios_text::PreparedText};

pub(crate) const SCHEMA: u32 = 26;

// Unconditional rather than `#[curios_archive::archived]`: this crate always archives, so there is no feature to gate on and nothing for the macro to do but forward bounds.
#[derive(curios_archive::Archive, curios_archive::Serialize, curios_archive::Deserialize)]
#[rkyv(
    crate = curios_archive::rkyv,
    serialize_bounds(
        __S: curios_archive::rkyv::ser::Writer + curios_archive::rkyv::ser::Allocator + curios_archive::rkyv::ser::Sharing,
        __S::Error: curios_archive::rkyv::rancor::Source
    ),
    deserialize_bounds(
        __D: curios_archive::rkyv::de::Pooling,
        __D::Error: curios_archive::rkyv::rancor::Source
    ),
    bytecheck(bounds(
        __C: curios_archive::rkyv::validation::ArchiveContext + curios_archive::rkyv::validation::SharedContext,
        __C::Error: curios_archive::rkyv::rancor::Source
    ))
)]
pub(crate) struct PreludeArchive {
    pub(crate) schema: u32,
    pub(crate) fingerprint: [u8; 32],
    pub(crate) prepared: PreparedText,
    pub(crate) core: Module,
    /// `curios_core::derived_binder_floor` over `core`, computed by the build that established this image. Carried so per-compile rechecking reads this floor instead of re-deriving it over every archived term — the same "already checked" argument the environment rests on, applied to a bound rather than a verdict.
    pub(crate) binder_floor: usize,
    pub(crate) ersd: ErasedArena,
}
