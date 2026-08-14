//! The build-script/library contract for Curios's compiler-local prelude image.
//!
//! This is deliberately not a stable interchange format. The schema only distinguishes incompatible compiler builds; Cargo regenerates the image in this crate's `OUT_DIR` whenever its inputs change.

use {curios_core::Module, curios_elab::ErasedArena, curios_text::PreparedText};

pub(crate) const SCHEMA: u32 = 26;

// `always` because this crate archives unconditionally — there is no `archive` feature here for a `cfg_attr` to name — and `recursive` for the bounds a self-reaching type needs.
#[curios_archive::archived(always, recursive)]
pub(crate) struct PreludeArchive {
    pub(crate) schema: u32,
    pub(crate) fingerprint: [u8; 32],
    pub(crate) prepared: PreparedText,
    pub(crate) core: Module,
    /// `curios_core::derived_binder_floor` over `core`, computed by the build that established this image. Carried so per-compile rechecking reads this floor instead of re-deriving it over every archived term — the same "already checked" argument the environment rests on, applied to a bound rather than a verdict.
    pub(crate) binder_floor: usize,
    pub(crate) ersd: ErasedArena,
}
