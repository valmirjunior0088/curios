//! The build-script/library contract for Curios's compiler-local prelude image.
//!
//! This is deliberately not a stable interchange format. The schema only
//! distinguishes incompatible compiler builds; Cargo regenerates the image in
//! this crate's `OUT_DIR` whenever its inputs change.

use curios_text::PreparedPrelude;

pub(crate) const SCHEMA: u32 = 10;

#[derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
#[rkyv(
    serialize_bounds(
        __S: rkyv::ser::Writer + rkyv::ser::Allocator + rkyv::ser::Sharing,
        __S::Error: rkyv::rancor::Source
    ),
    deserialize_bounds(
        __D: rkyv::de::Pooling,
        __D::Error: rkyv::rancor::Source
    ),
    bytecheck(bounds(
        __C: rkyv::validation::ArchiveContext + rkyv::validation::SharedContext,
        __C::Error: rkyv::rancor::Source
    ))
)]
pub(crate) struct PreludeArchive {
    pub(crate) schema: u32,
    pub(crate) fingerprint: [u8; 32],
    pub(crate) prepared: PreparedPrelude,
    pub(crate) core: curios_core::Module,
    pub(crate) body_type: curios_core::Term,
    pub(crate) ersd: curios_core::ErasedPrelude,
}
