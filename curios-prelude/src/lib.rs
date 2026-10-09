//! Curios's fixed prelude, certified by the independent kernel as a condition of this crate building.
//!
//! Everything here comes from [`curios_prelude_archive`], which elaborates the prelude's two roots — `/std` authored in `curios-std`, `/sys` projected whole by `curios-text`'s `sys_module` from `curios-abi`'s host store and the intrinsic table — into one serialized image per root. What this crate adds is a build script that restores those images and walks every item with `curios-cert`, each root against the roots before it, failing the build on any refusal — and the record that walk leaves of what it concluded, with which each image's uncertified unit becomes the [`Unit`] that [`with_prelude`] lends, so a later walk reads the certifier's verdicts on the prelude's definitions and never the stamps elaboration wrote.
//!
//! Why certification is a crate rather than a check, and why it is split from the archive's own build script, are `README.md`'s decisions.
//!
//! Depend on *this* crate, never on `curios-prelude-archive` directly: that one hands out an image no kernel has seen, and its uncertified restoration and its own `with_prelude` are for the build script, not for anything this crate lends.

use {
    curios_archive::{Bytes, parted},
    curios_core::Certification,
    curios_document::Documentation,
    curios_prelude_archive::{restore_archives, restore_documentation},
    curios_unit::Unit,
    std::cell::LazyCell,
};

/// The certifier's record of each root, in the fold's order — filed under `OUT_DIR` by this crate's build script as it certified the images.
const CERTIFICATION_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/certification.rkyv"));

/// What judging each definition of each root read, the roots' one after the other behind their lengths: read by a walk over a root as a baseline and by no other, so each stays the slice of the executable it is until one asks.
const READS_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/reads.rkyv"));

thread_local! {
    /// The restored fixed prelude, each root's unit carrying the record its certification filed. Per thread for the reason the archive's own restoration is: a `Unit` is not `Send`.
    static PRELUDE: LazyCell<[Unit; 2]> = LazyCell::new(|| {
        let certifications = curios_archive::from_bytes::<Vec<Certification>>(CERTIFICATION_BYTES)
            .unwrap_or_else(|error| panic!("the prelude's certification failed to restore: {error}"));
        let [sys_record, std_record]: [Certification; 2] =
            certifications.try_into().unwrap_or_else(|filed: Vec<_>| {
                panic!("the prelude's certification holds {} records for two roots", filed.len())
            });
        let [sys_reads, std_reads]: [Bytes; 2] = parted(&Bytes::from(READS_BYTES))
            .and_then(|filed| filed.try_into().ok())
            .unwrap_or_else(|| panic!("the prelude's reads are not two roots'"));
        let [sys, std] = restore_archives();

        [
            sys.unit.certified(sys_record.with_reads(sys_reads)),
            std.unit.certified(std_record.with_reads(std_reads)),
        ]
    });
}

/// Borrow this thread's restored prelude, certified, as the ordered predecessors every compilation starts from — `/sys` then `/std`, each unit carrying the certifier's record of its definitions.
///
/// A slice rather than one unit, and in dependency order rather than any: what `Predecessors::over` takes is exactly this, so a product puts the whole prelude in scope by handing it along instead of deciding how its roots compose.
pub fn with_prelude<R>(use_prelude: impl FnOnce(&[&Unit]) -> R) -> R {
    PRELUDE.with(|prelude| {
        let [sys, std] = &**prelude;
        use_prelude(&[sys, std])
    })
}

/// The documentation record of each of the prelude's roots, in the fold's order: what `document --std` renders, and what a unit adopting a root's declarations reads them out of.
///
/// Apart from [`with_prelude`], because no unit carries a record: a compilation restores none, and a command that documents restores these and no unit for them.
pub fn documentation() -> Vec<Documentation> {
    restore_documentation()
}
