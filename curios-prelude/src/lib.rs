//! Curios's fixed prelude, certified by the independent kernel as a condition of this crate building.
//!
//! Everything here comes from [`curios_prelude_archive`], which elaborates the two roots `curios-text` holds — `/std` authored there, `/sys` projected whole by its `sys_module` from `curios-abi`'s host store and the intrinsic table — into one serialized image per root. What this crate adds is a build script that restores those images and walks every item with `curios-cert`, each root against the roots before it, failing the build on any refusal — and the record that walk leaves of what it concluded, with which each image's uncertified unit becomes the [`Unit`] that [`with_prelude`] lends, so a later walk reads the certifier's verdicts on the prelude's definitions and never the stamps elaboration wrote.
//!
//! Why certification is a crate rather than a check, and why it is split from the archive's own build script, are `README.md`'s decisions.
//!
//! Depend on *this* crate, never on `curios-prelude-archive` directly: that one hands out an image no kernel has seen, and its uncertified restoration and its own `with_prelude` are for the build script, not for anything this crate lends.

use {
    curios_core::Certification, curios_prelude_archive::restore_archives, curios_unit::Unit,
    std::cell::LazyCell,
};

/// The certifier's record of each root, in the fold's order — filed under `OUT_DIR` by this crate's build script as it certified the images.
const CERTIFICATION_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/certification.rkyv"));

thread_local! {
    /// The restored fixed prelude, each root's unit carrying the record its certification filed. Per thread for the reason the archive's own restoration is: a `Unit` is not `Send`.
    static PRELUDE: LazyCell<[Unit; 2]> = LazyCell::new(|| {
        let certifications = curios_archive::from_bytes::<Vec<Certification>>(CERTIFICATION_BYTES)
            .unwrap_or_else(|error| panic!("the prelude's certification failed to restore: {error}"));
        let [sys_record, std_record]: [Certification; 2] =
            certifications.try_into().unwrap_or_else(|filed: Vec<_>| {
                panic!("the prelude's certification holds {} records for two roots", filed.len())
            });
        let [sys, std] = restore_archives();

        [sys.unit.certified(sys_record), std.unit.certified(std_record)]
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
