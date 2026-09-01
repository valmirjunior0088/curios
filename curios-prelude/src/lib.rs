//! Curios's fixed prelude, certified by the independent kernel as a condition of this crate building.
//!
//! Everything here comes from [`curios_prelude_archive`], which owns the authored `/syn` and `/std` sources, their elaboration, and the serialized image — plus `/sys`, which it mounts rather than authors: `curios-text`'s `sys_module` projects that root from `curios-abi`'s host store. What this crate adds is a build script that restores that image, walks every item with `curios-cert`, and fails the build on any refusal.
//!
//! Why certification is a crate rather than a check, and why it is split from the archive's own build script, are `README.md`'s decisions.
//!
//! Depend on *this* crate, never on `curios-prelude-archive` directly: that one hands out an image no kernel has seen.

pub use curios_prelude_archive::*;
