//! Curios's fixed prelude, certified by the independent kernel as a condition of this crate building.
//!
//! Everything here comes from [`curios_prelude_archive`], which owns the authored `/sys`, `/syn` and `/std` sources, their elaboration, and the serialized image. What this crate adds is a build script that restores that image, walks every item with `curios-cert`, and fails the build on any refusal.
//!
//! # Why that is a crate rather than a check
//!
//! The invariant the whole archive-verdict pattern rests on is *an archive that exists is one whose every item the kernel accepted*. Stated as a test, it is a convention: an image could exist, be compiled against, and never have been walked. Stated as a crate, it is a build-time impossibility — the only crate handing out the prelude is one that does not compile unless the kernel accepted it, which is Coq's `.vok` reached independently.
//!
//! The split is also what keeps the two halves' rebuilds apart. Cargo re-runs a build script whenever any of its dependencies change, so the single script this replaced re-elaborated the entire standard library for every `curios-cert` edit — 469 s of a ~570 s build, spent re-deriving something the certifier cannot affect. See `documentation/roadmap/compiler/10_PRELUDE_ENVIRONMENT_SPEC.md`, M5.
//!
//! Depend on *this* crate, never on `curios-prelude-archive` directly: that one hands out an image no kernel has seen.

pub use curios_prelude_archive::*;
