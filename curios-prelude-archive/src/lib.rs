//! The build-scoped images of Curios's fixed `/sys` and `/std` prelude: the elaboration of the `/sys` root `curios-text` generates and the `/std` package `curios-std` holds, and the [`curios_unit::Uncertified`] unit each root is archived as.
//!
//! Two units, folded in that order: `/sys` names nothing above it and `/std` names `/sys`, so [`with_prelude`] hands back the pair as an ordered prefix rather than as one merged image. Each image is framed exactly as a store slot is — the record of the tree the root was compiled from, ahead of the unit — and holds the unit before certification, which `curios-prelude` performs as it restores the images for a compilation.
//!
//! What `/sys` holds, and where a host operation is declared, are `curios-text/README.md`'s decision "/sys declares no host operation, and `Io` owns only the sequencing"; `documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md` states the invariant the `/std` wrappers rest on.
//!
//! # This image is not certified, and nothing should reach it here
//!
//! The build script lowers, elaborates, erases and serializes, and never runs the kernel: why it has no `curios-cert` dependency is `README.md`'s decision, and `curios-analysis/README.md` states the split that keeps the kernel out transitively.
//!
//! Certification is [`curios-prelude`](../curios_prelude/index.html)'s, whose own build script restores these images, walks each against the roots before it with the kernel, and fails the build on any refusal. **Consumers depend on that crate, never on this one.** The invariant an archive rests on — *one that exists is one whose every item the kernel accepted* — holds because the only crate that hands out the prelude is one that cannot build without certifying it.

#[cfg(test)]
mod tests;

mod restore;
pub use restore::*;
