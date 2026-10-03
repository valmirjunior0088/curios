//! The build-scoped images of Curios's fixed `/sys` and `/std` prelude: the elaboration of the sources `curios-text` holds, and the [`curios_unit::Uncertified`] unit each root is archived as.
//!
//! Two units, folded in that order: `/sys` names nothing above it and `/std` names `/sys`, so [`with_prelude`] hands back the pair as an ordered prefix rather than as one merged image. Each image is framed exactly as a store slot is — the record of the tree the root was compiled from, ahead of the unit — and holds the unit before certification, which `curios-prelude` performs as it restores the images for a compilation.
//!
//! `/sys` mirrors the host store one declaration per wire row, and every row returns an `Io` — a description of the call, not its result. `/sys/Io` holds the sequencing (`pure`, `bind`) and nothing else; `/std` owns the taxonomy that wraps them. See this crate's README for the placement law, and `documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md` for the invariant those wrappers rest on.
//!
//! # This image is not certified, and nothing should reach it here
//!
//! The build script lowers, elaborates, erases and serializes. It does **not** run the kernel, and it deliberately has no `curios-cert` dependency: Cargo's rebuild granularity is the build script, so a script needing both dependency sets re-elaborates the whole standard library for every certifier edit — this crate's `README.md`, and `curios-analysis/README.md` for the split that keeps the kernel out transitively.
//!
//! Certification is [`curios-prelude`](../curios_prelude/index.html)'s, whose own build script restores these images, walks each against the roots before it with the kernel, and fails the build on any refusal. **Consumers depend on that crate, never on this one.** The invariant an archive rests on — *one that exists is one whose every item the kernel accepted* — holds because the only crate that hands out the prelude is one that cannot build without certifying it.

#[cfg(test)]
mod tests;

mod restore;
pub use restore::*;
