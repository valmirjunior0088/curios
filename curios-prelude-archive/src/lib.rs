//! The build-scoped image of Curios's fixed `/sys`, `/syn`, and `/std` prelude: the authored sources, their elaboration, and the [`curios_unit::Unit`] they are archived as.
//!
//! `/sys` mirrors the host store one declaration per wire row, and every row returns an `Io` — a description of the call, not its result. `/sys/Io` holds the sequencing (`pure`, `bind`) and nothing else; `/std` owns the taxonomy that wraps them. See this crate's README for the placement law, and `documentation/design/language/effects-are-descriptions-and-the-carrier-has-no-eliminator.md` for the invariant those wrappers rest on.
//!
//! # This image is not certified, and nothing should reach it here
//!
//! The build script lowers, elaborates, erases and serializes. It does **not** run the kernel, and it deliberately has no `curios-cert` dependency: Cargo's rebuild granularity is the build script, so a script needing both dependency sets re-elaborates the whole standard library for every certifier edit; `curios-analysis/README.md` carries what that measured.
//!
//! Certification is [`curios-prelude`](../curios_prelude/index.html)'s, whose own build script restores this image, walks it with the kernel, and fails the build on any refusal. **Consumers depend on that crate, never on this one.** The invariant an archive rests on — *one that exists is one whose every item the kernel accepted* — holds because the only crate that hands out the prelude is one that cannot build without certifying it.

#[cfg(test)]
mod tests;

mod syntax;
pub use syntax::*;

#[cfg(test)]
mod sources;

mod restore;
pub use restore::*;

/// The reduction allowance the fixed prelude is certified at, re-exported so the certifying build script names one dependency rather than two.
pub use curios_elab::DEFAULT_STEP_BUDGET;
