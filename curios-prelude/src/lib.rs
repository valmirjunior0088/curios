//! Curios's fixed prelude: the certified units every compilation starts from, which this crate's build makes of `/sys` and `/std` through `curios-prelude-build`.
//!
//! The build lowers, elaborates and erases each root — `/sys` projected whole by the roster from `curios-abi`'s host store and the intrinsic table, then `/std` against it — walks every item with `curios-cert`, each root against the roots before it, failing the build on any refusal, and images each root as the certified unit it is, framed as a unit the cabinet keeps is and carrying the record the certifier's walk left of its definitions. [`with_prelude`] lends those units, so a later walk reads the certifier's verdicts on the prelude's definitions and never the stamps elaboration wrote, and [`documentation`] the roots' documentation records.
//!
//! What `/sys` holds, where a host operation is declared and how `/std` is laid out are `curios-prelude-build`'s decisions; `documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md` states the invariant the `/std` wrappers rest on. Why certification is a crate rather than a check, and why the prelude is one build, are `README.md`'s decisions.

#[cfg(test)]
mod tests;

mod restore;
pub use restore::{documentation, root_prefixes, with_prelude};
