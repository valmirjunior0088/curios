//! The analyses both Curios checkers run, and the seam they run behind.
//!
//! Each of these is a total function of post-zonk terms and declarations, so a second implementation would be a second run of the same function on the same input rather than a second opinion. That is why they are shared rather than duplicated, and it is also the one place where the two-checker design buys nothing: a rule here is a rule *neither* checker can catch the other getting wrong. `documentation/SOUNDNESS.md` grades them on that understanding.
//!
//! What each checker supplies for itself is [`Env`] — reduction, unfolding, fresh binders, and the registry fallback for declarations outside the analyzed set. `curios-elab` implements it over its elaboration `Context`; `curios-cert` implements it over its `Kernel`.
//!
//! # Why this is not part of `curios-cert`
//!
//! It was, and the split is about *rebuilds* rather than about trust. Elaboration needs these analyses, so `curios-elab` depended on `curios-cert`, so every crate whose build script reaches elaboration reached the kernel too — and Cargo re-runs a build script whenever any dependency changes. The fixed prelude's image was therefore re-elaborated for every certifier edit: 469 s of a ~570 s build, spent re-deriving something no kernel rule can affect.
//!
//! The trusted base is unchanged and is now two crates rather than one. These rules admit terms, so they are inside it; `cargo tree -p curios-cert` still enumerates them, one level further out. What the split buys is that a kernel edit no longer invalidates elaboration.

mod judge;
pub use judge::*;

mod satisfy;
pub use satisfy::*;

mod invert;
pub use invert::*;

mod positivity;
pub use positivity::*;

mod totality;
pub use totality::*;

mod erased;
pub use erased::*;
