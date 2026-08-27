//! The analyses both Curios checkers run, and the seam they run behind.
//!
//! Each of these is a total function of post-zonk terms and declarations, so a second implementation would be a second run of the same function on the same input rather than a second opinion. That is why they are shared rather than duplicated, and it is also the one place where the two-checker design buys nothing: a rule here is a rule *neither* checker can catch the other getting wrong. `documentation/soundness.md` grades them on that understanding.
//!
//! What each checker supplies for itself is [`Env`] — reduction, unfolding, fresh binders, and the registry fallback for declarations outside the analyzed set. `curios-elab` implements it over its elaboration `Context`; `curios-cert` implements it over its `Kernel`.
//!
//! The trusted base is unchanged by this crate existing, and is now two crates rather than one: these rules admit terms, so they are inside it, and `cargo tree -p curios-cert -e normal` still enumerates them one level further out. Why the split was made at all — it is about rebuild granularity, not about trust — is `README.md`'s to state, with the figure that motivated it.

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

// A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: `curios_analysis::fixture::SYNTAX` says at its use site that the caller reached for scaffolding rather than product API. The path is the warning label.
#[cfg(feature = "test-support")]
pub mod fixture;
