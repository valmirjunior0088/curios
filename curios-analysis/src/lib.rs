//! The analyses both Curios checkers run, and the seam they run behind.
//!
//! Each of these is a total function of the terms and declarations it is handed, and what differs between the two checkers' inputs arrives through [`Env`], so a second implementation would be a second run of the same function rather than a second opinion. That is why they are shared rather than duplicated, and it is also the one place where the two-checker design buys nothing: a rule here is a rule *neither* checker can catch the other getting wrong. `documentation/design/soundness/` argues them on that understanding.
//!
//! What each checker supplies for itself is [`Env`] — reduction, the local context and unfolding through a local's definition, fresh binders, and the registry fallback for declarations outside the analyzed set. `curios-elab` implements it over its elaboration `Context`; `curios-cert` implements it over its `Kernel`.
//!
//! The trusted base is unchanged by this crate existing: these rules admit terms, so they are inside it, and `cargo tree -p curios-cert -e normal` enumerates them. Why the split was made at all — it is about rebuild granularity, not about trust — is `README.md`'s to state.

mod judge;
pub use judge::*;

mod case_equation;
pub use case_equation::*;

mod conversion;
pub use conversion::*;

mod invert;
pub use invert::*;

mod specialize;
pub use specialize::*;

mod unfolding;
pub use unfolding::*;

mod positivity;
pub use positivity::*;

mod variance;
pub use variance::*;

mod totality;
pub use totality::*;

mod erased;
pub use erased::*;

// A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: `curios_analysis::test_support::SYNTAX` says at its use site that the caller reached for scaffolding rather than product API. The path is the warning label.
#[cfg(feature = "test-support")]
pub mod test_support;
