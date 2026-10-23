//! The analyses both Curios checkers run, and the seam they run behind.
//!
//! Each is a total function of the terms and declarations it is handed, and what differs between the two checkers' inputs arrives through [`Env`]: reduction, the local context and unfolding through a local's definition, fresh binders, and the registry fallback for declarations outside the analyzed set. `curios-elab` implements it over its elaboration `Context`; `curios-cert` implements it over its `Kernel`. Why the rules are shared rather than duplicated, and what that costs, is `README.md`'s decision, and `documentation/design/soundness/` argues them on that understanding.
//!
//! These rules admit terms, so they are inside the trusted base, which `cargo tree -p curios-cert -e normal` enumerates; why they are a crate of their own is `README.md`'s decision.

mod judge;
pub use judge::*;

mod case_equation;
pub use case_equation::*;

mod classify;
pub use classify::*;

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

#[cfg(test)]
mod walk_tests;

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_analysis::test_support::SYNTAX` says at its use site that the caller reached for scaffolding rather than product API. The path is the warning label.
#[cfg(feature = "test-support")]
pub mod test_support;
