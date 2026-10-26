//! Fixtures the reduction suites share: a name, a discharged bound, a context, and a literal.
//!
//! The module is `pub`: the sibling suites across `reduce` consume it. The discharged bound is the crate's test support's, which erasure's suite reads too.

pub use crate::test_support::qed;
use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Global, Intrinsic, Nat, Term},
    curios_utilities::Qualifier,
};

/// A declaration's name, from the path a test writes. Fixture-only.
pub fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

pub fn context() -> Context<'static> {
    Context::new(100_000, SYNTAX)
}

pub fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}
