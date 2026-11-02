//! Fixtures the reduction suites share: a name, a context, and a literal.
//!
//! The module is `pub`: the sibling suites across `reduce` consume it.

use {
    crate::*,
    curios_analysis::test_support::*,
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
