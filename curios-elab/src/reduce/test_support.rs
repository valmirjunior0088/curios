//! Fixtures the reduction suites share: a name, a discharged bound, a context, and a literal.
//!
//! The module is `pub`: the sibling suites across `reduce` consume it, and `into_ersd`'s suite reads `qed`.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Global, Intrinsic, Nat, Term},
    curios_utilities::Qualifier,
};

/// A declaration's name, from the path a test writes. Fixture-only.
pub fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A stand-in for a discharged bound. Reduction never inspects one — proof irrelevance makes its value unobservable, and these tests are about the fold laws rather than the obligation.
pub fn qed() -> Term {
    Term::free_var(&Free::local(9_999, Some("qed")))
}

pub fn context() -> Context<'static> {
    Context::new(100_000, SYNTAX)
}

pub fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}
