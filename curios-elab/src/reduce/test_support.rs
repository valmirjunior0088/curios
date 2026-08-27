//! Fixtures the reduction suites share: a name, a discharged bound, a context, and a literal.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `reduce`, and nothing outside it.

use curios_core::*;
use {crate::*, curios_utilities::Qualifier};

/// A declaration's name, from the path a test writes. Fixture-only.
pub(super) fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A stand-in for a discharged bound. Reduction never inspects one — proof irrelevance makes its value unobservable, and these tests are about the fold laws rather than the obligation.
pub(super) fn qed() -> Term {
    Term::free_var(&Free::local(9_999, Some("qed")))
}

pub(super) fn context() -> Context {
    Context::new(100_000, crate::SYNTAX)
}

pub(super) fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}
