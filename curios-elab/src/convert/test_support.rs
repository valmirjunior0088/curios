//! Fixtures the conversion suites share: names, a context, the `conv` entry point, and the term shapes every file builds from.
//!
//! `pub` in a private module: consumed by the sibling suites across `convert`, and nothing outside it.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Global, Intrinsic, Nat, ReduceError, Term},
    curios_utilities::Qualifier,
};

/// A declaration's name, from the path a test writes. Fixture-only.
pub fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

pub fn context() -> Context<'static> {
    Context::new(100_000, SYNTAX)
}

pub fn conv(context: &mut Context, this: &Term, that: &Term) -> Result<bool, ReduceError> {
    convert(context, &Term::type_ground(), this, that)
}

pub fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

/// Build a lambda whose argument domains are irrelevant to conversion (which compares only bodies); each parameter gets a placeholder `Type` domain.
pub fn func<const N: usize>(binders: [&Free; N], body: impl Into<Term>) -> Term {
    Term::func(
        binders.map(|binder| (*binder, Term::type_ground())),
        body.into(),
    )
}

/// A stand-in for a discharged bound. Congruence compares one at its *proposition*, where irrelevance discharges it without looking, so a fixture that only means to exercise the value operands supplies the same term on both sides.
pub fn qed() -> Term {
    Term::free_var(&Free::local(9_999, Some("qed")))
}

// === Spine inversion (contextual metavariables) =============================

pub fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}
