//! Fixtures the conversion suites share: names, a context, the `conv` entry point, and the term shapes every file builds from.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `convert`, and nothing outside it.

use curios_core::*;
use {crate::*, curios_utilities::Qualifier};

/// A declaration's name, from the path a test writes. Fixture-only.
pub(super) fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

pub(super) fn context() -> Context {
    Context::new(100_000, crate::SYNTAX)
}

pub(super) fn conv(context: &mut Context, this: &Term, that: &Term) -> Result<bool, ReduceError> {
    convert(context, &Term::type_ground(), this, that)
}

pub(super) fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

/// Build a lambda whose argument domains are irrelevant to conversion (which compares only bodies); each parameter gets a placeholder `Type` domain.
pub(super) fn func<const N: usize>(binders: [&Free; N], body: impl Into<Term>) -> Term {
    Term::func(
        binders.map(|binder| (binder.clone(), Term::type_ground())),
        body.into(),
    )
}

/// A stand-in for a discharged bound. Congruence compares one at its *proposition*, where irrelevance discharges it without looking, so a fixture that only means to exercise the value operands supplies the same term on both sides.
pub(super) fn qed() -> Term {
    Term::free_var(&Free::local(9_999, Some("qed")))
}

// === Spine inversion (contextual metavariables) =============================

pub(super) fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

// === The history key (cross-branch collisions) ==============================

/// Extract the `FuncType` payload of a term a fixture built through `Term::func_type`.
pub(super) fn as_func_type(term: Term) -> FuncType {
    let Subterm::FuncType(func_type) = Term::unwrap_or_clone(term) else {
        unreachable!()
    };
    func_type
}
