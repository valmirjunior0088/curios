//! Fixtures the conversion suites share: names, a context, the `conv` entry point, the term shapes every file builds from, and the families and propositions a suite declares.
//!
//! `pub` in a private module: consumed by the sibling suites across `convert`, and nothing outside it.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Nat, ReduceError, Telescope, Term,
        UniverseContext,
    },
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

/// A nominal family with no constructor, at `sort` and over `indices`.
pub fn declare_family(
    context: &mut Context,
    path: &str,
    indices: Telescope<()>,
    sort: Term,
) -> Global {
    register(context, path, indices, Vec::new(), sort)
}

/// A nominal family registered with `constructors`.
pub fn register(
    context: &mut Context,
    path: &str,
    indices: Telescope<()>,
    constructors: Vec<(Atom, InductParam)>,
    sort: Term,
) -> Global {
    context
        .register_induct(
            &nominal(path),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(indices),
                constructors: constructors.into(),
                result_sort: sort,
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();

    nominal(path)
}

/// A nominal family at `Prop` with no constructor, for the goals that need a base proposition.
pub fn declare_proposition(context: &mut Context, path: &str) -> Term {
    context
        .register_induct(
            &nominal(path),
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new().into(),
                result_sort: Term::prop(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();

    Term::induct_type(nominal(path), Vec::<Term>::new(), Vec::<Term>::new())
}
