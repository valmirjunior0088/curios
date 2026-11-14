//! Fixtures the elaboration suites share: names, a context, literals, and the families they declare.

use {
    crate::*,
    curios_analysis::test_support::*,
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Global, InductDecl, Intrinsic, Nat, Subterm, Telescope, Term, UniverseContext,
    },
};

/// A declaration's name, from the path a test writes. Fixture-only.
pub fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

pub fn context() -> Context<'static> {
    Context::with_default_budget(SYNTAX)
}

pub fn nat() -> Term {
    Subterm::Intrinsic(Intrinsic::NatType).into()
}

pub fn nat_lit(n: usize) -> Term {
    Subterm::Intrinsic(Intrinsic::Nat(Nat::new(n))).into()
}

// induct Opt : Type | none() | some(x : Nat) end — an unindexed, two-constructor data type, the minimal shape a `| _ =>` catch-all is interesting over.
pub fn register_opt(context: &mut Context) {
    let payload = context.fresh(Some("x"));
    context
        .register_induct(
            &nominal("Opt"),
            InductDecl::new(
                UniverseContext::empty(),
                Telescope::done(Telescope::done(())),
                Vec::from([
                    (
                        Atom::from("none"),
                        curios_core::test_support::case(0, Telescope::done(Vec::new()), vec![]),
                    ),
                    (
                        Atom::from("some"),
                        curios_core::test_support::case(
                            0,
                            Telescope::build(
                                [(payload, Term::intrinsic(Intrinsic::NatType))],
                                Vec::new(),
                            ),
                            vec![Plicity::Explicit],
                        ),
                    ),
                ])
                .try_into()
                .unwrap(),
                Term::type_ground(),
                Qualifier::empty(),
                true,
            ),
        )
        .unwrap();
}
