//! Fixtures the elaboration suites share: names, a context, literals, and the families they declare.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Global, InductDecl, InductParam, Intrinsic, Nat, Subterm, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
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
            InductDecl {
                universe_context: UniverseContext::empty(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::from([
                    (
                        Atom::from("none"),
                        InductParam::new(Telescope::done(Vec::new()), vec![]),
                    ),
                    (
                        Atom::from("some"),
                        InductParam::new(
                            Telescope::build(
                                [(payload, Term::intrinsic(Intrinsic::NatType))],
                                Vec::new(),
                            ),
                            vec![Plicity::Explicit],
                        ),
                    ),
                ])
                .into(),
                result_sort: Term::type_ground(),
                module: Qualifier::empty(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .unwrap();
}
