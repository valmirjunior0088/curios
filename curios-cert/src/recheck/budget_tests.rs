//! Each judgment of the walk spends a budget of its own.

use {
    super::test_support::*,
    crate::{Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Definition, Free, Global, InductDecl, InductParam, Intrinsic, Item, Many, Module,
        Nat, Nominal, RecGroup, RecItem, RecMemberScopes, ReduceError, Scope, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
    std::collections::BTreeSet,
};

fn good() -> Global {
    Global::Authored(Qualifier::from(["Good"]))
}

fn spent() -> Global {
    Global::Authored(Qualifier::from(["spent"]))
}

/// `induct Good | c(x : Alias)` with `Alias : Type = Good`, beside `spent = 0` declared at `Spin : Type = rec s : Type = s`, which no budget converts to `Nat`.
fn spent_item_beside_an_aliased_payload() -> Module {
    let alias = Global::Authored(Qualifier::from(["Alias"]));
    let spin = Global::Authored(Qualifier::from(["Spin"]));
    let s = Free::local(930, Some("s"));

    let group = RecGroup::new(vec![RecMemberScopes {
        type_: Scope::close(Many(1), &[&s], Term::type_ground()),
        body: Scope::close(Many(1), &[&s], Term::free_var(&s)),
    }]);

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build(
                    [(
                        Free::local(931, Some("x")),
                        Term::free_var(&Free::from(&alias)),
                    )],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    Module {
        mounts: Vec::new(),
        items: vec![
            authored(
                &alias,
                Term::type_ground(),
                Term::induct_type(good(), Vec::<Term>::new(), Vec::<Term>::new()),
            ),
            authored_partial(&spin, Term::type_ground(), Term::rec_proj(group, 0)),
            authored(
                &spent(),
                Term::free_var(&Free::from(&spin)),
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            ),
            // The family's former is the module's last item, so its entry is judged once `spent` has been.
            Item::Rec(RecItem::new(vec![Definition::former(
                good(),
                Qualifier::default(),
                Nominal::Inductive(declaration),
            )])),
        ]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// Strict positivity is judged on a budget of its own, not on what the item judged before it left.
///
/// `spent` spends everything it has: checking `0` against `Spin` converts forever. `Good`'s payload is `Alias`, which the analysis has to unfold to read the strict occurrence of `Good` behind it. `Good`'s former is the module's last item, so its entry is accepted once every other item has been judged: on `spent`'s leftover the unfold would be refused, the alias read at `Mixed`, and `Good` refused for a budget it never spent.
///
/// `spent`'s own exhaustion is the control: without it the leftover would be most of a budget, and the fixture would pass whether or not positivity restores one.
///
/// Mutation-checked against the restore: with the analysis run on what the judgment before it left, `Good` is refused for the budget.
#[test]
fn positivity_is_judged_on_its_own_budget() {
    let verdicts = fixture_verdicts(
        &spent_item_beside_an_aliased_payload(),
        100_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(spent())
            && matches!(verdict.error, Error::Reduce(ReduceError::Exhausted { .. }))),
        "the control stopped holding: `spent` must spend its whole budget: {verdicts:?}",
    );
    assert!(
        verdicts.iter().all(|verdict| verdict.name != Some(good())),
        "`Good` is strictly positive, and was judged on what `spent` left: {verdicts:?}",
    );
}
