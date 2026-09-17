//! Each judgment of the walk spends a budget of its own.

use {
    crate::{Globals, KernelError},
    curios_analysis::fixture::SYNTAX,
    curios_core::{
        Atom, Entrypoint, Free, Global, InductDecl, InductParam, Intrinsic, Many, Module, Nat,
        RecGroup, RecMemberScopes, ReduceError, Scope, Telescope, Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

fn good() -> Global {
    Global::Authored(Qualifier::from(["Good"]))
}

/// `induct Good | c(x : Alias)` with `Alias : Type = Good`, beside an entrypoint `0` declared at `Spin : Type = rec s : Type = s`, which no budget converts to `Nat`.
fn spent_entry_beside_an_aliased_payload() -> Module {
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
        ],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(good(), declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            type_: Some(Term::free_var(&Free::from(&spin))),
        }),
    }
}

/// Strict positivity is judged on a budget of its own, not on what the entrypoint left.
///
/// The entrypoint spends everything it has: checking `0` against `Spin` converts forever. `Good`'s payload is `Alias`, which the analysis has to unfold to read the strict occurrence of `Good` behind it. Judged on the entrypoint's leftover, the unfold was refused, the alias was read at `Mixed`, and `Good` was refused — as not strictly positive, and once the analysis carried the driver's refusal, for a budget `Good` never spent.
///
/// The entrypoint's own exhaustion is the control: without it the leftover would be most of a budget, and the fixture would pass whether or not positivity restores one.
#[test]
fn positivity_is_judged_on_its_own_budget() {
    let verdicts = fixture_verdicts(
        &spent_entry_beside_an_aliased_payload(),
        100_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| verdict.name.is_none()
            && matches!(
                verdict.error,
                KernelError::Reduce(ReduceError::Exhausted { .. })
            )),
        "the control stopped holding: the entrypoint must spend its whole budget: {verdicts:?}",
    );
    assert!(
        verdicts.iter().all(|verdict| verdict.name != Some(good())),
        "`Good` is strictly positive, and was judged on what the entrypoint left: {verdicts:?}",
    );
}
