//! Each judgment of the walk spends a budget of its own.

use {
    super::test_support::*,
    crate::{Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Definition, Free, Global, InductDecl, InductParam, Intrinsic, Item, Level, Many,
        Module, Nat, Nominal, RecGroup, RecItem, RecMemberScopes, ReduceError, Scope, Telescope,
        Term, UniverseContext, UniverseParam, Variance,
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

fn held() -> Global {
    Global::Authored(Qualifier::from(["Held"]))
}

/// How many iterations a costly `Count` takes to unfold, and a budget that types the module and stops short of them.
const COSTLY: usize = 20_000;
const BETWEEN: u64 = 2_000;

/// `induct Held.{u}: (i : Nat) -> Type u | hold() : (Count.{u}(Nat))` over `let Count.{u}(X : Type u) -> Nat = match unfolding | 0 => 0 | _ + 1; ih => ih end`. Typing the target reads `Count`'s type and never its value, and strict positivity reads no target; the variance walk reduces it, to see whether what is left still holds `u`, and that is the fold's every iteration.
fn held_at_a_counted_index(unfolding: usize) -> Module {
    let level = || Level::param(UniverseParam(0));
    let scheme = || UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };
    let nat_type = || Term::intrinsic(Intrinsic::NatType);
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let count = Global::Authored(Qualifier::from(["Count"]));
    let counted = Free::local(940, Some("X"));
    let (motive, pred, ih) = (
        Free::local(941, Some("m")),
        Free::local(942, Some("pred")),
        Free::local(943, Some("ih")),
    );
    let unfolded = Term::nat_match(
        nat(unfolding),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &ih,
        Term::free_var(&ih),
    );
    let alias = match authored(
        &count,
        Term::func_type([(counted, Term::type_at(level()))], nat_type()),
        Term::func([(counted, Term::type_at(level()))], unfolded),
    ) {
        Item::Let(definition) => Item::Let(Definition {
            universe_context: scheme(),
            ..definition
        }),
        item => item,
    };

    let declaration = InductDecl {
        universe_context: scheme(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(944, Some("i")), nat_type())],
            (),
        )),
        constructors: vec![(
            Atom::from("hold"),
            InductParam::new(
                Telescope::done(vec![Term::apply(
                    Term::instance_of(&Free::from(&count), vec![level()]),
                    [nat_type()],
                )]),
                Vec::new(),
            ),
        )],
        result_sort: Term::type_at(level()),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    Module {
        mounts: Vec::new(),
        items: vec![
            alias,
            Item::Rec(RecItem::new(vec![Definition::former(
                held(),
                Qualifier::default(),
                Nominal::Inductive(declaration),
            )])),
        ]
        .into(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// A variance walk the budget cuts short is refused for the budget and files no vector: a reduction the budget refuses reads its term as holding every level it carries, so what the walk computed past it may be the budget's and not the declaration's.
///
/// Two controls hold the budget to the walk alone. The same module with a count that unfolds in a step is accepted on the short budget, so typing fits it, and the costly one is accepted on a budget that runs the fold; each files `Held` irrelevant in its level, which its constructor's target does not hold once `Count` is unfolded.
///
/// Mutation-checked against the refusal: with a walk cut short filed as it stands, the costly module is accepted on the short budget.
#[test]
fn a_variance_walk_the_budget_cuts_short_is_refused_and_files_no_vector() {
    let certified = |unfolding: usize, budget: u64| {
        fixture_certified(
            &held_at_a_counted_index(unfolding),
            budget,
            &Globals::default(),
            SYNTAX,
        )
    };
    let irrelevant = Some(&[Variance::Irrelevant][..]);

    let cheap = certified(1, BETWEEN);
    assert_eq!(
        cheap.verdicts,
        Vec::new(),
        "the control stopped holding: the short budget must type the module"
    );
    assert_eq!(cheap.certification.variances(&held()), irrelevant);

    let cut = certified(COSTLY, BETWEEN);
    assert!(
        cut.verdicts.len() == 1
            && cut.verdicts[0].name == Some(held())
            && matches!(
                cut.verdicts[0].error,
                Error::Reduce(ReduceError::Exhausted { .. })
            ),
        "the walk must run out, and nothing else: {:?}",
        cut.verdicts,
    );
    assert_eq!(cut.certification.variances(&held()), None);

    let whole = certified(COSTLY, 1_000_000);
    assert_eq!(whole.verdicts, Vec::new());
    assert_eq!(whole.certification.variances(&held()), irrelevant);
}
