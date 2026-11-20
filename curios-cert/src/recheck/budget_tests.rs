//! Each judgment of the walk spends a budget of its own.

use {
    super::test_support::*,
    crate::*,
    curios_common::{Qualifier, fill},
    curios_core::{Cost, Global, ReduceError, Variance},
};

/// `Alias : Type = Good`, `Spin : Type` a group that does not descend, `spent : Spin = 0`, which runs a judgment out unfolding `Spin`, and last `Good`, whose one case holds an `Alias`.
const SPENT_BESIDE_AN_ALIASED_PAYLOAD: &str = r#"
let /Alias: Type =
    #{node = InductType} /Good();

let /Spin: Type =
    #{node = Rec}
    (let x: Type = x;
    x);

let /spent: /Spin =
    0;

induct /Good: pub Type
| c(x: /Alias)
end
"#;

/// `Count.{u} : (X : Type u) -> Nat`, two list folds nested over `${list}` that reach `0` in as many steps as its length squared, and `Held.{u} : (i : Nat) -> Type u` whose one case targets `Count(Nat)`: a variance walk reads the index by reducing it, which costs as much as the folds. A recursive call is its own weak-head form, so only a fold over a literal costs the walk its steps.
const HELD_AT_A_COUNTED_INDEX: &str = r#"
#{universe_context = {parameter_count = 1}}
let /Count: (X: #{level = u} Type) -> #NatType =
    (X: #{level = u} Type) => #ListFold(#NatType, #NatType, ${list}, 0, (a: #NatType, b: #NatType) => #ListFold(#NatType, #NatType, ${list}, b, (c: #NatType, d: #NatType) => #NatAdd(d, c)));

#{universe_context = {parameter_count = 1}}
induct /Held: (i: #NatType) -> pub #{level = u} Type
| hold(): ((#{universes = {levels = [u]}} /Count)(#NatType))
end
"#;

/// `Vouched`, a proposition, and `bad : Vouched` a group of one that names itself.
const PROOF_TYPED_NULLARY_GROUP: &str = r#"
induct /Vouched: pub Prop
| qed(u: {})
end

#{node = Rec}
let /bad: #{node = InductType} /Vouched() =
    /bad;
"#;

fn good() -> Global {
    Global::Authored(Qualifier::from(["Good"]))
}

fn spent() -> Global {
    Global::Authored(Qualifier::from(["spent"]))
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
    let verdicts = text_certified_within(SPENT_BESIDE_AN_ALIASED_PAYLOAD, 100_000).into_refusals();

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

/// How long each of a costly `Count`'s two lists is, whose square is the steps it takes to unfold, and a budget that types the module and stops short of them: four frames, which buy the levels its types are classified at.
const COSTLY: usize = 142;
const BETWEEN: u64 = 4 * Cost::FRAME.get();

/// A variance walk the budget cuts short is refused for the budget, so it files no vector, a refused walk filing no record: a reduction the budget refuses reads its term as holding every level it carries, so what the walk computed past it may be the budget's and not the declaration's.
///
/// Two controls hold the budget to the walk alone. The same module with a count that unfolds in a step is accepted on the short budget, so typing fits it, and the costly one is accepted on a budget that runs the folds; each files `Held` irrelevant in its level, which its constructor's target does not hold once `Count` is unfolded.
///
/// Mutation-checked against the refusal: with a walk cut short filed as it stands, the costly module is accepted on the short budget.
#[test]
fn a_variance_walk_the_budget_cuts_short_is_refused_and_files_no_vector() {
    let certified = |side: usize, budget: u64| {
        let list = fill!(
            "#List(#NatType, [${zeros}])",
            zeros = vec!["0"; side].join(", ")
        );

        text_certified_within(&fill!(HELD_AT_A_COUNTED_INDEX, list), budget)
    };
    let irrelevant = Some(&[Variance::Irrelevant][..]);

    let cheap = certified(1, BETWEEN);
    assert_eq!(
        cheap.refusals(),
        Vec::new(),
        "the control stopped holding: the short budget must type the module"
    );
    assert_eq!(cheap.certification().variances(&held()), irrelevant);

    let cut = certified(COSTLY, BETWEEN);
    assert!(
        cut.refusals().len() == 1
            && cut.refusals()[0].name == Some(held())
            && matches!(
                cut.refusals()[0].error,
                Error::Reduce(ReduceError::Exhausted { .. })
            ),
        "the walk must run out, and nothing else: {:?}",
        cut.refusals(),
    );

    let whole = certified(COSTLY, 1_000_000);
    assert_eq!(whole.refusals(), Vec::new());
    assert_eq!(whole.certification().variances(&held()), irrelevant);
}

/// No budget opens a window on a group that does not descend. Acceptance is upward closed in the budget ([the soundness board](../../../documentation/design/soundness/the-soundness-board.md)'s third boundary), so what has to be held is the other direction: a budget too small for the walk to file a verdict at all reads the group as not descending and refuses, rather than finding nothing to refuse.
///
/// [`proof_typed_nullary_group`] is `rec bad : Vouched = bad`, which `check_group`'s local gate owes a refusal: its member is a proof, so erasure deletes it. Every budget from one step up must refuse it, and the largest must refuse it by the rule rather than by the budget — below, the walk runs out before `close_group` files anything, and `group_verdict` answering `None` is what makes that a refusal.
#[test]
fn no_budget_accepts_a_proof_typed_group_that_does_not_descend() {
    let mut by_the_rule = 0;
    for budget in [1, 2, 10, 100, 1_000, 10_000, 1_000_000] {
        let verdicts = text_certified_within(PROOF_TYPED_NULLARY_GROUP, budget).into_refusals();
        assert!(
            !verdicts.is_empty(),
            "at a budget of {budget} the walk found nothing to refuse: {verdicts:?}",
        );
        if verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotDescending { .. }))
        {
            by_the_rule += 1;
        }
    }
    assert!(
        by_the_rule > 0,
        "no budget in the sweep reached the rule's own refusal, so the sweep says nothing about it",
    );
}
