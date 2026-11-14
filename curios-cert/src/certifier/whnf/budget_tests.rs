//! What a reduction is charged, and what happens when the budget runs out.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_core::{Category, Cost, Exhaustion, ReduceError, Reducer, Term},
};

/// The certifier is not strongly normalizing, and the budget is what makes every judgment terminate anyway. A group that consumes nothing spins until it runs out, which is an answer rather than a hang.
#[test]
fn a_non_productive_recursion_exhausts_the_budget() {
    let mut certifier = Certifier::new(1_000, SYNTAX);

    let term = term(
        "#{node = Rec} (let loop: (n: #NatType) -> #NatType = (n: #NatType) => loop(n); loop(1))",
    );

    assert!(
        certifier
            .reduce_forced(term)
            .is_err_and(|spent| spent.is_exhausted())
    );
}

/// Each judgment gets the full budget back, so one expensive declaration cannot starve the next.
///
/// An undefined variable costs exactly one *step* — it is looked at once and is already normal — on top of the one guarded level the reduction enters. So the smallest budget that affords exactly one reduction is a frame plus a step, spelled from the constants rather than as a number, and what the second and third calls do is entirely about the refill.
///
/// The frame is charged per new *peak* depth, so a second reduction at the same depth would be free of it — which is why the refill matters here twice over: `restore_budget` resets the peak as well as the budget, so the second call pays for its level again exactly as the first did.
///
/// Three *different* binders, because a term reduced once is remembered for the rest of the declaration — a local-bearing one too — and a second look at the same one would be a free hit rather than the reduction whose refusal this is about.
#[test]
fn restoring_the_budget_refills_it() {
    let mut certifier = Certifier::new(Cost::FRAME.get() + Cost::STEP.get(), SYNTAX);
    let [x, y, z] = <[Term; 3]>::try_from(components(&term("(x, y, z)"))).expect("three locals");

    assert_eq!(whnf(&mut certifier, x.clone()), Ok(x));
    assert!(whnf(&mut certifier, y).is_err_and(|spent| spent.is_exhausted()));

    certifier.restore_budget();
    assert_eq!(whnf(&mut certifier, z.clone()), Ok(z));
}

/// Depth is refused by the counter, and the refusal says so. Without the frame row, a reduction driven deep would take real stack the budget observes none of — `recurse` grows rather than aborting, so what bounded depth would be the host's memory rather than anything the program could be told about.
///
/// The subject is a chain of nested intrinsic operands over an *open* tip — a term the closed machine's gate declines, so the recursive strategy re-enters reduction once per link and the budget affords a handful of levels and no more. The closed twin of this chain does not trip the row at all, which is the machine's whole yield and is asserted by its own tests.
#[test]
fn a_deep_reduction_is_refused_and_the_refusal_names_depth() {
    let mut certifier = Certifier::new(Cost::FRAME.get() * 4, SYNTAX);
    let chain = open_chain(&mut certifier, 64);

    let refusal = certifier
        .reduce_forced(chain)
        .expect_err("four frames do not afford sixty-four levels");

    assert!(
        matches!(
            refusal,
            ReduceError::Exhausted {
                category: Category::Depth,
                ..
            }
        ),
        "expected a depth refusal, got {refusal:?}"
    );
}
