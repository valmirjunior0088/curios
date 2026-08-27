//! What a reduction is charged, and what happens when the budget or the retention quota runs out.

use {
    crate::{Kernel, whnf},
    curios_core::{Category, Cost, ReduceError, Reducer, Term},
};

use super::test_support::*;

/// The kernel is not strongly normalizing, and the budget is what makes every judgment terminate anyway. A group that consumes nothing spins until it runs out, which is an answer rather than a hang.
#[test]
fn a_non_productive_recursion_exhausts_the_budget() {
    let mut kernel = Kernel::new(1_000, crate::SYNTAX);
    kernel.set_local_floor(1_000);
    let loop_ = binder(0, "loop");
    let n = binder(1, "n");

    let body = Term::func(
        [(n.clone(), nat_type())],
        Term::apply(Term::free_var(&loop_), [Term::free_var(&n)]),
    );

    let term = Term::rec(
        [(
            loop_.clone(),
            Term::func_type([(n.clone(), nat_type())], nat_type()),
            body,
        )],
        Term::apply(Term::free_var(&loop_), [nat(1)]),
    );

    assert!(
        kernel
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
    let mut kernel = Kernel::new(Cost::FRAME.get() + Cost::STEP.get(), crate::SYNTAX);
    kernel.set_local_floor(1_000);
    let occurrence = |index: u32| Term::free_var(&binder(index, "x"));

    assert_eq!(whnf(&mut kernel, occurrence(0)), Ok(occurrence(0)));
    assert!(whnf(&mut kernel, occurrence(1)).is_err_and(|spent| spent.is_exhausted()));

    kernel.restore_budget();
    assert_eq!(whnf(&mut kernel, occurrence(2)), Ok(occurrence(2)));
}

/// Depth is refused by the counter, and the refusal says so. Before the frame row, a reduction driven deep took real stack and the budget observed none of it — `recurse` grows rather than aborting, so what bounded depth was the host's memory rather than anything the program could be told about.
///
/// The subject is a chain of nested intrinsic operands over an *open* tip — a term the closed machine's gate declines, so the recursive strategy re-enters reduction once per link and the budget affords a handful of levels and no more. The closed twin of this chain no longer trips the row at all, which is the machine's whole yield and is asserted by its own tests.
#[test]
fn a_deep_reduction_is_refused_and_the_refusal_names_depth() {
    let mut kernel = Kernel::new(Cost::FRAME.get() * 4, crate::SYNTAX);
    kernel.set_local_floor(1_000);
    let tip = binder(0, "tip");

    let refusal = kernel
        .reduce_forced(open_chain(64, &tip))
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

/// The retention quota degrades the cache rather than refusing the program: an allowance too small for any entry leaves every reduction correct and every one of them cold.
///
/// Correctness is the assertion, and what the allowance withholds is what makes it worth making: the name-keyed unfold table, which is what an exhausted allowance leaves cold across declarations. The term-keyed tables are not the allowance's to withhold — see the test below — so within one declaration the second reduction here is a hit either way, and what this pins is that a kernel with no allowance at all still answers what a warm one does.
#[test]
fn an_exhausted_retention_quota_leaves_the_answer_alone() {
    let mut warm = kernel();
    let mut cold = Kernel::with_retention(1_000_000, 0, crate::SYNTAX);
    cold.set_local_floor(1_000);

    let term = chain(64);
    let expected = warm.reduce_forced(term.clone()).expect("reduces");

    assert_eq!(cold.reduce_forced(term.clone()), Ok(expected.clone()));
    assert_eq!(cold.reduce_forced(term), Ok(expected));
    assert_eq!(cold.retained(), 0, "nothing was admitted, so nothing spent");
}

/// The retention allowance does not reach the term-keyed tables: a kernel with *no* allowance still hands a term's second reduction back for nothing within the declaration, exactly as one with the default does. Those tables live as long as the budget that built their entries and are bounded by it; what the allowance prices is the name-keyed table that outlives a declaration.
///
/// This used to assert the opposite — that a zero allowance made the second reduction re-derive — and that was the rule under which a thirteen-definition proof spent a third of the whole compilation's allowance on entries that died with its declaration.
#[test]
fn the_allowance_does_not_decide_a_second_reduction_within_a_declaration() {
    let term = chain(64);

    let mut warm = kernel();
    spent(&mut warm, term.clone());
    let warm_again = spent(&mut warm, term.clone());

    let mut unallowed = Kernel::with_retention(1_000_000, 0, crate::SYNTAX);
    unallowed.set_local_floor(1_000);
    spent(&mut unallowed, term.clone());
    let unallowed_again = spent(&mut unallowed, term);

    assert_eq!(warm_again, 0, "a remembered reduct is hit for nothing");
    assert_eq!(
        unallowed_again, 0,
        "and no allowance was needed to remember it"
    );
    assert_eq!(
        unallowed.retained(),
        0,
        "the term-keyed tables charge the allowance nothing"
    );
}
