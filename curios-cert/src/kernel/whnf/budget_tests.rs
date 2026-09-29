//! What a reduction is charged, and what happens when the budget runs out.

use {
    crate::{Kernel, whnf},
    curios_analysis::fixture::SYNTAX,
    curios_core::{Category, Cost, ReduceError, Reducer, Term},
};

use super::test_support::*;

/// The kernel is not strongly normalizing, and the budget is what makes every judgment terminate anyway. A group that consumes nothing spins until it runs out, which is an answer rather than a hang.
#[test]
fn a_non_productive_recursion_exhausts_the_budget() {
    let mut kernel = Kernel::new(1_000, SYNTAX);
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
    let mut kernel = Kernel::new(Cost::FRAME.get() + Cost::STEP.get(), SYNTAX);
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
    let mut kernel = Kernel::new(Cost::FRAME.get() * 4, SYNTAX);
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
