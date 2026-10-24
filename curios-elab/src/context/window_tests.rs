//! Settled windows: what a settlement remembered is kept for the next settlement made under the same refinements, for as long as the deepest frame it read still stands.
//!
//! A frame the settlement opened and left by its end is no part of what it rests on: what read that frame's refinements went at its exit, and what read its bindings names its locals, which nothing asks again.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Intrinsic, Nat, Term},
};

fn context() -> Context<'static> {
    Context::with_default_budget(SYNTAX)
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn two() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(2usize)))
}

/// A settlement under the frame assuming `x` reads a binding of a frame it opens and leaves, and remembers a reduct of `x`: the next settlement under that frame starts from what it remembered.
#[test]
fn a_window_whose_settlement_read_a_frame_it_left_answers_the_next_settlement() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    context.with_frame(|context| {
        context.assume(&x, &nat_type());
        let reduced = Term::free_var(&x);
        context.with_refinements_withheld_from(1, |context| {
            context.with_frame(|context| {
                context.assume(&y, &nat_type());
                assert!(context.assumption(&y).is_some());
            });
            context.reduce(reduced.clone(), &two());
        });

        let kept =
            context.with_refinements_withheld_from(1, |context| context.cached_reduced(&reduced));
        assert_eq!(kept, Some(two()));
    });
}
