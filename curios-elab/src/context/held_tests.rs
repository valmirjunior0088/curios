//! Frames held: what a parked problem holds of the context, and what a judgment standing on it reads.
//!
//! What parks takes the frames above the base as they stand, shared with the context. A frame written to since is copied for its writer, and a retry stands on the frames held with nothing assumed, defined or registered again: each guard stands in the frame it was registered in, and what a frame settled of its guards is read wherever the frame is held.

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

/// A guard: `f` applied to `n`, assumed true.
fn guard(context: &mut Context, n: usize) -> Term {
    let f = context.fresh(Some("f"));
    Term::apply(
        Term::free_var(&f),
        [Term::intrinsic(Intrinsic::Nat(Nat::new(n)))],
    )
}

fn truth() -> Term {
    Term::intrinsic(Intrinsic::Bool(true))
}

/// A frame written to while something holds it is copied for the writer: what parked before the write holds the frame as it stood, and a retry standing on it reads nothing written since.
#[test]
fn a_held_frame_keeps_what_stood_where_it_is_written_to_after() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));

    context.with_frame(|context| {
        context.assume(&x, &nat_type());
        let held = context.hold_frames();
        context.assume(&y, &nat_type());

        let (sees_x, sees_y) = context.standing_on(&held, |context| {
            (
                context.assumption(&x).is_some(),
                context.assumption(&y).is_some(),
            )
        });
        assert!(sees_x && !sees_y);
        assert!(context.assumption(&y).is_some());
    });
}

/// Two guards, one registered under the other, stand one under the other for a retry: each in the frame it was registered in, the inner one's inside the outer one's. Registered again side by side in one frame, the inner guard's reduced spelling would rest on nothing of the outer.
#[test]
fn a_retry_reads_each_guard_in_the_frame_it_was_registered_in() {
    let mut context = context();
    let (outer, inner) = (guard(&mut context, 0), guard(&mut context, 1));

    let held = context.with_frame(|context| {
        context.refine_scrutinee_spellings(vec![(outer.clone(), outer.clone(), false)], &truth());
        context.with_frame(|context| {
            context
                .refine_scrutinee_spellings(vec![(inner.clone(), inner.clone(), false)], &truth());
            context.hold_frames()
        })
    });

    let frames = context.standing_on(&held, |context| {
        context
            .visible_scrutinee_entries()
            .map(|(frame, guard, _)| (frame, guard.clone()))
            .collect::<Vec<_>>()
    });
    let [(inner_frame, inner_guard), (outer_frame, outer_guard)] = frames.as_slice() else {
        panic!("the retry reads both guards: {frames:?}");
    };
    assert_eq!((inner_guard, outer_guard), (&inner, &outer));
    assert_eq!(*inner_frame, outer_frame + 1);
}

/// A guard's settled spelling is its frame's: settled where the frame is live, it is read by a retry standing on the frame with no second settlement, and forgotten there as it is in the live context where what a key reduces to may have changed everywhere.
#[test]
fn a_settled_spelling_is_read_wherever_its_frame_is_held() {
    let mut context = context();
    let guard = guard(&mut context, 0);
    let settled = Settled {
        compared: truth(),
        unerased: truth(),
    };
    let entry = |context: &Context| {
        context
            .visible_scrutinee_entries()
            .map(|(frame, key, _)| (frame, key.clone()))
            .next()
            .expect("the guard is registered")
    };

    let held = context.with_frame(|context| {
        context.refine_scrutinee_spellings(vec![(guard.clone(), guard.clone(), false)], &truth());
        let (frame, key) = entry(context);
        context.record_settled_key(frame, key, Some(settled), false, None);
        context.hold_frames()
    });

    let read = context.standing_on(&held, |context| {
        let (frame, key) = entry(context);
        context.settled_key(frame, &key).is_some()
    });
    assert!(read, "the retry reads what the frame settled");

    context.typing.frames.forget_spellings();
    let read = context.standing_on(&held, |context| {
        let (frame, key) = entry(context);
        context.settled_key(frame, &key).is_some()
    });
    assert!(
        !read,
        "a frame only a parked problem holds forgets with the rest"
    );
}
