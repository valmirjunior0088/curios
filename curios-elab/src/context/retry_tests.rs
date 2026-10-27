//! A retry stands on the frames its problem holds: its locals, refinements, witnesses and guards, and an equation withheld for an arm.

use {
    super::test_support::{context, nat},
    crate::*,
    curios_core::{Intrinsic, Nat, Term},
};

fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
}

/// An arm re-types `x` under its own name; a problem parked there is retried where `x` is live at its unspecialized type — the enclosing frame, where a drain or a turnaround after the arm runs it. The retry sees the arm's type, because it stands on the frames the problem holds, the arm's among them.
#[test]
fn a_retry_sees_a_local_at_the_type_its_problem_holds() {
    let mut context = context();
    let x = context.fresh(Some("x"));

    context.with_frame(|context| {
        context.assume(&x, &nat());
        let held = context.with_frame(|context| {
            context.assume(&x, &bool_type());
            context.hold_frames()
        });

        assert_eq!(context.assumption(&x), Some(&nat()));
        let seen = context.standing_on(&held, |context| context.assumption(&x).cloned());
        assert_eq!(seen, Some(bool_type()));
        assert_eq!(context.assumption(&x), Some(&nat()));
    });
}

/// A refinement of the context a retry happens to run in is none of the problem's: the problem parked before the arm registering it was entered, so the retry, which sets that arm's frame aside, decides it without the equation, and the arm has it again once the retry is over.
#[test]
fn a_retry_does_not_see_a_refinement_of_the_context_it_runs_in() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0_usize)));

    context.with_frame(|context| {
        context.assume(&x, &nat());
        let held = context.hold_frames();

        context.with_frame(|context| {
            context.refine(&x, &zero);
            assert_eq!(context.var_reduct(&x), Some(&zero));

            let seen = context.standing_on(&held, |context| context.var_reduct(&x).cloned());
            assert_eq!(seen, None);
            assert_eq!(context.var_reduct(&x), Some(&zero));
        });
    });
}

/// A frame's guards are read back in the order they were recorded, by the context and by a judgment standing on the frame alike. A proof the elaborator writes admits its guards as it meets them, so their order is the proof's; a hash map's own order differs from one run to the next, and eight keys agreeing with it by chance is one run in forty thousand. Mutation-checked: reading the store back in its map's order fails it.
#[test]
fn a_frames_guards_are_read_back_in_the_order_they_were_recorded() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let guards = (0..8_usize)
        .map(|n| {
            Term::apply(
                Term::free_var(&f),
                [Term::intrinsic(Intrinsic::Nat(Nat::new(n)))],
            )
        })
        .collect::<Vec<_>>();
    let recorded = |context: &Context| {
        context
            .visible_scrutinee_entries()
            .map(|(_, guard, _)| guard.clone())
            .collect::<Vec<_>>()
    };

    context.with_frame(|context| {
        for guard in &guards {
            context.refine_scrutinee_spellings(vec![(guard.clone(), guard.clone(), false)], &truth);
        }
        assert_eq!(recorded(context), guards);

        let held = context.hold_frames();
        let stood = context.standing_on(&held, |context| recorded(context));
        assert_eq!(stood, guards);
    });
}

/// A frame's definitions are read back in the order they were defined, as its guards are: a report names an unfolded `rec` group by the first definition of its structure it finds, and what holds the frame reads them in that order too. Eight names agreeing with a hash map's own order by chance is one run in forty thousand.
#[test]
fn a_frames_definitions_are_read_back_in_the_order_they_were_defined() {
    let mut context = context();
    let names = (0..8_usize)
        .map(|n| context.fresh(Some(&format!("d{n}"))))
        .collect::<Vec<_>>();

    let held = context.with_frame(|context| {
        for (n, name) in names.iter().enumerate() {
            context.define(name, &Term::intrinsic(Intrinsic::Nat(Nat::new(n))), None);
        }

        context.hold_frames()
    });

    // Where the frame has left, no frame in view defines any of them, so a judgment standing on it reads every one.
    let defined = context
        .typing
        .frames
        .held_definitions_apart(&held)
        .into_iter()
        .map(|(name, _)| name)
        .collect::<Vec<_>>();
    assert_eq!(defined, names);
}

/// A metavariable born in a retry is born in the problem's context: its telescope holds the locals of the frames the problem holds, and none of the live context's, which its spine would otherwise carry into a term that never bound them.
#[test]
fn a_metavariable_born_in_a_retry_has_exactly_the_locals_its_problem_holds() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));

    let held = context.with_frame(|context| {
        context.assume(&a, &nat());
        context.hold_frames()
    });

    context.with_frame(|context| {
        context.assume(&b, &nat());
        let names = context.standing_on(&held, |context| {
            let (telescope, _) = context.identity_snapshot();
            telescope.iter().map(|(name, _)| *name).collect::<Vec<_>>()
        });
        assert_eq!(names, vec![a]);
    });
}

/// A retry resolves witnesses through the `use` binders of the frames its problem holds, never through the live context's.
#[test]
fn a_retry_resolves_through_the_witness_scope_its_problem_holds() {
    let mut context = context();
    let live = context.fresh(Some("live"));

    let held = context.with_frame(|context| context.hold_frames());

    context.with_frame(|context| {
        context.assume_witness(&live, &nat());
        let scope = context.standing_on(&held, |context| {
            context.witness_scope().cloned().collect::<Vec<_>>()
        });
        assert!(scope.is_empty(), "{scope:?}");
        assert_eq!(context.witness_scope().count(), 1);
    });
}

/// A retry inside a retry sets the outer retry's frames aside in turn, and gives them back when it ends: the live context the outer retry set aside stays aside until the outer retry is over.
#[test]
fn a_retry_inside_a_retry_gives_the_outer_one_its_frames_back() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let live = context.fresh(Some("live"));

    let outer = context.with_frame(|context| {
        context.assume(&a, &nat());
        context.hold_frames()
    });
    let inner = context.with_frame(|context| {
        context.assume(&b, &nat());
        context.hold_frames()
    });

    context.with_frame(|context| {
        context.assume(&live, &nat());
        context.standing_on(&outer, |context| {
            let (sees_a, sees_b) = context.standing_on(&inner, |context| {
                (
                    context.assumption(&a).is_some(),
                    context.assumption(&b).is_some(),
                )
            });
            assert!(!sees_a && sees_b);
            assert!(context.assumption(&a).is_some());
            assert!(
                context.assumption(&live).is_none(),
                "the outer retry's frames are back"
            );
        });
        assert!(context.assumption(&live).is_some());
    });
}

/// An equation whose scrutinee names no local once an arm refines a variable answers nothing in that arm, and answers again after it. The kernel restates the equations in force under an arm's solution and records none under a closed spelling, so an equation left answering here would accept in the arm a proof the kernel refuses.
///
/// The control is an equation that names a second variable the arm does not refine: its instance still names a local, the kernel still records it, and it answers at that instance, the recorded spelling stepping aside for the arm.
///
/// Mutation-checked: with `Context::refine` withholding no equation its refinement closes, the guard's instance at the arm's value is in force there, a closed term the kernel records nothing under.
#[test]
fn an_equation_a_refinement_closes_is_withheld_for_the_arm() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let m = context.fresh(Some("m"));
    let literal = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let closing = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), literal(3)));
    let open = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), Term::free_var(&m)));

    context.with_frame(|context| {
        context.assume(&n, &nat());
        context.assume(&m, &nat());
        context.refine_scrutinee_spellings(
            vec![
                (closing.clone(), closing.clone(), false),
                (open.clone(), open.clone(), false),
            ],
            &truth,
        );
        assert_eq!(
            context.scrutinee_reduct(&closing, &closing),
            Some(truth.clone())
        );

        context.with_frame(|context| {
            context.refine(&n, &literal(5));

            assert_eq!(
                context.scrutinee_reduct(&closing, &closing),
                None,
                "the guard is about a closed term in this arm"
            );
            let instance = Term::intrinsic(Intrinsic::nat_lt(literal(5), Term::free_var(&m)));
            assert_eq!(
                context.scrutinee_reduct(&instance, &instance),
                Some(truth.clone()),
                "an equation that still names a local stands, at the instance the arm is checked at"
            );
            assert_eq!(
                context.scrutinee_reduct(&open, &open),
                None,
                "and its recorded spelling steps aside for the instance"
            );
            assert_eq!(
                context
                    .visible_scrutinee_entries()
                    .map(|(_, key, _)| key.clone())
                    .collect::<Vec<_>>(),
                vec![instance]
            );
        });

        assert_eq!(
            context.scrutinee_reduct(&open, &open),
            Some(truth.clone()),
            "which answers again once the arm is left"
        );

        assert_eq!(
            context.scrutinee_reduct(&closing, &closing),
            Some(truth.clone()),
            "and the guard answers again once the arm is left"
        );
    });
}

/// A withheld equation stays withheld for a judgment standing on the frames it was withheld in, as what is judged of a metavariable born there and a parked problem's retry do: the entry that withholds it is its arm's frame's, and shadows the equation there as it did in the arm.
#[test]
fn a_withheld_equation_stays_withheld_where_its_frames_are_stood_on() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let literal = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));
    let guard = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), literal(3)));

    let (outside, inside) = context.with_frame(|context| {
        context.assume(&n, &nat());
        context.refine_scrutinee_spellings(vec![(guard.clone(), guard.clone(), false)], &truth);
        let outside = context.hold_frames();

        let inside = context.with_frame(|context| {
            context.refine(&n, &literal(5));
            context.hold_frames()
        });

        (outside, inside)
    });

    assert_eq!(
        context.at_birth(&outside, |context| context.scrutinee_reduct(&guard, &guard)),
        Some(truth),
        "the guard's own arm answers"
    );
    assert_eq!(
        context.at_birth(&inside, |context| context.scrutinee_reduct(&guard, &guard)),
        None,
        "the arm that closed it does not"
    );
}
