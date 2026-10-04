//! A scrutinee entry's reduced spelling: what leaves a settled one alone, and what forgets it.

use {
    super::test_support::{context, nat},
    crate::{Context, refine_head},
    curios_core::{Free, Intrinsic, MetavarId, Term},
};

/// What every fixture here reduces under: `small(x) = x < 10` and `n: Nat`.
struct Guarded {
    context: Context,
    small: Free,
    n: Free,
}

fn guarded() -> Guarded {
    let mut context = context();
    let small = context.fresh(Some("small"));
    let n = context.fresh(Some("n"));
    let (small_type, small_body) = below_ten(&mut context);

    context.assume(&small, &small_type);
    context.define(&small, &small_body, None);
    context.assume(&n, &Term::intrinsic(Intrinsic::NatType));

    Guarded { context, small, n }
}

/// `(x: Nat) -> Bool` and `(x) => match true | true => x < 10 | false => false end`: a guard through it reaches the comparison only by reducing, where a body that *is* the comparison would be met by the spelling its dispatch resolves to, and no reduced spelling would be asked for.
fn below_ten(context: &mut Context) -> (Term, Term) {
    let x = context.fresh(Some("x"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let bool_type = Term::intrinsic(Intrinsic::BoolType);

    (
        Term::func_type([(x, nat_type.clone())], bool_type.clone()),
        Term::func(
            [(x, nat_type)],
            Term::bool_match(
                truth(),
                None,
                bool_type,
                Term::intrinsic(Intrinsic::Bool(false)),
                Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&x), nat(10))),
            ),
        ),
    )
}

/// `match subject < 10 | true => Nat | false => Bool`: a term whose reduction presents `subject < 10` stuck, the guard `small(subject)` one unfolding down.
fn carrier(subject: Term) -> Term {
    Term::bool_match(
        Term::intrinsic(Intrinsic::nat_lt(subject, nat(10))),
        None,
        Term::type_ground(),
        Term::intrinsic(Intrinsic::BoolType),
        Term::intrinsic(Intrinsic::NatType),
    )
}

fn truth() -> Term {
    Term::intrinsic(Intrinsic::Bool(true))
}

/// The frame and key of the innermost scrutinee entry in sight.
fn entry(context: &Context) -> (usize, Term) {
    context
        .visible_scrutinee_entries()
        .map(|(frame, key, _)| (frame, key.clone()))
        .next()
        .expect("an entry is registered")
}

/// Whether the entry at `at` has a settled spelling a probe would read.
fn settled(context: &Context, at: &(usize, Term)) -> bool {
    context.settled_key(at.0, &at.1).is_some()
}

/// Register `guard` as true in the frame `context` is in, and reduce `probed` so that a probe asks for the entry's reduced spelling.
fn settle(context: &mut Context, guard: &Term, probed: Term) -> (usize, Term) {
    refine_head(context, guard, &truth()).expect("the arm's equation registers");
    let at = entry(context);
    assert!(!settled(context, &at), "nothing has asked for it yet");
    super::reduce(context, probed).expect("reduces");
    assert!(settled(context, &at), "the probe settled it");

    at
}

/// A settled spelling is left alone by what cannot change what its key reduces to: a refinement registered in a frame inside its entry's, that frame's exit, and a suppression bracket. Each of the three cleared every spelling while they were kept in a table beside the reducts, and settling again after them was most of what the escalation cost.
#[test]
fn a_settled_spelling_outlives_what_cannot_change_it() {
    let Guarded {
        mut context,
        small,
        n,
    } = guarded();
    let guard = Term::apply(Term::free_var(&small), [Term::free_var(&n)]);

    context.with_frame(|context| {
        let at = settle(context, &guard, carrier(Term::free_var(&n)));

        context.with_frame(|context| {
            let other = Term::intrinsic(Intrinsic::nat_lt(Term::free_var(&n), nat(3)));
            refine_head(context, &other, &truth()).expect("the arm's equation registers");
            assert!(settled(context, &at), "a registration inside its frame");
        });
        assert!(settled(context, &at), "an inner frame's exit");

        context.with_suppressed_refinements(|_| ());
        assert!(settled(context, &at), "a suppression bracket");
    });
}

/// And it is forgotten by what can: its key registered again, which is another entry; a redefinition, which may change what any key reduces to; and the declaration's boundary, which nothing a budget paid for outlives.
///
/// Mutation-checked: keeping the spelling where its key is registered again, where a name is redefined, or where the budget is restored each fails its row.
#[test]
fn a_settled_spelling_is_forgotten_where_its_key_may_reduce_otherwise() {
    type Forget = fn(&mut Context, &Free, &Term);
    let registered_again: Forget = |context, _, guard| {
        refine_head(context, guard, &Term::intrinsic(Intrinsic::Bool(false)))
            .expect("the arm's equation registers");
    };
    let redefined: Forget = |context, small, _| {
        let (_, body) = below_ten(context);
        context.define(small, &body, None);
    };
    let restored: Forget = |context, _, _| context.restore_budget();

    for (what, forget) in [
        ("its key registered again", registered_again),
        ("a redefinition", redefined),
        ("the declaration's boundary", restored),
    ] {
        let Guarded {
            mut context,
            small,
            n,
        } = guarded();
        let guard = Term::apply(Term::free_var(&small), [Term::free_var(&n)]);

        context.with_frame(|context| {
            let at = settle(context, &guard, carrier(Term::free_var(&n)));
            forget(context, &small, &guard);
            assert!(!settled(context, &at), "{what} left the spelling standing");
        });
    }
}

/// A fresh definition forgets the spellings that were stuck on the name it defines, as it forgets the reducts naming it, and leaves the others.
///
/// Mutation-checked: with a fresh definition leaving every spelling alone, the entry stuck on the name stays settled at its stuck form.
#[test]
fn a_fresh_definition_forgets_the_spellings_stuck_on_it() {
    let Guarded {
        mut context,
        small,
        n,
    } = guarded();
    let later = context.fresh(Some("later"));
    let (later_type, later_body) = below_ten(&mut context);
    context.assume(&later, &later_type);
    let defined = Term::apply(Term::free_var(&small), [Term::free_var(&n)]);
    let stuck = Term::apply(Term::free_var(&later), [Term::free_var(&n)]);

    context.with_frame(|context| {
        let defined_at = settle(context, &defined, carrier(Term::free_var(&n)));
        context.with_frame(|context| {
            let stuck_at = settle(context, &stuck, carrier(Term::free_var(&n)));

            context.define(&later, &later_body, None);

            assert!(!settled(context, &stuck_at));
            assert!(settled(context, &defined_at));
        });
    });
}

/// A spelling settled while its key held an unsolved metavariable is asked for again once a solution has landed, since the solution may be what its key was stuck on; one that held none is not.
///
/// Mutation-checked: with the solutions committed since left unread, the spelling over the metavariable stays settled after the solve.
#[test]
fn a_spelling_that_held_an_unsolved_metavariable_is_settled_again_after_a_solution() {
    let Guarded {
        mut context,
        small,
        n,
    } = guarded();
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::intrinsic(Intrinsic::NatType),
    );
    let ground = Term::free_var(&n);
    let open = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), Term::hole(0)));

    context.with_frame(|context| {
        let ground_at = settle(
            context,
            &Term::apply(Term::free_var(&small), [ground.clone()]),
            carrier(ground),
        );
        context.with_frame(|context| {
            let open_at = settle(
                context,
                &Term::apply(Term::free_var(&small), [open.clone()]),
                carrier(open),
            );

            context.solve_metavar(MetavarId(0), nat(1));

            assert!(!settled(context, &open_at));
            assert!(settled(context, &ground_at));
        });
    });
}

/// A spelling dies with its entry's frame: the same key registered in a frame entered at the same depth is another entry, with nothing settled.
///
/// Mutation-checked: with a left frame's spellings kept, the second entry reads the first one's.
#[test]
fn a_settled_spelling_dies_with_its_frame() {
    let Guarded {
        mut context,
        small,
        n,
    } = guarded();
    let guard = Term::apply(Term::free_var(&small), [Term::free_var(&n)]);

    context.with_frame(|context| {
        settle(context, &guard, carrier(Term::free_var(&n)));
    });
    context.with_frame(|context| {
        refine_head(context, &guard, &truth()).expect("the arm's equation registers");
        let at = entry(context);

        assert!(!settled(context, &at));
    });
}
