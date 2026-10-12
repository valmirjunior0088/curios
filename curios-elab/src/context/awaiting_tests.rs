//! Which term awaits a metavariable: where the relation is written, where it moves, and that it ends with the term.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, MetavarId, Nat, Term},
    std::collections::BTreeSet,
};

fn context() -> Context<'static> {
    Context::with_default_budget(SYNTAX)
}

fn birth(context: &mut Context, id: usize) {
    context.birth_metavar(
        MetavarId(id),
        Vec::<(Free, Term)>::new(),
        Term::type_ground(),
    );
}

fn awaited(context: &Context, id: usize) -> bool {
    context.awaits_any(&Term::hole(id))
}

/// A check parked on a type that mentions each of `ids`, so that the problem watches them together.
fn park_on(context: &mut Context, ids: &[usize]) {
    let literal = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let expected = Term::tuple(ids.iter().map(|id| Term::hole(*id)).collect::<Vec<_>>());
    let (placeholder, _) = context.fresh_placeholder(expected.clone(), None);
    context.park(
        ParkedWork::Checking {
            term: literal.clone(),
            expected,
            placeholder,
        },
        literal,
    );
}

#[test]
fn a_metavariable_is_awaited_by_the_term_it_is_born_in_until_that_term_completes() {
    let mut context = context();
    birth(&mut context, 0);

    context
        .awaiting(|context| {
            birth(context, 1);
            assert!(awaited(context, 1));
            assert!(!awaited(context, 0), "born outside any term");
            Ok(())
        })
        .unwrap();

    assert!(!awaited(&context, 1), "its term has completed");
}

// The inner term's metavariable is held to the outer's by the solution that mentions it, so the outer term awaits it once the inner has completed; one no solution mentions ends with its term.
#[test]
fn a_solution_hands_what_it_mentions_to_the_outer_term() {
    let mut context = context();

    context
        .awaiting(|context| {
            birth(context, 0);
            context.awaiting(|context| {
                birth(context, 1);
                birth(context, 2);
                context.solve_metavar(MetavarId(0), Term::tuple(vec![Term::hole(1)]));
                Ok(())
            })?;
            assert!(
                awaited(context, 1),
                "held to the outer term's by a solution"
            );
            assert!(!awaited(context, 2), "held to nothing");
            Ok(())
        })
        .unwrap();

    assert!(!awaited(&context, 1));
}

#[test]
fn a_parked_problem_hands_what_it_watches_to_the_outer_term() {
    let mut context = context();

    context
        .awaiting(|context| {
            birth(context, 0);
            context.awaiting(|context| {
                birth(context, 1);
                park_on(context, &[0, 1]);
                Ok(())
            })?;
            assert!(awaited(context, 1));
            Ok(())
        })
        .unwrap();
}

// A link has no direction: a lambda's unannotated domain, born under no term, waits with the call's implicit its body holds it to.
#[test]
fn one_no_term_awaits_is_awaited_once_linked_to_one_a_term_does() {
    let mut context = context();
    birth(&mut context, 0);

    context
        .awaiting(|context| {
            birth(context, 1);
            park_on(context, &[0, 1]);
            assert!(awaited(context, 0));
            Ok(())
        })
        .unwrap();

    assert!(!awaited(&context, 0));
}

#[test]
fn a_rollback_returns_a_metavariable_to_the_term_that_awaited_it() {
    let mut context = context();

    context
        .awaiting(|context| {
            birth(context, 0);
            context.awaiting(|context| {
                birth(context, 1);
                let mark = context.solution_mark();
                context.solve_metavar(MetavarId(0), Term::tuple(vec![Term::hole(1)]));
                context.rollback_solutions(mark);
                context.end_solutions(mark);
                Ok(())
            })?;
            assert!(
                !awaited(context, 1),
                "the solution that held it was unwound"
            );
            Ok(())
        })
        .unwrap();
}

// A wake finds a problem wherever a solution lands, inside terms that are not the problem's: what its retry mints is the awaiting term's that awaits what it watched, and no term's that happened to be read.
#[test]
fn a_retried_problem_reads_under_the_term_that_awaits_what_it_watched() {
    let mut context = context();

    context
        .awaiting(|context| {
            birth(context, 0);
            context.awaiting(|context| {
                context.retried_under(&BTreeSet::from([MetavarId(0)]), |context| {
                    birth(context, 1);
                });
                Ok(())
            })?;
            assert!(
                awaited(context, 1),
                "awaited by the term the problem is awaited by"
            );
            Ok(())
        })
        .unwrap();
}

#[test]
fn a_term_adopts_the_holes_it_reads_its_parts_against() {
    let mut context = context();
    birth(&mut context, 0);

    context
        .awaiting(|context| {
            context.adopt(&Term::hole(0));
            assert!(awaited(context, 0));
            Ok(())
        })
        .unwrap();

    assert!(!awaited(&context, 0));
}

// A metavariable re-expressed in another's context is solved to a stand-in, and a problem parked under the first's spelling waits on the stand-in: read by mention alone it would watch nothing, and nothing would wake it or await it.
#[test]
fn a_problem_waits_on_what_its_metavariables_were_solved_to() {
    let mut context = context();
    birth(&mut context, 0);
    birth(&mut context, 1);
    context.solve_metavar(MetavarId(0), Term::hole(1));
    park_on(&mut context, &[0]);

    context.solve_metavar(MetavarId(1), Term::type_ground());

    assert_eq!(context.wake_parked().len(), 1);
}

// A parked check states the choice a completing term may take for it, and the choices are taken in the order they are declared: a shape commits less than a default, and a default less than a lambda's check.
#[test]
fn a_parked_check_states_the_choice_its_term_may_take() {
    let parked = |term: Term| ParkedWork::Checking {
        term,
        expected: Term::hole(0),
        placeholder: MetavarId(1),
    };
    let number = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let binder = Free::local(0, None);
    let lambda = Term::func([(binder, Term::type_ground())], Term::free_var(&binder));

    assert_eq!(
        parked(Term::tuple(vec![number.clone()])).choice(),
        Some(Choice::Shape)
    );
    assert_eq!(
        parked(Term::num_lit_char('a')).choice(),
        Some(Choice::Numeral)
    );
    assert_eq!(parked(lambda).choice(), Some(Choice::Lambda));
    assert_eq!(
        parked(Term::tuple(Vec::<Term>::new())).choice(),
        None,
        "an empty tuple is its own type's"
    );
    assert_eq!(
        parked(number).choice(),
        None,
        "a realized number is no literal"
    );
    assert!(Choice::Shape < Choice::Numeral && Choice::Numeral < Choice::Lambda);
}
