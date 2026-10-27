//! An attempt and its rollback: what a state set aside keeps, what is minted, what an oracle records, and the reducts and memos a rollback clears or keeps.

use {
    super::test_support::{context, nat, unit_of_two},
    crate::*,
    curios_core::{
        Free, Global, Intrinsic, Level, MetavarId, Minted, Mints, Nat, Term,
        UniverseConstraintKind, UniverseConstraintOrigin, UniverseMetaId, UniverseRole,
    },
    curios_utilities::Qualifier,
    std::{collections::BTreeSet, rc::Rc},
};

#[test]
fn universe_dependencies_of_a_solved_meta_follow_only_its_materialized_solution() {
    let mut context = context();
    let result = UniverseMetaId(0);
    let telescope = UniverseMetaId(1);
    let solution = UniverseMetaId(2);

    let x = context.fresh(Some("x"));
    context.birth_metavar(
        MetavarId(0),
        vec![(x, Term::type_at(Level::meta(telescope)))],
        Term::type_at(Level::meta(result)),
    );
    context.solve_metavar(MetavarId(0), Term::type_at(Level::meta(solution)));

    assert_eq!(context.universe_metas_in(&Term::hole(0)), [solution].into());
}

/// A rolled-back bracket withdraws what it recorded for the totality obligations, and what was recorded before it stands: a term settled inside belonged to a candidate that is discarded.
#[test]
fn a_rollback_forgets_the_terms_settled_inside_it() {
    let mut context = context();
    let nat = Term::intrinsic(Intrinsic::NatType);
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    context.record_checked(&literal(0), &nat);

    let mark = context.solution_mark();
    // The type a candidate's subterm settled at, while the metavariable standing for it was solved.
    context.record_checked(&literal(1), &Term::hole(0));
    context.rollback_solutions(mark);
    context.end_solutions(mark);

    assert_eq!(context.checked().len(), 1);
    assert_eq!(context.checked()[0].0, literal(0));
}

#[test]
fn universe_dependencies_of_an_unsolved_meta_keep_its_birth_context() {
    let mut context = context();
    let result = UniverseMetaId(0);
    let telescope = UniverseMetaId(1);

    let x = context.fresh(Some("x"));
    context.birth_metavar(
        MetavarId(0),
        vec![(x, Term::type_at(Level::meta(telescope)))],
        Term::type_at(Level::meta(result)),
    );

    assert_eq!(
        context.universe_metas_in(&Term::hole(0)),
        [result, telescope].into()
    );
}

/// A declaration's state starts above what its own lowering minted, so the identities it mints are the ones it would mint elaborated alone, whatever was elaborated before it.
#[test]
fn an_attempt_mints_above_what_its_own_declaration_lowered() {
    let mut context = context();
    let [a, b] = unit_of_two(&mut context);
    let lowered = |binders| Mints {
        binders,
        ..Mints::default()
    };
    let mut seeded = Minted::default();
    seeded.entry = lowered(9);
    seeded.declare(&Rc::new(lowered(2)), [a]);
    seeded.declare(&Rc::new(lowered(5)), [b]);
    context.seed(&Rc::new(seeded));
    let minted = |context: &mut Context, names: &[&Global]| {
        context.attempt(names);
        [context.fresh(None), context.fresh(None)]
    };

    let alone = minted(&mut context, &[&b]);
    let [in_a, _] = minted(&mut context, &[&a]);
    let [in_entry, _] = minted(&mut context, &[]);

    assert_eq!(alone, [Free::local(5, None), Free::local(6, None)]);
    assert_eq!(in_a, Free::local(2, None));
    assert_eq!(in_entry, Free::local(9, None));
    assert_eq!(minted(&mut context, &[&b]), alone);
}

/// A state set aside takes what its attempt solved with it, and holds it wherever it is put back: the next attempt mints the same identity and reads nothing of it.
#[test]
fn a_state_set_aside_keeps_what_its_attempt_solved() {
    let mut context = context();
    context.attempt(&[]);
    context.birth_metavar(
        MetavarId(0),
        Vec::new(),
        Term::intrinsic(Intrinsic::NatType),
    );
    context.solve_metavar(
        MetavarId(0),
        Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))),
    );

    let state = context.set_aside();
    context.attempt(&[]);

    assert!(context.metavar_solution(MetavarId(0)).is_none());
    assert!(context.within(state, |context| {
        context.metavar_solution(MetavarId(0)).is_some()
    }));
    assert!(context.metavar_solution(MetavarId(0)).is_none());
}

/// A metavariable born at an id the counter has not reached is never minted again: the next fresh id lies past it. A fixture states its metavariables that way, and a mint landing on one births a second metavariable over its record — which is how a candidate's re-validation, minting while it checks, would replace a test's solved hole with its own.
#[test]
fn a_metavariable_born_ahead_of_the_counter_is_never_minted_again() {
    let mut context = context();
    context.birth_metavar(MetavarId(3), Vec::new(), Term::type_ground());

    let minted = context.mint_metavar();

    assert!(minted.0 > 3, "minted ?{} at or below a born id", minted.0);
}

/// Inside an oracle bracket, a term whose elaboration writes is elaborated once and answered from the bracket's table after: the verdict is all an oracle hands back, and a second run would repeat the first one's writes and reach it again. Outside a bracket the cache's purity gate keeps it out, and each ask runs — the control.
#[test]
fn an_oracle_elaborates_a_term_that_writes_once() {
    let mut context = context();
    let term = Term::intrinsic(Intrinsic::nat_add(nat(), nat()));
    let elaborate_writing = |context: &mut Context, runs: &mut usize| {
        context
            .get_or_init_elaborated(&term, None, |context| {
                *runs += 1;
                let hole = context.mint_metavar();
                context.birth_metavar(hole, Vec::new(), nat());
                Ok::<_, ()>((term.clone(), Term::type_ground()))
            })
            .expect("the run succeeds");
    };

    let mut outside = 0;
    elaborate_writing(&mut context, &mut outside);
    elaborate_writing(&mut context, &mut outside);
    let inside = context.with_oracle(|context| {
        let mut inside = 0;
        elaborate_writing(context, &mut inside);
        elaborate_writing(context, &mut inside);
        inside
    });

    assert_eq!(outside, 2);
    assert_eq!(inside, 1);
}

/// What an oracle declined is that oracle's record. A site that wants to park inside one is refused and the oracle records it; an oracle inside it starts with nothing declined and leaves the enclosing record as it found it; and outside every oracle a site may park and nothing is recorded.
#[test]
fn a_declined_park_is_recorded_by_the_oracle_it_was_declined_in() {
    let mut context = context();
    assert!(context.may_park());
    assert!(!context.declined_to_park());

    let recorded = context.with_oracle(|context| {
        let before = context.declined_to_park();
        let inner = context.with_oracle(|context| (context.may_park(), context.declined_to_park()));
        let after = context.declined_to_park();
        let may = context.may_park();

        (before, inner, after, may, context.declined_to_park())
    });

    assert_eq!(recorded, (false, (false, true), false, false, true));
    assert!(!context.declined_to_park());
    assert!(context.may_park());
}

/// A rollback invalidates what can rest on what it undid. One that unwound nothing keeps the cached reducts — a witness probe rolls back after every trial, and a clear at each would throw away the reducts the next node needs — while one that unwound a solution clears them, since a reduct cached since may have read it.
#[test]
fn a_rollback_keeps_the_reducts_unless_it_unwound_a_solution() {
    let mut context = context();
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let sum = Term::intrinsic(Intrinsic::nat_add(literal(1), literal(2)));
    context.reduce(sum.clone(), &literal(3));

    let mark = context.solution_mark();
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(context.cached_reduced(&sum), Some(literal(3)));

    let mark = context.solution_mark();
    context.birth_metavar(MetavarId(0), Vec::new(), nat());
    context.solve_metavar(MetavarId(0), literal(0));
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(context.cached_reduced(&sum), None);
}

/// A reduct is remembered for the declaration that computed it and no longer. A hit on it is free, so an entry that outlived its declaration would let the declarations compiled first decide what a later one can afford.
///
/// Mutation-checked: keeping the reduction table across [`Context::restore_budget`] fails it.
#[test]
fn a_closed_reduct_does_not_outlive_its_declaration() {
    let mut context = context();
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let sum = Term::intrinsic(Intrinsic::nat_add(literal(1), literal(2)));
    context.reduce(sum.clone(), &literal(3));
    assert_eq!(context.cached_reduced(&sum), Some(literal(3)));

    context.restore_budget();
    assert_eq!(context.cached_reduced(&sum), None);
}

/// What a declaration spends does not depend on what the declarations before it reduced. The first declaration here reduces a definition's body and then the definition's name, and the second reduces the name again: the second spends exactly what reducing it costs a context that reduced nothing before it — `curios-cert`'s test of the same name, put to the elaborator.
///
/// Mutation-checked: keeping the reduction table across [`Context::restore_budget`] fails it.
#[test]
fn what_a_declaration_spends_does_not_depend_on_what_was_reduced_before_it() {
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let name = Free::from(&Global::Authored(Qualifier::from(["chain"])));
    let body = (0..64).fold(literal(0), |sum, _| {
        Term::intrinsic(Intrinsic::nat_add(sum, literal(1)))
    });
    let occurrence = Term::free_var(&name);
    let defining = || {
        let mut context = context();
        context.define(&name, &body, None);
        context
    };
    let spent = |context: &mut Context, term: Term| {
        let before = context.consumed().units();
        reduce(context, term).expect("reduces");
        context.consumed().units() - before
    };

    let alone = spent(&mut defining(), occurrence.clone());

    let mut after = defining();
    spent(&mut after, body.clone());
    spent(&mut after, occurrence.clone());
    after.restore_budget();
    let again = spent(&mut after, occurrence);

    assert!(alone > 1, "reducing the name reduces its body");
    assert_eq!(
        again, alone,
        "the name costs what it costs a context that reduced nothing first"
    );
}

#[test]
fn a_proof_credits_the_written_binder_a_local_was_opened_from_and_a_rollback_withdraws_it() {
    let mut context = context();
    let declaration = Global::Authored(Qualifier::from(["f"]));
    context.enter_item(Some(declaration));
    let opened = context.fresh_for(Some("p"), Some(2));
    let unrelated = context.fresh(Some("q"));

    let mark = context.solution_mark();
    context.credit(&Term::free_var(&opened));
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert!(
        context.credited().is_empty(),
        "a proof written inside what is rolled back reads nothing that stands"
    );

    context.credit(&Term::free_var(&opened));
    context.credit(&Term::free_var(&unrelated));
    assert_eq!(context.credited(), BTreeSet::from([(Some(declaration), 2)]));
}

/// An elaboration is remembered by the reduction it ran under, in the cache and in an oracle's table: a run under plain reduction, which asks the elaborator's conversion nothing, is not answered from what a judgment's run remembered, nor a judgment's from plain reduction's. A judgment's run may rest on a fold or an equation its conversion decided, which plain reduction refuses, and asking writes nothing, so the purity gate lets such a run in.
///
/// Mutation-checked for each table: with its key read as a judgment's whichever reduction asks, the plain run is answered by the judgment's.
#[test]
fn an_elaboration_is_remembered_by_the_reduction_it_ran_under() {
    let mut context = context();
    // Two terms, so the oracle's half is not answered from what the cache's half remembered.
    let kept = Term::intrinsic(Intrinsic::nat_add(nat(), nat()));
    let written = Term::intrinsic(Intrinsic::NatMul(nat(), nat()));
    let elaborate_pure = |context: &mut Context, runs: &mut usize| {
        context
            .get_or_init_elaborated(&kept, None, |_| {
                *runs += 1;
                Ok::<_, ()>((kept.clone(), Term::type_ground()))
            })
            .expect("the run succeeds");
    };
    let elaborate_writing = |context: &mut Context, runs: &mut usize| {
        context
            .get_or_init_elaborated(&written, None, |context| {
                *runs += 1;
                let hole = context.mint_metavar();
                context.birth_metavar(hole, Vec::new(), nat());
                Ok::<_, ()>((written.clone(), Term::type_ground()))
            })
            .expect("the run succeeds");
    };
    // A judgment's run, plain reduction's twice, and a judgment's again: one run each where each keeps its own.
    let across = |context: &mut Context, elaborate: &dyn Fn(&mut Context, &mut usize)| {
        let (mut judged, mut plain) = (0, 0);
        elaborate(context, &mut judged);
        context.plainly(|context| {
            elaborate(context, &mut plain);
            elaborate(context, &mut plain);
        });
        elaborate(context, &mut judged);
        (judged, plain)
    };

    assert_eq!(across(&mut context, &elaborate_pure), (1, 1));
    let inside = context.with_oracle(|context| across(context, &elaborate_writing));
    assert_eq!(inside, (1, 1));
}

/// A sort is remembered by the reduction that read it, as an elaboration is: what plain reduction classified is not answered where a judgment asks, nor a judgment's where plain reduction does.
///
/// Mutation-checked both ways: with the table read as a judgment's whichever reduction asks, plain reduction finds nothing of what it filed, and with the answer filed there whichever reduction read it, a judgment is handed plain reduction's.
#[test]
fn a_sort_is_remembered_by_the_reduction_that_read_it() {
    let mut context = context();
    let read = Term::intrinsic(Intrinsic::BoolType);
    let judged = Term::prop();

    context
        .plainly(|context| Sort::of(context, &read))
        .expect("classifies");
    assert!(context.cached_sort(&read).is_none());
    assert!(
        context
            .plainly(|context| context.cached_sort(&read))
            .is_some()
    );

    Sort::of(&mut context, &judged).expect("classifies");
    assert!(
        context
            .plainly(|context| context.cached_sort(&judged))
            .is_none()
    );
    assert!(context.cached_sort(&judged).is_some());
}

/// A rollback that withdrew level constraints clears the reducts where a judgment's reduction put a question to conversion inside the scope, and only there. A question is answered on the constraints that stand, so a reduct taken inside may rest on one the rollback withdraws; a scope that asked nothing leaves the reducts alone, no rule of reduction reading a level. A reduct over a term that names no local is kept either way: reduction asks conversion nothing about one.
///
/// Mutation-checked both ways: with the reducts kept whatever was asked, the second scope's reduct outlives its constraint, and with them cleared whatever was asked, the first scope loses a reduct it had no reason to.
#[test]
fn a_rollback_that_withdrew_a_level_constraint_clears_what_a_question_may_rest_on() {
    let mut context = context();
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let x = context.fresh(Some("x"));
    context.assume(&x, &nat());
    let sum = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), literal(2)));
    let closed = Term::intrinsic(Intrinsic::nat_add(literal(1), literal(2)));
    context.reduce(closed.clone(), &literal(3));
    let this = Level::meta(context.universes_mut().fresh(UniverseRole::Flexible, None));
    let that = Level::meta(context.universes_mut().fresh(UniverseRole::Flexible, None));
    let equate = |context: &mut Context| {
        context
            .universes_mut()
            .add_eq(
                this.clone(),
                that.clone(),
                UniverseConstraintOrigin::new(UniverseConstraintKind::Conversion),
            )
            .expect("two fresh levels may be one");
    };

    context.reduce(sum.clone(), &sum);
    let mark = context.solution_mark();
    equate(&mut context);
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(
        context.cached_reduced(&sum),
        Some(sum.clone()),
        "no question was asked inside the scope"
    );

    let mark = context.solution_mark();
    equate(&mut context);
    context.note_question();
    context.rollback_solutions(mark);
    context.end_solutions(mark);
    assert_eq!(context.cached_reduced(&sum), None);
    assert_eq!(context.cached_reduced(&closed), Some(literal(3)));
}
