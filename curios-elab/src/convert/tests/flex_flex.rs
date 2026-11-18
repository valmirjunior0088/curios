//! Flex-flex problems: one metavariable on both sides converts through its spines, and two stay blocked until one is solved.

use {
    crate::convert::test_support::*,
    crate::*,
    curios_core::{DefinitionKind, Intrinsic, MetavarId, MetavarOrigin, Subterm, Term, Universes},
};

#[test]
fn flex_flex_equal_id_short_circuits() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ ?0 is trivially true and leaves the metavariable unsolved.
    assert_eq!(conv(&mut context, &Term::hole(0), &Term::hole(0)), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn flex_flex_distinct_is_residual() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetavarId(1), Vec::new(), Term::type_ground());

    // ?0 ≟ ?1 postpones with no way to progress — a residual constraint.
    assert_eq!(
        conv(&mut context, &Term::hole(0), &Term::hole(1)),
        Ok(false)
    );
}

#[test]
fn flex_flex_same_id_converts_through_equal_spines() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());

    // Two occurrences of the same unsolved metavariable whose spines differ syntactically but agree definitionally (`1 + 1` reduces to `2`): the congruence probe discharges the goal without solving anything.
    let sum: Term = Subterm::Intrinsic(Intrinsic::nat_add(nat(1), nat(1))).into();
    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![sum]);
    let that = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(2)]);

    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn flex_flex_same_id_with_disagreeing_spines_stays_blocked() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());

    // Disagreeing spines are not *unequal* — the solution may ignore the slot — so the pair parks rather than mismatching.
    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(1)]);
    let that = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![nat(2)]);

    let outcome = convert_outcome(&mut context, &Term::type_ground(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
}

#[test]
fn flex_flex_distinct_heads_with_a_common_solution_stays_blocked() {
    // Two *distinct* unsolved metavariables over compatible telescopes, met through the same live name, asked by conversion alone: solving one as the other would equate their types, levels included, before the constraints that would pin each apart, so the pair parks and stays undecided. Where a completing term holds the spines it awaits, the pair is solved ([`a_completing_term_solves_two_distinct_heads_the_younger_as_the_older`]).
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    context.birth_metavar(MetavarId(1), vec![(b, nat_type())], nat_type());

    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&x)]);
    let that = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![Term::free_var(&x)]);

    let outcome = convert_outcome(&mut context, &Term::type_ground(), &this, &that);
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
}

/// Two distinct heads met where a completing term holds the spines it awaits, the last resort imitation takes: the younger is solved as the older through the renaming, the older restricted to a stand-in where its birth reaches binders the younger's does not, and the pair converts, asked again with nothing more to solve. Mutation-checked: with the pair postponed there as anywhere, it stays blocked.
#[test]
fn a_completing_term_solves_two_distinct_heads_the_younger_as_the_older() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    context.birth_metavar(MetavarId(1), vec![(b, nat_type())], nat_type());

    let this = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&x)]);
    let that = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![Term::free_var(&x)]);

    let outcome = context
        .holding_spines(|context| convert_outcome(context, &Term::type_ground(), &this, &that));
    assert!(matches!(outcome, Ok(Outcome::Converts)), "{outcome:?}");
    assert!(
        context.metavar_solution(MetavarId(1)).is_some(),
        "the younger is solved as the older"
    );
    // Asked again, the two are one with nothing further to solve.
    assert_eq!(conv(&mut context, &this, &that), Ok(true));
}

/// Two distinct heads whose spines share only one variable, where a completing term holds them: the younger, over the variable alone, is solved as the older, which reads a second variable the younger's context does not hold, so the older is restricted to what both reach before the younger is solved as it. Both end solved through that restriction, and the pair converts.
#[test]
fn a_completing_term_solves_two_heads_sharing_some_variables_through_a_restriction() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let a2 = context.fresh(Some("a2"));
    let b = context.fresh(Some("b"));
    let x = context.fresh(Some("x"));
    let y = context.fresh(Some("y"));
    context.birth_metavar(
        MetavarId(0),
        vec![(a, nat_type()), (a2, nat_type())],
        nat_type(),
    );
    context.birth_metavar(MetavarId(1), vec![(b, nat_type())], nat_type());

    let this = Term::metavar_birthed(
        0,
        MetavarOrigin::Hole,
        vec![Term::free_var(&x), Term::free_var(&y)],
    );
    let that = Term::metavar_birthed(1, MetavarOrigin::Hole, vec![Term::free_var(&x)]);

    let outcome = context
        .holding_spines(|context| convert_outcome(context, &Term::type_ground(), &this, &that));
    assert!(matches!(outcome, Ok(Outcome::Converts)), "{outcome:?}");
    assert!(
        context.metavar_solution(MetavarId(0)).is_some(),
        "the older is restricted"
    );
    assert!(
        context.metavar_solution(MetavarId(1)).is_some(),
        "the younger is solved"
    );
}

/// `flat(?0)` against `flat(1)`, where `flat` forgets its argument: the two are equal whatever `?0` is, and their spines agree at `?0 := 1`.
fn two_calls_of_a_definition_that_forgets_its_argument(context: &mut Context) -> Problem {
    let flat = nominal("flat");
    let argument = context.fresh(Some("n"));
    context.define_declaring(
        &flat,
        &Term::func_type([(argument, nat_type())], nat_type()),
        &Term::func([(argument, nat_type())], nat(0)),
        &DefinitionKind::Authored,
    );
    context.birth_metavar(MetavarId(0), Vec::new(), nat_type());

    Problem {
        type_: nat_type(),
        this: Term::apply(Term::instance(flat, Universes::default()), [Term::hole(0)]),
        that: Term::apply(Term::instance(flat, Universes::default()), [nat(1)]),
        binders: Binders::default(),
        pairs: Pairs::default(),
        relation: Relation::Equal,
    }
}

// Two calls of one definition agree where their arguments do, and agreement that takes a solution is one answer among those a definition that is not injective has: a term's to take where it completes, and none of conversion's, which decides this pair by reduction and leaves the metavariable open. Asked what the spines would solve, the pair offers `?0 := 1` and commits nothing; held, it commits it.
#[test]
fn two_spines_offer_what_they_would_solve_and_hold_it_only_when_asked() {
    let mut context = context();
    let problem = two_calls_of_a_definition_that_forgets_its_argument(&mut context);

    assert_eq!(
        convert(&mut context, &problem.type_, &problem.this, &problem.that),
        Ok(true)
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), None);

    let agreed = HeldConversion {
        solved: vec![(MetavarId(0), nat(1))],
        left: Vec::new(),
    };
    assert_eq!(
        hold_spines(&mut context, &problem, Spines::Offered),
        Ok(Some(agreed.clone()))
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), None);

    assert_eq!(
        hold_spines(&mut context, &problem, Spines::Held),
        Ok(Some(agreed))
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
}
