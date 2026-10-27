//! A metavariable met at a type or a proposition from below or from above, which takes a level of its own, and an equation, which takes the type it meets.

use {
    super::test_support::*,
    crate::*,
    curios_core::{Level, MetavarId, Quality, Subterm, Term},
    std::collections::BTreeSet,
};

// === A metavariable met at below ============================================

// `Type 0 ≤ ?0` solves `?0` to a `Type` at a level of its own over zero, so `Type 1 ≤ ?0` still holds and the levels close. Solved by the equation, `?0` is `Type 0` and the second bound is refused.
#[test]
fn a_metavariable_met_at_a_type_from_below_takes_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::type_ground();
    let large = Term::type_at(Level::constant(1));
    assert!(matches!(
        subsume_outcome(&mut context, &small, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &large, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(
        context
            .finalize_universe_metas(BTreeSet::new(), BTreeSet::new(), BTreeSet::new())
            .is_ok()
    );
}

// Control: the same two bounds with the first an equation.
#[test]
fn a_metavariable_met_at_a_type_as_an_equation_is_that_type() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::type_ground();
    let large = Term::type_at(Level::constant(1));
    assert_eq!(conv(&mut context, &small, &Term::hole(0)), Ok(true));
    let second = subsume_outcome(&mut context, &large, &Term::hole(0), None);
    let closed = context.finalize_universe_metas(BTreeSet::new(), BTreeSet::new(), BTreeSet::new());
    assert!(
        !matches!(second, Ok(Outcome::Converts)) || closed.is_err(),
        "{second:?} {closed:?}"
    );
}

// The metavariable below: `?0 ≤ Type 1` solves `?0` to a `Type` at a level of its own under one, so `?0 ≤ Type 0` still holds.
#[test]
fn a_metavariable_met_at_a_type_from_above_takes_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::type_ground();
    let large = Term::type_at(Level::constant(1));
    assert!(matches!(
        subsume_outcome(&mut context, &Term::hole(0), &large, None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &Term::hole(0), &small, None),
        Ok(Outcome::Converts)
    ));
    assert!(
        context
            .finalize_universe_metas(BTreeSet::new(), BTreeSet::new(), BTreeSet::new())
            .is_ok()
    );
}

// A function type's codomain is the position: `(x: Nat) -> Type 0 ≤ ?0` then `(x: Nat) -> Type 1 ≤ ?0`.
#[test]
fn a_function_type_met_from_below_takes_its_codomain_at_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
    let family = |context: &mut Context, level: u32| {
        Term::func_type(
            [(context.fresh(Some("x")), nat_type())],
            Term::type_at(Level::constant(level)),
        )
    };

    let small = family(&mut context, 0);
    let large = family(&mut context, 1);
    assert!(matches!(
        subsume_outcome(&mut context, &small, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &large, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(
        context
            .finalize_universe_metas(BTreeSet::new(), BTreeSet::new(), BTreeSet::new())
            .is_ok()
    );
}

// A metavariable met at `Prop` from below is a `Prop`, at a level of its own over the one it met, so a larger proposition met after it is below it too and the levels close.
#[test]
fn a_metavariable_met_at_a_proposition_from_below_is_a_proposition_at_a_level_of_its_own() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));

    let small = Term::prop();
    let large = Term::prop_at(Level::constant(1));
    assert!(matches!(
        subsume_outcome(&mut context, &small, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(matches!(
        context
            .metavar_solution(MetavarId(0))
            .map(|solution| &**solution),
        Some(Subterm::Sort(Quality::Prop, _))
    ));
    assert!(matches!(
        subsume_outcome(&mut context, &large, &Term::hole(0), None),
        Ok(Outcome::Converts)
    ));
    assert!(
        context
            .finalize_universe_metas(BTreeSet::new(), BTreeSet::new(), BTreeSet::new())
            .is_ok()
    );
}

// No proposition stands where a type is wanted, so a `Type` met after the proposition is refused, and so is the proposition met after a `Type`: the metavariable is a sort of the kind it met first.
#[test]
fn a_type_and_a_proposition_met_from_below_are_refused_in_either_order() {
    let met = |first: Term, second: Term| {
        let mut context = context();
        context.birth_metavar(MetavarId(0), Vec::new(), Term::type_at(Level::constant(2)));
        assert!(matches!(
            subsume_outcome(&mut context, &first, &Term::hole(0), None),
            Ok(Outcome::Converts)
        ));

        subsume_outcome(&mut context, &second, &Term::hole(0), None)
    };

    assert!(matches!(
        met(Term::prop(), Term::type_ground()),
        Ok(Outcome::Mismatch(_))
    ));
    assert!(matches!(
        met(Term::type_ground(), Term::prop()),
        Ok(Outcome::Mismatch(_))
    ));
}
